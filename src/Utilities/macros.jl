##  FunctionMetadata  ##

"
Information that can be part of a function definition but isn't handled by MacroTools.
For example, `@inline`, `@generated`, and doc-strings.

Turn this back into code with `MacroTools.combinedef(::FunctionMetadata)`.
"
struct FunctionMetadata
    doc_string::Optional{AbstractString}
    inline::Bool
    generated::Bool
    core_expr # The actual function definition with everything stripped out
end

"
Returns `nothing` if the expression doesn't look like a valid function definition
    (i.e. it's wrapped by an unexpected macro, or not even a function in the first place).

Pass false for `check_function_grammar` to ignore the actual function inside the macros.
"
function FunctionMetadata(expr, check_function_grammar::Bool = true)::Optional{FunctionMetadata}
    if @capture expr (@name_ args__)
        if name == GlobalRef(Core, Symbol("@doc"))
            doc_string = args[1]
            inner = FunctionMetadata(args[2])
            return FunctionMetadata(doc_string, inner.inline, inner.generated, inner.core_expr)
        elseif name == Symbol("@inline")
            inner = FunctionMetadata(args[1])
            return FunctionMetadata(inner.doc_string, true, inner.generated, inner.core_expr)
        elseif name == Symbol("@generated")
            inner = FunctionMetadata(args[1])
            return FunctionMetadata(inner.doc_string, inner.inline, true, inner.core_expr)
        else
            return nothing
        end
    #NOTE: this snippet comes from MacroTools source; unfortunately it isn't provided explicitly
    elseif !check_function_grammar || @capture(longdef(expr), function (fcall_ | fcall_) body_ end)
        return FunctionMetadata(nothing, false, false, expr)
    else
        return nothing
    end
end

export FunctionMetadata

"Converts a `FunctionMetadata` to the original Expr it came from"
function MacroTools.combinedef(m::FunctionMetadata)
    output = m.core_expr
    if m.generated
        output = :( @generated $output )
    end
    if m.inline
        output = :( @inline $output )
    end
    if exists(m.doc_string)
        output = :( Core.@doc($(m.doc_string), $output) )
    end
    return output
end


##  Functions  ##

"
Checks whether an expression contains valid function metadata
  (doc-string, `@inline`, etc., or several at once, or none of them)
"
function_wrapping_is_valid(expr)::Bool = exists(FunctionMetadata(expr))
export function_wrapping_is_valid


"
Checks that an expression is a Symbol, possibly nested within `.` operators.
For example, `Base.empty`.

Optionally allows type parameters at the end of the expression, like `:( A.B{C} )`.
"
function is_scopable_name(expr, allow_type_params::Bool = false)::Bool
    if isexpr(expr, :curly)
        if allow_type_params
            return is_scopable_name(expr.args[1])
        else
            return false
        end
    elseif expr isa Symbol
        return true
    else
        return isexpr(expr, :.) && (expr.args[2] isa QuoteNode)
    end
end

"Gets whether an expression looks like a function call"
function is_function_call(expr)::Bool
    return @capture(expr, (f_(i__)) |
                          (f_(i__; j__)) |
                          (f_(i__)::R_) |
                          (f_(i__; j__)::R_) |
                          (f_(i__) where {T__}) |
                          (f_(i__; j__) where {T__}) |
                          (f_(i__)::R_ where {T__}) |
                          (f_(i__; j__)::R_ where {T__})) &&
           is_scopable_name(f)
end

"
Checks if an expression is a short-form function declaration (like `f() = 5`).
Note that MacroTools' version of this function accepts things that are not actually functions.

If `check_components` is true, then the checks become a little stricter,
    such as checking that the function name is a valid expression.
"
function is_short_function_decl(expr)::Bool
    # Peel off the metadata.
    metadata = FunctionMetadata(expr)
    if isnothing(metadata)
        return false
    end
    expr = metadata.core_expr

    # Check that the grammar is correct.
    #TODO: Support operator-style declarations, such as (a::T + b::T) = T(a.i + b.i)
    if !isexpr(expr, :(=)) || !is_function_call(expr.args[1])
        return false
    end

    return isexpr(expr, :(=)) && is_function_call(expr.args[1])
end

"
Checks if an expression is a valid function declaration.
Note that MacroTools' version of this function is overly permissive.
"
is_function_decl(expr)::Bool = is_short_function_decl(shortdef(expr))

is_macro_invocation(expr) = isexpr(expr, :macrocall)

export is_scopable_name, is_function_call,
       is_short_function_decl, is_function_decl,
       is_macro_invocation


"Deep-copies an expression AST, except for things that should not be copied like literal modules"
expr_deepcopy(ast) = MacroTools.postwalk(ast) do e
    if e isa Union{Module, GlobalRef, String, UnionAll, Type}
        e
    elseif e isa Expr
        Expr(e.head, expr_deepcopy.(e.args)...)
    else
        deepcopy(e)
    end
end
export expr_deepcopy


#######################

"
Some data representation of a particular kind of expression.

Turn it back into the original expression with `combine_expr(e)`.
"
abstract type AbstractSplitExpr end
combine_expr(a::AbstractSplitExpr) = error("combine_expr(::", typeof(a), ") not implemented")

export AbstractSplitExpr, combine_expr


##  SplitArg  ##

"
A data represenation of an argument declaration (a.k.a. the output of `MacroTools.splitarg()`).
Also handles the whole argument declaration being wrapped in an `esc()`, which `splitarg()` does not.
"
mutable struct SplitArg <: AbstractSplitExpr
    name # Almost always a Symbol, but technically could be other syntax structures (like 'esc()').
    type # Usually a Symbol, or dot expression like `Random.AbstractRNG`.
         # If no type was given, or the type was ':Any', this will be set to 'nothing'.
    is_splat::Bool # Does it end in a '...'?
    default_value::Optional # `nothing` if not given.
                            # Note that the actual default value `nothing` will show up here as `:nothing`.
                            # But if you interpolated a literal `nothing` into the default value expression,
                            #    it will get mistaken here for "no default value".
    is_escaped::Bool # If true, the whole thing was wrapped in an 'esc()'.

    function SplitArg(expr)
        val = if isexpr(expr, :escape)
            new(splitarg(expr.args[1])..., true)
        else
            new(splitarg(expr)..., false)
        end
        if (val.type == :Any)
            val.type = nothing
        end
        return val
    end
    SplitArg(src::SplitArg) = new(expr_deepcopy(src.name), expr_deepcopy(src.type),
                                  src.is_splat, expr_deepcopy(src.default_value),
                                  src.is_escaped)
    SplitArg(name, type, is_splat, default_value, is_escaped) = new(name, type, is_splat, default_value, is_escaped)
end

function MacroTools.combinearg(a::SplitArg)
    # MacroTools treats '::Any' and no typing as the same,
    #    so I'm ignoring their implementation.
    raw_expr =
        if exists(a.type)
            if exists(a.default_value)
                if a.is_splat
                    :( $(a.name)::$(a.type)... = $(a.default_value) )
                else
                    :( $(a.name)::$(a.type) = $(a.default_value) )
                end
            else
                if a.is_splat
                    :( $(a.name)::$(a.type)... )
                else
                    :( $(a.name)::$(a.type) )
                end
            end
        else
            if exists(a.default_value)
                if a.is_splat
                    :( $(a.name)... = $(a.default_value) )
                else
                    :( $(a.name) = $(a.default_value) )
                end
            else
                if a.is_splat
                    :( $(a.name)... )
                else
                    a.name
                end
            end
        end
    return a.is_escaped ? esc(raw_expr) : raw_expr
end
MacroTools.combinearg(a::SplitArg) = let inner_expr = combinearg(a.name, a.type, a.is_splat, a.default_value)
    if a.is_escaped
        return esc(inner_expr)
    else
        return inner_expr
    end
end
combine_expr(a::SplitArg) = MacroTools.combinearg(a)

export SplitArg


##  SplitType  ##

"
A data representation of a type declaration, such as `C{R, T<:Integer} <: B`.
Analogous to `SplitDef` and `SplitArg`.

The constructor returns `nothing` if the expression isn't a macro invocation.

If you want to skip type checking (such as the name being a Symbol),
    pass `false` in the constructor.
"
mutable struct SplitType <: AbstractSplitExpr
    name # Must be a Symbol in 'strict' mode
    type_params::Vector # Elements must be Symbol or 'T<:expr' in 'strict' mode.
                        # Usually the expr will be a scoped name, but technically could be any expression.
    parent::Optional # Usually a scoped name (e.x. 'A' or 'M1.M2.A'),
                     #    but technically could be any expression.
    is_escaped::Bool

    function SplitType(expr, strict_mode::Bool = true)
        is_escaped = isexpr(expr, :escape)
        if is_escaped
            expr = expr.args[1]
        end

        local output::SplitType
        if @capture(expr, n_{t__} <: b_)
            output = new(n, t, b, is_escaped)
        elseif @capture(expr, n_{t__})
            output = new(n, t, b, is_escaped)
        elseif @capture(expr, n_<:b_)
            output = new(n, [], b, is_escaped)
        else
            output = new(expr, [], nothing, is_escaped)
        end

        if strict_mode
            if !isa(output.name, Symbol)
                return nothing
            end
            for tt in output.type_params
                if !(tt isa Symbol) &&
                   (!isexpr(tt, :<:) || (length(tt.args) != 2) || !(tt.args[1] isa Symbol))
                #begin
                    return nothing
                end
            end
            if !(output.parent isa Union{Nothing, Symbol, Expr})
                return nothing
            end
        end

        return output
    end
    SplitType(name, type_params, parent, is_escaped) = new(name, type_params, parent, is_escaped)
end

function combinetype(st::SplitType)
    raw_expr = 
        if isempty(st.type_params)
            if isnothing(st.parent)
                st.name
            else
                :( $(st.name) <: $(st.parent) )
            end
        else
            if isnothing(st.parent)
                :( $(st.name){$(st.type_params...)} )
            else
                :( $(st.name){$(st.type_params...)} <: $(st.parent) )
            end
        end
    return st.is_escaped ? esc(raw_expr) : raw_expr
end
combine_expr(st::SplitType) = combinetype(st)

export SplitType, combinetype


##  SplitDef  ##

"
A data representation of the output of `MacroTools.splitdef()`,
    plus the ability to recognize meta-data like doc-strings and `@inline`.

For convenience, it can also represent function signatures (i.e. calls),
    by setting the body to `nothing` (not to be confused with `:nothing`).
"
mutable struct SplitDef <: AbstractSplitExpr
    name::Optional # `nothing` if this is a lambda
    args::Vector{SplitArg}
    kw_args::Vector{SplitArg}
    body
    return_type::Optional # `nothing` if not given
    where_params::Vector{SplitType}
    doc_string::Optional{AbstractString}
    inline::Bool
    generated::Bool
    is_escaped::Bool

    function SplitDef(expr)
        is_escaped::Bool = isexpr(expr, :escape)
        if is_escaped
            expr = expr.args[1]
        end

        metadata = FunctionMetadata(expr, false)
        if isnothing(metadata)
            error("Invalid function declaration syntax: ", expr)
        end
        expr = metadata.core_expr

        # If it's just a function call, give it a Nothing body.
        if !isexpr(expr, :->) && !MacroTools.isshortdef(shortdef(expr))
            expr = :( $expr = $nothing )
        end

        dict = splitdef(expr)
        return new(
            get(dict, :name, nothing),
            SplitArg.(dict[:args]),
            SplitArg.(dict[:kwargs]),
            dict[:body],
            get(dict, :rtype, nothing),
            [SplitType(w) for w in dict[:whereparams]],
            metadata.doc_string, metadata.inline, metadata.generated,
            is_escaped
        )
    end

    # Deep-copying the function body could get crazy expensive, so avoid if if you don't need it.
    SplitDef(s::SplitDef, copy_body::Bool = true) = new(
        expr_deepcopy(s.name),
        SplitArg.(s.args),
        SplitArg.(s.kw_args),
        copy_body ? expr_deepcopy(s.body) : s.body,
        expr_deepcopy(s.return_type),
        SplitType.(s.where_params),
        expr_deepcopy(s.doc_string),
        s.inline, s.generated, s.is_escaped
    )

    SplitDef(name, args, kw_args, body, return_type, where_params, doc_string, inline, generated) = new(
        name, args, kw_args, body, return_type, where_params, doc_string, inline, generated 
    )
end
function MacroTools.combinedef(struct_representation::SplitDef)
    definition = combinedef(Dict(
        @optional(exists(struct_representation.name),
                  :name => struct_representation.name),
        :args => combinearg.(struct_representation.args),
        :kwargs => combinearg.(struct_representation.kw_args),
        :body => struct_representation.body,
        :whereparams => combinetype.(struct_representation.where_params),
        @optional(exists(struct_representation.return_type),
                  :rtype => struct_representation.return_type)
    ))
    raw_expr = combinedef(FunctionMetadata(
        struct_representation.doc_string,
        struct_representation.inline,
        struct_representation.generated,
        definition
    ))
    return struct_representation.is_escaped ? esc(raw_expr) : raw_expr
end

"Like `MacroTools.combinedef()`, but emits a function call instead of a definition"
function combinecall(struct_representation::SplitDef)
    @bp_check(exists(struct_representation.name),
              "A function call/signature must have a name")

    # Ordered and unordered parameters look identical in the AST.
    # For ordered parameters, we want to replace `a::T = v` with `a`.
    # For named parameters, we want to replace `a::T = v` with `a=a`.
    args = map(a -> a.is_splat ? :( $(a.name)... ) : a.name,
               struct_representation.args)
    kw_args = map(a -> a.is_splat ? :( $(a.name)... ) : Expr(:kw, a.name, a.name),
                  struct_representation.kw_args)

    raw_expr = Expr(:call,
        struct_representation.name,
        @optional(!isempty(kw_args), Expr(:parameters, kw_args...)),
        args...
    )
    return struct_representation.is_escaped ? esc(raw_expr) : raw_expr
end

function combine_expr(sd::SplitDef)
    if isnothing(sd.body)
        return combinecall(sd)
    else
        return MacroTools.combinedef(sd)
    end
end

export SplitDef, combinecall


##  SplitMacro  ##

"
A data representation of a macro invocation, analogous to `SplitDef` and `SplitArg`.
The constructor returns `nothing` if the expression isn't a macro invocation.

Turn this struct back into a macro call with `combinemacro()`.
"
mutable struct SplitMacro <: AbstractSplitExpr
    name::Symbol
    source::LineNumberNode
    args::Vector

    function SplitMacro(expr)
        if isexpr(expr, :macrocall)
            return new(
                expr.args[1],
                expr.args[2],
                expr.args[3:end]
            )
        else
            return nothing
        end
    end
    SplitMacro(src::SplitMacro, deepcopy_args::Bool = true) = new(
        expr_deepcopy(src.name),
        expr_deepcopy(src.source),
        map(a -> deepcopy_args ? expr_deepcopy(a) : a,
            src.args)
    )
    SplitMacro(name, source, args) = new(name, source, args)
end

"Turns the data representaton of a macro call into an AST"
function combinemacro(m::SplitMacro)
    return Expr(:macrocall,
        m.name,
        m.source,
        m.args...
    )
end

export SplitMacro, combinemacro


##  Assignment operators  ##

"Maps a modifying assignment operator (like `*=`) to its underlying operator (like `*`)"
const ASSIGNMENT_INNER_OP = Dict(
    :+= => :+,
    :-= => :-,
    :*= => :*,
    :/= => :/,
    :^= => :^,
    :÷= => :÷,
    :%= => :%,

    :|= => :|,
    :&= => :&,

    :⊻= => :⊻,
    :<<= => :<<,
    :>>= => :>>,
)
"Converts an operator (like `*`) to its assignment operation (like `*=`)"
const ASSIGNMENT_WITH_OP = Dict(v => k for (k,v) in ASSIGNMENT_INNER_OP)
export ASSIGNMENT_INNER_OP, ASSIGNMENT_WITH_OP

"
Computes one of the modifying assignments (`*=`, `&=`, etc) given it and its inputs.
Also implements `=` for completeness.
"
@inline compute_op(s::Symbol, a, b) = compute_op(Val(s), a, b)
@inline compute_op(::Val{:(=)}, a, b) = b
for (name, op) in ASSIGNMENT_INNER_OP
    @eval @inline compute_op(::Val{Symbol($(string(name)))}, a, b) = $op(a, b)
end
export compute_op