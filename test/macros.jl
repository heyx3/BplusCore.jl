# Test SplitArg:
for (input, expected_field_values) in [  (:a, (
                                             :a,
                                             nothing,
                                             false,
                                             nothing,
                                             false
                                         )),
                                         (:( i=7 ), (
                                             :i,
                                             nothing,
                                             false,
                                             7,
                                             false
                                         )),
                                         (:( i::Int ), (
                                             :i,
                                             :Int,
                                             false,
                                             nothing,
                                             false
                                         )),
                                         (:( $(esc(:i))::Int = 10 ), (
                                             esc(:i),
                                             :Int,
                                             false,
                                             10,
                                             false
                                         )),
                                         (esc(:( i::Int... )), (
                                             :i,
                                             :Int,
                                             true,
                                             nothing,
                                             true
                                         ))
                                      ]
    split = SplitArg(input)
    for (field_name, field_expected_value) in zip(fieldnames(SplitArg), expected_field_values)
        @bp_check(getfield(split, field_name) == field_expected_value,
                  "In arg expression '$input', expected SplitArg.$field_name ",
                    "to be $field_expected_value, but it was ", getfield(split, field_name))
    end
end

# Test SplitDef:
for (input, expected) in [ (
                               :( f() = 5 ),
                               (:f, [ ], [ ], 5, nothing, [], nothing, false)
                           ),
                           (
                               :( (def, ghi::Int) -> def*ghi ),
                               (nothing, SplitArg.([ :def, :( ghi::Int ) ]), [ ],
                                :( def * ghi ), nothing,
                                [], nothing, false)
                           ),
                           (
                               ( quote
                                   function ghi(jkl::T; mnop, qrs=5) where {T}
                                       return jkl
                                   end
                               end ).args[2], # Get the actual :macrocall expr
                               (
                                   :ghi,
                                   SplitArg.([ :( jkl::T ) ]),
                                   SplitArg.([ :mnop, Expr(:kw, :qrs, 5) ]),
                                   :( return jkl ),
                                   nothing,
                                   SplitType.([ :T ]),
                                   nothing, false
                               )
                           ),
                           (
                               ( quote
                                   "ABCdef"
                                   @inline function ghi(jkl::T; mnop, qrs=5) where {T}
                                       return jkl
                                   end
                               end ).args[2], # Get the actual :macrocall expr
                               (
                                   :ghi,
                                   SplitArg.([ :( jkl::T ) ]),
                                   SplitArg.([ :mnop, Expr(:kw, :qrs, 5) ]),
                                   :( return jkl ),
                                   nothing,
                                   SplitType.([ :T ]),
                                   "ABCdef",
                                   true
                               )
                           ),
                           (
                               :( abc(def, ghi::Int)::Float32 ),
                               (:abc, SplitArg.([ :def, :( ghi::Int ) ]), [ ],
                                nothing, :Float32,
                                [], nothing, false)
                           )
                         ]
    input = rmlines(input)
    actual = SplitDef(input)
    field_names = fieldnames(SplitDef)
    actual_fields = getproperty.(Ref(actual), field_names)
    for (name, f_actual, f_expected) in zip(field_names, actual_fields, expected)
        f_actual = prettify(f_actual)

        # AbstractSplitExpr types are mutable, and must be compared
        #    by turning them into a tuple of their type and fields.
        streamline_value(m) = if m isa AbstractVector
            streamline_value.(m)
        elseif m isa AbstractDict
            Dict((streamline_value(k)=>streamline_value(v) for (k,v) in m)...)
        elseif ismutable(m)
            (typeof(m), getfield.(Ref(m), fieldnames(typeof(m)))...)
        else
            m
        end
        f_expected = streamline_value(f_expected)
        f_actual = streamline_value(f_actual)

        @bp_check(f_actual == f_expected,
                  "SplitDef.$name should be $f_expected, but was $f_actual; in def expression:\n",
                    "$input\n\n")
    end
end

# Test SplitMacro:
@bp_check(isnothing(SplitMacro(:( f() = 5 ))))
let sm = SplitMacro(:( @a(b, c) ))
    @bp_check(exists(sm))
    @bp_check(sm.name == Symbol("@a"), sm)
    @bp_check(sm.args == [ :b, :c ], sm)
    @bp_check(string(combinemacro(sm)) ==
                "$(sm.source) @a b c",
              "Got: ", string(combinemacro(sm)))
end

# Test SplitType:
@bp_check(isnothing(SplitType(:( f() = 5 ))))
@bp_check(isnothing(SplitType(:( A.B{C} <: D ))),
          "Qualified names shouldn't be allowed in 'strict' mode")
@bp_check(isnothing(SplitType(:( A{<:B} <: C ))),
          "Tried to fool SplitType within the type params")
let st = SplitType(:A)
    @bp_check(exists(st))
    @bp_check(st.name == :A)
    @bp_check(st.type_params == [])
    @bp_check(st.parent === nothing)
    @bp_check(combinetype(st) == :A)
end
let st = SplitType(:( A{B, C<:D} <: E() ))
    @bp_check(exists(st))
    @bp_check(st.name == :A)
    @bp_check(st.type_params == [ :B, :(C<:D) ])
    @bp_check(st.parent == :( E() ))
    @bp_check(combinetype(st) == :( A{B, C<:D} <: E() ))
end
let st = SplitType(:( A.B{23} <: D ), false)
    @bp_check(exists(st), "Non-strict mode is broken")
    @bp_check(st.name == :(A.B))
    @bp_check(st.type_params == [ 23 ])
    @bp_check(st.parent == :D)
    @bp_check(combinetype(st) == :( A.B{23} <: D ))
end

# Test combinecall():
for (input, expected) in [ (:( f(a) ), :( f(a) )),
                           (
                                :( f(a, b::Int, c=:c, d::Float64=10.5) ),
                                :( f(a, b, c, d) )
                           ),
                           (
                                :( f(; a, b::Int, c=3, d::UInt8=4) ),
                                :( f(; a=a, b=b, c=c, d=d) )
                           ),
                           (
                                :( f(a, b::Int, c=3, d::UInt8=4
                                     ;
                                     e, f::String, g=:hi, h::UInt=42) ),
                                :( f(a, b, c, d; e=e, f=f, g=g, h=h) )
                           )
                         ]
    split = SplitDef(:( $input = nothing ))
    actual = combinecall(split)
    # It's actually hard to compare two AST's, as far as I know.
    @bp_check(string(expected) == string(actual),
              "Expected ", input, " to convert to ",
                string(expected), ", but got ", string(actual))
end

# Test is_function_decl():
run_test(expr, expected=true) = @bp_check(is_function_decl(expr) == expected,
                                          expected ? "Valid" : "Invalid",
                                            " function declaration not caught by is_function_decl(): ",
                                            expr)
run_tests_1(header, valid_header=true) = begin # Test function call and then function definition
    run_test(header, false)
    run_test(:( $header = nothing ), valid_header)
end
run_tests_2(signature, valid_signature=true) = begin # Test with/without return values and type params
    run_tests_1(:( $signature ), valid_signature)
    run_tests_1(:( $signature::Int ), valid_signature)
    run_tests_1(:( $signature::T where {T} ), valid_signature)
    run_tests_1(:( $signature::T where {T<:AbstractArmAndALeg} ), valid_signature)
    run_tests_1(:( $signature where {T} ), valid_signature)
    run_tests_1(:( $signature where {T<:HelloWorld} ), valid_signature)
end
run_tests_3(name, valid_name=true) = begin # Test with various parameter combinations
    run_tests_2(:( $name() ), valid_name)
    run_tests_2(:( $name(i) ), valid_name)
    run_tests_2(:( $name(i=6) ), valid_name)
    run_tests_2(:( $name(i::Int = 6) ), valid_name)
    run_tests_2(:( $name(; j) ), valid_name)
    run_tests_2(:( $name(; j=7) ), valid_name)
    run_tests_2(:( $name(; j::Int) ), valid_name)
    run_tests_2(:( $name(; j::Int = 7) ), valid_name)
    run_tests_2(:( $name(i; j) ), valid_name)
    run_tests_2(:( $name(i; j=7) ), valid_name)
    run_tests_2(:( $name(i; j::Int) ), valid_name)
    run_tests_2(:( $name(i; j::Int = 7) ), valid_name)
    run_tests_2(:( $name(i=6; j) ), valid_name)
    run_tests_2(:( $name(i=6; j=7) ), valid_name)
    run_tests_2(:( $name(i=6; j::Int) ), valid_name)
    run_tests_2(:( $name(i=6; j::Int = 7) ), valid_name)
    run_tests_2(:( $name(i::Int; j) ), valid_name)
    run_tests_2(:( $name(i::Int; j=7) ), valid_name)
    run_tests_2(:( $name(i::Int; j::Int) ), valid_name)
    run_tests_2(:( $name(i::Int; j::Int = 7) ), valid_name)
    run_tests_2(:( $name(i::Int = 6; j) ), valid_name)
    run_tests_2(:( $name(i::Int = 6; j=7) ), valid_name)
    run_tests_2(:( $name(i::Int = 6; j::Int) ), valid_name)
    run_tests_2(:( $name(i::Int = 6; j::Int = 7) ), valid_name)
end
run_tests_3(:f)
run_tests_3(:(Base.f))
run_tests_3(:(Base.f))
run_tests_3(:(a + b), false)
run_tests_3(:(a()), false)

# Test visit_exprs:
visit_expr_test() = begin
    e = Expr(:root,
        Expr(:a,
            1,
            "2",
            Expr(:b,
                3,
                Symbol("4")
            )
        ),
        5,
        Expr(:c),
        "6"
    )
    paths = Vector{Vector{Int}}()
    exprs = Vector{Any}()
    visit_exprs(e) do path_indices, path
        push!(paths, copy(path_indices))
        push!(exprs, path[end])
    end
    @bp_check(
        paths == [
            [ ],
            [ 1 ],
            [ 1, 1 ],
            [ 1, 2 ],
            [ 1, 3 ],
            [ 1, 3, 1 ],
            [ 1, 3, 2 ],
            [ 2 ],
            [ 3 ],
            [ 4 ]
        ],
        "Actual path: ", paths
    )
    @bp_check(
        exprs == [
            e,
            e.args[1],
              e.args[1].args[1], e.args[1].args[2],
              e.args[1].args[3], e.args[1].args[3].args[1], e.args[1].args[3].args[2],
            e.args[2], e.args[3], e.args[4]
        ],
        "Actual exprs: ", exprs
    )
end; visit_expr_test()