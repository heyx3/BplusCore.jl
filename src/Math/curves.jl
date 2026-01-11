# Note: anything that wants to be the T in Curve{T} must support the below interface;
#    a good reference example is Quaternion's specialization.

const CurveKeyframeCoord{T} = Tuple{Float32, T}

"Linearly interpolates between two curve values"
curve_coord_lerp(a::T, b::T, t::Float32) where {T} = lerp(a, b, t)
curve_coord_lerp(a::Quaternion{F}, b::Quaternion{F}, t::Float32) where {F} = q_slerp(a, b, t)

"Pretty-prints a coordinate value"
curve_print(io::IO, x) = print(io, x)


######################################
##    Slopes

"
Interpolates between `a` and `b` using `t` (guaranteed to sit between them),
    following the definition of this slope.
"
curve_slope_eval(slope,
                 a::CurveKeyframeCoord, b::CurveKeyframeCoord,
                 t::Float32
                )::Float32 = error("Unhandled: ", typeof(s))

"A linear ramp from the previous keyframe to the next"
struct CurveLinearSlope end
function curve_slope_eval(::CurveLinearSlope,
                          a::CurveKeyframeCoord{T}, b::CurveKeyframeCoord{T},
                          t::Float32) where {T}
    return curve_coord_lerp(a[2], b[2],
                            inv_lerp(a[1], b[1], t))
end
curve_print(io::IO, ::CurveLinearSlope) = print(io, "linear")

"
An exponential curve transitioning from the previous keyframe to the next
  (>1 for slow start and fast end; 0-1 for fast start and slow end).
"
struct CurveExponentialSlope
    exponent::Float32
end
function curve_slope_eval(e::CurveExponentialSlope,
                          a::CurveKeyframeCoord{T}, b::CurveKeyframeCoord{T},
                          t::Float32) where {T}
    return curve_coord_lerp(a[2], b[2],
                            inv_lerp(a[1], b[1], t) ^ e.exponent)
end
curve_print(io::IO, e::CurveExponentialSlope) = print(io, "pow(", e.exponent, ")")

"
A 1D cubic bezier curve from the previous keyframe to the next,
  pushing and pulling the `t` coordinate.
Each control point is given as an offset from the start or end keyframe.
"
struct CurveBezierSlope
    first_control_offset::Float32
    second_control_offset::Float32
end
function curve_slope_eval(bez::CurveBezierSlope,
                          a::CurveKeyframeCoord{T}, b::CurveKeyframeCoord{T},
                          t::Float32) where {T}
    t_local = inv_lerp(a[1], b[1], t)

    bezier_t_points = (
        a[1], a[1] + bez.first_control_offset,
        b[1] + bez.second_control_offset, b[1]
    )
    t_inv = 1.0f0 - t_local
    t_bezier = (bezier_t_points[1] * t_inv * t_inv * t_inv) +
               (bezier_t_points[2] * 3.0f0 * t_inv * t_inv * t_local) +
               (bezier_t_points[3] * 3.0f0 * t_inv * t_local * t_local) +
               (bezier_t_points[4] * t_local * t_local * t_local)

    return curve_coord_lerp(a[2], b[2], inv_lerp(a[1], b[1], t_bezier))
end
curve_print(io::IO, b::CurveBezierSlope) = print(io,
    "bezier(a", (b.first_control_offset>=0) ? "+" : "", b.first_control_offset,
            ", b", (b.second_control_offset>=0) ? "+" : "", b.second_control_offset,
            ")"
)

const CurveSlope = Union{CurveLinearSlope, CurveExponentialSlope, CurveBezierSlope}



##########################################
##    Keyframes

"
One key-frame of a `Curve{T}` (where `T` is some `lerp()`-able data)
   and the slope towards the next point.

When constructing an instance, the slope defaults to linear
   and you may pass the individual components of `point` or pass them as one tuple.

When constructing a key for a float vector type `Vec{N, F}`,
   you may also insert the time coordinate as the first axis (`Vec{N+1, F}`).
"
struct CurveKey{T}
    point::CurveKeyframeCoord{T}
    slope_to_next::CurveSlope

    CurveKey(t::Real, value, slope_to_next = CurveLinearSlope()) =
        new{typeof(value)}((t, value), slope_to_next)
    CurveKey{T}(t::Real, value, slope_to_next = CurveLinearSlope()) where {T} =
        new{T}((t, convert(T, value)), slope_to_next)

    CurveKey(point::CurveKeyframeCoord{T}, slope_to_next = CurveLinearSlope()) where {T} =
        new{T}(point, slope_to_next)
    CurveKey{T}(point::CurveKeyframeCoord, slope_to_next = CurveLinearSlope()) where {T} =
        new{T}((point[1], convert(T, point[2])), slope_to_next)
end
Base.convert(T, k::CurveKey{T2}) where {T2} = CurveKey{T}((k.point[1], convert(T, k.point[2])), k.slope_to_next)

function CurveKey{F}(v::Vec{2, F2}, slope_to_next = CurveLinearSlope()) where {F<:Real, F2}
    return CurveKey{F}(convert(Float32, v.x),
                       convert(F, v.y),
                       slope_to_next)
end
function CurveKey{Vec{N, F}}(v::Vec{M, F2}, slope_to_next = CurveLinearSlope()) where {N, M, F, F2}
    if M != N+1
        error("Tried constructing a ", N+1, "D curve with a ", M, "D vector")
    else
        return CurveKey{Vec{N, F}}(
            convert(Float32, v.x),
            convert(Vec{N, F}, v[2:end]),
            slope_to_next
        )
    end
end
function CurveKey(v::Vec{N, F}, slope_to_next = CurveLinearSlope()) where {N, F}
    if N < 2
        error("Curve point must be at least 2D; got ", N, "D")
    else
        return CurveKey{Vec{N-1, F}}(
            convert(Float32, v.x),
            v[2:end],
            slope_to_next
        )
    end
end
function CurveKey(v::Vec{2, F}, slope_to_next = CurveLinearSlope()) where {F}
    return CurveKey{F}(convert(Float32, v.x),
                       v.y,
                       slope_to_next)
end

function curve_print(io::IO, k::CurveKey{T}) where {T}
    compact_to_vec = isa(T, Vec)
    print_slope = !isa(k.slope_to_next, CurveLinearSlope)

    if compact_to_vec && !print_slope
        print(io, Vec(k.point[1], k.point[2]...))
    else
        print(io, "< ")

        if compact_to_vec
            print(io, Vec(k.point[1], k.point[2]...))
        else
            print(io, k.point[1], "|")
            curve_print(io, k.point[2])
        end

        if print_slope
            print(io, " ")
            curve_print(io, k.slope_to_next)
        end

        print(io, " >")
    end

    return nothing
end

"
A timed sequence of some `lerp()`-able data.
Perlin noise can be injected into the time coordinate to make the curve organic and varied each time.

It can be constructed with a list of keyframes or with a sequence of individual keys.

When modifying the keyframes, it's recommended to call `curve_sanitize!` afterwards
   to ensure cached data is recomputed.
"
mutable struct Curve{T}
    keyframes::Vector{CurveKey{T}}
    perlin_strength::Float32
    #TODO: perlin window

    Curve(keyframes_to_be_copied::Vector{CurveKey{T}}, perlin_strength = 0.0) where {T} =
        let c = new{T}(copy(keyframes_to_be_copied), perlin_strength)
            curve_sanitize!(c)
            c
        end
    Curve{T}(keyframes_to_be_copied::Vector{<:CurveKey}, perlin_strength = 0.0) where {T} =
        let c = new{T}(collect(CurveKey{T}, keyframes_to_be_copied))
            curve_sanitize!(c)
            c
        end

    Curve(keys::CurveKey{T}...; perlin_strength::Float32 = 0.0f0) where {T} =
        let c = new{T}([ keys... ], perlin_strength)
            curve_sanitize!(c)
            c
        end
    Curve{T}(keys::CurveKey...; perlin_strength::Float32 = 0.0f0) where {T} =
        let c = new{T}([ (convert(CurveKey{T}, k) for k in keys)... ], perlin_strength)
            curve_sanitize!(c)
            c
        end
end

function Base.print(io::IO, c::Curve)
    print(io, typeof(c), "(")

    if !iszero(c.perlin_strength)
        print(io, "perlin=", c.perlin_strength, " ")
    end

    print(io, "[ ")
    for (i, k) in enumerate(c.keyframes)
        curve_print(io, k)
        if i > 1
            print(io, ",")
        end
        print(io, " ")
    end
    print(io, "]")

    print(io, ")")
end

"If the given curve is malformed, quietly fixes its elements"
function curve_sanitize!(c::Curve)
    # Move keys around so their times are sorted.
    @inline sort!(c.keyframes,
        by=(k -> k.point[1]),
        # Make sure keys with the same time are not re-ordered.
        alg=Base.DEFAULT_STABLE
    )

    # For keys with duplicate times, push one a bit past the other.
    # Whenever we do push it may intersect with neighboring keyframes,
    #    so we need to handle that too.
    for i in 2:length(c.keyframes)
        if c.keyframes[i-1].point[1] >= c.keyframes[i].point[1]
            c.keyframes[i] = let k = c.keyframes[i]
                @set! k.point[1] = nextfloat(c.keyframes[i-1].point[1])
                k
            end
        end
    end

    return nothing
end

"Provides a default for the given curve type, mainly used when a curve has no keyframes"
function curve_default_value(::Type{T})::T where {T}
    return zero(T)
end
curve_default_value(T::Type{<:Quaternion}) = T()

"
Evaluates the curve at the given time.
If the curve type `T` doesn't define `zero(T)`, then you must provide
"
function curve_eval(c::Curve{T}, _t,
                    perlin_seeds::Tuple = (0x1, )
                   )::T where {T}
    # Edge-case: no keyframes to interpolate.
    if isempty(c.keyframes)
        return curve_default_value(T)
    end

    # Sanitize and process 't'.
    t = convert(Float32, _t)
    if c.perlin_strength != 0
        offset = lerp(-c.perlin_strength, c.perlin_strength,
                      perlin(t, perlin_seeds))
        t += offset
    end

    # Find the relevant keyframes.
    t_keyframe = CurveKey{T}(t, curve_default_value(T), CurveLinearSlope())
    idx_of_prev = searchsortedlast(c.keyframes, t_keyframe, by=(k->k.point[1]))
    if idx_of_prev == length(c.keyframes)
        return c.keyframes[end].point[2]
    elseif idx_of_prev == 0
        return c.keyframes[begin].point[2]
    end

    # Interpolate.
    return curve_slope_eval(c.keyframes[idx_of_prev].slope_to_next,
                            c.keyframes[idx_of_prev].point,
                            c.keyframes[idx_of_prev+1].point,
                            t)
end

println("#TODO: Add curve-editor to GUI module")


export CurveSlope, CurveLinearSlope, CurveExponentialSlope, CurveBezierSlope,
       CurveKey, Curve,
       curve_sanitize!, curve_eval