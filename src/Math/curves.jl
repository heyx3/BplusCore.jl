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


#########################################
##    CurvePerlin

"
Configures the organic noise added to a `Curve{T}`.

The noise can be configured to fade in at the start of the curve;
  if the curve does not loop then it also symetrically fades out at the end.

The `scale` is in `t` units, for example a scale of 2 means there are new gradients at `t=0,2,4,...`.

The default constructor turns off the effect by setting strength to 0.
"
struct CurvePerlin
    strength::Float32
    scale::Float32
    fade_duration::Float32
    fade_exponent::Float32

    CurvePerlin(strength=0, scale=0.4,
                fade_duration = 0.3, fade_exponent = 1.3) = new(strength, scale,
                                                                fade_duration, fade_exponent)
end

function curve_perlin_strength(t::Float32, t_start::Float32, t_end::Float32,
                               settings::CurvePerlin, looping::Bool)
    t_edge_offset = if looping
        abs(t - t_start)
    else
        min(t - t_start, t_end - t)
    end
    window_strength = if settings.fade_duration == 0
        1.0f0
    else
        saturate(inv_lerp(0.0f0, settings.fade_duration, t_edge_offset)) ^
          settings.fade_exponent
    end

    return window_strength * settings.strength
end

curve_perlin_sanitize(p::CurvePerlin)::CurvePerlin = CurvePerlin(
    max(0.0f0, p.strength),
    max(0.00000000000000000001f0, p.scale),
    max(0.0f0, p.fade_duration),
    max(0.0f0, p.fade_exponent)
)


############################################
##    Curve{T}

"
A timed sequence of some `lerp()`-able data.
Perlin noise can be injected into the time coordinate to make the curve organic and varied each time.

It can be constructed with a list of keyframes or with a sequence of individual keys.

When modifying the keyframes, it's recommended to call `curve_sanitize!` afterwards
   to ensure cached data is recomputed.
"
mutable struct Curve{T}
    keyframes::Vector{CurveKey{T}}
    perlin_settings::CurvePerlin

    Curve(keyframes_to_be_copied::AbstractVector{CurveKey{T}}, perlin_settings = CurvePerlin()) where {T} =
        let c = new{T}(collect(keyframes_to_be_copied), perlin_settings)
            curve_sanitize!(c)
            c
        end
    Curve{T}(keyframes_to_be_copied::AbstractVector{<:CurveKey}, perlin_settings = CurvePerlin()) where {T} =
        let c = new{T}(collect(CurveKey{T}, keyframes_to_be_copied), perlin_settings)
            curve_sanitize!(c)
            c
        end

    Curve(keys::CurveKey{T}...; perlin_settings = CurvePerlin()) where {T} =
        let c = new{T}([ keys... ], perlin_settings)
            curve_sanitize!(c)
            c
        end
    Curve{T}(keys::CurveKey...; perlin_settings = CurvePerlin()) where {T} =
        let c = new{T}([ (convert(CurveKey{T}, k) for k in keys)... ], perlin_settings)
            curve_sanitize!(c)
            c
        end
end

function Base.print(io::IO, c::Curve)
    print(io, typeof(c), "(")

    if !iszero(c.perlin_settings.strength)
        print(io, "perlin=", c.perlin_settings.strength, " ")
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
    c.perlin_settings = curve_perlin_sanitize(c.perlin_settings)

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
Optionally loops the `t` input around the curve's time range.

If using perlin noise for organic randomization,
  feed a tuple of RNG seeds in to pick a specific randomized version of the curve.
"
function curve_eval(c::Curve{T}, _t,
                    perlin_seeds::Tuple = (0x1, )
                    ;
                    looping::Bool = false
                   )::T where {T}
    # Edge-case: no keyframes to interpolate.
    if isempty(c.keyframes)
        return curve_default_value(T)
    elseif length(c.keyframes) == 1
        return c.keyframes[1].point[2]
    end

    # Sanitize and process 't'.
    t = convert(Float32, _t)
    if c.perlin_settings.strength != 0
        p = perlin(t / c.perlin_settings.scale, perlin_seeds)
        offset = (2.0f0 * p) - 1.0f0
        t += offset * curve_perlin_strength(t,
                                            c.keyframes[begin].point[1],
                                            c.keyframes[end].point[1],
                                            c.perlin_settings, looping)
    end
    if looping
        t = wraparound(c.keyframes[begin].point[1],
                       c.keyframes[end].point[1],
                       t)
    end

    # Find the relevant keyframes.
    t_keyframe = CurveKey{T}(t, curve_default_value(T), CurveLinearSlope())
    idx_of_prev = searchsortedlast(c.keyframes, t_keyframe, by=(k->k.point[1]))
    idx_of_next = idx_of_prev + 1
    if looping
        if idx_of_prev == length(c.keyframes)
            idx_of_next = 1
        elseif idx_of_prev == 0
            idx_of_prev = length(c.keyframes)
        end
    else
        if idx_of_prev == length(c.keyframes)
            return c.keyframes[end].point[2]
        elseif idx_of_prev == 0
            return c.keyframes[begin].point[2]
        end
    end

    # Interpolate.
    return curve_slope_eval(c.keyframes[idx_of_prev].slope_to_next,
                            c.keyframes[idx_of_prev].point,
                            c.keyframes[idx_of_next].point,
                            t)
end


##############################################

export CurveSlope, CurveLinearSlope, CurveExponentialSlope, CurveBezierSlope,
       CurveKey, CurvePerlin, Curve,
       curve_sanitize!, curve_eval