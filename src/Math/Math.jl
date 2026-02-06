module Math

using Random
using Setfield, StaticArrays, StructTypes, NamedTupleTools
using ..Utilities

# Define @bp_math_assert.
@make_toggleable_asserts bp_math_

include("functions.jl")
include("vec.jl")
include("contiguous.jl")

include("mat.jl")
include("quat.jl")

include("ray.jl")
include("shapes/Shapes.jl")

include("transformations.jl")
include("random_distributions.jl")
include("noise.jl")
include("curves.jl")


# Precompile common math using PrecompileTools.jl.
# This doubles as a smoke test for syntax errors!
using PrecompileTools: @setup_workload, @compile_workload
@compile_workload begin
    function do_work(::Type{T}) where {T}
        ONE = one(T)
        ZERO = zero(T)

        a = square(ONE) +
            wraparound(ONE, ZERO, ZERO) + wraparound(0, 10, ONE) +
            typemin_finite(T) + typemax_finite(T)
        if !(T <: Integer)
            HALF = ONE / convert(T, 2)
            V_X = Vec(ONE, ZERO, ZERO)
            V_ZERO = Vec(ZERO, ZERO, ZERO)
            V_ONE = Vec(ONE, ONE, ONE)
            V_BAS = VBasis{T}(V_X, Vec(ZERO, ONE, ZERO), Vec(ZERO, ZERO, ONE))
            QT = Quaternion{T}
            CK = CurveKey{T}

            a += lerp(ONE, ZERO, HALF) + lerp(ONE, ZERO, 0) + lerp(ONE, ZERO, true) +
                 smoothstep(HALF) + smootherstep(HALF) +
                 inv_lerp(ONE, ZERO, HALF) + inv_lerp(0, 1, HALF) +
                 fract(HALF) + saturate(HALF) +
                 solve_quadratic(ONE, ZERO, ZERO)[1] +
                 round_up_to_multiple(HALF, ONE) + round_up_to_multiple(HALF, 1) +
                 rand_in_sphere(T).x +
                 perlin(HALF) + perlin(HALF, (zero(UInt32), )) + perlin(HALF, (zero(UInt64), ))
            # Quaternion ops:
            a += vlength(reduce(*, [
                Quaternion(ZERO, ZERO, ZERO, ONE), Quaternion(ZERO, ZERO, ZERO, 1),
                Quaternion((ZERO, ZERO, ZERO, ONE)),  Quaternion(Vec(ZERO, ZERO, ZERO, ONE)),
                QT(),
                Quaternion(V_X, HALF),  QT(V_X, HALF),
                QT(ZERO, ZERO, ZERO, ONE),  QT(ZERO, ZERO, ZERO, 1),
                QT((ZERO, ZERO, ZERO, ONE)),  QT(Vec(ZERO, ZERO, ZERO, ONE)),
                Quaternion(V_X, V_X),
                Quaternion(Vec(0, 0, 1), V_X),
                QT(Vec(0, 0, 1), V_X),
                Quaternion(m_identity(3, 3, T)),
                QT(m_identity(3, 3, T)),
                Quaternion(V_BAS),
                Quaternion(V_BAS, V_BAS),
                QT(V_BAS, V_BAS),
                Quaternion(QT(), QT()),
                Quaternion(QT(), QT(), QT()),
                ((isapprox(QT(), QT()) &&
                    q_is_normalized(QT()) &&
                    q_is_identity(QT())
                    ) ? Quaternion(V_X, V_X) : Quaternion(V_X, HALF)),
                Quaternion(q_apply(QT(), V_X)..., ONE),
                q_slerp(QT(), QT(), HALF),
                lerp(QT(), QT(), HALF),
                (QT() << QT()),
                (QT() >> QT()),
                Quaternion(q_basis(QT()))
            ]).xyzw)
            # Other quaternion math:
            a += q_axisangle(QT())[2] +
                 q_mat3x3(QT())[1] +
                 q_mat4x4(QT())[1]
            # 2D Matrix:
            a += reduce(*, [
                m_scale(Vec(HALF, ONE))
            ])[1] +
              # 3D Matrix:
              reduce(*, [
                  m3_translate(Vec(ONE, ONE)),
                  m_scale(Vec(ONE, HALF, ONE)),
                  m3_rotateX(HALF), m3_rotateY(HALF), m3_rotateZ(HALF),
                  m3_rotate(QT()),
                  m3_look_at(vbasis(V_X, Vec(ZERO, ONE, ZERO))),
                  m3_look_at(vbasis(V_X, Vec(ZERO, ONE, ZERO)),
                          vbasis(Vec(ZERO, ONE, ZERO), V_X)),
              ])[1] +
              # 4D Matrix:
              reduce(*, [
                  m4_translate(Vec(ONE, ZERO, ONE)),
                  m_scale(Vec(ONE, HALF, ONE, HALF)),
                  m4_rotateX(HALF), m4_rotateY(HALF), m4_rotateZ(HALF),
                  m4_rotate(QT()),
                  m4_world(Vec(HALF, ONE, ZERO), QT(), Vec(ONE, HALF, ONE)),
                  m4_look_at(Vec(ZERO, ZERO, ZERO), Vec(ONE, -HALF, HALF), V_X),
                  m4_projection(HALF, ONE, ONE + ONE, ONE + ONE + ONE),
                  m4_ortho(Box(min=Vec(ZERO, ZERO, ZERO), max=Vec(ONE, ONE, HALF)))
              ])[1]
            # Curves
            a += reduce(+, curve_eval.(
                Ref(Curve([
                    CurveKey{T}(0, ONE, CurveLinearSlope()),
                    CurveKey{T}(HALF * HALF, 3, CurveExponentialSlope(0.5f0)),
                    CurveKey{T}(HALF, 0.5, CurveExponentialSlope(0.5f0)),
                    CurveKey{T}(ONE, 0.0f0, CurveBezierSlope(0.2f0, -0.6f0))
                ], CurvePerlin(1.0))),
                [ -ONE, ZERO,
                  HALF * HALF * HALF,   HALF * HALF,
                  HALF,
                  ONE - (HALF * HALF),   ONE - (HALF * HALF * HALF),
                  ONE, ONE + ONE
                ],
                looping=true
            )) +
              reduce(+, curve_eval.(
                Ref(Curve([
                    CurveKey(Vec(ZERO, ONE), CurveLinearSlope()),
                    CurveKey(Vec(HALF, HALF), CurveExponentialSlope(0.5f0)),
                    CurveKey(Vec(ONE, ZERO), CurveBezierSlope(0.2f0, -0.6f0))
                ], CurvePerlin(0.3))),
                [ -ONE, ZERO,
                  HALF * HALF * HALF,   HALF * HALF,
                  HALF,
                  ONE - (HALF * HALF),   ONE - (HALF * HALF * HALF),
                  ONE, ONE + ONE
                ],
                Ref((zero(UInt32), ))
              ))
            # Vector ops that also support scalars:
            a += vselect(ONE, ZERO, true) + vselect(ONE, ZERO, 0)
          end
        return a
    end
    function do_work(::Type{T}, ::Val{N}) where {T, N}
        return 0
        VT = Vec{T}
        V = Vec{N, T}
        IsInt = (T<:Integer) && (T!=Bool)

        # Helper to handle division ops with integer data:
        fix_float(f) = IsInt ? round(T, f) : convert(T, f)
        fix_float(v::Vec) = IsInt ? round(V, v) : convert(V, v)

        # Vec constructors:
        ONE::V = one(V)
        ZERO::V = zero(V)
        if T != Bool
            TWO = V(i->2)
            THREE = V(ntuple(i->3, Val(N)))
            FOUR = V(ntuple(i->4, Val(N))...)
            FIVE = Vec(ntuple(i->convert(T, 5), Val(N)))
            SIX = Vec(ntuple(i->convert(T, 6), Val(N))...)
            SEVEN = vappend(Vec{N-1, T}(ntuple(i->convert(T, 7), Val(N-1))),
                            convert(T, 7))
            EIGHT = VT(ntuple(i->8, Val(N)))
            NINE = VT(ntuple(i->convert(T, 9), Val(N)))
        end

        # Vec swizzling:
        a::(T==Bool ? Int : T) = ZERO.data[1]
        for s in [ :x, :xx, :xxx, :xxxx,
                   :r,
                   # Add Y component:
                   ((N < 2) ? [] : [
                       :y, :yy, :yyy, :yyyy,
                       :g,
                       :xy, :rg
                   ])...,
                   # Add Z component:
                   ((N < 3) ? [] : [
                       :z, :zz, :zzz, :zzzz,
                       :b, :bbb,
                       :xz, :yz,
                       :xyz, :rgb,
                       :xyz0, :xyz1, :rgb0, :rgb1,
                       :rgbΔ, :rgb∇
                   ])...,
                   # Add W component:
                   ((N < 4) ? [] : [
                       :w, :ww, :www, :wwww,
                       :a,
                       :xw, :ra
                   ])... ]
            a += getproperty(FIVE, s)[1]
        end

        # Numeric ops:
        (T != Bool) && (a += reduce(+, [
            (typemin(V) + (typemax(V) * ZERO)),
            ((typemin_finite(V) + typemax_finite(V)) * ZERO),
            fix_float(min(SEVEN, FIVE) / max(TWO, THREE)),
            (reduce(^, minmax(NINE, TWO)) ÷ abs(ONE)),
            reduce(%, divrem(NINE, SEVEN)),
            round(V, THREE + (ONE / TWO)),
            round(Vec{N, Int8}, THREE + (ONE / THREE)),
            reinterpret(V, if T <: Signed
                reinterpret(Vec{N, unsigned(T)}, ZERO)
            elseif T <: Unsigned
                reinterpret(Vec{N, signed(T)}, ZERO)
            # Other cases are most likely floats.
            elseif sizeof(T) == 2
                reinterpret(Vec{N, Int16}, ZERO)
            elseif sizeof(T) == 4
                reinterpret(Vec{N, Int32}, ZERO)
            elseif sizeof(T) == 8
                reinterpret(Vec{N, Int64}, ZERO)
            else
                zero(V)
            end)
        ]).x)

        # Basic math:
        (T != Bool) && (a += reduce(+, [
            clamp(THREE, -1, 5), clamp(THREE, ONE, TWO),
            floor(V, ONE / TWO), ceil(V, ONE / THREE),
            (isapprox(ONE, TWO) ? ONE : TWO),
            (reverse(FOUR) + FIVE[1]),
            fix_float(THREE / collect(ONE)[1]),
            (TWO - ONE[1]),
            (FOUR * TWO[1]),
            (FIVE ÷ THREE[1]),
            (fix_float(ONE/TWO) ^ SIX[1]),
            (FIVE % THREE[1]),
            (FIVE[1] + FOUR),
            fix_float(ONE[1] / THREE),
            (TWO[1] - ONE),
            (FOUR[1] * TWO),
            (FIVE[1] ÷ THREE),
            (fix_float(ONE/TWO)[1] ^ SIX),
            (FIVE[1] % THREE),
            ((ONE == TWO) ? THREE : FOUR),
            ((ONE != TWO) ? THREE : FOUR),
            if T <: Integer
                ((((((ONE & TWO) | THREE) ⊻ FOUR) ⊼ FIVE) << ONE) >> SIX)
            else
                ZERO
            end,
            foldl(+, FOUR), foldl(-, FIVE),
            foldr(-, SEVEN),
            foldl(*, ONE), foldr(*, TWO),
            mod(SEVEN, SIX)
        ]).x)

        # Float-compatible math:
        !(T <: Integer) && (a += reduce(+, [
            lerp(ONE, TWO, ONE/TWO),
            lerp(ONE[1], TWO, ONE/TWO),
            lerp(ONE, TWO[1], ONE/TWO),
            lerp(ONE, TWO, (ONE/TWO)[1]),
            lerp(ONE[1], TWO[1], ONE/TWO),
            lerp(ONE[1], TWO, (ONE/TWO)[1]),
            lerp(ONE, TWO[1], (ONE/TWO)[1]),
        ]).x)

        # Bool-compatible math and logic:
        a += reduce((T==Bool ? (&) : (+)), [
            rand(V),
            convert(Vec{N, Int8}, ONE),
            vselect(ONE, ZERO,     ONE < ZERO),
            vselect(ONE, ZERO,     ONE > ZERO),
            vselect(ONE, ZERO,     ONE <= ZERO),
            vselect(ONE, ZERO,     ONE >= ZERO),
            vselect(ONE, ZERO,     !(ONE > ZERO)),
            vselect(ONE, ZERO,     vequal(ONE, ZERO)),
            vselect(ONE[1], ZERO,  ONE < ZERO),
            vselect(ONE, ZERO[1],  ONE < ZERO),
            vselect(ONE, ZERO,     (ONE < ZERO)[1])
        ]).x

        # Ranges:
        IsInt && (a += reduce(+, [
            first(ZERO : EIGHT),
            last(TWO : ONE : FOUR),
            step(THREE[1] : ONE : EIGHT),
            first(ZERO : FOUR[1] : EIGHT),
            first(ZERO : FOUR : EIGHT[1]),
            first(ZERO[1] : FOUR[1] : EIGHT),
            first(ZERO[1] : FOUR : EIGHT[1]),
            first(ZERO : FOUR[1] : EIGHT[1]),
            last(ZERO[1] : ONE),
            last(ZERO : ONE[1]),
            collect(ZERO : ONE : EIGHT)[1],
            (ZERO : THREE)[1],
            vselect(FOUR, ZERO, (ONE in ZERO:THREE)),
            vselect(FIVE, ZERO, (ONE in 0:3)),
            rand(TWO:FIVE),
            (isempty(THREE:ZERO) ? TWO : ZERO)
        ]).x)

        # Vector math:
        a += reduce(+, [
            vdot(ONE, THREE),
            fix_float(vdist(ONE, THREE)),
            fix_float(vlength(THREE)),
            fix_float(vnorm(FIVE)),
            (N == 3) ? vcross(ONE, THREE) : ZERO,
            vreflect(THREE, ONE),
            fix_float(vrefract(ONE, ZERO, 4.5f0)),
            (N == 3 && !(T <: Integer)) ? vbasis(ONE).forward : ZERO,
            v_is_normalized(ONE) ? TWO : THREE
        ]).x

        return a
    end
    function do_work(::Type{T}, ::Val{Cols}, ::Val{Rows}) where {T, Cols, Rows}
        L = Cols * Rows
        M = @Mat(Cols, Rows, T)
        VC = Vec{Cols, T}
        VC1 = Vec{Cols-1, T}

        id::M = m_identity(Cols, Rows, T)
        vc = one(VC)
        vc1 = one(VC1)

        a::T = 0
        a += reduce(+, [
            convert(T, prod(mat_params(M)[1:2]))
        ])
        a += reduce(*, [
            m_invert(id),
            m_transpose(id),
            (Cols == Rows == 4) ? m_to_mat4x4(m_identity(3, 3, T)) : id,
            (Cols == Rows == 3) ? m_to_mat3x3(m_identity(4, 4, T)) : id,
            m_combine(id, id),
            m_combine(id, id, id),
            m_combine(id, id, id, id)
        ])[1]
        a += reduce(+, [
            m_apply_point(id, vc),
            m_apply_vector(id, vc)
        ]).x
        (Cols > 2) && (a += reduce(+, [
            m_apply_point(id, vc1),
            m_apply_point_affine(id, vc1),
            m_apply_vector(id, vc1),
            m_apply_vector_affine(id, vc1)
        ]).x)

        return a
    end

    for T in [ Float32, Int32, UInt32, UInt8 ]
        for N in [ 2, 3, 4 ]
            do_work(T, Val(N))
            (N > 1) && !(T <: Integer) && do_work(T, Val(N), Val(N))
        end
        do_work(T)
    end
end

end # module