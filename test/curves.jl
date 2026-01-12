tests = [
    (
        Curve(CurveKey(zero(v2f)), CurveKey(one(v2f))),
        (t, loop) -> loop ? wraparound(0.0f0, 1.0f0, t) : clamp(t, 0.0f0, 1.0f0),
        Float32.((0, 1))...,
        "Trivial identity 0-1 curve"
    ),
    (
        Curve([
            CurveKey{v2f}(v3f(-1.0, 2.5, 3.6),
                          CurveExponentialSlope(3.5)),
            CurveKey(1.2, v2f(-1.3, 10.22))
        ]),
        (t, loop) -> lerp(
            v2f(2.5, 3.6), v2f(-1.3, 10.22),
            (loop ? wraparound(0.0f0, 1.0f0, t) : clamp(t, 0.0f0, 1.0f0)) ^ 3.5f0
        ),
        Float32.((-1.0, 1.2))...,
        "Exponential Curved v2f"
    ),
    (
        Curve([
            CurveKey(v4f(0.2, 1, 2, 3),
                     CurveBezierSlope(2.0f0, -0.1f0)),
            CurveKey(v4f(0.3, 4, 6, 8))
        ]),
        (t, loop) -> lerp(v3f(1, 2, 3), v3f(4, 6, 8),
            let t_sanitized = loop ? wraparound(0.0f0, 1.0f0, t) : clamp(t, 0.0f0, 1.0f0),
                t_inv = 1.0f0 - t_sanitized,
                t2 = t_sanitized * t_sanitized,
                t2_inv = t_inv * t_inv
                t_bez = (t2_inv * t_inv * 0.2f0) +
                         (3.0f0 * t2_inv * t_sanitized * (0.2f0 + 2.0f0)) +
                         (3.0f0 * t_inv * t2 * (0.3f0 - 0.1f0)) +
                         (t2 * t_sanitized * 0.3f0)
              inv_lerp(0.2, 0.3, t_bez)
            end),
        Float32.((0.2, 0.3))...,
        "Bezier Curved v3f"
    ),
    (
        Curve([
            CurveKey(v2f(-0.2f0, 0.0f0)),
            CurveKey(v2f(0.2f0, 0.5f0)),
            CurveKey(v2f(0.8f0, 0.1f0)),
            CurveKey(v2f(1.0f0, 1.0f0))
        ]),
        (_t, loop) -> let t = lerp(-0.2f0, 1.0f0,
                                   loop ? wraparound(0.0f0, 1.0f0, _t) : clamp(_t, 0.0f0, 1.0f0))
            if t < 0.2
                lerp(0.0f0, 0.5f0, inv_lerp(-0.2f0, 0.2f0, t))
            elseif t < 0.8
                lerp(0.5f0, 0.1f0, inv_lerp(0.2f0, 0.8f0, t))
            else
                lerp(0.1f0, 1.0f0, inv_lerp(0.8f0, 1.0f0, t))
            end
        end,
        Float32.((-0.2, 1.0))...,
        "Linear Piecewise Float"
    )
]

for (curve, expected_fn, t_min, t_max, name) in tests
    t_range = t_max - t_min
    t_inflation = t_range/5.0f0
    t_step = t_range/20.0f0
    for loops::Bool in (true, false)
        for t in (t_min-t_inflation) : t_step : (t_max+t_inflation)
            get_test_error(a::Vec, b::Vec) = vdist(a, b)
            get_test_error(a::Real, b::Real) = abs(a - b)
            @noinline get_test_data() = let actual = curve_eval(curve, t, looping=loops),
                                            expected = expected_fn(inv_lerp(t_min, t_max, t), loops)
                (actual, expected, get_test_error(actual, expected))
            end

            @bp_test_no_allocations(get_test_data()[3] < 0.00005f0,
                                    true,
                                    name, ": ", curve, " at t=", t,
                                    loops ? "(looping)" : "",
                                    "; (actual, expected, err)=", get_test_data())
        end
    end
end