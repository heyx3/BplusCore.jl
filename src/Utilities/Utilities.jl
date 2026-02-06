module Utilities

using StaticArrays, MacroTools, Dates, StructTypes, BitFlags

include("asserts.jl")
@make_toggleable_asserts bp_utils_

include("basic.jl")
include("numbers.jl")
include("unions.jl")
include("macros.jl")

include("strings.jl")
include("enums.jl")

include("up_to.jl")

include("prng.jl")
include("rand_iterator.jl")


# Precompile common math using PrecompileTools.jl.
# This doubles as a smoke test for syntax errors!
using PrecompileTools: @setup_workload, @compile_workload
@bp_enum(_PrecompileDummyEnum, a, b)
@bp_bitflag(_PrecompileDummyBitflag,
    a, b,
    @ab a|b
)
@compile_workload begin
    return [
        reinterpret_bytes(0xaabbccdd, NTuple{4, UInt8}),
        reinterpret_bytes(0xaabbccdd, NTuple{2, UInt16}),
        reinterpret_bytes((0xab, 0xcd, 0xef, 0x00), UInt32),
        reinterpret_bytes(0xaabbccdd, Float32),
        reinterpret_bytes(0.5f0, UInt32),
        (
            PRNG(ntuple(i -> f, n)...)
              for f in [ 0.5f0, 0.5, 0xaa, 0xaabbccdd ],
                  n in 2:3
        )...,
        PRNG(0xaabb), PRNG(0xaabbccddeeff0011),
        let p = PRNG()
            (
                rand(p, T)
                  for T in [ Int8, UInt8, Float32, Float64, UInt32, Int64, Bool ]
            )
        end...
    ]
end

end