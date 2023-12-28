"Basic math and utilities for the B+ library"
module BplusCore

include("Utilities/Utilities.jl")
export Utilities

include("Math/Math.jl")
export Math


"Imports all Core B+ modules"
macro using_bplus_core()
    return :( using BplusCore.Utilities, BplusCore.Math )
end
export @using_bplus_core


"
`eval()` this code to run B+ package tests.
Refer to the source file for more information.
"
const TEST_RUNNER_CODE = open(io -> read(io, String),
                              joinpath(@__DIR__, "test_runner.jl"))
include_dependency("test_runner.jl")


end # module BplusCore
