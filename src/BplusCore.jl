"Basic math and utilities for the B+ library"
module BplusCore

include("Utilities/Utilities.jl")
export Utilities

include("Math/Math.jl")
export Math


# Helper macro to import all BplusCore stuff.
const MODULES = tuple(:Utilities, :Math)
const MODULES_USING_STATEMENTS = [:( using BplusCore.$m ) for m in MODULES]
"Imports all Core B+ modules"
macro using_bplus_core()
    return quote $(MODULES_USING_STATEMENTS...) end
end
export @using_bplus_core


"The path to the file that can run unit tests for a B+ package"
const TEST_RUNNER_PATH = joinpath(@__DIR__, "..", "scripts", "test_runner.jl")


end # module BplusCore
