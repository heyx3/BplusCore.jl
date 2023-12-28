PROJECT_DIR = joinpath(@__DIR__, "..")

# Make sure the test is always running in the same directory and within the same project.
using Pkg
cd(PROJECT_DIR)
Pkg.activate(".")

# Enable asserts for B+ Core.
using BplusCore
@using_bplus_core
BplusCore.Utilities.bp_utils_asserts_enabled() = true
BplusCore.Math.bp_math_asserts_enabled() = true

# Execute the tests.
const TEST_HEADER_EXTRA = quote
    using JSON3
end
include_string(@__MODULE__, BplusCore.TEST_RUNNER_CODE,
               joinpath(pathof(BplusCore), "..", "test_runner.jl"))