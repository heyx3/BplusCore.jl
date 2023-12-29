# Make sure the test is always running for *this* project.
using Pkg
cd(joinpath(@__DIR__, ".."))
Pkg.activate(".")

using BplusCore; @using_bplus_core

# Enable asserts for B+ Core.
BplusCore.Utilities.bp_utils_asserts_enabled() = true
BplusCore.Math.bp_math_asserts_enabled() = true

# Execute the tests.
const TEST_HEADER_EXTRA = quote
    using JSON3

    # Sadly, the macros to auto-import B+ do not work right in here.
    using BplusCore
    for use in BplusCore.MODULES_USING_STATEMENTS
        eval(use)
    end
end
include(BplusCore.TEST_RUNNER_PATH)