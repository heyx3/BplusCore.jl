# This file contains the test-running code for all B+ package projects.
# Trying to expose this with normal meta-programming didn't work
#    due to the amount of interpolated-code-within-interpolated-code.
# So instead, this file data is meant to be read as a string and 'eval()'-ed.


#####################
##  Configuration

# You should tell this script where the project folder is.
if !@isdefined PROJECT_DIR
    error("You need to configure the unit tests by setting 'PROJECT_DIR'")
end

# You can choose which files to test by passing them as command-line args,
#    or through a global named 'TEST_NAMES'.
# The '.jl' extension is automatically appended if necessary.
const TEST_DIR = joinpath(PROJECT_DIR, "test")
const TEST_FILES =
    # If desired test files are explicitly given, just run them.
    if @isdefined TEST_NAMES
        [joinnpath(TEST_DIR, n) for n in TEST_NAMES]
    # If desired test files are provided as command-line args, use those.
    elseif !isempty(ARGS)
        [joinpath(TEST_DIR, a) for a in ARGS]
    # Otherwise, pick all Julia files in this folder,
    #    excluding the standard root file 'runtests.jl'.
    else
        files = filter(readdir(TEST_DIR, join=true)) do name
            !endswith(name, "runtests.jl") && endswith(name, ".jl")
        end
        # For some reason, most of the simplest/lowest-level test files come near last alphabetically.
        reverse!(files)
        files
    end
map!(TEST_FILES, TEST_FILES) do user_provided_name
    if endswith(user_provided_name, ".jl")
        user_provided_name
    else
        user_provided_name * ".jl"
    end
end

# You can customize the test setup by setting a global, 'TEST_HEADER_EXTRA',
#    which is injected into each test.
if !@isdefined(TEST_HEADER_EXTRA)
    TEST_HEADER_EXTRA = :()
end


#######################
##   Definitions

insert!(LOAD_PATH, 1, TEST_DIR)

const TEST_HEADER = quote
    using Random, TupleTools, MacroTools, Setfield, StaticArrays, StructTypes

    using BplusCore
    using BplusCore.Utilities, BplusCore.Math,
          BplusCore.SceneTree, BplusCore.ECS

    # Some helpers for iterating over a variety of types:
    const ALL_SIGNED = (Int8, Int16, Int32, Int64, Int128)
    const ALL_UNSIGNED = map(unsigned, ALL_SIGNED)
    const ALL_INTEGERS = TupleTools.vcat(ALL_SIGNED, ALL_UNSIGNED)
    const ALL_FLOATS = (Float16, Float32, Float64)
    const ALL_REALS = TupleTools.vcat(ALL_INTEGERS, ALL_FLOATS, (Bool, ))

    # Macros to perform a test while making sure no allocations happen.
    #TODO: Look at existing attempts to test memory allocations in Julia: https://github.com/JuliaObjects/ConstructionBase.jl/blob/master/test/runtests.jl#L349-L355
    """
    Tests that the given expression equals the given value,
    and does not allocate any heap memory when evaluating.
    NOTE: the expression could be evaluated more than once, to ensure it's precompiled,
    so be careful with mutating expressions!
    """
    macro bp_test_no_allocations(expr, expected_value, msg...)
        return impl_bp_test_no_allocations(expr, expected_value, msg, :())
    end
    """
    Tests that the given expression equals the given value,
    and does not allocate any heap memory when evaluating.
    Also takes some code which does initial setup, and may allocate.
    NOTE: the expression could be evaluated more than once, to ensure it's precompiled,
    so be careful with mutating expressions!
    """
    macro bp_test_no_allocations_setup(setup, expr, expected_value, msg...)
        return impl_bp_test_no_allocations(expr, expected_value, msg, setup)
    end

    function impl_bp_test_no_allocations(expr, expected_value, msg, startup)
        expr_str = string(prettify(expr))
        expected_str = string(prettify(expected_value))
        expr = esc(expr)
        expected_value = esc(expected_value)
        msg = map(esc, msg)
        startup = esc(startup)
        expr = quote
            # Hide the expressions in a function to avoid global scope.
            @noinline function run_test()
                $startup
                # Try several times until we get a result which does't allocate,
                #    to handle precompilation *and* the GC doing anything weird.
                result = nothing
                n_tries::Int = 0
                for i in 1:10
                    result = @timed($expr)
                    n_tries += 1
                    if result.bytes < 1
                        break
                    end
                end
                actual_value = result.value
                # Get the expected value, and repeat its operation the same number of times
                #    in case the expressions mutate something.
                expected_value = nothing
                for i in 1:n_tries
                    expected_value = $expected_value
                end
                # Test.
                @bp_check(actual_value == expected_value, "\n",
                        "    Expected: `", $expected_str, "` => `", expected_value, "`.\n",
                        "      Actual: `", $expr_str,     "` => `", actual_value, "`.\n",
                        "\t", $(msg...))
                @bp_check(true || result.bytes == 0,
                        "The expression `", $expr_str,
                        "` allocated ", Base.format_bytes(result.bytes),
                        ". ", $(msg...))
            end
            run_test()
        end
        # println("Expression: ", prettify(expr))
        return expr
    end

    $TEST_HEADER_EXTRA
end


#######################
##   Execution

for f_path in TEST_FILES
    f_name = basename(f_path)
    println("\nRunning ", f_name, "...")
    try
        module_name = Symbol(:UnitTests_, f_name)
        @eval module $module_name
            $TEST_HEADER
            include($(f_path))
        end
    finally end
end

println("Tests finished!")