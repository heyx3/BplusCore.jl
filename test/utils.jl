# Test unzip():
@bp_test_no_allocations(unzip(zip(1:10, 2:20)),
                        (1:10, 2:20),
                        "Unzipping a zip() call")
@bp_test_no_allocations(begin
                            iterator = unzip(((1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7), (7, 8), (8, 9), (9, 10), (10, 11)))
                            # Put the unzipped data into tuples for easy comparison.
                            Tuple(Tuple(i) for i in iterator)
                        end,
                        ((1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                         (2, 3, 4, 5, 6, 7, 8, 9, 10, 11)),
                        "Unzipping pseudo-zipped data")
# Test that unzip() can be inlined/folded into something that doesn't require the heap.
@bp_test_no_allocations(begin
        # To prevent compiling into a simple "return [constant]",
        #    run unzip in a big loop and perform some mixing of tuple data.
        outp::Tuple{NTuple{3, Int}, NTuple{3, Int}} = ((0, 0, 0), (0, 0, 0))
        for i in 1:2000
            z = zip((1, i, 3), (-i, 2, 6))
            mm = (minmax(a, b) for (a,b) in z)
            (a, b) = unzip(mm, 2)
            outp = (Tuple(b), outp[1])
        end
        outp # Return the data so it doesn't get compiled out
    end,
    # I have no idea what this actually computed
    ((1, 2000, 6), (1, 1999, 6))
)

# Test UpTo{N, T}:
@bp_check(collect(UpTo{4, Int}()) == Int[ ])
@bp_check(collect(UpTo{4, Int}((3, 2))) == [ 3, 2 ])
# Conversion:
@inline function f_upto(i::T)::UpTo{3, Int} where {T}
    return i
end
@bp_test_no_allocations(f_upto(()), UpTo{3, Int}(( )))
@bp_test_no_allocations(f_upto(2), UpTo{3, Int}((2, )))
@bp_test_no_allocations(f_upto((3, 4)), UpTo{3, Int}((3, 4)))
# Equality with raw data:
@bp_test_no_allocations(UpTo{3, Int}((2, 3, 4)), (2, 3, 4))
@bp_test_no_allocations(UpTo{3, Int}((2, )), (2, ))
@bp_test_no_allocations(UpTo{3, Int}((2, )), 2)
# Append:
do_append() = append(UpTo{2, Int}((1, )),
                     UpTo{5, Float32}(Float32.((2, 3, 4))))
@bp_test_no_allocations(typeof(do_append()), UpTo{7, Float32})
@bp_test_no_allocations(do_append(), Float32.((1.0, 2.0, 3.0, 4.0)))

# Test reduce_some()
@bp_test_no_allocations(reduce_some(+, b->(b%2==0), 1:10),
                        reduce(+, 2:2:10))
@bp_test_no_allocations(reduce_some(*, b->(b%2==0), 1:10; init=1),
                        reduce(*, 2:2:10))

# Test drop_last()
try
    print(collect(drop_last(1:0)))
    error("Should have failed to drop_last() from an empty collection!")
catch e; end
@bp_check(collect(drop_last(1:1)) == [ ])
@bp_check(collect(drop_last(1:10)) == collect(1:9))

# Test iter_join()
@bp_check(collect(iter_join(1:5, ',')) ==
            [ 1, ',', 2, ',', 3, ',', 4, ',', 5],
          collect(iter_join(1:5, ',')))

# Test @unionspec
@bp_check(@unionspec(Vector{_}, Int, Float64) ==
            Union{Vector{Int}, Vector{Float64}},
          @unionspec(Vector{_}, Int, Float64))
@bp_check(@unionspec(Vector{_}, Tuple{Int, Float64}, String) ==
            Union{Vector{Tuple{Int, Float64}}, Vector{String}},
          @unionspec(Vector{_}, Tuple{Int, Float64}, String))
@bp_check(@unionspec(Array{_, 2}, Int, Float64) ==
            Union{Array{Int, 2}, Array{Float64, 2}},
          @unionspec(Array{_, 2}, Int, Float64))
@bp_check(@unionspec(Array{Int, _}, 4, 5) ==
            Union{Array{Int, 4}, Array{Int, 5}},
          @unionspec(Array{Int, _}, 4, 5))

# Test reinterpret_bytes()
#    Immutable bytes:
@bp_test_no_allocations(reinterpret_bytes(4.5f0, NTuple{4, UInt8}), (0x00, 0x00, 0x90, 0x40))
@bp_test_no_allocations(reinterpret_bytes((0x00, 0x00, 0x90, 0x40), Float32), 4.5f0)
@bp_test_no_allocations(reinterpret_bytes(-4345.35001, NTuple{8, UInt8}),
                        (0x46, 0x5f, 0x41, 0x9a, 0x59, 0xf9, 0xb0, 0xc0))
@bp_test_no_allocations(reinterpret_bytes((0x46, 0x5f, 0x41, 0x9a, 0x59, 0xf9, 0xb0, 0xc0), Float64),
                        -4345.35001)
@bp_test_no_allocations(reinterpret_bytes(Vec{4, UInt8}(2, 3, 4, 5), NTuple{4, UInt8}),
                        (0x02, 0x03, 0x04, 0x05))
@bp_test_no_allocations(reinterpret_bytes((0x02, 0x03, 0x04, 0x05),
                                          Vec{4, UInt8}),
                        Vec{4, UInt8}(2, 3, 4, 5))
#    Mutable bytes:
@bp_test_no_allocations_setup(
    a = UInt32[ 0 ],
    begin; reinterpret_bytes(4.5f0, a); a[1]; end,
    0x40900000 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    a = Ref(UInt32(0)),
    begin; reinterpret_bytes(4.5f0, a); a[]; end,
    0x40900000 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    a = UInt32[ 0, 1 ],
    GC.@preserve(a, begin
        reinterpret_bytes(4.5f0, pointer(a, 1) + 4)
        a[2]
    end),
    0x40900000 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    a = UInt32[ 0, 1 ],
    begin; reinterpret_bytes(4.5f0, @view(a[2:end]) ); a[2]; end,
    0x40900000 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    begin
        a_in = UInt32[ 0, 1 ]
        a_out = UInt32[ 3, 4 ]
    end,
    begin
        reinterpret_bytes(a_in, a_out)
        a_out
    end,
    UInt32[ 0, 1 ]
)
@bp_test_no_allocations_setup(
    begin
        a_in = UInt32[ 0, 1 ]
        a_out = UInt32[ 3, 4 ]
    end,
    begin
        reinterpret_bytes(a_in[2], a_out)
        a_out
    end,
    UInt32[ 1, 4 ]
)
@bp_test_no_allocations_setup(
    begin
        a_in = UInt32[ 0, 1 ]
        a_out = UInt32[ 3, 4 ]
    end,
    begin
        reinterpret_bytes(@view(a_in[1:end]), a_out)
        a_out
    end,
    UInt32[ 0, 1 ]
)
@bp_test_no_allocations_setup(
    begin
        a_in = UInt32[ 0, 1 ]
        a_out = UInt32[ 3, 4 ]
    end,
    begin
        reinterpret_bytes((Ref(a_in, 1), 2), a_out)
        a_out
    end,
    UInt32[ 0, 1 ]
)
@bp_test_no_allocations_setup(
    a = UInt32[ 0x40900000 ],
    reinterpret_bytes(a, Float32),
    4.5f0 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    a = Ref{UInt32}(0x40900000),
    reinterpret_bytes(a, Float32),
    4.5f0 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    a = UInt32[ 0, 0x40900000 ],
    GC.@preserve(a, begin
        reinterpret_bytes(pointer(a, 1) + 4, Float32)
    end),
    4.5f0 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
@bp_test_no_allocations_setup(
    a = UInt32[ 0, 0x40900000 ],
    reinterpret_bytes(@view(a[2:end]), Float32),
    4.5f0 #NOTE: If this test fails, you may be on a machine with different endianness than mine
)
struct ByteReinterpretable
    i32::Int32
    f32::Float32
    d64::UInt64
end
@bp_test_no_allocations(reinterpret_bytes(ByteReinterpretable(reinterpret(Int32, 0xabcdef34),
                                                              reinterpret(Float32, 0x98765432),
                                                              0xabcdef1234567890),
                                          NTuple{sizeof(ByteReinterpretable), UInt8}),
                        (
                            # Watch endianness! If this test fails for you,
                            #    perhaps you're on a machine with a different endianness?
                            0x34, 0xef, 0xcd, 0xab,
                            0x32, 0x54, 0x76, 0x98,
                            0x90, 0x78, 0x56, 0x34, 0x12, 0xef, 0xcd, 0xab
                        ))
@bp_test_no_allocations(reinterpret_bytes(
                            (
                                0x34, 0xef, 0xcd, 0xab,
                                0x32, 0x54, 0x76, 0x98,
                                0x90, 0x78, 0x56, 0x34, 0x12, 0xef, 0xcd, 0xab
                            ),
                            ByteReinterpretable
                        ),
                        # Watch endianness! If this test fails for you,
                        #    perhaps you're on a machine with a different endianness?
                        ByteReinterpretable(reinterpret(Int32, 0xabcdef34),
                                            reinterpret(Float32, 0x98765432),
                                            0xabcdef1234567890)
                        )

# Test ConstVector:
@bp_check((4.0, 3.0, 1.0, 4.0) isa ConstVector{Float64})

# Test Contiguous:
@bp_check(v2f <: Contiguous{Float32})
@bp_check(v4f <: Contiguous{Float32})
@bp_check(!(v4f <: Contiguous{Float64}))
@bp_check([ 4, 5.0, 6, 7 ] isa Contiguous{Float64})
@bp_check(!([ 4, 5, 6, :hi ] isa Contiguous{Float64}))
@bp_check([ 4 5.0 ; 6 7 ] isa Contiguous{Float64})
@bp_check(Vec{2, v2f} <: Contiguous{Float32})
@bp_check(Vec{20, v2f} <: Contiguous{Float32})
@bp_check(Int16 <: Contiguous{Int16})
@bp_check(Tuple{Int16} <: Contiguous{Int16})
@bp_check(NTuple{400, Int16} <: Contiguous{Int16})
@bp_check(Vec{20, NTuple{4, Int8}} <: Contiguous{Int8})
@bp_check([ v2f(), v2f() ] isa Contiguous{Float32})
@bp_check([ (v2b(), v2b()), (v2b(), v2b()) ] isa Contiguous{Bool})
@bp_check(NTuple{5, Vec{2, Float64}} <: Contiguous{Float64})
@bp_check(Vec{1, Vec{2, Vec{3, Bool}}} <: Contiguous{Bool})
@bp_check(Vec{1, NTuple{5, SVector{6, Float16}}} <: Contiguous{Float16})
@bp_check(Vector{Vec{1, NTuple{5, SVector{6, Float16}}}} <: Contiguous{Float16})
@bp_check(MMatrix{6, 17, Vec{1, NTuple{5, SArray{Tuple{6}, Float16, 1, 6}}}, 6*17} <: Contiguous{Float16})
@bp_check(@view([4 5.0 ; 6 7][1:1, 2:2]) isa Contiguous{Float64})
@bp_check(!(Set([4, 5, 6.0]) isa Contiguous{Float64}))
@bp_check(!(keys(Dict(1.0=>5, 4.0=>7, 2.345354=>123.123)) isa Contiguous{Float64}))
@bp_check(!(keys(Dict(1.0=>5, 4.0=>7, 2.345354=>"hi")) isa Contiguous{Float64}))
@bp_check(contiguous_length(5.3, Float64) == 1)
@bp_check(contiguous_length([ 5 ], Int) == 1)
@bp_check(contiguous_length([ 5, 4, 3 ], Int) == 3)
@bp_check(contiguous_length([ v2f(), v2f() ], Float32) == 4)
@bp_check(contiguous_length([ (v2f(), v2f()), (v2f(), v2f()) ], Float32) == 8)
@bp_check(contiguous_length([ (Vec((1, 2), (3, 4)), Vec((5, 6), (7, 8))) ], Int) == 8)
contiguous_data = [ (Vec((1, 2), (3, 4)), Vec((5, 6), (7, 8))),
                    (Vec((9, 10), (11, 12)), Vec((13, 14), (15, 16))) ]
contiguous_data_ref = contiguous_ref(contiguous_data, Int)
@bp_check(contiguous_data isa Contiguous{Int})
@bp_check(contiguous_length(contiguous_data, Int) == 16)
@bp_check(ntuple(i -> unsafe_load(contiguous_ptr(contiguous_data_ref, Int, i)),
                 16) == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

# Test find_matching():
@bp_test_no_allocations_setup(
    vec = [ 4, 5, 6, 7 ],
    find_matching(5, vec),
    2
)

# Test preallocated_vector():
@bp_test_no_allocations_setup(
    v = preallocated_vector(Int, 1000),
    begin
        for i in 1000:-1:1
            push!(v, i*2)
        end
        v
    end,
    map(i -> i*2, 1000:-1:1)
)

# Test @do_while:
@bp_test_no_allocations_setup(
    i::Int = 0,
    begin
        @do_while((i += 1), (i < 5))
        i
    end,
    5
)
@bp_test_no_allocations_setup(
    i::Int = 0,
    begin
        @do_while((i += 1), false)
        i
    end,
    1
)

# Test @optional
@bp_check(tuple(@optional 4>0   3 4.0 true "hi" :world) ==
           (3, 4.0, true, "hi", :world))
@bp_check(tuple(@optional 4<0   3 4.0 true "hi" :world) == ())

# Test InteropString
const IS = InteropString("abcdef")
@bp_test_no_allocations(IS.julia, "abcdef")
@bp_test_no_allocations(IS.c_buffer[7], 0)
@bp_test_no_allocations(begin
                            BplusCore.Utilities.update!(IS, "1234") #TODO: Why do I need to qualify 'update!' explicitly?
                            IS.julia
                        end,
                        "1234")
@bp_test_no_allocations(@view(IS.c_buffer[1:5]),
                        vcat(codeunits("1234"), 0))

# Test IterSome
@bp_test_no_allocations(Tuple(IterSome(i -> (i>3) ? nothing : Some(i))), (1, 2, 3))
    # No manual type inference provided in the above line, but the compiler should be able to deduce it.
@bp_test_no_allocations(Tuple(IterSome(i -> nothing, Float64)), ())
@bp_test_no_allocations(Tuple(IterSome(i -> (i > 4) ? nothing : Some(Float64(i)), Float64)), (1.0, 2.0, 3.0, 4.0))

# Test union type wrangling:
#TODO: Why was @assert used here? Switch to @bp_check
@assert union_types(Float32) == (Float32, )
@assert Set(union_types(Union{Int64, Float32})) ==
        Set([ Int64, Float32 ])
@assert Set(union_types(Union{Float32, Int64, String})) ==
        Set([ Int64, Float32, String])
@assert Set(union_types(Union{Int, Float64, String, Vector{Float64}})) ==
        Set([ Int, Float64, String, Vector{Float64} ])
println("#TODO: Adding a type like `Dict{<:AbstractString, <:Integer} breaks this. Why?")
@assert union_parse_order(Union{Int, Float64}) ==
          union_parse_order(Union{Float64, Int}) ==
          [ Int, Float64]
@assert union_parse_order(Union{Float32, Int64, String}) ==
          [ Int64, Float32, String ]
@assert union_parse_order(Union{Int, String, Float64}) ==
          union_parse_order(Union{Float64, Int, String}) ==
          union_parse_order(Union{String, Float64, Int}) ==
          union_parse_order(Union{Int, Float64, String}) ==
          [ Int, Float64, String ]
@assert union_parse_order(Union{Int, Float64, String, Vector, Dict}) ==
          [ Int, Float64, String, Dict, Vector ]

# Test SerializedUnion's ability to serialize things that StructTypes on its own can't.
@enum SU1 su1_a su1_b su1_c
@enum SU2 su2_a su2_b su2_c
@assert(su2_c == JSON3.read(JSON3.write(su2_c), SerializedUnion{Union{SU1, SU2}}))
@assert(su2_c == JSON3.read(JSON3.write(su2_c), SerializedUnion{Union{SU2, SU1}}))
@assert(su2_c == JSON3.read(JSON3.write(su2_c), @SerializedUnion(SU1, SU2)))

# Test the Binary decorator.
function test_binary_print(n, type, leading_zeros, expected)
    n′ = reinterpret(type, n)
    value = sprint(io -> show(io, Binary(n′, leading_zeros)))
    @bp_check(value == expected,
              "Expected binary print of ", type, "(", n′, "), ",
              (leading_zeros ? "with leading zeros" : "without leading zeros"),
              ", to be '", expected, "' but got ", value)
end
for type8 in union_types(Scalar8)
    test_binary_print(0b11001100, type8, false, "0b11001100")
    test_binary_print(0b00110011, type8, false,  "0b110011")
    test_binary_print(0b00110011, type8, true, "0b00110011")
    test_binary_print(0x0, type8, false, "0b0")
    test_binary_print(0x0, type8, true, "0b00000000")
end
for type16 in union_types(Scalar16)
    test_binary_print(0b100110011, type16, false,       "0b100110011")
    test_binary_print(0b100110011, type16, true, "0b0000000100110011")
    test_binary_print(0x0000, type16, false, "0b0")
    test_binary_print(0x0000, type16, true, "0b0000000000000000")
end
for type32 in union_types(Scalar32)
    test_binary_print(0b10101010101010101, type32, false,
                      "0b10101010101010101")
    test_binary_print(0b10101010101010101, type32, true,
                      "0b00000000000000010101010101010101")
    test_binary_print(0x00000000, type32, false, "0b0")
    test_binary_print(0x00000000, type32, true, "0b00000000000000000000000000000000")
end
for type64 in union_types(Scalar64)
    test_binary_print(0b111111111111111111110000000000111, type64, false,
                      "0b111111111111111111110000000000111")
    test_binary_print(0b111111111111111111110000000000111, type64, true,
                      "0b0000000000000000000000000000000111111111111111111110000000000111")
    test_binary_print(0x0000000000, type64, false, "0b0")
    test_binary_print(0x0000000000, type64, true,
                      "0b0000000000000000000000000000000000000000000000000000000000000000")
end