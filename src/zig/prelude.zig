const std = @import("std");
// const GC = @import("gc").bdwgc;
var debug_allocator_state = std.heap.DebugAllocator(.{}){};
var debug_allocator: std.mem.Allocator = undefined;
// var gc_allocator: std.mem.Allocator = undefined;

pub fn __zig_print_0(value: String_t) void {
    _ = std.fs.File.stdout().write(value.bytes.buffer) catch unreachable;
    _ = std.fs.File.stdout().write("\n") catch unreachable;
}

pub fn __zig_eq_0(left: anytype, right: anytype) bool {
    return left == right;
}
pub fn __zig_neq_0(left: anytype, right: anytype) bool {
    return left != right;
}
pub fn __zig_lt_0(left: anytype, right: anytype) bool {
    return left < right;
}
pub fn __zig_gt_0(left: anytype, right: anytype) bool {
    return left > right;
}
pub fn __zig_lte_0(left: anytype, right: anytype) bool {
    return left <= right;
}
pub fn __zig_gte_0(left: anytype, right: anytype) bool {
    return left >= right;
}

pub fn __zig_add_uint_0(left: usize, right: usize) usize {
    return left + right;
}
pub fn __zig_sub_uint_0(left: usize, right: usize) usize {
    return left - right;
}
pub fn __zig_mul_uint_0(left: usize, right: usize) usize {
    return left * right;
}
pub fn __zig_div_uint_0(left: usize, right: usize) usize {
    return left / right;
}
pub fn __zig_neg_int_0(int: anytype) isize {
    return -int;
}
pub fn __zig_add_int_0(left: isize, right: isize) isize {
    return left + right;
}
pub fn __zig_sub_int_0(left: isize, right: isize) isize {
    return left - right;
}
pub fn __zig_mul_int_0(left: isize, right: isize) isize {
    return left * right;
}
pub fn __zig_div_int_0(left: isize, right: isize) isize {
    return left / right;
}
pub fn __zig_neg_float_0(float: anytype) f64 {
    return -float;
}
pub fn __zig_add_float_0(left: f64, right: f64) f64 {
    return left + right;
}
pub fn __zig_sub_float_0(left: f64, right: f64) f64 {
    return left - right;
}
pub fn __zig_mul_float_0(left: f64, right: f64) f64 {
    return left * right;
}
pub fn __zig_div_float_0(left: f64, right: f64) f64 {
    return left / right;
}

/// Conversion Functions
pub fn __zig_nat2int_0(nat: usize) isize {
    return @intCast(nat);
}
pub fn __zig_nat2float_0(nat: usize) f64 {
    return @floatFromInt(nat);
}
pub fn __zig_nat2string_0(nat: usize) String_t {
    const buffer = std.fmt.allocPrint(debug_allocator, "{d}", .{nat}) catch unreachable;
    return String_t{ .bytes = Array_t(u8){ .buffer = buffer, .size = buffer.len } };
}
pub fn __zig_int2nat_0(int: isize) usize {
    return @intCast(int);
}
pub fn __zig_int2float_0(int: isize) f64 {
    return @floatFromInt(int);
}
pub fn __zig_int2string_0(int: isize) String_t {
    const buffer = std.fmt.allocPrint(debug_allocator, "{d}", .{int}) catch unreachable;
    return String_t{ .bytes = Array_t(u8){ .buffer = buffer, .size = buffer.len } };
}
pub fn __zig_float2int_0(float: f64) isize {
    return @intFromFloat(float);
}
pub fn __zig_float2nat_0(float: f64) usize {
    return @intFromFloat(float);
}
pub fn __zig_float2string_0(float: f64) String_t {
    const buffer = std.fmt.allocPrint(debug_allocator, "{d}", .{float}) catch unreachable;
    return String_t{ .bytes = Array_t(u8){ .buffer = buffer, .size = buffer.len } };
}
pub fn __zig_string2int_0(string: []const u8) isize {
    return std.fmt.parseInt(isize, string, 10) catch unreachable;
}
pub fn __zig_string2float_0(string: []const u8) f64 {
    return std.fmt.parseFloat(f64, string) catch unreachable;
}

/// Pointer/Slice Allocation
pub fn __zig_alloc_0(comptime T: type, size: usize) []T {
    return debug_allocator.alloc(T, size) catch unreachable;
}
pub fn __zig_free_0(comptime T: type, ptr: []T) void {
    debug_allocator.free(ptr);
}
pub fn __zig_slice_size_0(comptime T: type, slice: []const T) usize {
    return slice.len;
}
pub fn __zig_slice2ptr_0(comptime T: type, slice: []T) *T {
    return @ptrCast(slice);
}
pub fn __zig_ptr2slice_0(comptime T: type, ptr: *T, size: usize) []T {
    return ptr[0..size];
}

fn string_from_literal(comptime literal: []const u8) String_t {
    const buffer = debug_allocator.dupe(u8, literal) catch unreachable;
    const char_array = Array_t(u8){
        .buffer = buffer,
        .size = literal.len,
    };
    return String_t{ .bytes = char_array };
}

pub fn main() !void {
    // gc_allocator = GC.allocator();
    debug_allocator = debug_allocator_state.allocator();
    main_0();
}

//////////////////////
/// user code
/////////////////////