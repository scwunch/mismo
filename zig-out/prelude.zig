///////////////////////////////
/// prulude.zig
///////////////////////////////
const std = @import("std");
// const GC = @import("gc").bdwgc;
var debug_allocator_state = std.heap.DebugAllocator(.{}){};
var debug_allocator: std.mem.Allocator = undefined;
var global_io: std.Io = undefined;
// var gc_allocator: std.mem.Allocator = undefined;

pub fn __zig_print(value: *const String_t) void {
    // _ = std.fs.File.stdout().write(value.bytes.buffer) catch unreachable;
    // _ = std.fs.File.stdout().write("\n") catch unreachable;
    std.Io.File.stdout()
        .writeStreamingAll(global_io, value.bytes.buffer) catch @panic("failed to write to stdout");
}
pub fn __zig_panic(value: *const String_t) noreturn {
    _ = std.fs.File.stdout().write("\x1B[90mPanic: ") catch unreachable;
    _ = std.fs.File.stdout().write(value.bytes.buffer) catch unreachable;
    _ = std.fs.File.stdout().write("\x1B[0m\n") catch unreachable;
    std.process.exit(1);
}

pub fn __zig_eq(left: anytype, right: anytype) bool {
    return left == right;
}
pub fn __zig_neq(left: anytype, right: anytype) bool {
    return left != right;
}
pub fn __zig_lt(left: anytype, right: anytype) bool {
    return left < right;
}
pub fn __zig_gt(left: anytype, right: anytype) bool {
    return left > right;
}
pub fn __zig_lte(left: anytype, right: anytype) bool {
    return left <= right;
}
pub fn __zig_gte(left: anytype, right: anytype) bool {
    return left >= right;
}

pub fn __zig_add_uint(left: usize, right: usize) usize {
    return left + right;
}
pub fn __zig_sub_uint(left: usize, right: usize) usize {
    return left - right;
}
pub fn __zig_mul_uint(left: usize, right: usize) usize {
    return left * right;
}
pub fn __zig_div_uint(left: usize, right: usize) usize {
    return left / right;
}
pub fn __zig_neg_int(int: anytype) isize {
    return -int;
}
pub fn __zig_add_int(left: isize, right: isize) isize {
    return left + right;
}
pub fn __zig_sub_int(left: isize, right: isize) isize {
    return left - right;
}
pub fn __zig_mul_int(left: isize, right: isize) isize {
    return left * right;
}
pub fn __zig_div_int(left: isize, right: isize) isize {
    return @divTrunc(left, right);
}
pub fn __zig_neg_float(float: anytype) f64 {
    return -float;
}
pub fn __zig_add_float(left: f64, right: f64) f64 {
    return left + right;
}
pub fn __zig_sub_float(left: f64, right: f64) f64 {
    return left - right;
}
pub fn __zig_mul_float(left: f64, right: f64) f64 {
    return left * right;
}
pub fn __zig_div_float(left: f64, right: f64) f64 {
    return left / right;
}

/// Conversion Functions
pub fn __zig_nat2int(nat: usize) isize {
    return @intCast(nat);
}
pub fn __zig_nat2float(nat: usize) f64 {
    return @floatFromInt(nat);
}
pub fn __zig_nat2string(nat: usize) String_t {
    const buffer = std.fmt.allocPrint(debug_allocator, "{d}", .{nat}) catch unreachable;
    return String_t{ .bytes = Array_t(u8){ .buffer = buffer, .size = buffer.len } };
}
pub fn __zig_int2nat(int: isize) usize {
    return @intCast(int);
}
pub fn __zig_int2float(int: isize) f64 {
    return @floatFromInt(int);
}
pub fn __zig_int2string(int: isize) String_t {
    const buffer = std.fmt.allocPrint(debug_allocator, "{d}", .{int}) catch unreachable;
    return String_t{ .bytes = Array_t(u8){ .buffer = buffer, .size = buffer.len } };
}
pub fn __zig_float2int(float: f64) isize {
    return @intFromFloat(float);
}
pub fn __zig_float2nat(float: f64) usize {
    return @intFromFloat(float);
}
pub fn __zig_float2string(float: f64) String_t {
    const buffer = std.fmt.allocPrint(debug_allocator, "{d}", .{float}) catch unreachable;
    return String_t{ .bytes = Array_t(u8){ .buffer = buffer, .size = buffer.len } };
}
pub fn __zig_string2int(string: []const u8) isize {
    return std.fmt.parseInt(isize, string, 10) catch unreachable;
}
pub fn __zig_string2float(string: []const u8) f64 {
    return std.fmt.parseFloat(f64, string) catch unreachable;
}

/// Pointer/Slice Allocation
pub fn __zig_alloc(comptime T: type, size: usize) []T {
    return debug_allocator.alloc(T, size) catch unreachable;
}
pub fn __zig_slice2ptr(comptime T: type, slice: []T) *T {
    return @ptrCast(slice);
}
pub fn __zig_ptr2slice(comptime T: type, ptr: *T, size: usize) []T {
    return @as([*]T, @ptrCast(ptr))[0..size];
}

pub fn __zig_free(ptr: anytype) void {
    debug_allocator.free(ptr);
}
pub fn __zig_slice_size(slice: anytype) usize {
    return slice.len;
}
// pub fn __zig_slice2ptr(slice: anytype) *std.meta.Child(@TypeOf(slice)) {
//     return @ptrCast(slice);
// }
// pub fn __zig_ptr2slice(ptr: anytype, size: usize) []std.meta.Child(@TypeOf(ptr)) {
//     const T = std.meta.Child(@TypeOf(ptr));
//     return @as([*]T, @ptrCast(ptr))[0..size];
// }

fn string_from_literal(comptime literal: []const u8) String_t {
    const buffer = debug_allocator.dupe(u8, literal) catch unreachable;
    const char_array = Array_t(u8){
        .buffer = buffer,
        .size = literal.len,
    };
    return String_t{ .bytes = char_array };
}

pub fn main(init: std.process.Init) !void {
    // gc_allocator = GC.allocator();
    debug_allocator = debug_allocator_state.allocator();
    global_io = init.io;
    main_0();
}

pub const String_t = struct { bytes: Array_t(u8) };
pub fn Array_t(comptime T0: type) type {
    return struct { buffer: []T0, size: usize };
}
