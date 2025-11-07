const std = @import("std");
// const GC = @import("gc").bdwgc;
var debug_allocator_state = std.heap.DebugAllocator(.{}){};
var debug_allocator: std.mem.Allocator = undefined;
// var gc_allocator: std.mem.Allocator = undefined;

pub fn __zig_print_0(value: String_t) void {
    _ = std.fs.File.stdout().write(value.bytes.buffer) catch unreachable;
    _ = std.fs.File.stdout().write("\n") catch unreachable;
}
pub fn __zig_panic_0(value: String_t) noreturn {
    _ = std.fs.File.stdout().write("\x1B[90mPanic: ") catch unreachable;
    _ = std.fs.File.stdout().write(value.bytes.buffer) catch unreachable;
    _ = std.fs.File.stdout().write("\x1B[0m\n") catch unreachable;
    std.process.exit(1);
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
    return @divTrunc(left, right);
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
pub fn __zig_alloc(comptime T: type, size: usize) []T {
    return debug_allocator.alloc(T, size) catch unreachable;
}
pub fn __zig_slice2ptr(comptime T: type, slice: []T) *T {
    return @ptrCast(slice);
}
pub fn __zig_ptr2slice(comptime T: type, ptr: *T, size: usize) []T {
    return @as([*]T, @ptrCast(ptr))[0..size];
}

pub fn __zig_free_0(ptr: anytype) void {
    debug_allocator.free(ptr);
}
pub fn __zig_slice_size_0(slice: anytype) usize {
    return slice.len;
}
// pub fn __zig_slice2ptr_0(slice: anytype) *std.meta.Child(@TypeOf(slice)) {
//     return @ptrCast(slice);
// }
// pub fn __zig_ptr2slice_0(ptr: anytype, size: usize) []std.meta.Child(@TypeOf(ptr)) {
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

pub fn main() !void {
    // gc_allocator = GC.allocator();
    debug_allocator = debug_allocator_state.allocator();
    main_0();
}

//////////////////////
/// user code
/////////////////////
pub const String_t = struct {
  bytes: Array_t(u8)
};
pub fn Array_t(comptime T0: type) type {
  return struct {
    buffer: []T0,
    size: usize
  };
}
pub fn Option_t(comptime T0: type) type {
  return union(enum) {
    Some: struct {T0},
    None: struct {},
    
  };
}

pub fn main_0() void {
  return @"block$1":  {
    var greeting: String_t = undefined; greeting = string_from_literal("Hello World");
    print_0(string_0(1234));
    print_0(greeting);
    var unused: String_t = undefined; unused = string_from_literal("I'm not used anywehere");
    var a: isize = undefined; a = 0;
    var b: isize = undefined; b = 1;
    @"block$2":  {
      const @"temp$0" = a;
      if (@"temp$0" < 0) break :@"block$2" print_0(string_0(a));
      if (@"temp$0" < -10) break :@"block$2" print_0(string_from_literal("will not print"));
      break :@"block$2" @"block$3":  {
        b = b + 1;
        break :@"block$3" print_0(string_0(b));
      };
    }
    var @"bool": bool = undefined; @"bool" = true;
    var nat: usize = undefined; nat = nat_0(1);
    var int: isize = undefined; int = -2;
    var float: f64 = undefined; float = 3.14;
    print_0(generic_0(string_from_literal("IT WORKED!!!")));
    drop_0(string_from_literal("drop me"));
    print_0(string_1(@"+_1"(@"bool", nat)));
    print_0(string_0(@"-_0"(nat, int)));
    print_0(string_0(@"/_0"(int, @"bool")));
    print_0(string_1(@"/_1"(nat, nat)));
    print_0(string_0(@"/_2"(nat, int)));
    print_0(string_0(@"/_3"(int, nat)));
    print_0(string_0(@"/_4"(int, int)));
    print_0(string_2(@"/_5"(float, float)));
    var slice: []String_t = undefined; slice = __zig_alloc(String_t, nat_0(10));
    print_1(size_0(slice));
    free_0(slice);
    var opt: Option_t(isize) = undefined; opt = @"Option.Some_0"(5);
    break :@"block$1" print_0(string_from_literal("done"));
  };
}
pub fn string_0(self: isize) String_t {
  return __zig_int2string_0(self);
}
pub fn string_1(self: usize) String_t {
  return __zig_nat2string_0(self);
}
pub fn string_2(self: f64) String_t {
  return __zig_float2string_0(self);
}
pub fn string_3(self: String_t) String_t {
  return self;
}
pub fn print_0(val: String_t) void {
  return __zig_print_0(string_3(val));
}
pub fn print_1(val: usize) void {
  return __zig_print_0(string_1(val));
}
pub fn @"<_0"(self: isize, other: isize) bool {
  return __zig_lt_0(self, other);
}
pub fn @"+_0"(self: isize, other: isize) isize {
  return __zig_add_int_0(self, other);
}
pub fn @"+_1"(self: bool, other: usize) usize {
  return __zig_add_uint_0(nat_1(self), other);
}
pub fn nat_0(self: isize) usize {
  return __zig_int2nat_0(self);
}
pub fn nat_1(self: bool) usize {
  return nat_0(@"block$1":  {
    if (self) break :@"block$1" 1;
    break :@"block$1" 0;
  });
}
pub fn generic_0(t: String_t) String_t {
  return t;
}
pub fn drop_0(self: String_t) void {
  return drop_1(bytes_0(self));
}
pub fn drop_1(self: Array_t(u8)) void {
  return free_1(buffer_0(self));
}
pub fn @"-_0"(self: usize, other: isize) isize {
  return __zig_sub_int_0(int_0(self), other);
}
pub fn @"/_0"(self: isize, other: bool) isize {
  return __zig_div_int_0(self, int_1(other));
}
pub fn @"/_1"(self: usize, other: usize) usize {
  return __zig_div_uint_0(self, other);
}
pub fn @"/_2"(self: usize, other: isize) isize {
  return __zig_div_int_0(int_0(self), other);
}
pub fn @"/_3"(self: isize, other: usize) isize {
  return __zig_div_int_0(self, int_0(other));
}
pub fn @"/_4"(self: isize, other: isize) isize {
  return __zig_div_int_0(self, other);
}
pub fn @"/_5"(self: f64, other: f64) f64 {
  return __zig_div_float_0(self, other);
}
pub fn size_0(self: []String_t) usize {
  return __zig_slice_size_0(self);
}
pub fn free_0(self: []String_t) void {
  return __zig_free_0(self);
}
pub fn free_1(self: []u8) void {
  return __zig_free_0(self);
}
pub fn @"Option.Some_0"(@"0": isize) Option_t(isize) {
  return @"block$1":  {
    
  };
}
pub fn bytes_0(self: String_t) Array_t(u8) {
  return self.bytes;
}
pub fn int_0(self: usize) isize {
  return __zig_nat2int_0(self);
}
pub fn int_1(self: bool) isize {
  return @"block$1":  {
    if (self) break :@"block$1" 1;
    break :@"block$1" 0;
  };
}
pub fn buffer_0(self: Array_t(u8)) []u8 {
  return self.buffer;
}
