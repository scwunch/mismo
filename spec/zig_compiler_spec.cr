require "./spec_helper"


describe "call_zig" do
  it "compiles and runs a simple zig program" do
    zig_code = <<-ZIG
    const std = @import("std");
    const stdout = std.fs.File.stdout();

    pub fn main() !void {
        // const stdout = std.io.getStdOut().writer();
        std.debug.print("hello from zig", .{});
        _ = try stdout.write("hello from zig\\n");
    }
    ZIG

    result = call_zig(zig_code)
    result.should eq("hello from zig\n")
  end

  it "can return and parse a number from zig" do
    zig_code = <<-ZIG
    const std = @import("std");
    const stdout = std.fs.File.stdout();

    pub fn main() !void {
        // const stdout = std.io.getStdOut().writer();
        _ = try stdout.write("42");
    }
    ZIG

    result = call_zig(zig_code)
    result.should eq "42"
    result.to_i?.should eq 42
  end

#   it "can return and parse a JSON object from zig" do
#     zig_code = <<-ZIG
#     const std = @import("std");

#     pub fn main() !void {
#         const stdout = std.io.getStdOut().writer();
#         try stdout.print("{\"a\": 1, \"b\": \"hello\"}", .{});
#     }
#     ZIG

#     result_str = call_zig(zig_code)
#     json_obj = JSON.parse(result_str)
#     json_obj["a"].as_i.should eq(1)
#     json_obj["b"].as_s.should eq("hello")
#   end
end
