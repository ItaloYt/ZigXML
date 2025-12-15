const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zxml = b.addLibrary(.{
        .name = "zxml",
        .version = .{ .major = 1, .minor = 0, .patch = 1, },
        .root_module = b.addModule("zxml", .{
            .target = target,
            .optimize = optimize,
            .root_source_file = b.path("zxml.zig"),
        }),
    });

    b.installArtifact(zxml);
}
