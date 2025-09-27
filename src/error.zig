const std = @import("std");

const module = @import("module.zig");

/// A function can take a reference to this to pass extra error information to the caller.
/// A function that does this guarantees the reference will be populated if it returns error.SeeContext.
/// Error implements a format function.
/// The same error instance can be re-used for multiple calls.
///
/// Example usage:
/// ----
/// var zware_error: Error = undefined;
/// foo(&zware_error) catch |err| switch (err) {
///     error.SeeContext => std.log.err("foo failed: {}", .{zware_error}),
///     else => |err| return err,
/// };
/// ---
pub const Error = union(enum) {
    missing_import: module.Import,
    any: anyerror,

    /// Called by a function that wants to both populate this error instance and let the caller
    /// know it's been populated by returning error.SeeContext.
    pub fn set(self: *Error, e: Error) error{SeeContext} {
        self.* = e;
        return error.SeeContext;
    }
    pub fn format(
        self: Error,
        writer: *std.Io.Writer,
    ) !void {
        switch (self) {
            .missing_import => |import| try writer.print(
                "missing {s} import '{s}' from module '{s}'",
                .{ @tagName(import.desc_tag), import.name, import.module },
            ),
            .any => |e| try writer.print("{s}", .{@errorName(e)}),
        }
    }
};
