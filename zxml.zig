//! This module is useful for parsing and retrieving information about xml files.

const std = @import("std");

pub const UnicodeError = error {
    OutOfBounds,
    CorruptedCodepoint,
    InvalidCodepoint,
    NotImplemented,
};

pub const CodepointIterator = struct {
    data: []const u8,
    offset: u32 = 0,
    unicode: Unicode,
    
    pub fn next(self: *CodepointIterator) UnicodeError!u21 {
        const value = try self.unicode.getCodepoint(self.data[self.offset..]);
        const size = try self.unicode.getCodepointSize(self.data[self.offset]);
        self.offset += size;
        return value;
    }

    pub fn peekn(self: *CodepointIterator) UnicodeError!u21 {
        return try self.unicode.getCodepoint(self.data[self.offset..]);
    }

    pub fn isEmpty(self: *CodepointIterator) bool {
        return self.offset == self.data.len;
    }
};

pub const Unicode = struct {
    getCodepointSize: *const fn(codepoint: u8) UnicodeError!u8,
    getCodepoint: *const fn(data: []const u8) UnicodeError!u21,
    fromCodepoint: *const fn(codepoint: u21) []const u8,
    decode: *const fn(data: []const u8, allocator: std.mem.Allocator) (std.mem.Allocator.Error || UnicodeError)!std.ArrayList(u21),
    encode: *const fn(data: []const u21, allocator: std.mem.Allocator) (std.mem.Allocator.Error || UnicodeError)!std.ArrayList(u8),
};

pub const UnicodeUTF8 = struct {
    /// Calculates the utf-8 codepoint size using its first byte
    pub fn getCodepointSize(codepoint: u8) UnicodeError!u8 {
        if (codepoint & 0x80 == 0x00) return 1; // codepoint & 0b10000000 == 0b00000000
        if (codepoint & 0xe0 == 0xc0) return 2; // codepoint & 0b11100000 == 0b11000000
        if (codepoint & 0xf0 == 0xe0) return 3; // codepoint & 0b11110000 == 0b11100000
        if (codepoint & 0xf8 == 0xf0) return 4; // codepoint & 0b11111000 == 0b11110000
        return UnicodeError.InvalidCodepoint;
    }

    /// Converts an utf-8 slice of bytes into an u21 codepoint
    pub fn getCodepoint(data: []const u8) UnicodeError!u21 {
        if (data.len == 0) return UnicodeError.OutOfBounds;

        const size = try UnicodeUTF8.getCodepointSize(data[0]);
        if (data.len < size) return UnicodeError.OutOfBounds;

        for (data[1..size]) |byte| if (byte & 0xc0 != 0x80) return UnicodeError.CorruptedCodepoint;

        switch (size) {
            1 => return data[0] & 0x7f, // & 0b01111111
            2 => return 
                (@as(u21, data[0] & 0x1f) <<  6) | // & 0b00011111
                (@as(u21, data[1] & 0x3f) <<  0),  // & 0b00111111
            3 => return 
                (@as(u21, data[0] & 0x0f) << 12) | // & 0b00001111
                (@as(u21, data[1] & 0x3f) <<  6) | // & 0b00111111
                (@as(u21, data[2] & 0x3f) <<  0),  // & 0b00111111
            4 => return 
                (@as(u21, data[0] & 0x07) << 18) | // & 0b00000111
                (@as(u21, data[1] & 0x3f) << 12) | // & 0b00111111
                (@as(u21, data[2] & 0x3f) <<  6) | // & 0b00111111
                (@as(u21, data[3] & 0x3f) <<  0),  // & 0b00111111
            else => unreachable,
        }
    }

    /// Converts an u21 codepoint to an utf-8 slice of bytes.
    pub fn fromCodepoint(codepoint: u21) []const u8 {
        var data: [4]u8 = undefined;
        const size: u8 = 
            if (codepoint <= 0x7f) 1
            else if (codepoint <= 0x7ff) 2
            else if (codepoint <= 0xffff) 3
            else 4;

        switch (size) {
            1 => data[0] = @intCast(codepoint & 0x7f),
            2 => { 
                data[0] = @intCast((codepoint >>  6) & 0x1f | 0xc0);
                data[1] = @intCast((codepoint >>  0) & 0x3f | 0x80);
            },
            3 => { 
                data[0] = @intCast((codepoint >> 12) & 0x0f | 0xe0);
                data[1] = @intCast((codepoint >>  6) & 0x3f | 0x80);
                data[2] = @intCast((codepoint >>  0) & 0x3f | 0x80); 
            },
            4 => { 
                data[0] = @intCast((codepoint >> 18) & 0x07 | 0xf0);
                data[1] = @intCast((codepoint >> 12) & 0x3f | 0x80);
                data[2] = @intCast((codepoint >>  6) & 0x3f | 0x80);
                data[3] = @intCast((codepoint >>  0) & 0x3f | 0x80);
            },
            else => unreachable,
        }

        return data[0..size];
    }

    /// Decodes an utf-8 slice of bytes into a list of u21 codepoints.
    pub fn decode(data: []const u8, allocator: std.mem.Allocator) (std.mem.Allocator.Error || UnicodeError)!std.ArrayList(u21) {
        var decoded = std.ArrayList(u21).empty;
        errdefer decoded.deinit(allocator);

        var offset: u32 = 0;
        while (offset < data.len) {
            try decoded.append(allocator, try UnicodeUTF8.getCodepoint(data[offset..]));

            offset += try UnicodeUTF8.getCodepointSize(data[offset]);
        }

        return decoded;
    }

    /// Encodes a slice of u21 codepoints into an utf-8 slice of bytes.
    pub fn encode(data: []const u21, allocator: std.mem.Allocator) (std.mem.Allocator.Error || UnicodeError)!std.ArrayList(u8) {
        var encoded = std.ArrayList(u8).empty;
        errdefer encoded.deinit(allocator);

        for (data) |codepoint| try encoded.appendSlice(allocator, UnicodeUTF8.fromCodepoint(codepoint));
        
        return encoded;
    }

    /// Retrieves the UnicodeUTF8 as an Unicode
    pub fn unicode() Unicode {
        return Unicode {
            .getCodepointSize = UnicodeUTF8.getCodepointSize,
            .getCodepoint = UnicodeUTF8.getCodepoint,
            .fromCodepoint = UnicodeUTF8.fromCodepoint,
            .decode = UnicodeUTF8.decode,
            .encode = UnicodeUTF8.encode
        };
    }
};

test "UnicodeUTF8 Codepoints" {
    std.debug.print("UnicodeUTF8 Codepoints\n", .{});

    const data = "❤️ amor é felicidade 😁";
    var offset: u32 = 0;

    while (offset < data.len) {
        const size: u8 = (try UnicodeUTF8.getCodepointSize(data[offset]));
        const codepoint = try UnicodeUTF8.getCodepoint(data[offset..]);
        std.debug.print("zig-glypheme='{s}', glypheme='{u}', size={}\n", .{ data[offset..(offset + size)], codepoint, size });

        offset += size;
    }

    std.debug.print("Success!\n\n", .{});
}

test "UnicodeUTF8 Decoding" {
    std.debug.print("UnicodeUTF8 Decoding\n", .{});

    const data = "❤️ amor é felicidade 😁";
    var decoded = try UnicodeUTF8.decode(data, std.heap.page_allocator);
    defer decoded.deinit(std.heap.page_allocator);

    std.debug.print("{} codepoints!\n", .{ decoded.items.len });

    for (decoded.items, 0..) |codepoint, i| {
        std.debug.print("codepoint-{} = 0x{x:08} = '{u}'\n", .{ i, codepoint, codepoint });
    }

    std.debug.print("Success!\n\n", .{});
}

test "UnicodeUTF8 Encoding" {
    std.debug.print("UnicodeUTF8 Encoding\n", .{});

    const data: []const u21 = &[_]u21{ 0x00002764, 0x0000fe0f, 0x00000020, 0x00000061, 0x0000006d, 0x0000006f, 0x00000072, 0x00000020, 0x000000e9, 0x00000020, 0x00000066, 0x00000065, 0x0000006c, 0x00000069, 0x00000063, 0x00000069, 0x00000064, 0x00000061, 0x00000064, 0x00000065, 0x00000020, 0x0001f601, };

    var encoded = try UnicodeUTF8.encode(data, std.heap.page_allocator);
    defer encoded.deinit(std.heap.page_allocator);

    std.debug.print("Encoded: {s}\n", .{ encoded.items });

    std.debug.print("Success!\n\n", .{});
}

pub const Error = error {
    NotImplemented,
    MultipleDoctypedeclFound,
    ExpectedXMLDeclStartTag,
    ExpectedXMLEndTag,
    ExpectedWhiteSpace,
    ExpectedVersionField,
    ExpectedEq,
    ExpectedDoubleQuote,
    ExpectedSingleQuote,
    ExpectedString,
    ExpectedVersionNum,
    SubstringNotFound,
    CodepointNotFound,
    ExpectedAlphabetic,
    ExpectedStandaloneField,
    ExpectedYesOrNo,
    ExpectedMisc,
    ExpectedCommentStartTag,
    ExpectedCommentEndTag,
    InvalidComment,
    InvalidChar,
    ExpectedXMLStartTag,
    ReservedPITargetName,
    InvalidNameStartChar,
    InvalidNameChar,
    ExpectedDoctypedeclStartTag,
    ExpectedCloseTag,
    ExpectedCloseBracket,
    ExpectedSystemOrPublic,
    InvalidPubidChar,
    ExpectedMarkupdecl,
    ExpectedElementdeclStartTag,
    ExpectedContentspec,
    ExpectedOpenParenthesis,
    ExpectedCloseParenthesis,
    ExpectedMixedEndTag,
    ExpectedPCDATA,
    ExpectedChildren,
    ExpectedCp,
    ExpectedChoiceSeparator,
    ExpectedSeqSeparator,
    ExpectedAttlistDeclStartTag,
    ExpectedAttType,
    ExpectedStringType,
    ExpectedTokenizedType,
    ExpectedEnumeratedType,
    ExpectedNotation,
    ExpectedNameChar,
    ExpectedDefaultDecl,
    UnexpectedCodepoint,
    ExpectedReference,
    ExpectedAmpersand,
    ExpectedSemicolon,
    ExpectedHexadecimal,
    ExpectedDecimal,
    ExpectedCharRef,
    ExpectedEntityDecl,
    ExpectedEntityStartTag,
    ExpectedPercentageSign,
    ExpectedNData,
    ExpectedPEDef,
    ExpectedNotationDeclStartTag,
    ExpectedExternalOrPublicID,
    ExpectedPublic,
    ExpectedDeclSep,
    ExpectedOpenTag,
    ExpectedEndTag,
    EmptyCharData,
    ExpectedCDStart,
    ExpectedCDEnd,
    ExpectedEncodingField,
} || std.mem.Allocator.Error || UnicodeError;

/// Document = Prolog Element Misc*
pub const Document = struct {
    /// This field is intended to be accessed directly.
    prolog: Prolog,

    /// This field is intended to be accessed directly.
    element: Element,

    /// This field is intended to be accessed directly.
    misc: std.ArrayList(Misc),

    /// Matches a valid document in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Document {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var prolog = try Prolog.match(iter, allocator);
        errdefer prolog.deinit(allocator);

        var element = try Element.match(iter, allocator);
        errdefer element.deinit(allocator);

        var misc = std.ArrayList(Misc).empty;
        errdefer {
            for (misc.items) |*item| item.deinit(allocator);
            misc.deinit(allocator);
        }
        
        while (true) { try misc.append(allocator, Misc.match(iter, allocator) catch break); }

        return Document { .prolog = prolog, .element = element, .misc = misc };
    }

    /// Deinitializes a document returned by Document.match
    pub fn deinit(self: *Document, allocator: std.mem.Allocator) void {
        for (self.misc.items) |*item| item.deinit(allocator);
        self.misc.deinit(allocator);
        self.element.deinit(allocator);
        self.prolog.deinit(allocator);
    }
};

/// Prolog = XMLDecl? Misc* (doctypedecl Misc*)?
pub const Prolog = struct {
    /// This field is intended to be accessed directly.
    xmldecl: ?XMLDecl,

    /// This field is intended to be accessed directly.
    doctypedecl: ?Doctypedecl,

    /// This field is intended to be accessed directly.
    misc: std.ArrayList(Misc),

    /// Matches a valid prolog in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Prolog {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var xmldecl = XMLDecl.match(iter, allocator) catch null;
        errdefer if (xmldecl) |*decl| decl.deinit(allocator);

        var doctypedecl: ?Doctypedecl = null;
        errdefer if (doctypedecl) |*v| v.deinit(allocator);

        var misc = std.ArrayList(Misc).empty;
        errdefer {
            for (misc.items) |*item| item.deinit(allocator);
            misc.deinit(allocator);
        }

        while(true) {
            if (Misc.match(iter, allocator)) |m| { try misc.append(allocator, m); continue; } else |_| {}

            var doctype = Doctypedecl.match(iter, allocator) catch break;
            errdefer doctype.deinit(allocator);

            if (doctypedecl != null) return Error.MultipleDoctypedeclFound;
            doctypedecl = doctype;
        }

        return Prolog { .xmldecl = xmldecl, .doctypedecl = doctypedecl, .misc = misc };
    }

    /// Deinitializes a prolog returned by Prolog.match
    pub fn deinit(self: *Prolog, allocator: std.mem.Allocator) void {
        for (self.misc.items) |*item| item.deinit(allocator);
        self.misc.deinit(allocator);
        if (self.doctypedecl) |*decl| decl.deinit(allocator);
        if (self.xmldecl) |*decl| decl.deinit(allocator);
    }
};

/// XMLDecl = '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
pub const XMLDecl = struct {
    /// This field is intended to be accessed directly.
    version: VersionInfo,

    /// This field is intended to be accessed directly.
    encoding: ?EncodingDecl,

    /// This field is intended to be accessed directly.
    standalone: ?SDDecl,

    /// Matches a valid XMLDecl in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!XMLDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<?xml") catch return Error.ExpectedXMLDeclStartTag;

        var version = try VersionInfo.match(iter, allocator);
        errdefer version.deinit(allocator);

        var encoding: ?EncodingDecl = EncodingDecl.match(iter, allocator) catch null;
        errdefer if (encoding) |*v| v.deinit(allocator);

        var standalone: ?SDDecl = SDDecl.match(iter, allocator) catch null;
        errdefer if (standalone) |*v| v.deinit(allocator);

        S.match(iter) catch {};
        Label.match(iter, "?>") catch return Error.ExpectedXMLEndTag;

        return XMLDecl { .version = version, .encoding = encoding, .standalone = standalone };
    }

    /// Deinitializes a XMLDecl returned by XMLDecl.match
    pub fn deinit(self: *XMLDecl, allocator: std.mem.Allocator) void {
        if (self.standalone) |*standalone| standalone.deinit(allocator);
        if (self.encoding) |*encoding| encoding.deinit(allocator);
        self.version.deinit(allocator);
    }
};

/// Matches substrings in the text.
pub const Label = struct {
    /// Matches a specific substring str in the text or returns an error.
    pub fn match(iter: *CodepointIterator, str: []const u8) Error!void {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var count: u32 = 0;
        while (count < str.len) : (count += 1) {
            // std.debug.print("{x:02}('{u}') vs {x:02}('{c}')\n", .{ try iter.peekn(), try iter.peekn(), str[count], str[count] });
            if (try iter.next() == str[count]) continue;

            return Error.SubstringNotFound;
        }
    }

    /// Matches a specific codepoint or returns an error.
    pub fn matchCodepoint(iter: *CodepointIterator, codepoint: u21) Error!void {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (try iter.next() == codepoint) return;
        return Error.CodepointNotFound;
    }
};

test "Label" {
    std.debug.print("Label\n", .{});

    var iter = CodepointIterator {
        .data = "Simple Iterator 🔥",
        .offset = 0,
        .unicode = UnicodeUTF8.unicode(),
    };

    try Label.match(&iter, "Simple Iterator ");
    try Label.matchCodepoint(&iter, '🔥');

    std.debug.print("Success!\n\n", .{});
}

/// VersionInfo = S 'version' Eq ('\'' VersionNum '\'' | '"' VersionNum '"')
pub const VersionInfo = struct {
    /// This field is intended to be accessed directly.
    version: VersionNum,

    /// Matches a valid version info in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!VersionInfo {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        try S.match(iter);
        Label.match(iter, "version") catch return Error.ExpectedVersionField;
        try Eq.match(iter);

        if (Label.matchCodepoint(iter, '"')) |_| {
            var version = try VersionNum.match(iter, allocator);
            errdefer version.deinit(allocator);

            Label.matchCodepoint(iter, '"') catch return Error.ExpectedDoubleQuote;
            return VersionInfo { .version = version };
        } else |_| {}

        if (Label.matchCodepoint(iter, '\'')) |_| {
            var version = try VersionNum.match(iter, allocator);
            errdefer version.deinit(allocator);

            Label.matchCodepoint(iter, '\'') catch return Error.ExpectedSingleQuote;
            return VersionInfo { .version = version };
        } else |_| {}

        return Error.ExpectedString;
    }

    /// Deinitializes a VersionInfo returned by VersionInfo.match
    pub fn deinit(self: *VersionInfo, allocator: std.mem.Allocator) void {
        self.version.deinit(allocator);
    }
};

/// S = (#x20 | #x9 | #xd | #xa)+
pub const S = struct {
    /// Matches a valid white space in the text or returns an error.
    pub fn match(iter: *CodepointIterator) Error!void {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        try S.matchWhiteSpace(iter);
        while (true) { S.matchWhiteSpace(iter) catch break; }
    }

    fn matchWhiteSpace(iter: *CodepointIterator) Error!void {
        if (Label.matchCodepoint(iter, 0x20)) |_| return else |_| {}
        if (Label.matchCodepoint(iter, 0x09)) |_| return else |_| {}
        if (Label.matchCodepoint(iter, 0x0d)) |_| return else |_| {}
        if (Label.matchCodepoint(iter, 0x0a)) |_| return else |_| {}

        return Error.ExpectedWhiteSpace;
    }
};

/// Eq = S? '=' S?
pub const Eq = struct {
    /// Matches a valid equals in the text or returns an error.
    pub fn match(iter: *CodepointIterator) Error!void {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        S.match(iter) catch {};
        Label.matchCodepoint(iter, '=') catch return Error.ExpectedEq;
        S.match(iter) catch {};
    }
};

/// VersionNum = '1.' [0-9]+
pub const VersionNum = struct {
    /// This field is intended to be accessed directly.
    version: std.ArrayList(u21),

    /// Matches a valid version number in the text and retrieves it or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!VersionNum {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "1.") catch return Error.ExpectedVersionNum;
        if (!isNum(try iter.next())) return Error.ExpectedVersionNum;
        while (!iter.isEmpty() and isNum(try iter.peekn())) { _ = iter.next() catch {}; }

        return VersionNum { .version = try iter.unicode.decode(iter.data[offset..iter.offset], allocator) };
    }

    /// Deinitializes a VersionNum returned by VersionNum.match
    pub fn deinit(self: *VersionNum, allocator: std.mem.Allocator) void {
        self.version.deinit(allocator);
    }
};

/// EncodingDecl = S 'encoding' Eq ('"' EncName '"' | '\'' EncName '\'')
pub const EncodingDecl = struct {
    /// This field is intended to be accessed directly.
    encoding: EncName,

    /// Matches a valid encoding declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EncodingDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        try S.match(iter);
        Label.match(iter, "encoding") catch return Error.ExpectedEncodingField;
        try Eq.match(iter);

        if (Label.matchCodepoint(iter, '"')) |_| {
            var encoding = try EncName.match(iter, allocator);
            errdefer encoding.deinit(allocator);

            Label.matchCodepoint(iter, '"') catch return Error.ExpectedDoubleQuote;
            return EncodingDecl { .encoding = encoding };
        } else |_| {}

        if (Label.matchCodepoint(iter, '\'')) |_| {
            var encoding = try EncName.match(iter, allocator);
            errdefer encoding.deinit(allocator);

            Label.matchCodepoint(iter, '\'') catch return Error.ExpectedSingleQuote;
            return EncodingDecl { .encoding = encoding };
        } else |_| {}

        return Error.ExpectedString;
    }

    /// Deinitializes an EncodingDecl returned by EncodingDecl.match
    pub fn deinit(self: *EncodingDecl, allocator: std.mem.Allocator) void {
        self.encoding.deinit(allocator);
    }
};

/// EncName = [A-Za-z] ([A-Za-z0-9._] | '-')*
pub const EncName = struct {
    /// This field is intented to be accessed directly.
    encoding: std.ArrayList(u21),

    /// Matches a valid encoding name in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EncName {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (!isAlpha(try iter.next())) return Error.ExpectedAlphabetic;
        while (!iter.isEmpty() and EncName.isValid(try iter.peekn())) { _ = iter.next() catch {}; }

        return EncName { .encoding = try iter.unicode.decode(iter.data[offset..iter.offset], allocator) };
    }

    /// Deinitializes an EncName returned by EncName.match
    pub fn deinit(self: *EncName, allocator: std.mem.Allocator) void {
        self.encoding.deinit(allocator);
    }

    /// Verify if codepoint is valid.
    fn isValid(codepoint: u32) bool {
        return isAlpha(codepoint) or isNum(codepoint) or codepoint == '.' or codepoint == '_' or codepoint == '-';
    }
};

/// SDDecl = S 'standalone' Eq (('\'' ('yes' | 'no') '\'') | ('"' ('yes' | 'no') '"'))
pub const SDDecl = struct {
    /// This field is intented to be accessed directly.
    standalone: bool,

    /// Matches a valid standalone declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!SDDecl {
        _ = allocator;

        const offset = iter.offset;
        errdefer iter.offset = offset;

        try S.match(iter);
        Label.match(iter, "standalone") catch return Error.ExpectedStandaloneField;
        try Eq.match(iter);

        if (Label.matchCodepoint(iter, '\'')) |_| {
            const value = try SDDecl.getValue(iter);
            Label.matchCodepoint(iter, '\'') catch return Error.ExpectedSingleQuote;
            return SDDecl { .standalone = value };
        } else |_| {}

        if (Label.matchCodepoint(iter, '"')) |_| {
            const value = try SDDecl.getValue(iter);
            Label.matchCodepoint(iter, '"') catch return Error.ExpectedDoubleQuote;
            return SDDecl { .standalone = value };
        } else |_| {}

        return Error.ExpectedString;
    }

    /// Deinitializes a SDDecl returned by SDDecl.match
    pub fn deinit(self: *SDDecl, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }

    /// Converts a yes or no into a bool.
    fn getValue(iter: *CodepointIterator) Error!bool {
        if (Label.match(iter, "yes")) |_| return true else |_| {}
        if (Label.match(iter, "no")) |_| return false else |_| {}

        return Error.ExpectedYesOrNo;
    }
};

/// Misc = Comment | PI | S
pub const Misc = union(enum) {
    /// This field is intended to be accessed directly.
    comment: Comment,

    /// This field is intended to be accessed directly.
    pi: PI,

    /// This field is intended to be accessed directly.
    s: void,

    /// Matches a valid miscellaneous in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Misc {
        if (Comment.match(iter, allocator)) |comment| return Misc { .comment = comment } else |_| {}
        if (PI.match(iter, allocator)) |pi| return Misc { .pi = pi } else |_| {}
        if (S.match(iter)) |_| return Misc { .s = void{} } else |_| {}

        return Error.ExpectedMisc;
    }

    /// Deinitializes a Misc returned by Misc.match
    pub fn deinit(self: *Misc, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .comment => |*comment| comment.deinit(allocator),
            .pi => |*pi| pi.deinit(allocator),
            .s => return,
        }
    }
};

/// Comment = '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
pub const Comment = struct {
    /// This field is intented to be used directly.
    content: std.ArrayList(u21),

    /// Matches a valid comment in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Comment {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!--") catch return Error.ExpectedCommentStartTag;

        const start = iter.offset;
        var end = iter.offset;

        while (true) {
            end = iter.offset;
            if (Label.match(iter, "-->")) |_| break else |_| {}

            var c1 = try Char.match(iter, allocator);
            defer c1.deinit(allocator);

            if (c1.codepoint != '-') continue;

            var c2 = try Char.match(iter, allocator);
            defer c2.deinit(allocator);

            if (c2.codepoint == '-') return Error.InvalidComment;
        }

        return Comment { .content = try iter.unicode.decode(iter.data[start..end], allocator) };
    }

    /// Deinitializes a Comment returned by Comment.match
    pub fn deinit(self: *Comment, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// Char = #x9 | #xa | #xd | [#x20-#x7e] | #x85 | [#xa0-#xd7ff] | [#xe000-#xfdcf] | [#xfdf0-#xfffd] | [#x10000-#x1fffd] | [#x20000-#x2fffd] | [#x30000-#x3fffd] | [#x40000-#x4fffd] | [#x50000-#x5fffd] | [#x60000-#x6fffd] | [#x70000-#x7fffd] | [#x80000-#x8fffd] | [#x90000-#x9fffd] | [#xa0000-#xafffd] | [#xb0000-#xbfffd] | [#xc0000-#xcfffd] | [#xd0000-#xdfffd] | [#xe0000-#xefffd] | [#xf0000-#xffffd] | [#x100000-#x10FFFd]
pub const Char = struct {
    /// This field is intended to be accessed directly.
    codepoint: u32,

    /// Matches a valid char in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Char {
        _ = allocator;

        const offset = iter.offset;
        errdefer iter.offset = offset;

        const codepoint = try iter.next();
        if (
            codepoint == 0x9 or codepoint == 0xa or codepoint == 0xd or (codepoint >= 0x20 and codepoint <= 0x7e) or codepoint == 0x85 or (codepoint >= 0xa0 and codepoint <= 0xd7ff) or (codepoint >= 0xe000 and codepoint <= 0xfdcf) or (codepoint >= 0xfdf0 and codepoint <= 0xfffd) or
            (codepoint >= 0x10000 and codepoint <= 0x1fffd) or
            (codepoint >= 0x20000 and codepoint <= 0x2fffd) or
            (codepoint >= 0x30000 and codepoint <= 0x3fffd) or
            (codepoint >= 0x40000 and codepoint <= 0x4fffd) or
            (codepoint >= 0x50000 and codepoint <= 0x5fffd) or
            (codepoint >= 0x60000 and codepoint <= 0x6fffd) or
            (codepoint >= 0x70000 and codepoint <= 0x7fffd) or
            (codepoint >= 0x80000 and codepoint <= 0x8fffd) or
            (codepoint >= 0x90000 and codepoint <= 0x9fffd) or
            (codepoint >= 0xa0000 and codepoint <= 0xafffd) or
            (codepoint >= 0xb0000 and codepoint <= 0xbfffd) or
            (codepoint >= 0xc0000 and codepoint <= 0xcfffd) or
            (codepoint >= 0xd0000 and codepoint <= 0xdfffd) or
            (codepoint >= 0xe0000 and codepoint <= 0xefffd) or
            (codepoint >= 0xf0000 and codepoint <= 0xffffd) or
            (codepoint >= 0x100000 and codepoint <= 0x10fffd)
        ) return Char { .codepoint = codepoint };

        return Error.InvalidChar;
    }

    /// Deinitializes a Char returned by Char.match
    pub fn deinit(self: *Char, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }
};

/// PI = '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
pub const PI = struct {
    /// This field is intended to be accessed directly.
    target: PITarget,

    /// This field is intended to be accessed directly.
    content: std.ArrayList(u21),

    /// Matches a valid processing instruction in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PI {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<?") catch return Error.ExpectedXMLStartTag;

        var target = try PITarget.match(iter, allocator);
        errdefer target.deinit(allocator);

        if (S.match(iter)) |_| {
            const start = iter.offset;
            var end = iter.offset;
            while (true) {
                end = iter.offset;
                if (Label.match(iter, "?>")) |_| break else |_| {}
                
                var c1 = try Char.match(iter, allocator);
                c1.deinit(allocator);
            }

            return PI { .target = target, .content = try iter.unicode.decode(iter.data[start..end], allocator) };
        } else |_| {}

        Label.match(iter, "?>") catch return Error.ExpectedXMLEndTag;

        return PI { .target = target, .content = std.ArrayList(u21).empty };
    }

    /// Deinitializes a PI returned by PI.match
    pub fn deinit(self: *PI, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
        self.target.deinit(allocator);
    }
};

/// PITarget = Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
pub const PITarget = struct {
    /// This field is intended to be accessed directly.
    target: Name,

    /// Matches a valid processing instruction target in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PITarget {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var target = try Name.match(iter, allocator);
        errdefer target.deinit(allocator);

        if (target.name.items.len == 3 and (target.name.items[0] == 'X' or target.name.items[0] == 'x') and (target.name.items[1] == 'M' or target.name.items[1] == 'm') and (target.name.items[2] == 'L' or target.name.items[2] == 'l')) return Error.ReservedPITargetName;
        
        return PITarget { .target = target };
    }

    /// Deinitializes a PITarget returned by PITarget.match
    pub fn deinit(self: *PITarget, allocator: std.mem.Allocator) void {
        self.target.deinit(allocator);
    }
};

/// Name = NameStartChar (NameChar)*
pub const Name = struct {
    /// This field is intended to be accessed directly.
    name: std.ArrayList(u21),

    /// Matches a valid name in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Name {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var c1 = try NameStartChar.match(iter, allocator);
        c1.deinit(allocator);

        while (true) { 
            var c2 = NameChar.match(iter, allocator) catch break;
            c2.deinit(allocator);
        }

        return Name { .name = try iter.unicode.decode(iter.data[offset..iter.offset], allocator) };
    }

    /// Deinitializes a Name returned by Name.match
    pub fn deinit(self: *Name, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};

/// NameStartChar = ':' | [A-Za-z] | '_' | [#xc0-#xd6] | [#xd8-#xf6] | [#xf8-#x2ff] | [#x370-#x37d] | [#x37f-#x1fff] | [#x200c-#x200d] | [#x2070-#x218f] | [#x2c00-#x2fef] | [#x3001-#xd7ff] | [#xf900-#xfdcf] | [#xfdf0-#xfffd] | [#x10000-#xeffff]
pub const NameStartChar = struct {
    /// This field is intended to be accessed directly.
    codepoint: u21,

    /// Matches a valid name start character in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!NameStartChar {
        _ = allocator;

        const offset = iter.offset;
        errdefer iter.offset = offset;

        const codepoint = try iter.next();
        if (codepoint == ':' or isAlpha(codepoint) or codepoint == '_' or (codepoint >= 0xc0 and codepoint <= 0xd6) or (codepoint >= 0xd8 and codepoint <= 0xf6) or (codepoint >= 0xf8 and codepoint <= 0x2ff) or (codepoint >= 0x370 and codepoint <= 0x37d) or (codepoint >= 0x37f and codepoint <= 0x1fff) or (codepoint >= 0x200c and codepoint <= 0x200d) or (codepoint >= 0x2070 and codepoint <= 0x218f) or (codepoint >= 0x2c00 and codepoint <= 0x2fef) or (codepoint >= 0x3001 and codepoint <= 0xd7ff) or (codepoint >= 0xf900 and codepoint <= 0xfdcf) or (codepoint >= 0xfdf0 and codepoint <= 0xfffd) or (codepoint >= 0x10000 and codepoint <= 0xeffff)) return NameStartChar { .codepoint = codepoint };

        return Error.InvalidNameStartChar;
    }

    /// Deinitializes a NameStartChar returned by NameStartChar.match
    pub fn deinit(self: *NameStartChar, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }
};

/// NameChar = NameStartChar | '-' | '.' | [0-9] | #xb7 | [#x300-#x36f] | [#x203f-#x2040]
pub const NameChar = struct {
    /// This field is intended to be accessed directly.
    codepoint: u21,

    /// Matches a valid name character in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!NameChar {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var c1 = NameStartChar.match(iter, allocator);
        if (c1) |*c| {
            defer c.deinit(allocator);
            return NameChar { .codepoint = c.codepoint };
        } else |_| {}

        const codepoint = try iter.next();

        if (codepoint == '-' or codepoint == '.' or isNum(codepoint) or codepoint == 0xb7 or (codepoint >= 0x300 and codepoint <= 0x36f) or (codepoint >= 0x203f and codepoint <= 0x2040)) return NameChar { .codepoint = codepoint };

        return Error.InvalidNameChar;
    }

    /// Deinitializes a NameChar returned by Namechar.match
    pub fn deinit(self: *NameChar, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }
};

/// Doctypedecl = '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
pub const Doctypedecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    external: ?ExternalID,

    /// This field is intended to be accessed directly.
    intSubset: ?IntSubset,

    /// Matches a valid document type declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Doctypedecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!DOCTYPE") catch return Error.ExpectedDoctypedeclStartTag;
        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        var external: ?ExternalID = null;
        errdefer if (external) |*v| v.deinit(allocator);

        var intSubset: ?IntSubset = null;
        errdefer if (intSubset) |*v| v.deinit(allocator);

        S.match(iter) catch {};
        if (ExternalID.match(iter, allocator)) |v| {
            external = v;
            S.match(iter) catch {};
        } else |_| {}

        if (Label.matchCodepoint(iter, '[')) |_| {
            intSubset = try IntSubset.match(iter, allocator);
            Label.matchCodepoint(iter, ']') catch return Error.ExpectedCloseBracket;
            S.match(iter) catch {};
        } else |_| {}

        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return Doctypedecl { .name = name, .external = external, .intSubset = intSubset };
    }

    /// Deinitializes a Doctypedecl returned by Doctypedecl.match
    pub fn deinit(self: *Doctypedecl, allocator: std.mem.Allocator) void {
        if (self.intSubset) |*intSubset| intSubset.deinit(allocator);
        if (self.external) |*external| external.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// ExternalID = 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
pub const ExternalID = union(enum) {
    /// This field is intended to be accessed directly.
    system: SystemLiteral,

    /// This field is intended to be accessed directly.
    public: struct { pubid: PubidLiteral, system: SystemLiteral, },

    /// Matches a valid external id in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!ExternalID {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (Label.match(iter, "SYSTEM")) |_| {
            try S.match(iter);
            return ExternalID { .system = try SystemLiteral.match(iter, allocator) };
        } else |_| {}

        if (Label.match(iter, "PUBLIC")) |_| {
            try S.match(iter);
            var pubid = try PubidLiteral.match(iter, allocator);
            errdefer pubid.deinit(allocator);
            try S.match(iter);
            return ExternalID { .public = .{ .pubid = pubid, .system = try SystemLiteral.match(iter, allocator) } };
        } else |_| {}

        return Error.ExpectedSystemOrPublic;
    }

    /// Deinitializes a ExternalID returned by ExternalID.match
    pub fn deinit(self: *ExternalID, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .system => |*system| system.deinit(allocator),
            .public => |*public| { 
                public.pubid.deinit(allocator);
                public.system.deinit(allocator);
            },
        }
    }
};

/// SystemLiteral = '"' [^"]* '"' | '\'' [^']* '\''
pub const SystemLiteral = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(u21),

    /// Matches a valid system literal in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!SystemLiteral {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (Label.matchCodepoint(iter, '"')) |_| {
            const start = iter.offset;
            var end = iter.offset;
            while (!iter.isEmpty()) {
                end = iter.offset;
                Label.matchCodepoint(iter, '"') catch continue;

                return SystemLiteral { .content = try iter.unicode.decode(iter.data[start..end], allocator) };
            }

            return Error.ExpectedDoubleQuote;
        } else |_| {}

        if (Label.matchCodepoint(iter, '\'')) |_| {
            const start = iter.offset;
            var end = iter.offset;
            while (!iter.isEmpty()) {
                end = iter.offset;
                Label.matchCodepoint(iter, '\'') catch continue;

                return SystemLiteral { .content = try iter.unicode.decode(iter.data[start..end], allocator) };
            }

            return Error.ExpectedSingleQuote;
        } else |_| {}

        return Error.ExpectedString;
    }

    /// Deinitializes a SystemLiteral returned by SystemLiteral.match
    pub fn deinit(self: *SystemLiteral, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// PubidLiteral = '"' PubidChar* '"' | '\'' (PubidChar - '\'')* '\''
pub const PubidLiteral = struct {
    content: std.ArrayList(u21),

    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PubidLiteral {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (Label.matchCodepoint(iter, '"')) |_| {
            const start = iter.offset;
            while (true) {
                var c1 = PubidChar.match(iter, allocator) catch break;
                c1.deinit(allocator);
            }

            const end = iter.offset;

            Label.matchCodepoint(iter, '"') catch return Error.ExpectedDoubleQuote;

            return PubidLiteral { .content = try iter.unicode.decode(iter.data[start..end], allocator) };
        } else |_| {}

        if (Label.matchCodepoint(iter, '\'')) |_| {
            const start = iter.offset;
            while (true) { 
                var c1 = PubidChar.match(iter, allocator) catch break;
                c1.deinit(allocator);
            }

            const end = iter.offset;

            Label.matchCodepoint(iter, '\'') catch return Error.ExpectedSingleQuote;

            return PubidLiteral { .content = try iter.unicode.decode(iter.data[start..end], allocator) };
        } else |_| {}

        return Error.ExpectedString;
    }

    pub fn deinit(self: *PubidLiteral, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// PubidChar = #x20 | #xd | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
pub const PubidChar = struct {
    /// This field is intended to be accessed directly.
    codepoint: u21,

    /// Matches a valid pubid char in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PubidChar {
        _ = allocator;
        const offset = iter.offset;
        errdefer iter.offset = offset;

        const codepoint = try iter.next();
        if (codepoint == 0x20 or codepoint == 0xd or codepoint == 0xa or isAlpha(codepoint) or isNum(codepoint) or codepoint == '-' or codepoint == '\'' or codepoint == '(' or codepoint == ')' or codepoint == '+' or codepoint == ',' or codepoint == '.' or codepoint == '/' or codepoint == ':' or codepoint == '=' or codepoint == '?' or codepoint == ';' or codepoint == '!' or codepoint == '*' or codepoint == '#' or codepoint == '@' or codepoint == '$' or codepoint == '_' or codepoint == '%') return PubidChar { .codepoint = codepoint };

        return Error.InvalidPubidChar;
    }

    /// Deinitializes a PubidChar returned by PubidChar.match
    pub fn deinit(self: *PubidChar, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }
};

/// IntSubset = (markupdecl | DeclSep)*
pub const IntSubset = struct {
    /// This field is intended to be accessed directly.
    items: std.ArrayList(union(enum) { markupdecl: Markupdecl, declsep: DeclSep }),

    /// Matches an internal subset in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!IntSubset {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var items = @FieldType(IntSubset, "items").empty;
        errdefer {
            for (items.items) |*item| switch (item.*) {
                .markupdecl => |*markup| markup.deinit(allocator),
                .declsep => |*decl| decl.deinit(allocator),
            };
            items.deinit(allocator);
        }

        while (true) {
            if (Markupdecl.match(iter, allocator)) |markup| {
                try items.append(allocator, .{ .markupdecl = markup });
                continue;
            } else |_| {}

            if (DeclSep.match(iter, allocator)) |sep| {
                try items.append(allocator, .{ .declsep = sep });
                continue;
            } else |_| {}

            break;
        }

        return IntSubset { .items = items };
    }

    /// Deinitializes an IntSubset returned by IntSubset.match
    pub fn deinit(self: *IntSubset, allocator: std.mem.Allocator) void {
        for (self.items.items) |*item| switch (item.*) {
            .markupdecl => |*markup| markup.deinit(allocator),
            .declsep => |*sep| sep.deinit(allocator),
        };
        self.items.deinit(allocator);
    }
};

/// MarkupDecl = Elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
pub const Markupdecl = union(enum) {
    /// This field is intended to be accessed directly.
    elementdecl: Elementdecl,

    /// This field is intended to be accessed directly.
    attlistdecl: AttlistDecl,

    /// This field is intended to be accessed directly.
    entitydecl: EntityDecl,

    /// This field is intended to be accessed directly.
    notationdecl: NotationDecl,

    /// This field is intended to be accessed directly.
    pi: PI,

    /// This field is intended to be accessed directly.
    comment: Comment,

    /// Matches a valid markup declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Markupdecl {
        if (Elementdecl.match(iter, allocator)) |elementdecl| return Markupdecl { .elementdecl = elementdecl } else |_| {}
        if (AttlistDecl.match(iter, allocator)) |attlistdecl| return Markupdecl { .attlistdecl = attlistdecl } else |_| {}
        if (EntityDecl.match(iter, allocator)) |entitydecl| return Markupdecl { .entitydecl = entitydecl } else |_| {}
        if (NotationDecl.match(iter, allocator)) |notationdecl| return Markupdecl { .notationdecl = notationdecl } else |_| {}
        if (PI.match(iter, allocator)) |pi| return Markupdecl { .pi = pi } else |_| {}
        if (Comment.match(iter, allocator)) |comment| return Markupdecl { .comment = comment } else |_| {}

        return Error.ExpectedMarkupdecl;
    }

    /// Deinitializes a Markupdecl returned by Markupdecl.match
    pub fn deinit(self: *Markupdecl, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .elementdecl => |*elementdecl| elementdecl.deinit(allocator),
            .attlistdecl => |*attlistdecl| attlistdecl.deinit(allocator),
            .entitydecl => |*entitydecl| entitydecl.deinit(allocator),
            .notationdecl => |*notationdecl| notationdecl.deinit(allocator),
            .pi => |*pi| pi.deinit(allocator),
            .comment => |*comment| comment.deinit(allocator),
        }
    }
};

/// ElementDecl = '<!ELEMENT' S Name S Contentspec S? '>'
pub const Elementdecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    content: Contentspec,

    /// Matches a valid element declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Elementdecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!ELEMENT") catch return Error.ExpectedElementdeclStartTag;
        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        try S.match(iter);

        var content = try Contentspec.match(iter, allocator);
        errdefer content.deinit(allocator);

        S.match(iter) catch {};
        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return Elementdecl { .name = name, .content = content };
    }

    /// Deinitializes an Elementdecl returned by Elementdecl.match
    pub fn deinit(self: *Elementdecl, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// Contentspec = 'EMPTY' | 'ANY' | Mixed | Children
pub const Contentspec = union(enum) {
    /// This field is intended to be accessed directly.
    empty: void,

    /// This field is intended to be accessed directly.
    any: void,

    /// This field is intended to be accessed directly.
    mixed: Mixed,

    /// This field is intended to be accessed directly.
    children: Children,

    /// Matches a valid contentspec in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Contentspec {
        if (Label.match(iter, "EMPTY")) |_| return Contentspec { .empty = void{} } else |_| {}
        if (Label.match(iter, "ANY")) |_| return Contentspec { .any = void{} } else |_| {}
        if (Mixed.match(iter, allocator)) |mixed| return Contentspec { .mixed = mixed } else |_| {}
        if (Children.match(iter, allocator)) |children| return Contentspec { .children = children } else |_| {}

        return Error.ExpectedContentspec;
    }

    /// Deinitializes a Contentspec returned by Contentspec.match
    pub fn deinit(self: *Contentspec, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .empty => return,
            .any => return,
            .mixed => |*mixed| mixed.deinit(allocator),
            .children => |*children| children.deinit(allocator),
        }
    }
};

/// Mixed = '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
pub const Mixed = struct {
    /// This field is intended to be accessed directly.
    names: std.ArrayList(Name),

    /// Matches a valid mixed in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Mixed {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '(') catch return Error.ExpectedOpenParenthesis;
        S.match(iter) catch {};
        Label.match(iter, "#PCDATA") catch return Error.ExpectedPCDATA;
        S.match(iter) catch {};

        var names = std.ArrayList(Name).empty;
        errdefer {
            for (names.items) |*item| item.deinit(allocator);
            names.deinit(allocator);
        }

        if (Label.match(iter, ")*")) |_| return Mixed { .names = names } else |_| {}
        if (Label.matchCodepoint(iter, ')')) |_| return Mixed { .names = names } else |_| {}

        while (true) {
            Label.matchCodepoint(iter, '|') catch break;
            S.match(iter) catch {};
            try names.append(allocator, try Name.match(iter, allocator));
            S.match(iter) catch {};
        }

        Label.match(iter, ")*") catch return Error.ExpectedMixedEndTag;

        return Mixed { .names = names };
    }

    /// Deinitializes a Mixed returned by Mixed.match
    pub fn deinit(self: *Mixed, allocator: std.mem.Allocator) void {
        for (self.names.items) |*item| item.deinit(allocator);
        self.names.deinit(allocator);
    }
};

/// Optional character following a Children or a Cp
pub const Modifier = enum {
    optional,
    any,
    /// At Least One
    alo,
};

/// Children = (Choice | Seq) ('?' | '*' | '+')?
pub const Children = struct {
    /// This field is intended to be accessed directly.
    content: union(enum) { choice: Choice, seq: Seq },

    /// This field is intended to be accessed directly.
    modifier: ?Modifier,
    
    /// Matches a valid children in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Children {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var content: @FieldType(Children, "content") =
            if (Choice.match(iter, allocator)) |choice| .{ .choice = choice }
            else |_| if(Seq.match(iter, allocator)) |seq| .{ .seq = seq }
            else |_| return Error.ExpectedChildren;
        errdefer switch (content) {
            .choice => |*choice| choice.deinit(allocator),
            .seq => |*seq| seq.deinit(allocator),
        };

        const modifier: ?Modifier =
            if (Label.matchCodepoint(iter, '?')) |_| .optional
            else |_| if (Label.matchCodepoint(iter, '*')) |_| .any
            else |_| if (Label.matchCodepoint(iter, '+')) |_| .alo
            else |_| null;

        return Children { .content = content, .modifier = modifier };
    }

    /// Deinitializes a Children returned by Children.match
    pub fn deinit(self: *Children, allocator: std.mem.Allocator) void {
        switch (self.content) {
            .choice => |*choice| choice.deinit(allocator),
            .seq => |*seq| seq.deinit(allocator),
        }
    }
};

/// Choice = '(' S? Cp ( S? '|' S? cp )+ S? ')'
pub const Choice = struct {
    /// This field is intended to be accessed directly.
    cps: std.ArrayList(Cp),

    /// Matches a valid choice in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Choice {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '(') catch return Error.ExpectedOpenParenthesis;
        S.match(iter) catch {};

        var cps = try std.ArrayList(Cp).initCapacity(allocator, 2);
        errdefer cps.deinit(allocator);

        cps.items[0] = try Cp.match(iter, allocator);
        errdefer cps.items[0].deinit(allocator);

        S.match(iter) catch {};
        if (Label.matchCodepoint(iter, ')')) |_| return Error.ExpectedCp else |_| {}

        Label.matchCodepoint(iter, '|') catch return Error.ExpectedChoiceSeparator;
        S.match(iter) catch {};

        cps.items[1] = try Cp.match(iter, allocator);
        errdefer cps.items[1].deinit(allocator);

        S.match(iter) catch {};

        errdefer for (cps.items[2..]) |*item| item.deinit(allocator);

        while (true) {
            Label.matchCodepoint(iter, '|') catch break;
            S.match(iter) catch {};
            try cps.append(allocator, try Cp.match(iter, allocator));
            S.match(iter) catch {};
        }

        Label.matchCodepoint(iter, ')') catch return Error.ExpectedCloseParenthesis;

        return Choice { .cps = cps };
    }

    /// Deinitializes a Choice returned by Choice.match
    pub fn deinit(self: *Choice, allocator: std.mem.Allocator) void {
        for (self.cps.items) |*cp| cp.deinit(allocator);
        self.cps.deinit(allocator);
    }
};

/// Cp = (Name | Choice | Seq) ('?' | '*' | '+')?
pub const Cp = struct {
    /// This field is intended to be accessed directly.
    content: union(enum) { name: Name, choice: Choice, seq: Seq },

    /// This field is intended to be accessed directly.
    modifier: ?Modifier,

    /// Matches a valid content particle in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Cp {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var content: @FieldType(Cp, "content") =
            if (Name.match(iter, allocator)) |name| .{ .name = name }
            else |_| if (Choice.match(iter, allocator)) |choice| .{ .choice = choice }
            else |_| if (Seq.match(iter, allocator)) |seq| .{ .seq = seq }
            else |_| return Error.ExpectedCp;
        errdefer switch (content) {
            .name => |*name| name.deinit(allocator),
            .choice => |*choice| choice.deinit(allocator),
            .seq => |*seq| seq.deinit(allocator),
        };

        const modifier: ?Modifier =
            if (Label.matchCodepoint(iter, '?')) |_| .optional
            else |_| if (Label.matchCodepoint(iter, '*')) |_| .any
            else |_| if (Label.matchCodepoint(iter, '+')) |_| .alo
            else |_| null;

        return Cp { .content = content, .modifier = modifier };
    }
    
    /// Deinitializes a Cp returned by Cp.match
    pub fn deinit(self: *Cp, allocator: std.mem.Allocator) void {
        switch(self.content) {
            .name => |*name| name.deinit(allocator),
            .choice => |*choice| choice.deinit(allocator),
            .seq => |*seq| seq.deinit(allocator),
        }
    }
};

/// Seq = '(' S? Cp ( S? ',' S? cp )* S? ')'
pub const Seq = struct {
    /// This field is intended to be accessed directly.
    cps: std.ArrayList(Cp),

    /// Matches a valid sequence in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Seq {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '(') catch return Error.ExpectedOpenParenthesis;
        S.match(iter) catch {};

        var cps = try std.ArrayList(Cp).initCapacity(allocator, 1);
        errdefer cps.deinit(allocator);

        cps.items[0] = try Cp.match(iter, allocator);
        errdefer for (cps.items) |*item| item.deinit(allocator);

        S.match(iter) catch {};

        while (true) {
            Label.matchCodepoint(iter, ',') catch break;
            S.match(iter) catch {};
            try cps.append(allocator, try Cp.match(iter, allocator));
            S.match(iter) catch {};
        }

        Label.matchCodepoint(iter, ')') catch return Error.ExpectedCloseParenthesis;

        return Seq { .cps = cps };
    }

    /// Deinitializes a Seq returned by Seq.match
    pub fn deinit(self: *Seq, allocator: std.mem.Allocator) void {
        for (self.cps.items) |*cp| cp.deinit(allocator);
        self.cps.deinit(allocator);
    }
};

/// AttlistDecl = '<!ATTLIST' S Name AttDef* S? '>'
pub const AttlistDecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    defs: std.ArrayList(AttDef),

    /// Matches a valid attribute list declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!AttlistDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!ATTLIST") catch return Error.ExpectedAttlistDeclStartTag;
        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        var defs = std.ArrayList(AttDef).empty;
        errdefer {
            for (defs.items) |*item| item.deinit(allocator);
            defs.deinit(allocator);
        }

        while (true) { try defs.append(allocator, AttDef.match(iter, allocator) catch break); }
        S.match(iter) catch {};

        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return AttlistDecl { .name = name, .defs = defs };
    }

    /// Deinitializes an AttlistDecl returned by AttlistDecl.match
    pub fn deinit(self: *AttlistDecl, allocator: std.mem.Allocator) void {
        for (self.defs.items) |*item| item.deinit(allocator);
        self.defs.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// AttDef = S Name S AttType S DefaultDecl
pub const AttDef = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    type: AttType,

    /// This field is intended to be accessed directly.
    default: DefaultDecl,

    /// Matches a valid attribute definition in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!AttDef {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        try S.match(iter);

        var attType = try AttType.match(iter, allocator);
        errdefer attType.deinit(allocator);

        try S.match(iter);

        var default = try DefaultDecl.match(iter, allocator);
        errdefer default.deinit(allocator);

        return AttDef { .name = name, .type = attType, .default = default };
    }

    /// Deinitializes an AttDef returned by AttDef.match
    pub fn deinit(self: *AttDef, allocator: std.mem.Allocator) void {
        self.default.deinit(allocator);
        self.type.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// AttType = StringType | TokenizedType | EnumeratedType
pub const AttType = union(enum) {
    /// This field is intended to be accessed directly.
    string: StringType,

    /// This field is intended to be accessed directly.
    tokenized: TokenizedType,

    /// This field is intended to be accessed directly.
    enumerated: EnumeratedType,

    /// Matches a valid attribute type in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!AttType {
        if (StringType.match(iter, allocator)) |string| return AttType { .string = string } else |_| {}
        if (TokenizedType.match(iter, allocator)) |tokenized| return AttType { .tokenized = tokenized } else |_| {}
        if (EnumeratedType.match(iter, allocator)) |enumerated| return AttType { .enumerated = enumerated } else |_| {}
        
        return Error.ExpectedAttType;
    }

    /// Deinitializes an AttType returned by AttType.match
    pub fn deinit(self: *AttType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |*string| string.deinit(allocator),
            .tokenized => |*tokenized| tokenized.deinit(allocator),
            .enumerated => |*enumerated| enumerated.deinit(allocator),
        }
    }
};

/// StringType = 'CDATA'
pub const StringType = struct {
    /// Matches a valid string type in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!StringType {
        _ = allocator;

        Label.match(iter, "CDATA") catch return Error.ExpectedStringType;

        return StringType {};
    }

    /// Deinitializes a StringType returned by StringType.match
    pub fn deinit(self: *StringType, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }
};

/// TokenizedType = 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
pub const TokenizedType = union(enum) {
    /// This field is intended to be accessed directly.
    id: void,

    /// This field is intended to be accessed directly.
    idref: void,

    /// This field is intended to be accessed directly.
    idrefs: void,

    /// This field is intended to be accessed directly.
    entity: void,

    /// This field is intended to be accessed directly.
    entities: void,

    /// This field is intended to be accessed directly.
    nmtoken: void,

    /// This field is intended to be accessed directly.
    nmtokens: void,

    /// Matches a valid tokenized type in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!TokenizedType {
        _ = allocator;

        if (Label.match(iter, "ID")) |_| return TokenizedType { .id = void{} } else |_| {}
        if (Label.match(iter, "IDREF")) |_| return TokenizedType { .idref = void{} } else |_| {}
        if (Label.match(iter, "IDREFS")) |_| return TokenizedType { .idrefs = void{} } else |_| {}
        if (Label.match(iter, "ENTITY")) |_| return TokenizedType { .entity = void{} } else |_| {}
        if (Label.match(iter, "ENTITIES")) |_| return TokenizedType { .entities = void{} } else |_| {}
        if (Label.match(iter, "NMTOKEN")) |_| return TokenizedType { .nmtoken = void{} } else |_| {}
        if (Label.match(iter, "NMTOKENS")) |_| return TokenizedType { .nmtokens = void{} } else |_| {}

        return Error.ExpectedTokenizedType;
    }

    /// Deinitializes a TokenizedType returned by TokenizedType.match
    pub fn deinit(self: *TokenizedType, allocator: std.mem.Allocator) void {
        _ = .{ self, allocator };
    }
};

/// EnumeratedType = NotationType | Enumeration
pub const EnumeratedType = union(enum) {
    /// This field is intended to be accessed directly.
    notation: NotationType,

    /// This field is intended to be accessed directly.
    enumeration: Enumeration,

    /// Matches a valid enumerated type in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EnumeratedType {
        if (NotationType.match(iter, allocator)) |notation| return EnumeratedType { .notation = notation } else |_| {}
        if (Enumeration.match(iter, allocator)) |enumeration| return EnumeratedType { .enumeration = enumeration } else |_| {}

        return Error.ExpectedEnumeratedType;
    }

    /// Deinitializes an EnumeratedType returned by EnumeratedType.match
    pub fn deinit(self: *EnumeratedType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .notation => |*notation| notation.deinit(allocator),
            .enumeration => |*enumeration| enumeration.deinit(allocator),
        }
    }
};

/// NotationType = 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
pub const NotationType = struct {
    /// This field is intended to be accessed directly.
    names: std.ArrayList(Name),

    /// Matches a valid notation type in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!NotationType {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "NOTATION") catch return Error.ExpectedNotation;
        try S.match(iter);
        Label.matchCodepoint(iter, '(') catch return Error.ExpectedOpenParenthesis;
        S.match(iter) catch {};

        var names = try std.ArrayList(Name).initCapacity(allocator, 1);
        errdefer names.deinit(allocator);
        
        names.items[0] = try Name.match(iter, allocator);
        errdefer for (names.items) |*item| item.deinit(allocator);

        S.match(iter) catch {};

        while (true) {
            Label.matchCodepoint(iter, '|') catch break;
            S.match(iter) catch {};
            try names.append(allocator, try Name.match(iter, allocator));
            S.match(iter) catch {};
        }

        Label.matchCodepoint(iter, ')') catch return Error.ExpectedCloseParenthesis;

        return NotationType { .names = names };
    }

    /// Deinitializes a NotationType returned by NotationType.match
    pub fn deinit(self: *NotationType, allocator: std.mem.Allocator) void {
        for (self.names.items) |*item| item.deinit(allocator);
        self.names.deinit(allocator);
    }
};

/// Enumeration = '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
pub const Enumeration = struct {
    /// This field is intended to be accessed directly.
    tokens: std.ArrayList(Nmtoken),

    /// Matches a valid enumeration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Enumeration {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '(') catch return Error.ExpectedOpenParenthesis;
        S.match(iter) catch {};

        var tokens = try std.ArrayList(Nmtoken).initCapacity(allocator, 1);
        errdefer tokens.deinit(allocator);

        tokens.items[0] = try Nmtoken.match(iter, allocator);
        errdefer for (tokens.items) |*item| item.deinit(allocator);

        S.match(iter) catch {};

        while (true) {
            Label.matchCodepoint(iter, '|') catch break;
            S.match(iter) catch {};
            try tokens.append(allocator, try Nmtoken.match(iter, allocator));
            S.match(iter) catch {};
        }

        Label.matchCodepoint(iter, ')') catch return Error.ExpectedCloseParenthesis;

        return Enumeration { .tokens = tokens };
    }

    /// Deinitializes an Enumeration returned by Enumeration.match
    pub fn deinit(self: *Enumeration, allocator: std.mem.Allocator) void {
        for (self.tokens.items) |*item| item.deinit(allocator);
        self.tokens.deinit(allocator);
    }
};

/// Nmtoken = (NameChar)+
pub const Nmtoken = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(u21),

    /// Matches a valid name token in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Nmtoken {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var c1 = NameChar.match(iter, allocator) catch return Error.ExpectedNameChar;
        c1.deinit(allocator);

        while (true) { 
            var c2 = NameChar.match(iter, allocator) catch break;
            c2.deinit(allocator);
        }

        return Nmtoken { .content = try iter.unicode.decode(iter.data[offset..iter.offset], allocator) };
    }

    /// Deinitializes a Nmtoken returned by Nmtoken.match
    pub fn deinit(self: *Nmtoken, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// DefaultDecl = '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
pub const DefaultDecl = union(enum) {
    /// This field is intended to be accessed directly.
    required: void,

    /// This field is intended to be accessed directly.
    implied: void,

    /// This field is intended to be accessed directly.
    fixed: AttValue,

    /// Matches a valid default declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!DefaultDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (Label.match(iter, "#REQUIRED")) |_| return DefaultDecl { .required = void{} } else |_| {}
        if (Label.match(iter, "#IMPLIED")) |_| return DefaultDecl { .implied = void{} } else |_| {}

        if (Label.match(iter, "#FIXED")) |_| try S.match(iter) else |_| {}
        if (AttValue.match(iter, allocator)) |value| return DefaultDecl { .fixed = value } else |_| {}

        return Error.ExpectedDefaultDecl;
    }

    /// Deinitializes a DefaultDecl returned by DefaultDecl.match
    pub fn deinit(self: *DefaultDecl, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .required => return,
            .implied => return,
            .fixed => |*fixed| fixed.deinit(allocator),
        }
    }
};

/// AttValue = '"' ([^<&"] | Reference)* '"' | '\'' ([^<&'] | Reference)* '\''
pub const AttValue = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(union(enum) { string: std.ArrayList(u21), reference: Reference }),

    /// Matches a valid attribute value in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!AttValue {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var content = @FieldType(AttValue, "content").empty;
        errdefer {
            for (content.items) |*item| switch (item.*) {
                .string => |*str| str.deinit(allocator),
                .reference => |*ref| ref.deinit(allocator),
            };
            content.deinit(allocator);
        }

        if (Label.matchCodepoint(iter, '"')) |_| {
            var start = iter.offset;
            var end = iter.offset;

            while (true) {
                if (Reference.match(iter, allocator)) |ref| {
                    if (end > start) {
                        try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                        start = end;
                    }

                    try content.append(allocator, .{ .reference = ref });
                    continue;
                } else |_| {}

                if (Label.matchCodepoint(iter, '"')) |_| break else |_| {}

                if (Label.matchCodepoint(iter, '<')) |_| return Error.UnexpectedCodepoint else |_| {}
                if (Label.matchCodepoint(iter, '&')) |_| return Error.UnexpectedCodepoint else |_| {}

                _ = iter.next() catch {};
                end = iter.offset;
            }

            if (end > start) {
                try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                start = end;
            }

            return AttValue { .content = content };
        } else |_| {}
        
        if (Label.matchCodepoint(iter, '\'')) |_| {
            var start = iter.offset;
            var end = iter.offset;

            while (true) {
                if (Reference.match(iter, allocator)) |ref| {
                    if (end > start) {
                        try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                        start = end;
                    }

                    try content.append(allocator, .{ .reference = ref });
                    continue;
                } else |_| {}

                if (Label.matchCodepoint(iter, '\'')) |_| break else |_| {}

                if (Label.matchCodepoint(iter, '<')) |_| return Error.UnexpectedCodepoint else |_| {}
                if (Label.matchCodepoint(iter, '&')) |_| return Error.UnexpectedCodepoint else |_| {}

                _ = iter.next() catch {};
                end = iter.offset;
            }

            if (end > start) {
                try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                start = end;
            }

            return AttValue { .content = content };
        } else |_| {}

        return Error.ExpectedString;
    }

    /// Deinitializes an AttValue returned by AttValue.match
    pub fn deinit(self: *AttValue, allocator: std.mem.Allocator) void {
        for (self.content.items) |*item| switch (item.*) {
            .string => |*str| str.deinit(allocator),
            .reference => |*ref| ref.deinit(allocator),
        };
        self.content.deinit(allocator);
    }
};

/// Reference = EntityRef | CharRef
pub const Reference = union(enum) {
    /// This field is intended to be accessed directly.
    entity: EntityRef,

    /// This field is intended to be accessed directly.
    char: CharRef,

    /// Matches a valid reference in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Reference {
        if (EntityRef.match(iter, allocator)) |entity| return Reference { .entity = entity } else |_| {}
        if (CharRef.match(iter, allocator)) |char| return Reference { .char = char } else |_| {}

        return Error.ExpectedReference;
    }

    /// Deinitializes a Reference returned by Reference.match
    pub fn deinit(self: *Reference, allocator: std.mem.Allocator) void {
        switch(self.*) {
            .entity => |*entity| entity.deinit(allocator),
            .char => |*char| char.deinit(allocator),
        }
    }
};

/// EntityRef = '&' Name ';'
pub const EntityRef = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// Matches a valid entity reference in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EntityRef {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '&') catch return Error.ExpectedAmpersand;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        Label.matchCodepoint(iter, ';') catch return Error.ExpectedSemicolon;

        return EntityRef { .name = name };
    }

    /// Deinitializes a EntityRef returned by EntityRef.match
    pub fn deinit(self: *EntityRef, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};

/// CharRef = '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
pub const CharRef = union(enum) {
    /// This field is intended to be accessed directly.
    decimal: std.ArrayList(u21),

    /// This field is intended to be accessed directly.
    hexadecimal: std.ArrayList(u21),

    /// Matches a valid char reference in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!CharRef {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (Label.match(iter, "&#x")) |_| {
            const start = iter.offset;
            while (isHex(try iter.peekn())) { _ = iter.next() catch {}; }
            const end = iter.offset;

            if (start == end) return Error.ExpectedHexadecimal;
            Label.matchCodepoint(iter, ';') catch return Error.ExpectedSemicolon;

            return CharRef { .hexadecimal = try iter.unicode.decode(iter.data[start..end], allocator) };
        } else |_| {}

        if (Label.match(iter, "&#")) |_| {
            const start = iter.offset;
            while (isNum(try iter.peekn())) { _ = iter.next() catch {}; }
            const end = iter.offset;

            if (start == end) return Error.ExpectedDecimal;
            Label.matchCodepoint(iter, ';') catch return Error.ExpectedSemicolon;

            return CharRef { .decimal = try iter.unicode.decode(iter.data[start..end], allocator) };
        } else |_| {}

        return Error.ExpectedCharRef;
    }

    /// Deinitializes a CharRef returned by CharRef.match
    pub fn deinit(self: *CharRef, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .decimal => |*decimal| decimal.deinit(allocator),
            .hexadecimal => |*hex| hex.deinit(allocator),
        }
    }
};

/// EntityDecl = GEDecl | PEDecl
pub const EntityDecl = union(enum) {
    /// This field is intended to be accessed directly.
    ge: GEDecl,

    /// This field is intended to be accessed directly.
    pe: PEDecl,

    /// Matches a valid entity declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EntityDecl {
        if (GEDecl.match(iter, allocator)) |ge| return EntityDecl { .ge = ge } else |_| {}
        if (PEDecl.match(iter, allocator)) |pe| return EntityDecl { .pe = pe } else |_| {}

        return Error.ExpectedEntityDecl;
    }

    /// Deinitializes an EntityDecl returned by EntityDecl.match
    pub fn deinit(self: *EntityDecl, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .ge => |*ge| ge.deinit(allocator),
            .pe => |*pe| pe.deinit(allocator),
        }
    }
};

/// GEDecl = '<!ENTITY' S Name S EntityDef S? '>'
pub const GEDecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    def: EntityDef,

    /// Matches a valid ge declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!GEDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!ENTITY") catch return Error.ExpectedEntityStartTag;
        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        try S.match(iter);

        var def = try EntityDef.match(iter, allocator);
        errdefer def.deinit(allocator);

        S.match(iter) catch {};
        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return GEDecl { .name = name, .def = def };
    }

    /// Deinitializes a GEDecl returned by GEDecl.match
    pub fn deinit(self: *GEDecl, allocator: std.mem.Allocator) void {
        self.def.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// EntityDef = EntityValue | (ExternalID NDataDecl?)
pub const EntityDef = union(enum) {
    entity: EntityValue,
    external: struct { external: ExternalID, data: ?NDataDecl },

    /// Matches a valid entity definition in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EntityDef {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (EntityValue.match(iter, allocator)) |entity| return EntityDef { .entity = entity } else |_| {}

        var external = try ExternalID.match(iter, allocator);
        errdefer external.deinit(allocator);

        var data = NDataDecl.match(iter, allocator) catch null;
        errdefer if (data) |*v| v.deinit(allocator);

        return EntityDef { .external = .{ .external = external, .data = data } };
    }

    /// Deinitializes an EntityDef returned by EntityDef.match
    pub fn deinit(self: *EntityDef, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .entity => |*entity| entity.deinit(allocator),
            .external => |*external| {
                if (external.data) |*data| data.deinit(allocator);
                external.external.deinit(allocator);
            }
        }
    }
};

/// EntityValue = '"' ([^%&"]) | PEReference | Reference)* '"' | '\'' ([^%&']) | PEReference | Reference)* '\''
pub const EntityValue = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(union(enum) { string: std.ArrayList(u21), peref: PEReference, reference: Reference }),

    /// Matches a valid entity value in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EntityValue {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var content = @FieldType(EntityValue, "content").empty;
        errdefer {
            for (content.items) |*item| switch(item.*) {
                .string => |*str| str.deinit(allocator),
                .peref => |*peref| peref.deinit(allocator),
                .reference => |*ref| ref.deinit(allocator),
            };
            content.deinit(allocator);
        }

        if (Label.matchCodepoint(iter, '"')) |_| {
            var start = iter.offset;
            var end = iter.offset;

            while (true) {
                if (PEReference.match(iter, allocator)) |peref| {
                    if (end > start) {
                        try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                        start = end;
                    }

                    try content.append(allocator, .{ .peref = peref });
                    continue;
                } else |_| {}

                if (Reference.match(iter, allocator)) |ref| {
                    if (end > start) {
                        try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                        start = end;
                    }

                    try content.append(allocator, .{ .reference = ref });
                    continue;
                } else |_| {}

                if (Label.matchCodepoint(iter, '"')) |_| break else |_| {}

                if (Label.matchCodepoint(iter, '%')) |_| return Error.UnexpectedCodepoint else |_| {}
                if (Label.matchCodepoint(iter, '&')) |_| return Error.UnexpectedCodepoint else |_| {}

                _ = iter.next() catch {};
                end = iter.offset;
            }

            if (end > start) {
                try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                start = end;
            }

            return EntityValue { .content = content };
        } else |_| {}
        
        if (Label.matchCodepoint(iter, '\'')) |_| {
            var start = iter.offset;
            var end = iter.offset;

            while (true) {
                if (PEReference.match(iter, allocator)) |peref| {
                    if (end > start) {
                        try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                        start = end;
                    }

                    try content.append(allocator, .{ .peref = peref });
                    continue;
                } else |_| {}

                if (Reference.match(iter, allocator)) |ref| {
                    if (end > start) {
                        try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                        start = end;
                    }

                    try content.append(allocator, .{ .reference = ref });
                    continue;
                } else |_| {}

                if (Label.matchCodepoint(iter, '\'')) |_| break else |_| {}

                if (Label.matchCodepoint(iter, '%')) |_| return Error.UnexpectedCodepoint else |_| {}
                if (Label.matchCodepoint(iter, '&')) |_| return Error.UnexpectedCodepoint else |_| {}

                _ = iter.next() catch {};
                end = iter.offset;
            }

            if (end > start) {
                try content.append(allocator, .{ .string = try iter.unicode.decode(iter.data[start..end], allocator) });
                start = end;
            }

            return EntityValue { .content = content };
        } else |_| {}

        return Error.ExpectedString;
    }

    /// Deinitializes an EntityValue returned by EntityValue.match
    pub fn deinit(self: *EntityValue, allocator: std.mem.Allocator) void {
        for (self.content.items) |*item| switch (item.*) {
            .string => |*str| str.deinit(allocator),
            .peref => |*peref| peref.deinit(allocator),
            .reference => |*ref| ref.deinit(allocator),
        };
        self.content.deinit(allocator);
    }
};

/// PEReference = '%' Name ';'
pub const PEReference = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// Matches a valid pe reference in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PEReference {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '%') catch return Error.ExpectedPercentageSign;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        Label.matchCodepoint(iter, ';') catch return Error.ExpectedSemicolon;

        return PEReference { .name = name };
    }

    /// Deinitializes a PEReference returned by PEReference.match
    pub fn deinit(self: *PEReference, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};

/// NDataDecl = S 'NDATA' S Name
pub const NDataDecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// Matches a valid ndata declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!NDataDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        try S.match(iter);
        Label.match(iter, "NDATA") catch return Error.ExpectedNData;
        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        return NDataDecl { .name = name };
    }

    /// Deinitializes a NDataDecl returned by NDataDecl.match
    pub fn deinit(self: *NDataDecl, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};

/// PEDecl = '<!ENTITY' S '%' S Name S PEDef S? '>'
pub const PEDecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    def: PEDef,

    /// Matches a valid pe declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PEDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!ENTITY") catch return Error.ExpectedEntityStartTag;
        try S.match(iter);
        Label.matchCodepoint(iter, '%') catch return Error.ExpectedPercentageSign;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        try S.match(iter);

        var def = try PEDef.match(iter, allocator);
        errdefer def.deinit(allocator);

        S.match(iter) catch {};
        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return PEDecl { .name = name, .def = def };
    }

    /// Deinitializes a PEDecl returned by PEDecl.match
    pub fn deinit(self: *PEDecl, allocator: std.mem.Allocator) void {
        self.def.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// PEDef = EntityValue | ExternalID
pub const PEDef = union(enum) {
    /// This field is intended to be accessed directly.
    entity: EntityValue,

    /// This field is intended to be accessed directly.
    external: ExternalID,

    /// Matches a valid pe definition in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PEDef {
        if (EntityValue.match(iter, allocator)) |entity| return PEDef { .entity = entity } else |_| {}
        if (ExternalID.match(iter, allocator)) |external| return PEDef { .external = external } else |_| {}

        return Error.ExpectedPEDef;
    }

    /// Deinitializes a PEDef returned by PEDef.match
    pub fn deinit(self: *PEDef, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .entity => |*entity| entity.deinit(allocator),
            .external => |*external| external.deinit(allocator),
        }
    }
};

/// NotationDecl = '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
pub const NotationDecl = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    id: union(enum) { external: ExternalID, public: PublicID },

    /// Matches a valid notation declaration in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!NotationDecl {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "<!NOTATION") catch return Error.ExpectedNotationDeclStartTag;
        try S.match(iter);

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        try S.match(iter);

        var id: @FieldType(NotationDecl, "id") = 
            if (ExternalID.match(iter, allocator)) |external| .{ .external = external }
            else |_| if (PublicID.match(iter, allocator)) |public| .{ .public = public }
            else |_| return Error.ExpectedExternalOrPublicID;
        errdefer switch (id) {
            .external => |*external| external.deinit(allocator),
            .public => |*public| public.deinit(allocator),
        };

        S.match(iter) catch {};
        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return NotationDecl { .name = name, .id = id };
    }

    /// Deinitializes a NotationDecl returned by NotationDecl.match
    pub fn deinit(self: *NotationDecl, allocator: std.mem.Allocator) void {
        switch (self.id) {
            .external => |*external| external.deinit(allocator),
            .public => |*public| public.deinit(allocator),
        }

        self.name.deinit(allocator);
    }
};

/// PublicID = 'PUBLIC' S PubidLiteral
pub const PublicID = struct {
    /// This field is intended to be accessed directly.
    content: PubidLiteral,

    /// Matches a valid public id in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!PublicID {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "PUBLIC") catch return Error.ExpectedPublic;
        try S.match(iter);

        var content = try PubidLiteral.match(iter, allocator);
        errdefer content.deinit(allocator);

        return PublicID { .content = content };
    }

    /// Deinitializes a PublicId returned by PublicID.match
    pub fn deinit(self: *PublicID, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// DeclSep = PEReference | S
pub const DeclSep = union(enum) {
    /// This field is intended to be accessed directly.
    peref: PEReference,

    /// This field is intended to be accessed directly.
    s: void,

    /// Matches a valid declaration separator in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!DeclSep {
        if (PEReference.match(iter, allocator)) |peref| return DeclSep { .peref = peref } else |_| {}
        if (S.match(iter)) |_| return DeclSep { .s = void{} } else |_| {}

        return Error.ExpectedDeclSep;
    }

    /// Deinitializes a DeclSep returned by DeclSep.match
    pub fn deinit(self: *DeclSep, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .peref => |*peref| peref.deinit(allocator),
            .s => return,
        }
    }
};

/// Element = EmptyElemTag | STag Content ETag
pub const Element = union(enum) {
    /// This field is intended to be accessed directly.
    empty: EmptyElemTag,

    /// This field is intended to be accessed directly.
    element: struct { start: STag, content: Content, end: ETag },

    /// Matches a valid element in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Element {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        if (EmptyElemTag.match(iter, allocator)) |empty| return Element { .empty = empty } else |_| {}

        var start = try STag.match(iter, allocator);
        errdefer start.deinit(allocator);

        var content = try Content.match(iter, allocator);
        errdefer content.deinit(allocator);

        var end = try ETag.match(iter, allocator);
        errdefer end.deinit(allocator);

        return Element { .element = .{ .start = start, .content = content, .end = end } };
    }

    /// Deinitializes an Element returned by Element.match
    pub fn deinit(self: *Element, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .empty => |*empty| empty.deinit(allocator),
            .element => |*element| {
                element.end.deinit(allocator);
                element.content.deinit(allocator);
                element.start.deinit(allocator);
            },
        }
    }
};

/// EmptyElemTag = '<' Name (S Attribute)* S? '/>'
pub const EmptyElemTag = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    attributes: std.ArrayList(Attribute),

    /// Matches a valid empty element tag in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!EmptyElemTag {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '<') catch return Error.ExpectedOpenTag;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        var attributes = std.ArrayList(Attribute).empty;
        errdefer {
            for (attributes.items) |*item| item.deinit(allocator);
            attributes.deinit(allocator);
        }

        while (true) {
            S.match(iter) catch break;
            try attributes.append(allocator, Attribute.match(iter, allocator) catch break);
        }

        Label.match(iter, "/>") catch return Error.ExpectedEndTag;

        return EmptyElemTag { .name = name, .attributes = attributes };
    }

    /// Deinitializes an EmptyElemTag returned by EmptyElemTag.match
    pub fn deinit(self: *EmptyElemTag, allocator: std.mem.Allocator) void {
        for (self.attributes.items) |*item| item.deinit(allocator);
        self.attributes.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// Attribute = Name Eq AttValue
pub const Attribute = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    value: AttValue,

    /// Matches a valid attribute in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Attribute {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        try Eq.match(iter);

        var value = try AttValue.match(iter, allocator);
        errdefer value.deinit(allocator);

        return Attribute { .name = name, .value = value };
    }

    /// Deinitializes an Attribute returned by Attribute.match
    pub fn deinit(self: *Attribute, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// STag = '<' Name (S Attribute)* S? '>'
pub const STag = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// This field is intended to be accessed directly.
    attributes: std.ArrayList(Attribute),

    /// Matches a valid start tag in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!STag {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.matchCodepoint(iter, '<') catch return Error.ExpectedOpenTag;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        var attributes = std.ArrayList(Attribute).empty;
        errdefer {
            for (attributes.items) |*item| item.deinit(allocator);
            attributes.deinit(allocator);
        }

        while (true) {
            S.match(iter) catch break;
            try attributes.append(allocator, Attribute.match(iter, allocator) catch break);
        }

        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return STag { .name = name, .attributes = attributes };
    }

    /// Deinitializes a STag returned by STag.match
    pub fn deinit(self: *STag, allocator: std.mem.Allocator) void {
        for (self.attributes.items) |*item| item.deinit(allocator);
        self.attributes.deinit(allocator);
        self.name.deinit(allocator);
    }
};

/// Content = CharData? ((Element | Reference | CDSect | PI | Comment) CharData?)*
pub const Content = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(union(enum) { data: CharData, element: Element, reference: Reference, cdsect: CDSect, pi: PI, comment: Comment }),

    /// Matches a valid content in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!Content {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var content = @FieldType(Content, "content").empty;
        errdefer {
            for (content.items) |*item| switch (item.*) {
                .data => |*data| data.deinit(allocator),
                .element => |*element| element.deinit(allocator),
                .reference => |*ref| ref.deinit(allocator),
                .cdsect => |*cdsect| cdsect.deinit(allocator),
                .pi => |*pi| pi.deinit(allocator),
                .comment => |*comment| comment.deinit(allocator),
            };
            content.deinit(allocator);
        }

        if (CharData.match(iter, allocator)) |data| try content.append(allocator, .{ .data = data }) else |_| {}

        while (true) {
            if (Element.match(iter, allocator)) |element| try content.append(allocator, .{ .element = element }) else |_|
            if (Reference.match(iter, allocator)) |ref| try content.append(allocator, .{ .reference = ref }) else |_|
            if (CDSect.match(iter, allocator)) |cdsect| try content.append(allocator, .{ .cdsect = cdsect }) else |_|
            if (PI.match(iter, allocator)) |pi| try content.append(allocator, .{ .pi = pi }) else |_|
            if (Comment.match(iter, allocator)) |comment| try content.append(allocator, .{ .comment = comment }) else |_| break;

            if (CharData.match(iter, allocator)) |data| try content.append(allocator, .{ .data = data }) else |_| {}
        }

        return Content { .content = content };
    }

    /// Deinitializes a Content returned by Content.match
    pub fn deinit(self: *Content, allocator: std.mem.Allocator) void {
        for (self.content.items) |*item| switch (item.*) {
            .data => |*data| data.deinit(allocator),
            .element => |*element| element.deinit(allocator),
            .reference => |*ref| ref.deinit(allocator),
            .cdsect => |*cdsect| cdsect.deinit(allocator),
            .pi => |*pi| pi.deinit(allocator),
            .comment => |*comment| comment.deinit(allocator),
        };
        self.content.deinit(allocator);
    }
};

/// CharData = [^<&]* - ([^<&]* ']]>' [^<&]*)
pub const CharData = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(u21),

    /// Matches a valid char data in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!CharData {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var end = iter.offset;

        while (true) {
            end = iter.offset;

            if (Label.match(iter, "]]>")) |_| {
                iter.offset = end;
                break;
            } else |_| {}
            
            if (Label.matchCodepoint(iter, '<')) |_| {
                iter.offset = end;
                break;
            } else |_| {}

            if (Label.matchCodepoint(iter, '&')) |_| {
                iter.offset = end;
                break;
            } else |_| {}

            _ = iter.next() catch {};
        }

        if (offset == end) return Error.EmptyCharData;

        return CharData { .content = try iter.unicode.decode(iter.data[offset..end], allocator) };
    }

    /// Deinitializes a CharData returned by CharData.match
    pub fn deinit(self: *CharData, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// CDSect = CDStart CData CDEnd
pub const CDSect = struct {
    /// This field is intended to be accessed directly.
    data: CData,

    /// Matches a valid cd sect in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!CDSect {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        try CDStart.match(iter);

        var data = try CData.match(iter, allocator);
        errdefer data.deinit(allocator);

        try CDEnd.match(iter);

        return CDSect { .data = data };
    }

    /// Deinitializes a CDSect returned by CDSect.match
    pub fn deinit(self: *CDSect, allocator: std.mem.Allocator) void {
        self.data.deinit(allocator);
    }
};

/// CDStart = '<![CDATA['
pub const CDStart = struct {
    /// Matches a valid cd start in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator) Error!void {
        Label.match(iter, "<![CDATA[") catch return Error.ExpectedCDStart;
    }
};

/// CData = Char* - (Char* ']]>' Char*)
pub const CData = struct {
    /// This field is intended to be accessed directly.
    content: std.ArrayList(u21),

    /// Matches a valid c data in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!CData {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        var end = iter.offset;

        while (true) {
            end = iter.offset;

            if (Label.match(iter, "]]>")) |_| {
                iter.offset = end;
                break;
            } else |_| {}

            var c1 = Char.match(iter, allocator) catch break;
            c1.deinit(allocator);
        }

        return CData { .content = try iter.unicode.decode(iter.data[offset..end], allocator) };
    }

    /// Deinitializes a CData returned by CData.match
    pub fn deinit(self: *CData, allocator: std.mem.Allocator) void {
        self.content.deinit(allocator);
    }
};

/// CDEnd = ']]>'
pub const CDEnd = struct {
    /// Matches a valid cd end in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator) Error!void {
        Label.match(iter, "]]>") catch return Error.ExpectedCDEnd;
    }
};

/// ETag = '</' Name S? '>'
pub const ETag = struct {
    /// This field is intended to be accessed directly.
    name: Name,

    /// Matches a valid end tag in the text and retrieves it parsed or returns an error.
    pub fn match(iter: *CodepointIterator, allocator: std.mem.Allocator) Error!ETag {
        const offset = iter.offset;
        errdefer iter.offset = offset;

        Label.match(iter, "</") catch return Error.ExpectedEndTag;

        var name = try Name.match(iter, allocator);
        errdefer name.deinit(allocator);

        S.match(iter) catch {};

        Label.matchCodepoint(iter, '>') catch return Error.ExpectedCloseTag;

        return ETag { .name = name };
    }

    /// Deinitializes a ETag returned by ETag.match
    pub fn deinit(self: *ETag, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};

/// Parses a xml document
pub fn parse(data: []const u8, allocator: std.mem.Allocator) Error!Document {
    var iterator = CodepointIterator {
        .data = data,
        .offset = 0,
        .unicode = UnicodeUTF8.unicode(),
    };

    return Document.match(&iterator, allocator);
}

fn isNum(codepoint: u32) bool {
    return codepoint >= '0' and codepoint <= '9';
}

fn isAlpha(codepoint: u32) bool {
    return (codepoint >= 'A' and codepoint <= 'Z') or (codepoint >= 'a' and codepoint <= 'z');
}

fn isHex(codepoint: u32) bool {
    return isNum(codepoint) or (codepoint >= 'a' and codepoint <= 'f') or (codepoint >= 'A' and codepoint <= 'F');
}
