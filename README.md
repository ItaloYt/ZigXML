
# ZigXML - An XML parser in zig!

ZigXML is an xml parser made completely in zig, it retrieves informations such as encoding, elements, comments, etc.

# How does it works?

It basicaly works as a regex parser, matching the patterns described at the [XML 5th Edition Specification](https://www.w3.org/TR/REC-xml/). So every structure represents an entity and, therefore, has a method called 'match' that(surprisingly) matches that structure in the text and retrieves its contents or returns an error if it didn't matched. Some structures return void because they only need to match in the text, that is, they don't need to retrieve any information, such as whitespaces and labels(substrings).

To match a whole document, you'll need to call 'Document.match'.

Every 'match' function has an iterator as a parameter, this iterator iterates over codepoints(unicode characters) with any encoding, so every function uses it since it can work with any encoding. A CodepointIterator is very simple, it has: a data field which is the raw data it is gonna iterate, an offset field which is the data's offset in **bytes**, an unicode field which is an interface to an encoding implementation, a next method which retrieves the next codepoint and advances the offset, a peekn method which retrieves the next codepoint without changing the offset and an isEmpty method that returns if there isn't any more codepoints.

To abstract a little bit more, there's a 'parse' function which creates an iterator with utf-8 encoding and calls the 'Document.match', so you may parse a document by only calling 'parse'.

The UnicodeUTF8, UnicodeUTF16, Unicode* are encoding implementations and have methods to get codepoints, decode, encode and retrieve its unicode interface implementation.

Every function has an allocator parameter, because some functions need to allocate memory to do some operations or to store a few matched informations, so when you're sure you won't need the retrieved data anymore, you should call the 'deinit' method to free every allocated memory. Almost every structure has a 'deinit' method.

# TODO

* Implement UTF-16 and UTF-32 encoding.
* Change encoding after parsing the XMLDecl or the Prolog.
* Implement a few structures such as Nmtokens.
