use core::fmt;

use crate::parse;

/// An Objective-C type encoding.
///
/// For more information, see Apple's documentation:
/// <https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html>
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Encoding<'a> {
    /// A C `char`. Corresponds to the `c` code.
    Char,
    /// A C `short`. Corresponds to the `s` code.
    Short,
    /// A C `int`. Corresponds to the `i` code.
    Int,
    /// A C `long`. Corresponds to the `l` code.
    Long,
    /// A C `long long`. Corresponds to the `q` code.
    LongLong,
    /// A C `unsigned char`. Corresponds to the `C` code.
    UChar,
    /// A C `unsigned short`. Corresponds to the `S` code.
    UShort,
    /// A C `unsigned int`. Corresponds to the `I` code.
    UInt,
    /// A C `unsigned long`. Corresponds to the `L` code.
    ULong,
    /// A C `unsigned long long`. Corresponds to the `Q` code.
    ULongLong,
    /// A C `float`. Corresponds to the `f` code.
    Float,
    /// A C `double`. Corresponds to the `d` code.
    Double,
    /// A C++ `bool` / C99 `_Bool`. Corresponds to the `B` code.
    Bool,
    /// A C `void`. Corresponds to the `v` code.
    Void,
    /// A C `char *`. Corresponds to the `*` code.
    String,
    /// An Objective-C object (`id`). Corresponds to the `@` code.
    Object,
    /// An Objective-C block. Corresponds to the `@?` code.
    Block,
    /// An Objective-C class (`Class`). Corresponds to the `#` code.
    Class,
    /// An Objective-C selector (`SEL`). Corresponds to the `:` code.
    Sel,
    /// An unknown type. Corresponds to the `?` code.
    Unknown,
    /// A bitfield with the given number of bits.
    ///
    /// Corresponds to the `b`num code.
    BitField(u32),
    /// A pointer to the given type.
    ///
    /// Corresponds to the `^`type code.
    Pointer(&'a Encoding<'a>),
    /// An array with the given length and type.
    ///
    /// Corresponds to the `[len type]` code.
    Array(u32, &'a Encoding<'a>),
    /// A struct with the given name and fields.
    ///
    /// Corresponds to the `{name=fields...}` code.
    Struct(&'a str, &'a [Encoding<'a>]),
    /// A union with the given name and fields.
    ///
    /// Corresponds to the `(name=fields...)` code.
    Union(&'a str, &'a [Encoding<'a>]),
}

impl fmt::Display for Encoding<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use Encoding::*;
        let code = match *self {
            Char => "c",
            Short => "s",
            Int => "i",
            Long => "l",
            LongLong => "q",
            UChar => "C",
            UShort => "S",
            UInt => "I",
            ULong => "L",
            ULongLong => "Q",
            Float => "f",
            Double => "d",
            Bool => "B",
            Void => "v",
            String => "*",
            Object => "@",
            Block => "@?",
            Class => "#",
            Sel => ":",
            Unknown => "?",
            BitField(b) => {
                return write!(formatter, "b{}", b);
            }
            Pointer(t) => {
                return write!(formatter, "^{}", t);
            }
            Array(len, item) => {
                return write!(formatter, "[{}{}]", len, item);
            }
            Struct(name, fields) => {
                write!(formatter, "{{{}=", name)?;
                for field in fields {
                    fmt::Display::fmt(field, formatter)?;
                }
                return formatter.write_str("}");
            }
            Union(name, members) => {
                write!(formatter, "({}=", name)?;
                for member in members {
                    fmt::Display::fmt(member, formatter)?;
                }
                return formatter.write_str(")");
            }
        };
        formatter.write_str(code)
    }
}

impl PartialEq<str> for Encoding<'_> {
    fn eq(&self, other: &str) -> bool {
        parse::eq_enc(other, self)
    }
}

impl PartialEq<Encoding<'_>> for str {
    fn eq(&self, other: &Encoding) -> bool {
        parse::eq_enc(self, other)
    }
}

#[cfg(test)]
mod tests {
    use super::Encoding;
    use alloc::string::ToString;

    #[test]
    fn test_array_display() {
        let e = Encoding::Array(12, &Encoding::Int);
        assert_eq!(e.to_string(), "[12i]");
        assert_eq!(&e, "[12i]");
    }

    #[test]
    fn test_pointer_display() {
        let e = Encoding::Pointer(&Encoding::Int);
        assert_eq!(e.to_string(), "^i");
        assert_eq!(&e, "^i");
    }

    #[test]
    fn test_pointer_eq() {
        let i = Encoding::Int;
        let p = Encoding::Pointer(&Encoding::Int);

        assert!(p == p);
        assert!(p != i);
    }

    #[test]
    fn test_int_display() {
        assert_eq!(Encoding::Int.to_string(), "i");
        assert_eq!(&Encoding::Int, "i");
    }

    #[test]
    fn test_eq() {
        let i = Encoding::Int;
        let c = Encoding::Char;

        assert!(i == i);
        assert!(i != c);
    }

    #[test]
    fn test_struct_display() {
        let s = Encoding::Struct("CGPoint", &[Encoding::Char, Encoding::Int]);
        assert_eq!(s.to_string(), "{CGPoint=ci}");
        assert_eq!(&s, "{CGPoint=ci}");
    }

    #[test]
    fn test_struct_eq() {
        let s = Encoding::Struct("CGPoint", &[Encoding::Char, Encoding::Int]);
        assert!(s == s);
        assert!(s != Encoding::Int);
    }

    #[test]
    fn test_union_display() {
        let u = Encoding::Union("Onion", &[Encoding::Char, Encoding::Int]);
        assert_eq!(u.to_string(), "(Onion=ci)");
        assert_eq!(&u, "(Onion=ci)");
    }

    #[test]
    fn test_union_eq() {
        let u = Encoding::Union("Onion", &[Encoding::Char, Encoding::Int]);
        assert!(u == u);
        assert!(u != Encoding::Int);
    }
}
