//! Parsing encodings from their string representation.

use crate::Encoding;

const QUALIFIERS: &'static [char] = &[
    'r', // const
    'n', // in
    'N', // inout
    'o', // out
    'O', // bycopy
    'R', // byref
    'V', // oneway
];

fn rm_enc_prefix<'a>(s: &'a str, enc: &Encoding) -> Option<&'a str> {
    use Encoding::*;
    let code = match *enc {
        Char      => "c",
        Short     => "s",
        Int       => "i",
        Long      => "l",
        LongLong  => "q",
        UChar     => "C",
        UShort    => "S",
        UInt      => "I",
        ULong     => "L",
        ULongLong => "Q",
        Float     => "f",
        Double    => "d",
        Bool      => "B",
        Void      => "v",
        String    => "*",
        Object    => "@",
        Block     => "@?",
        Class     => "#",
        Sel       => ":",
        Unknown   => "?",
        BitField(b) => {
            let s = rm_prefix(s, "b")?;
            return rm_int_prefix(s, b);
        }
        Pointer(t) => {
            let s = rm_prefix(s, "^")?;
            return rm_enc_prefix(s, t);
        }
        Array(len, item) => {
            let mut s = s;
            s = rm_prefix(s, "[")?;
            s = rm_int_prefix(s, len)?;
            s = rm_enc_prefix(s, item)?;
            return rm_prefix(s, "]");
        }
        Struct(name, fields) => {
            let mut s = s;
            s = rm_prefix(s, "{")?;
            s = rm_prefix(s, name)?;
            s = rm_prefix(s, "=")?;
            for field in fields {
                s = rm_enc_prefix(s, field)?;
            }
            return rm_prefix(s, "}");
        }
        Union(name, members) => {
            let mut s = s;
            s = rm_prefix(s, "(")?;
            s = rm_prefix(s, name)?;
            s = rm_prefix(s, "=")?;
            for member in members {
                s = rm_enc_prefix(s, member)?;
            }
            return rm_prefix(s, ")");
        }
    };

    rm_prefix(s, code)
}

fn chomp_int(s: &str) -> Option<(u32, &str)> {
    // Chomp until we hit a non-digit
    let (num, t) = match s.find(|c: char| !c.is_digit(10)) {
        Some(i) => s.split_at(i),
        None => (s, ""),
    };
    num.parse().map(|n| (n, t)).ok()
}

fn rm_int_prefix(s: &str, other: u32) -> Option<&str> {
    chomp_int(s)
        .and_then(|(n, t)| if other == n { Some(t) } else { None })
}

fn rm_prefix<'a>(s: &'a str, other: &str) -> Option<&'a str> {
    if s.starts_with(other) {
        Some(&s[other.len()..])
    } else {
        None
    }
}

pub fn eq_enc(s: &str, enc: &Encoding) -> bool {
    // strip qualifiers
    let s = s.trim_start_matches(QUALIFIERS);

    // if the given encoding can be successfully removed from the start
    // and an empty string remains, they were equal!
    rm_enc_prefix(s, enc).map_or(false, str::is_empty)
}

enum EncodingToken<'a> {
    Primitive(Encoding<'static>),
    Pointer,
    ArrayStart(u32),
    ArrayEnd,
    StructStart(&'a str),
    StructEnd,
    UnionStart(&'a str),
    UnionEnd,
}

fn chomp(s: &str) -> Option<(EncodingToken, &str)> {
    let (h, t) = {
        let mut chars = s.chars();
        match chars.next() {
            Some(h) => (h, chars.as_str()),
            None => return None,
        }
    };

    let primitive = match h {
        'c' => Encoding::Char,
        's' => Encoding::Short,
        'i' => Encoding::Int,
        'l' => Encoding::Long,
        'q' => Encoding::LongLong,
        'C' => Encoding::UChar,
        'S' => Encoding::UShort,
        'I' => Encoding::UInt,
        'L' => Encoding::ULong,
        'Q' => Encoding::ULongLong,
        'f' => Encoding::Float,
        'd' => Encoding::Double,
        'B' => Encoding::Bool,
        'v' => Encoding::Void,
        '*' => Encoding::String,
        '@' => {
            // Special handling for blocks
            if t.starts_with('?') {
                return Some((EncodingToken::Primitive(Encoding::Block), &t[1..]));
            }
            Encoding::Object
        }
        '#' => Encoding::Class,
        ':' => Encoding::Sel,
        '?' => Encoding::Unknown,
        'b' => {
            return chomp_int(t).map(|(b, t)| {
                (EncodingToken::Primitive(Encoding::BitField(b)), t)
            });
        }
        '^' => return Some((EncodingToken::Pointer, t)),
        '[' => {
            return chomp_int(t).map(|(n, t)| {
                (EncodingToken::ArrayStart(n), t)
            });
        }
        ']' => return Some((EncodingToken::ArrayEnd, t)),
        '{' => {
            return t.find('=').map(|i| {
                (EncodingToken::StructStart(&t[..i]), &t[i+1..])
            });
        }
        '}' => return Some((EncodingToken::StructEnd, t)),
        '(' => {
            return t.find('=').map(|i| {
                (EncodingToken::UnionStart(&t[..i]), &t[i+1..])
            });
        }
        ')' => return Some((EncodingToken::UnionEnd, t)),
        _ => return None,
    };
    Some((EncodingToken::Primitive(primitive), t))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nested() {
        let enc = Encoding::Struct("A", &[
            Encoding::Struct("B", &[
                Encoding::Char,
                Encoding::Int,
            ]),
            Encoding::Char,
            Encoding::Int,
        ]);
        assert!(eq_enc("{A={B=ci}ci}", &enc));
        assert!(!eq_enc("{A={B=ci}ci", &enc));

    }

    #[test]
    fn test_bitfield() {
        assert!(eq_enc("b32", &Encoding::BitField(32)));
        assert!(!eq_enc("b", &Encoding::BitField(32)));
        assert!(!eq_enc("b-32", &Encoding::BitField(32)));
    }

    #[test]
    fn test_qualifiers() {
        assert!(eq_enc("Vv", &Encoding::Void));
        assert!(eq_enc("r*", &Encoding::String));
    }

    #[test]
    fn test_unicode() {
        let fields = &[Encoding::Char, Encoding::Int];
        assert!(eq_enc("{☃=ci}", &Encoding::Struct("☃", fields)));
    }
}
