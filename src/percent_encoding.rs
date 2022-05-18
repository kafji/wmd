/*!
Memory efficient operations for percent encoding.
*/

use once_cell::sync::Lazy;
use std::{borrow::Cow, collections::HashSet};

/// Set of special characters. Taken from https://developer.mozilla.org/en-US/docs/Glossary/percent-encoding.
static SPECIAL_CHARS: Lazy<HashSet<char>> = Lazy::new(|| {
    HashSet::from([
        ':', '/', '?', '#', '[', ']', '@', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=',
        '%', ' ',
    ])
});

/// Returns byte positions of special character.
fn special_chars_positions(s: &str) -> Vec<usize> {
    s.char_indices()
        .filter_map(|(pos, c)| {
            if SPECIAL_CHARS.contains(&c) {
                Some(pos)
            } else {
                None
            }
        })
        .collect()
}

/// Percent encoded string.
#[derive(Debug, PartialEq, Clone)]
pub struct PercentEncoded<'a>(Cow<'a, str>);

impl PercentEncoded<'_> {
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

/// Encode to percent encoding.
pub fn encode<'s>(s: &'s str) -> PercentEncoded {
    let special_chars = special_chars_positions(s);

    if special_chars.is_empty() {
        return PercentEncoded(Cow::Borrowed(s));
    }

    // rebuild string
    let expected_len = s.len() + special_chars.len() * 2;
    let mut out = String::with_capacity(expected_len);
    let mut cursor = 0;
    for byte_idx in special_chars {
        if cursor < byte_idx {
            let end = byte_idx;
            out.push_str(&s[cursor..end]);
            cursor = end;
        }
        let sc = s.as_bytes()[byte_idx];
        let encoded = format!("%{:X}", sc);
        out.push_str(&encoded);
        cursor += 1;
    }
    out.push_str(&s[cursor..]);
    debug_assert_eq!(out.len(), expected_len);

    PercentEncoded(Cow::Owned(out))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_string_containing_plus_character_and_space() {
        let encoded = encode("+mtl reader");
        assert_eq!(encoded.as_str(), "%2Bmtl%20reader");
    }

    #[test]
    fn test_encode_empty_string() {
        let encoded = encode("");
        assert_eq!(encoded.as_str(), "");
    }
}
