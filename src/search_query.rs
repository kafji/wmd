use std::fmt;

pub struct SearchQuery<'a> {
    pub prefix: Option<&'a str>,
    pub keywords: &'a str,
}

impl fmt::Display for SearchQuery<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = self.prefix {
            write!(f, "{} ", x)?;
        }
        write!(f, "{}", &self.keywords)?;
        Ok(())
    }
}

pub fn parse_search_query(s: &str) -> SearchQuery {
    let sep_idx = s.char_indices().find(|(_, c)| *c == ' ').map(|(i, _)| i);
    let (prefix, keywords) = match sep_idx {
        Some(sep_idx) => {
            let pf = &s[..sep_idx];
            let kw = &s[sep_idx + 1..];
            (Some(pf), kw)
        }
        None => {
            let kw = &s[..];
            (None, kw)
        }
    };
    SearchQuery { prefix, keywords }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trivial() {
        let query = parse_search_query("rs result");
        assert_eq!(query.prefix, Some("rs"));
        assert_eq!(query.keywords, "result");
    }

    #[test]
    fn test_missing_prefix() {
        let query = parse_search_query("result");
        assert_eq!(query.prefix, None);
        assert_eq!(query.keywords, "result");
    }
}
