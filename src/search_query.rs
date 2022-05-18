pub struct SearchQuery<'a> {
    original: &'a str,
    prefix: Option<&'a str>,
    keywords: &'a str,
}

impl<'a> SearchQuery<'a> {
    pub fn new<'s>(query: &'s str) -> Self
    where
        's: 'a,
    {
        parse_search_query(query)
    }

    pub fn prefix(&self) -> Option<&str> {
        self.prefix
    }

    pub fn keywords(&self) -> &str {
        self.keywords
    }

    pub fn into_str(&self) -> &'a str {
        self.original
    }
}

fn parse_search_query(query: &str) -> SearchQuery {
    let sep_idx = query
        .char_indices()
        .find(|(_, c)| *c == ' ')
        .map(|(i, _)| i);
    let (prefix, keywords) = match sep_idx {
        Some(sep_idx) => {
            let pf = &query[..sep_idx];
            let kw = &query[sep_idx + 1..];
            (Some(pf), kw)
        }
        None => {
            let kw = &query[..];
            (None, kw)
        }
    };
    SearchQuery {
        original: query,
        prefix,
        keywords,
    }
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
