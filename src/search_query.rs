#[derive(PartialEq, Clone, Debug)]
pub struct SearchQuery {
    pub query: String,
    keyword_start: usize,
}

impl SearchQuery {
    pub fn prefix(&self) -> Option<&str> {
        if self.keyword_start == 0 {
            None
        } else {
            let p = &self.query[..self.keyword_start - 1];
            Some(p)
        }
    }

    pub fn keywords(&self) -> &str {
        &self.query[self.keyword_start..]
    }
}

impl From<String> for SearchQuery {
    fn from(q: String) -> Self {
        let keyword_start = q.find(' ').map(|x| x + 1).unwrap_or_default();
        Self { query: q, keyword_start }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use pretty_assertions::assert_eq;
    use std::convert::TryInto;

    #[test]
    fn test() {
        let query: SearchQuery = "rs result".to_owned().try_into().unwrap();
        assert_eq!(query.prefix(), Some("rs"));
        assert_eq!(query.keywords(), "result");
    }

    #[test]
    fn test_with_missing_prefix() {
        let query: SearchQuery = "result".to_owned().try_into().unwrap();
        assert_eq!(query.prefix(), None);
        assert_eq!(query.keywords(), "result");
    }
}
