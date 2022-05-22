use crate::{app::SearchTarget, percent_encoding::encode, search_query::SearchQuery};
use anyhow::{anyhow, ensure, Error};
use std::{collections::HashMap, sync::Arc};
use url::Url;

#[derive(Clone)]
pub struct TargetUrlMaker {
    inner: Arc<Inner>,
}

struct Inner {
    registry: HashMap<
        String,                                      /* prefix */
        Box<dyn (Fn(&str) -> String) + Send + Sync>, /* factory */
    >,
}

impl TargetUrlMaker {
    pub fn new(targets: &[SearchTarget]) -> Result<Self, Error> {
        ensure!(!targets.is_empty());
        let registry = targets
            .iter()
            .cloned()
            .map(|tgt| {
                let f: Box<dyn Fn(&str) -> String + Send + Sync> =
                    Box::new(move |kw| tgt.url_template.replace("{keywords}", kw));
                (tgt.prefix, f)
            })
            .collect::<HashMap<_, _>>();
        let s = Self {
            inner: Arc::new(Inner { registry }),
        };
        Ok(s)
    }

    /// Makes URL for the given search query.
    pub fn make_url(&self, query: &SearchQuery) -> Option<Result<Url, Error>> {
        let prefix = match query.prefix() {
            Some(x) => x,
            None => return None,
        };
        let maker = match self.inner.registry.get(prefix) {
            Some(x) => x,
            None => return None,
        };
        let url = maker(encode(query.keywords()).as_str());
        Some(Url::parse(&url).map_err(|err| anyhow!(err)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trivial() {
        let targets = vec![SearchTarget {
            name: String::from("Example"),
            prefix: String::from("ex"),
            url_template: String::from("http://example.com?q={keywords}"),
        }];
        let maker = TargetUrlMaker::new(&targets).unwrap();

        let url = maker
            .make_url(&SearchQuery::new("ex hello"))
            .unwrap()
            .unwrap();

        assert_eq!(url.to_string(), "http://example.com/?q=hello");
    }

    #[test]
    fn test_keywords_contains_special_character() {
        let targets = vec![
            SearchTarget {
                name: String::from("Example"),
                prefix: String::from("ex"),
                url_template: String::from("http://example.com?q={keywords}"),
            },
            SearchTarget {
                name: String::from("Hoogle"),
                prefix: String::from("hg"),
                url_template: String::from("https://hoogle.haskell.org/?hoogle={keywords}"),
            },
        ];
        let maker = TargetUrlMaker::new(&targets).unwrap();

        let url = maker
            .make_url(&SearchQuery::new("hg +mtl reader"))
            .unwrap()
            .unwrap();

        assert_eq!(
            url.to_string(),
            "https://hoogle.haskell.org/?hoogle=%2Bmtl%20reader"
        );
    }

    #[test]
    fn test_with_missing_prefix() {
        let targets = vec![SearchTarget {
            name: String::from("Example"),
            prefix: String::from("ex"),
            url_template: String::from("http://example.com?q={keywords}"),
        }];
        let maker = TargetUrlMaker::new(&targets).unwrap();

        let url = maker.make_url(&SearchQuery::new("hello world"));
        assert!(url.is_none())
    }
}
