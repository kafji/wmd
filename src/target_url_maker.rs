use crate::{app::SearchTarget, search_query::SearchQuery};
use anyhow::{ensure, Error};
use std::{collections::HashMap, sync::Arc};
use url::Url;

#[derive(Clone)]
pub struct TargetUrlMaker {
    inner: Arc<Inner>,
}

struct Inner {
    default: Box<dyn (Fn(&str) -> String) + Send + Sync>,
    registry: HashMap<
        String,                                      /* prefix */
        Box<dyn (Fn(&str) -> String) + Send + Sync>, /* factory */
    >,
}

impl TargetUrlMaker {
    pub fn new(base_url: &Url, targets: &[SearchTarget]) -> Result<Self, Error> {
        ensure!(!targets.is_empty());
        let default = {
            let base_url = base_url.to_owned();
            let f: Box<dyn Fn(&str) -> String + Send + Sync> = Box::new(move |kw| {
                let mut url = base_url.clone();
                url.set_query(Some(&format!("q={}", kw)));
                url.to_string()
            });
            f
        };
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
            inner: Arc::new(Inner { default, registry }),
        };
        Ok(s)
    }

    /// Makes URL for the given search query.
    pub fn make_url(&self, query: &SearchQuery) -> Result<Url, Error> {
        let url = query
            .prefix
            .and_then(|prefix| {
                let reg = &self.inner.registry;
                reg.get(prefix).map(|f| f(query.keywords.as_str()))
            })
            .unwrap_or_else(|| (self.inner.default)(query.keywords.as_str()));
        Url::parse(&url).map_err(Error::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search_query::parse_search_query;

    #[test]
    fn test_trivial() {
        let targets = vec![SearchTarget {
            name: String::from("Example"),
            prefix: String::from("ex"),
            url_template: String::from("http://example.com?q={keywords}"),
        }];
        let maker =
            TargetUrlMaker::new(&Url::parse("http://localhost").unwrap(), &targets).unwrap();

        let url = maker.make_url(&parse_search_query("ex hello")).unwrap();

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
        let maker =
            TargetUrlMaker::new(&Url::parse("http://localhost").unwrap(), &targets).unwrap();

        let url = maker
            .make_url(&parse_search_query("hg +mtl reader"))
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
        let maker =
            TargetUrlMaker::new(&Url::parse("http://localhost").unwrap(), &targets).unwrap();

        let url = maker.make_url(&parse_search_query("hello")).unwrap();

        assert_eq!(url.to_string(), "http://localhost/?q=hello");
    }
}
