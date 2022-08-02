//! Transforms keywords in known mobile URL to its non-mobile URL.
//! For example, wikipedia https://en.m.wikipedia.org/wiki/Bakso to https://en.wikipedia.org/wiki/Bakso

// todo(kfj): is there a way to make this configurable like the search url mapper?

use anyhow::{anyhow, bail};
use std::borrow::Cow;
use url::{Host, Url};

pub fn ripm(keywords: &str) -> Result<Url, anyhow::Error> {
    let url = Url::parse(keywords)?;

    let host = url.host().ok_or_else(|| anyhow!("URL doesn't have host"))?;

    let domain = match host {
        Host::Domain(x) => x,
        _ => bail!("host is not a domain name"),
    };

    let path = url.path();

    let transformers = vec![WikipediaTransform];

    let mut transformed = None;
    for transformer in transformers {
        if transformer.can_transform(domain, path) {
            transformed = Some(transformer.transform(domain, path));
        }
    }

    let transformed = transformed.ok_or_else(|| anyhow!("found no valid transformer"))?;

    let mut new_url = url.clone();
    new_url.set_host(Some(&*transformed.0))?;
    new_url.set_path(&*transformed.1);

    Ok(new_url)
}

trait Transform {
    fn can_transform(&self, domain: &str, path: &str) -> bool;
    fn transform<'a>(&self, domain: &'a str, path: &'a str) -> (Cow<'a, str>, Cow<'a, str>);
}

struct WikipediaTransform;

impl Transform for WikipediaTransform {
    fn can_transform(&self, domain: &str, _path: &str) -> bool {
        let parts = domain.split('.').collect::<Vec<_>>();
        if parts.len() != 4 {
            return false;
        }
        parts[2] == "wikipedia" && parts[3] == "org"
    }

    fn transform<'a>(&self, domain: &'a str, path: &'a str) -> (Cow<'a, str>, Cow<'a, str>) {
        // intersperse is blocked https://github.com/rust-lang/rust/issues/79524
        // and I don't want to add itertools
        let parts = domain.split('.').collect::<Vec<_>>();
        let mut domain = String::new();
        domain.push_str(parts[0]);
        domain.push('.');
        domain.push_str(parts[2]);
        domain.push('.');
        domain.push_str(parts[3]);

        (domain.into(), path.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rip_wikipedia() {
        let url = "https://en.m.wikipedia.org/wiki/Bakso";
        let new_url = ripm(&url).unwrap();
        assert_eq!(new_url.as_str(), "https://en.wikipedia.org/wiki/Bakso");
    }
}
