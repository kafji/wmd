use crate::config::{Configuration, Target};
use thiserror::Error;
use url::Url;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] url::ParseError),
}

#[derive(PartialEq, Debug)]
pub struct UrlTemplate<'a> {
    pub prefix: &'a str,
    pub name: &'a str,
    pub template: &'a str,
}

impl<'a> From<&'a Target> for UrlTemplate<'a> {
    fn from(s: &'a Target) -> Self {
        let Target {
            prefix,
            name,
            url_template: template,
            ..
        } = s;
        Self {
            prefix,
            name,
            template,
        }
    }
}

impl UrlTemplate<'_> {
    pub fn build_url(&self, keywords: &str) -> Result<Url, Error> {
        let tmplt = self.template;
        let url = tmplt.replace("{keywords}", keywords);
        let url = url.parse()?;
        Ok(url)
    }
}

#[cfg(test)]
mod url_template_tests {

    use super::*;

    #[test]
    fn test_build_url() {
        let template: UrlTemplate = UrlTemplate {
            prefix: "eg",
            name: "Example",
            template: "https://example.com/q={keywords}",
        };
        let url = template.build_url("hello world").unwrap();
        assert_eq!(url.to_string(), "https://example.com/q=hello%20world");
    }
}

pub trait UrlTemplates<'a> {
    fn template_for(&'a self, prefix: &str) -> Option<UrlTemplate<'a>>;

    fn default_template(&'a self) -> Option<UrlTemplate<'a>>;
}

impl<'a> UrlTemplates<'a> for Configuration {
    fn template_for(&'a self, prefix: &str) -> Option<UrlTemplate<'a>> {
        self.targets
            .iter()
            .find(|Target { prefix: x, .. }| x == prefix)
            .map(Into::into)
    }

    fn default_template(&'a self) -> Option<UrlTemplate<'a>> {
        self.targets.first().map(Into::into)
    }
}

#[cfg(test)]
mod url_templates_tests {

    use super::*;
    use std::path::Path;

    #[tokio::test]
    async fn test_configuration() {
        let path = Path::new("./wmd.example.toml");
        let config = Configuration::from_path(path).await.unwrap();

        let template = config.template_for("rs");

        assert_eq!(
            template,
            Some(UrlTemplate {
                prefix: "rs",
                name: "The Rust Standard Library",
                template: "https://doc.rust-lang.org/std/index.html?search={keywords}"
            })
        );

        let template = config.template_for("gg");
        assert_eq!(template, None);
    }
}
