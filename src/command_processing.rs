use crate::{percent_encoding, ripm};
use std::collections::HashMap;
use url::Url;

#[derive(Debug)]
pub struct Registry {
    url_map: HashMap<String, UrlTemplate>,
}

impl Registry {
    pub fn new<'a>(
        url_templates: impl Iterator<Item = (impl Into<String>, impl Into<String>)>,
    ) -> Self {
        let url_map = url_templates
            .map(|(x, y)| (x.into(), UrlTemplate { template: y.into() }))
            .collect();
        Self { url_map }
    }
}

#[derive(Debug)]
struct UrlTemplate {
    template: String,
}

impl UrlTemplate {
    fn create_url(&self, keywords: &str) -> Result<Url, anyhow::Error> {
        let keywords = percent_encoding::encode(&keywords);
        let url = self.template.replace("{keywords}", keywords.as_str());
        let url = Url::parse(&url)?;
        Ok(url)
    }
}

#[derive(Debug)]
pub enum Output {
    /// Command is to redirect to the specified URL.
    Redirect(Result<Url, anyhow::Error>),

    /// Processor can't handle the command.
    Unhandled,
}

pub fn process(registry: &Registry, command: &str) -> Output {
    let sep = match command.find(' ') {
        None => return Output::Unhandled,
        Some(x) => x,
    };

    let prefix = &command[..sep];
    let keywords = &command[sep + 1..].trim();

    if prefix == "ripm" {
        return Output::Redirect(ripm::ripm(keywords));
    }

    match registry.url_map.get(prefix) {
        None => Output::Unhandled,
        Some(template) => Output::Redirect(template.create_url(&keywords)),
    }
}
