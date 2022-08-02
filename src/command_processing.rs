use crate::{percent_encoding, ripm};
use anyhow::Error;
use std::collections::HashMap;
use url::Url;

#[derive(Debug)]
pub struct Registry<'app> {
    url_map: HashMap<&'app str, UrlTemplate<'app>>,
}

impl<'app> Registry<'app> {
    pub fn new(url_templates: impl Iterator<Item = (&'app str, &'app str)>) -> Self {
        let url_map = url_templates
            .map(|(x, y)| (x, UrlTemplate { template: y }))
            .collect();
        Self { url_map }
    }
}

#[derive(Debug)]
struct UrlTemplate<'app> {
    template: &'app str,
}

impl<'app> UrlTemplate<'app> {
    fn create_url(&self, keywords: &str) -> Result<Url, Error> {
        let keywords = percent_encoding::encode(&keywords);
        let url = self.template.replace("{keywords}", keywords.as_str());
        let url = Url::parse(&url)?;
        Ok(url)
    }
}

#[derive(Debug)]
pub enum Output {
    /// Command is to redirect to the specified URL.
    Redirect(Result<Url, Error>),

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
        Some(template) => {
            let url = template.create_url(&keywords);
            Output::Redirect(url)
        }
    }
}
