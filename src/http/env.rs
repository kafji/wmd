use crate::{
    app::SearchTarget,
    command_processing::{self, Registry},
    templating::{create_templates, Templates},
};
use anyhow::{anyhow, Error};
use std::sync::Arc;
use url::Url;

/// HTTP server environment.
#[derive(Clone)]
pub struct Env(Arc<Inner>);

struct Inner {
    base_url: Url,
    templates: Templates,
    search_targets: Vec<SearchTarget>,
    command_registry: Registry,
    default_target: SearchTarget,
}

impl Env {
    pub fn new(base_url: Url, search_targets: Vec<SearchTarget>) -> Result<Self, Error> {
        let templates = create_templates()?;

        let command_registry = command_processing::Registry::new(
            search_targets
                .iter()
                .map(|x| (x.prefix.as_str(), x.url_template.as_str())),
        );

        let default_target = search_targets
            .first()
            .ok_or_else(|| anyhow!("search targets is empty"))?
            .clone();

        let i = Inner {
            base_url,
            templates,

            search_targets,

            command_registry,

            default_target,
        };
        let s = Self(Arc::new(i));
        Ok(s)
    }

    pub fn base_url(&self) -> &Url {
        &self.0.base_url
    }

    pub fn templates(&self) -> &Templates {
        &self.0.templates
    }

    pub fn search_targets(&self) -> &[SearchTarget] {
        &self.0.search_targets
    }

    pub fn default_target(&self) -> &SearchTarget {
        &self.0.default_target
    }

    pub fn command_registry(&self) -> &Registry {
        &self.0.command_registry
    }
}
