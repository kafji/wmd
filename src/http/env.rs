use crate::{
    app::SearchTarget,
    command_processing::{self, Registry},
    search_targets::SearchTargets,
    templating::{create_templates, Templates},
};
use anyhow::Error;
use std::sync::Arc;
use url::Url;

/// HTTP server environment.
#[derive(Clone)]
pub struct Env(Arc<Inner>);

struct Inner {
    base_url: Url,
    templates: Templates,
    search_targets: Vec<SearchTarget>,
    search_targets2: SearchTargets,
    command_registry: Registry,
}

impl Env {
    pub fn new(base_url: Url, search_targets: Vec<SearchTarget>) -> Result<Self, Error> {
        let templates = create_templates()?;
        let search_targets2 = SearchTargets::new(search_targets.clone())?;

        let command_registry = command_processing::Registry::new(
            search_targets
                .iter()
                .map(|x| (x.prefix.as_str(), x.url_template.as_str())),
        );

        let i = Inner {
            base_url,
            templates,
            search_targets,
            search_targets2,
            command_registry,
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

    pub fn search_targets2(&self) -> &SearchTargets {
        &self.0.search_targets2
    }

    pub fn command_registry(&self) -> &Registry {
        &self.0.command_registry
    }
}
