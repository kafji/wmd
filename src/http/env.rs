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
}

impl Env {
    pub fn new(base_url: Url, search_targets: Vec<SearchTarget>) -> Result<Self, Error> {
        let templates = create_templates()?;

        let i = Inner {
            base_url,
            templates,
            search_targets,
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

    pub fn default_target(&self) -> Result<&SearchTarget, anyhow::Error> {
        self.0
            .search_targets
            .first()
            .ok_or_else(|| anyhow!("search targets is empty"))
    }

    pub fn command_registry<'env>(&'env self) -> Registry<'env> {
        command_processing::Registry::new(
            self.0
                .search_targets
                .iter()
                .map(|x| (&*x.prefix, &*x.url_template)),
        )
    }
}
