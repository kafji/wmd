use crate::{
    app::SearchTarget,
    search_targets::SearchTargets,
    url_maker::UrlMaker,
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
    url_maker: UrlMaker,
    search_targets: Vec<SearchTarget>,
    search_targets2: SearchTargets,
}

impl Env {
    pub fn new(base_url: Url, search_targets: Vec<SearchTarget>) -> Result<Self, Error> {
        let templates = create_templates()?;
        let url_maker = UrlMaker::new(&search_targets)?;
        let search_targets2 = SearchTargets::new(search_targets.clone())?;
        let i = Inner {
            base_url,
            templates,
            url_maker,
            search_targets,
            search_targets2,
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

    pub fn target_url_maker(&self) -> &UrlMaker {
        &self.0.url_maker
    }

    pub fn search_targets(&self) -> &[SearchTarget] {
        &self.0.search_targets
    }

    pub fn search_targets2(&self) -> &SearchTargets {
        &self.0.search_targets2
    }
}
