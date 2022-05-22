use crate::app::SearchTarget;
use anyhow::{ensure, Error};

#[derive(Debug, PartialEq)]
pub struct SearchTargets(Vec<SearchTarget>);

impl SearchTargets {
    pub fn new(search_targets: Vec<SearchTarget>) -> Result<Self, Error> {
        ensure!(!search_targets.is_empty());
        Ok(Self(search_targets))
    }

    pub fn default(&self) -> &SearchTarget {
        self.0.first().unwrap()
    }
}
