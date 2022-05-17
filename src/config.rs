use crate::app::SearchTarget;
use anyhow::Error;
use serde::Deserialize;
use std::{env, path::Path};
use tokio::fs::{self};
use tracing::debug;
use url::Url;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Source<'a> {
    /// Read config from a toml file.
    File(&'a str),

    /// Read config from an environment variable.
    EnvVar(&'a str),
}

pub const DEFAULT_SOURCES: &[Source<'static>] =
    &[Source::EnvVar("WMD_CONFIG"), Source::File("./wmd.toml")];

#[derive(Deserialize, PartialEq, Clone, Debug)]
pub struct Target {
    name: String,
    prefix: String,
    url_template: String,
}

impl Into<SearchTarget> for Target {
    fn into(self) -> SearchTarget {
        let Target {
            name,
            prefix,
            url_template,
        } = self;
        SearchTarget {
            name,
            prefix,
            url_template,
        }
    }
}

#[derive(Deserialize, PartialEq, Clone, Debug)]
struct ConfigFile {
    base_url: Url,
    targets: Vec<Target>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Config {
    pub base_url: Url,
    pub search_targets: Vec<SearchTarget>,
}

impl From<ConfigFile> for Config {
    fn from(ConfigFile { base_url, targets }: ConfigFile) -> Self {
        let search_targets = targets.into_iter().map(Into::into).collect();
        Self {
            base_url,
            search_targets,
        }
    }
}

impl Config {
    async fn from_file(path: &Path) -> Result<Self, Error> {
        let raw = fs::read_to_string(path).await?;
        let cfg: ConfigFile = toml::from_str(&raw)?;
        Ok(cfg.into())
    }

    fn from_base64(s: &str) -> Result<Self, Error> {
        let cfg = base64::decode(s)?;
        let cfg: ConfigFile = toml::from_slice(&cfg)?;
        Ok(cfg.into())
    }

    pub async fn from_source<'a>(source: &Source<'a>) -> Result<Self, Error> {
        let s = match source {
            Source::File(path) => {
                debug!(?path, "reading config from file");
                Self::from_file(Path::new(*path)).await?
            }
            Source::EnvVar(var) => {
                debug!(?var, "reading config from env var");
                let val = env::var(*var)?;
                Self::from_base64(&val)?
            }
        };
        Ok(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_from_path() {
        let path = Path::new("./wmd.example.toml");
        let config = Config::from_file(path).await.unwrap();

        assert_eq!(config.base_url.to_string(), "http://127.0.0.1:3000/");

        let targets = config.search_targets;
        assert_eq!(targets.len(), 2);

        let tgt = &targets[0];
        assert_eq!(tgt.prefix, "rs");
        assert_eq!(tgt.name, "The Rust Standard Library");
        assert_eq!(
            tgt.url_template,
            "https://doc.rust-lang.org/std/index.html?search={keywords}"
        );
    }

    #[test]
    fn test_from_base64() {
        let cfg = Config::from_base64("IyB1c2VkIHRvIGdlbmVyYXRlIG9wZW4gc2VhcmNoIGRlc2NyaXB0aW9uCmJhc2VfdXJsID0gImh0dHA6Ly8xMjcuMC4wLjE6ODAwMC8iCgpbW3RhcmdldHNdXQpwcmVmaXggPSAicnMiCm5hbWUgPSAiVGhlIFJ1c3QgU3RhbmRhcmQgTGlicmFyeSIKdXJsX3RlbXBsYXRlID0gImh0dHBzOi8vZG9jLnJ1c3QtbGFuZy5vcmcvc3RkL2luZGV4Lmh0bWw/c2VhcmNoPXtrZXl3b3Jkc30iCgpbW3RhcmdldHNdXQpwcmVmaXggPSAiZHJ4IgpuYW1lID0gIkRvY3MucnMgKGRpcmVjdCkiCiMgYGtleXdvcmRzYCB3aWxsIGJlIHJlcGxhY2VkIGJ5IHRoZSBhY3R1YWwga2V5d29yZHMKdXJsX3RlbXBsYXRlID0gImh0dHBzOi8vZG9jcy5ycy97a2V5d29yZHN9Igo=",).unwrap();

        let targets = cfg.search_targets;
        assert_eq!(targets.len(), 2);

        let tgt = &targets[0];
        assert_eq!(tgt.prefix, "rs");
        assert_eq!(tgt.name, "The Rust Standard Library");
        assert_eq!(
            tgt.url_template,
            "https://doc.rust-lang.org/std/index.html?search={keywords}"
        );
    }
}
