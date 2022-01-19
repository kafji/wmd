use serde::Deserialize;
use std::path::Path;
use thiserror::Error;
use tokio::{fs::File, io::AsyncReadExt};
use url::Url;

#[derive(Deserialize, PartialEq, Clone, Debug)]
pub struct Configuration {
    pub base_url: Url,
    pub privacy_policy: Option<Url>,
    pub targets: Vec<Target>,
}

#[derive(Deserialize, PartialEq, Clone, Debug)]
pub struct Target {
    pub prefix: String,
    pub name: String,
    pub url_template: String,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Deserialization(#[from] toml::de::Error),
}

type Result<T> = std::result::Result<T, Error>;

impl Configuration {
    pub async fn from_path(path: &Path) -> Result<Self> {
        let mut file = File::open(path).await?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).await?;
        let config = toml::from_slice(&buf)?;
        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::{fs::File, io::AsyncWriteExt, task::spawn_blocking};

    #[tokio::test]
    async fn test_parse() {
        let path = Path::new("./wmd.example.toml");
        let config = Configuration::from_path(path).await.unwrap();

        assert_eq!(config.base_url.to_string(), "http://127.0.0.1:8000/");

        let targets = config.targets;
        assert_eq!(targets.len(), 2);

        let tgt = &targets[0];
        assert_eq!(tgt.prefix, "rs");
        assert_eq!(tgt.name, "The Rust Standard Library");
        assert_eq!(tgt.url_template, "https://doc.rust-lang.org/std/index.html?search={keywords}");
    }

    #[tokio::test]
    async fn test_parse_invalid_path() {
        let path = Path::new("./cmd.toml");
        let error = Configuration::from_path(path).await.unwrap_err();
        assert!(matches!(error, Error::Io(_)));
    }

    #[tokio::test]
    async fn test_parse_malformat() {
        let (_dir, path) = {
            let dir = spawn_blocking(|| tempfile::tempdir()).await.unwrap().unwrap();
            let path = {
                let mut p = dir.path().to_owned();
                p.push("wmd.toml");
                p
            };
            (dir, path)
        };

        let mut file = File::create(&path).await.unwrap();
        file.write_all("{}".as_bytes()).await.unwrap();

        let error = Configuration::from_path(&path).await.unwrap_err();
        assert!(matches!(error, Error::Deserialization(_)));
    }
}
