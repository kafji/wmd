use anyhow::Error;
use serde::Deserialize;
use std::path::Path;
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

impl Configuration {
    pub async fn from_path(path: &Path) -> Result<Self, Error> {
        let mut file = File::open(path).await?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).await?;
        let config = toml::from_slice(&buf)?;
        Ok(config)
    }

    pub fn from_base64(s: &str) -> Result<Self, Error> {
        let cfg = base64::decode(s)?;
        let cfg = toml::from_slice(&cfg)?;
        Ok(cfg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_from_path() {
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

    #[test]
    fn test_from_env() {
        let cfg = Configuration::from_base64("IyB1c2VkIHRvIGdlbmVyYXRlIG9wZW4gc2VhcmNoIGRlc2NyaXB0aW9uCmJhc2VfdXJsID0gImh0dHA6Ly8xMjcuMC4wLjE6ODAwMC8iCgpbW3RhcmdldHNdXQpwcmVmaXggPSAicnMiCm5hbWUgPSAiVGhlIFJ1c3QgU3RhbmRhcmQgTGlicmFyeSIKdXJsX3RlbXBsYXRlID0gImh0dHBzOi8vZG9jLnJ1c3QtbGFuZy5vcmcvc3RkL2luZGV4Lmh0bWw/c2VhcmNoPXtrZXl3b3Jkc30iCgpbW3RhcmdldHNdXQpwcmVmaXggPSAiZHJ4IgpuYW1lID0gIkRvY3MucnMgKGRpcmVjdCkiCiMgYGtleXdvcmRzYCB3aWxsIGJlIHJlcGxhY2VkIGJ5IHRoZSBhY3R1YWwga2V5d29yZHMKdXJsX3RlbXBsYXRlID0gImh0dHBzOi8vZG9jcy5ycy97a2V5d29yZHN9Igo=",).unwrap();

        let targets = cfg.targets;
        assert_eq!(targets.len(), 2);

        let tgt = &targets[0];
        assert_eq!(tgt.prefix, "rs");
        assert_eq!(tgt.name, "The Rust Standard Library");
        assert_eq!(tgt.url_template, "https://doc.rust-lang.org/std/index.html?search={keywords}");
    }
}
