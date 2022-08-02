use crate::{
    config::{Config, Source},
    http,
    http::ServerEnv,
};
use anyhow::{anyhow, Error};
use futures::{FutureExt, TryFutureExt};
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Deserialize, Serialize, PartialEq, Clone, Debug)]
pub struct SearchTarget {
    pub name: String,
    pub prefix: String,
    pub url_template: String,
}

pub async fn start(port: u16, cfgsrcs: &[Source<'_>]) -> Result<(), Error> {
    let config = cfgsrcs
        .into_iter()
        .map(|src| Config::from_source(src).boxed())
        .reduce(|acc, el| acc.or_else(|_| el).boxed());
    let config = match config {
        None => return Err(anyhow!("config sources is empty")),
        Some(x) => x.await?,
    };
    info!(?config);

    let server_env = ServerEnv::new(config.base_url, config.search_targets)?;
    http::start_server(port, server_env).await?;

    Ok(())
}
