use crate::{
    cli::{Serve, Subcommand},
    config::Configuration,
};
use anyhow::Result;
use std::net::SocketAddr;
use tracing::{debug, info};
use tracing_subscriber::EnvFilter;

mod cli;
mod config;
mod search_query;
mod server;
mod url_template;

async fn serve(cmd: &Serve) -> Result<()> {
    let cfg = Configuration::from_path(&cmd.config).await?;
    debug!(?cfg, "starting");

    let addr: SocketAddr = ([127, 0, 0, 1], cmd.port).into();
    info!(?addr, "start listening");

    server::server(cfg).bind(addr).await;

    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let cmd = cli::cmd();
    debug!(?cmd, "launched");

    match cmd.cmd {
        Subcommand::Serve(cmd) => serve(&cmd).await?,
    };

    info!("exiting");
    Ok(())
}
