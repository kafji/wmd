mod cli;
mod config;
mod search_query;
mod http_server;
mod url_template;

use crate::{
    cli::{Serve, Subcommand},
    config::Configuration,
};
use anyhow::Result;
use std::net::SocketAddr;
use tracing::{debug, info};
use tracing_subscriber::EnvFilter;

async fn serve(cmd: &Serve) -> Result<()> {
    let cfg = {
        let base64 = std::env::var("WMD_CONFIG").ok();
        if let Some(s) = base64 {
            Configuration::from_base64(&s)
        } else {
            Configuration::from_path(&cmd.config).await
        }
    }?;
    debug!(?cfg, "starting");

    let addr: SocketAddr = ([0, 0, 0, 0], cmd.port).into();
    info!(?addr, "start listening");

    http_server::create_server(cfg).bind(addr).await;

    Ok(())
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_thread_ids(true)
        .init();

    let cmd = cli::cmd();
    debug!(?cmd, "launched");

    if cmd.version {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return;
    }

    if let None = cmd.cmd {
        println!("expecting a command, was none");
        return;
    }

    match cmd.cmd.unwrap() {
        Subcommand::Serve(cmd) => serve(&cmd).await.unwrap(),
    };

    info!("exiting");
}
