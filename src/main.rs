use std::error::Error;
use tracing_subscriber::EnvFilter;
use wmd;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_thread_ids(true)
        .init();
    wmd::start_app(3000, wmd::DEFAULT_CONFIG_SOURCES).await?;
    Ok(())
}
