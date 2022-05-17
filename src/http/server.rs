use super::{env::Env, handlers::*};
use anyhow::Error;
use axum::{routing::get, Extension, Router, Server};
use tower_http::trace::TraceLayer;

pub fn router(env: Env) -> Router {
    Router::new()
        .route("/", get(get_index))
        .route("/search", get(get_search))
        .route("/prefixes", get(get_prefixes))
        .route("/opensearch.xml", get(get_opensearch_xml))
        .route("/robots.txt", get(get_robots_txt))
        .layer(Extension(env))
}

/// Starts server.
pub async fn start(port: u16, env: Env) -> Result<(), Error> {
    let app = router(env).layer(TraceLayer::new_for_http());
    let addr = ([0, 0, 0, 0], port).into();
    Server::bind(&addr).serve(app.into_make_service()).await?;
    Ok(())
}
