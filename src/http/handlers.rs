/*!
HTTP requests handlers.
*/

use super::env::Env;
use crate::{search_query::parse_search_query, templating::Template::*};
use axum::{
    body::Full,
    extract::Query,
    http::StatusCode,
    response::{Html, IntoResponse, Redirect, Response},
    Extension,
};
use handlebars::RenderError;
use serde::Deserialize;
use serde_json::json;
use std::fmt::Debug;

#[cfg(debug_assertions)]
use axum_macros::debug_handler;

/// Produces UTF-8 HTML reply.
macro_rules! reply_with_html {
    ($html:expr) => {
        return Ok(Html($html).into_response())
    };
    ($tmplts:expr, $tmplt:ident, $tmplt_ctx:expr) => {{
        let html = $tmplts.render($tmplt.name(), $tmplt_ctx)?;
        reply_with_html!(html)
    }};
}

/// Produces UTF-8 plain text reply.
macro_rules! reply_with_text {
    ($text:expr) => {
        return {
            let resp = Response::builder()
                .header("content-type", mime::TEXT_PLAIN_UTF_8.to_string())
                .body(Full::from($text))
                .unwrap();
            Ok(resp.into_response())
        }
    };
    ($tmplts:expr, $tmplt:ident, $tmplt_ctx:expr) => {{
        let text = $tmplts.render($tmplt.name(), $tmplt_ctx)?;
        reply_with_text!(text)
    }};
}

macro_rules! respond_failure {
    ($desc:expr, $code:expr, $display:expr) => {{
        tracing::error!("{}", $desc);
        ($code, $display).into_response()
    }};
}

/// Result of response handler.
type Reply = Result<Response, Error>;

/// Denotes failure when handling response.
#[derive(Debug)]
pub enum Error {
    Renderer { desc: String },
    InvalidTargetUrl { desc: String },
}

impl From<RenderError> for Error {
    fn from(s: RenderError) -> Self {
        Self::Renderer { desc: s.desc }
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> Response {
        match self {
            Error::Renderer { desc } => respond_failure!(
                desc,
                StatusCode::INTERNAL_SERVER_ERROR,
                "Failed to generate HTML."
            ),
            Error::InvalidTargetUrl { desc } => respond_failure!(
                desc,
                StatusCode::INTERNAL_SERVER_ERROR,
                "Failed to make target URL."
            ),
        }
    }
}

#[cfg_attr(debug_assertions, axum_macros::debug_handler)]
pub async fn get_index(Extension(env): Extension<Env>) -> Reply {
    reply_with_html!(env.templates(), HomePageHtml, &())
}

#[derive(Debug, Deserialize)]
pub struct QueryParams {
    q: String,
}

pub async fn get_search(
    Query(params): Query<QueryParams>,
    Extension(env): Extension<Env>,
) -> Reply {
    let query = parse_search_query(&params.q);
    let url = env
        .target_url_maker()
        .make_url(&query)
        .map_err(|x| Error::InvalidTargetUrl {
            desc: x.to_string(),
        })?;
    let resp = Redirect::temporary(url.as_str());
    Ok(resp.into_response())
}

#[cfg_attr(debug_assertions, debug_handler)]
pub async fn get_prefixes(Extension(env): Extension<Env>) -> Reply {
    let targets = env.search_targets();
    reply_with_html!(
        env.templates(),
        PrefixesPageHtml,
        &json!({
            "prefixes": targets,
        })
    )
}

#[cfg_attr(debug_assertions, debug_handler)]
pub async fn get_opensearch_xml(Extension(env): Extension<Env>) -> Reply {
    let mime_type = r"application/opensearchdescription+xml; charset=utf-8";
    let body = env.templates().render(
        OpensearchXml.name(),
        &json!({
            "baseUrl": env.base_url(),
        }),
    )?;
    Ok(Response::builder()
        .header("content-type", mime_type)
        .body(Full::from(body))
        .unwrap()
        .into_response())
}

#[cfg_attr(debug_assertions, debug_handler)]
pub async fn get_robots_txt(Extension(env): Extension<Env>) -> Reply {
    reply_with_text!(env.templates(), RobotsTxt, &())
}
