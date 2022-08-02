/*!
HTTP requests handlers.
*/

use super::env::Env;
use crate::{command_processing, templating::Template::*};
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

#[derive(Debug, Deserialize)]
pub struct IndexParams {
    k: Option<String>,
}

#[cfg_attr(debug_assertions, axum_macros::debug_handler)]
pub async fn get_index(Extension(env): Extension<Env>, Query(params): Query<IndexParams>) -> Reply {
    let query: Option<String> = params.k.map(|keywords| {
        let prefix = &env.search_targets2().default().prefix;
        let mut query = String::with_capacity(prefix.len() + 1 + keywords.len());
        query.push_str(prefix);
        query.push(' ');
        query.push_str(&keywords);
        query
    });
    reply_with_html!(
        env.templates(),
        HomePageHtml,
        &json!({
            "query": query,
        })
    )
}

#[derive(Debug, Deserialize)]
pub struct SearchParams {
    q: String,
}

#[cfg_attr(debug_assertions, debug_handler)]
pub async fn get_search(
    Extension(env): Extension<Env>,
    Query(params): Query<SearchParams>,
) -> Reply {
    let registry = env.command_registry();
    let output = command_processing::process(&registry, &params.q);
    let response = match output {
        command_processing::Output::Redirect(url) => {
            let url = url.map_err(|x| Error::InvalidTargetUrl {
                desc: x.to_string(),
            })?;
            Redirect::temporary(url.as_str()).into_response()
        }
        command_processing::Output::Unhandled => {
            // processor can't handle this command, redirect to homepage/search-form-page
            let mut url = env.base_url().clone();
            let params = Some(format!("k={}", &params.q));
            url.set_query(params.as_deref());
            Redirect::temporary(url.as_str()).into_response()
        }
    };
    Ok(response)
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
