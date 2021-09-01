use crate::{
    config::{Configuration, Target},
    search_query::SearchQuery,
    url_template::{Error as UrlTemplateError, UrlTemplates},
};
use indoc::{formatdoc, indoc};
use itertools::Itertools;
use serde::Deserialize;
use std::{convert::TryInto, sync::Arc};
use thiserror::Error;
use tracing::debug;
use warp::{
    get,
    http::{header::CONTENT_TYPE, HeaderValue, Response, Uri},
    path::{end, path},
    query, redirect,
    reject::{self, Reject},
    reply, Filter, Rejection, Reply,
};

impl Reject for UrlTemplateError {}

/// New type for `warp::http::uri::InvalidUri` so it can be marked with `Reject`.
#[derive(Error, Debug)]
#[error(transparent)]
struct InvalidUri(#[from] warp::http::uri::InvalidUri);

impl Reject for InvalidUri {}

fn search_query() -> impl Filter<Extract = (SearchQuery,), Error = Rejection> + Clone {
    #[derive(Deserialize, Debug)]
    struct Query {
        q: String,
    }
    query().and_then(|query: Query| async {
        debug!(?query);
        let Query { q } = query;
        let query = q.into();
        Result::<_, Rejection>::Ok(query)
    })
}

trait HtmlRepr {
    /// Returns HTML representation.
    fn repr(&self) -> String;
}

impl HtmlRepr for Vec<Target> {
    fn repr(&self) -> String {
        self.iter()
            .map(|Target { prefix, name, url_template }| {
                format!("<li><b>{}</b> for {}, <code>{}</code></li>", prefix, name, url_template)
            })
            .join("")
    }
}

fn index(
    config: Arc<Configuration>,
) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let path = end();
    get().and(path).and_then(move || {
        let body = formatdoc! {r#"
            <!DOCTYPE html>
            <html>
                <head>
                    <meta charset="utf-8">
                    <title>wmd</title>
                    <link
                        rel="search"
                        href="/opensearch.xml"
                        type="application/opensearchdescription+xml"
                        title="wmd">
                </head>
                <body>
                    <h1>wmd</h1>
                    <h2>Prefixes</h2>
                    <ul>
                        {targets}
                    </ul>
                </body>
            </html>
            "#,
            targets = config.targets.repr(),
        };
        async move {
            let body = body.clone();
            let reply = reply::html(body);
            Result::<_, Rejection>::Ok(reply)
        }
    })
}

fn open_search(
    config: Arc<Configuration>,
) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let path = path("opensearch.xml").and(end());
    get().and(path).and_then(move || {
        let body = formatdoc! {r#"
                <?xml version="1.0" encoding="utf-8"?>
                <OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/">
                    <ShortName>wmd</ShortName>
                    <Description>Web search hub.</Description>
                    <Url type="text/html" template="{url}search?q={{searchTerms}}"/>
                </OpenSearchDescription>
            "#,
            url = &config.url
        };
        async move {
            let reply = {
                let mut resp = Response::new(body);
                let headers = resp.headers_mut();
                headers.insert(
                    CONTENT_TYPE,
                    HeaderValue::from_static(
                        "application/opensearchdescription+xml; charset=utf-8",
                    ),
                );
                resp
            };
            Result::<_, Rejection>::Ok(reply)
        }
    })
}

fn robots_txt() -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let body = indoc! {"
        User-agent: *
        Disallow: /
    "};
    let path = path("robots.txt").and(end());
    get().and(path).and_then(move || async move {
        let mut reply = Response::new(body);
        reply
            .headers_mut()
            .insert(CONTENT_TYPE, HeaderValue::from_static("text/plain; charset=utf-8"));
        Result::<_, Rejection>::Ok(reply)
    })
}

fn search(
    config: Arc<Configuration>,
) -> impl Filter<Extract = impl Reply, Error = Rejection> + Clone {
    let path = path("search").and(end());
    get().and(path).and(search_query()).and_then(move |query: SearchQuery| {
        let config = config.clone();
        async move {
            let prefix = query.prefix();

            // get query keywords by checking prefix
            let keywords: &str = match prefix {
                Some(prefix) => {
                    // check if query prefix is known
                    let known_prefix = config.has_template_for(prefix);
                    if known_prefix {
                        query.keywords()
                    } else {
                        // if not treat prefix as keywords
                        &query.query
                    }
                }
                None => query.keywords(),
            };

            // get template
            let template = prefix
                .map(|x| config.template_for(x))
                .flatten()
                .or_else(|| config.default_template());

            match template {
                None => {
                    // found no matching template
                    // serve 404
                    Err(reject::not_found())
                }
                Some(template) => {
                    // found matching template
                    // build url
                    let url = template.build_url(keywords)?;
                    // build uri....
                    let url: Uri = url.as_str().try_into().map_err(Into::<InvalidUri>::into)?;
                    // serve redirect
                    Ok(redirect::temporary(url))
                }
            }
        }
    })
}

fn routes(config: Configuration) -> impl Filter<Extract = impl Reply> + Clone {
    let config = Arc::new(config);
    index(config.clone())
        .or(open_search(config.clone()))
        .or(robots_txt())
        .or(search(config.clone()))
        .with(warp::trace::request())
}

pub fn server(config: Configuration) -> warp::Server<impl Filter<Extract = impl Reply> + Clone> {
    let routes = routes(config);
    warp::serve(routes)
}

#[cfg(test)]
mod integration_tests {

    use super::*;
    use pretty_assertions::assert_eq;
    use std::path::Path;
    use warp::{
        http::{self, StatusCode},
        test::request,
    };

    async fn routes() -> impl Filter<Extract = impl Reply> + Clone {
        let config = Configuration::from_path(Path::new("./wmd.example.toml")).await.unwrap();
        let routes = super::routes(config);
        routes
    }

    #[tokio::test]
    async fn test_get_index() {
        let routes = routes().await;

        let resp = request().method("GET").path("/").reply(&routes).await;

        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(resp.headers().get(CONTENT_TYPE).unwrap(), "text/html; charset=utf-8");
        assert_eq!(
            resp.body(),
            indoc! {r#"
                <!DOCTYPE html>
                <html>
                    <head>
                        <meta charset="utf-8">
                        <title>wmd</title>
                        <link
                            rel="search"
                            href="/opensearch.xml"
                            type="application/opensearchdescription+xml"
                            title="wmd">
                    </head>
                    <body>
                        <h1>wmd</h1>
                        <h2>Prefixes</h2>
                        <ul>
                            <li><b>rs</b> for The Rust Standard Library, <code>https://doc.rust-lang.org/std/index.html?search={keywords}</code></li><li><b>drx</b> for Docs.rs (direct), <code>https://docs.rs/{keywords}</code></li>
                        </ul>
                    </body>
                </html>
            "#}
        );
    }

    #[tokio::test]
    async fn test_get_opensearch() {
        let routes = routes().await;

        let resp = request().method("GET").path("/opensearch.xml").reply(&routes).await;

        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(
            resp.headers().get(CONTENT_TYPE).unwrap(),
            "application/opensearchdescription+xml; charset=utf-8"
        );
        assert_eq!(
            resp.body(),
            indoc! {r#"
                <?xml version="1.0" encoding="utf-8"?>
                <OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/">
                    <ShortName>wmd</ShortName>
                    <Description>Web search hub.</Description>
                    <Url type="text/html" template="http://127.0.0.1:39496/search?q={searchTerms}"/>
                </OpenSearchDescription>
            "#}
        );
    }

    #[tokio::test]
    async fn test_get_search() {
        let routes = routes().await;

        let resp = request().method("GET").path("/search?q=rs+result").reply(&routes).await;

        assert_eq!(resp.status(), StatusCode::TEMPORARY_REDIRECT);
        assert_eq!(
            resp.headers().get(http::header::LOCATION).map(|x| x.to_str()).unwrap().unwrap(),
            "https://doc.rust-lang.org/std/index.html?search=result"
        );
    }

    #[tokio::test]
    async fn test_get_search_without_prefix() {
        // On receiving search request without prefix
        // if there's default url template, serve redirect to url built with default url template
        // otherwise serve 404.

        // todo(kfj): missing test for 404.

        let routes = routes().await;

        let resp = request().method("GET").path("/search?q=result").reply(&routes).await;

        assert_eq!(resp.status(), StatusCode::TEMPORARY_REDIRECT);
        assert_eq!(
            resp.headers().get(http::header::LOCATION).map(|x| x.to_str()).unwrap().unwrap(),
            "https://doc.rust-lang.org/std/index.html?search=result"
        );
    }

    #[tokio::test]
    async fn test_get_search_with_unknown_prefix() {
        // On receiving request with unknown prefix
        // treat the unknown prefix as keywords for default url template
        // if there's no default template, serve 404.

        // todo(kfj): missing test for 404.

        let routes = routes().await;

        let resp = request().method("GET").path("/search?q=gg+result").reply(&routes).await;

        assert_eq!(resp.status(), StatusCode::TEMPORARY_REDIRECT);
        assert_eq!(
            resp.headers().get(http::header::LOCATION).map(|x| x.to_str()).unwrap().unwrap(),
            "https://doc.rust-lang.org/std/index.html?search=gg%20result"
        );
    }

    #[tokio::test]
    async fn test_get_robots_txt() {
        let routes = routes().await;

        let resp = request().method("GET").path("/robots.txt").reply(&routes).await;

        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(resp.headers().get(CONTENT_TYPE).unwrap(), "text/plain; charset=utf-8");
        assert_eq!(
            resp.body(),
            indoc! {"
                User-agent: *
                Disallow: /
            "}
        );
    }
}
