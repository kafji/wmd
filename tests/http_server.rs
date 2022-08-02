use http::StatusCode;
use reqwest::redirect::Policy;
use std::time::Duration;
use tokio::{sync::oneshot, task};

macro_rules! launch_server {
    ($ready_rx:ident) => {
        let (ready_tx, $ready_rx) = oneshot::channel();
        let _server = task::spawn(async move {
            let port = getport::reserve_tcp_port();
            let port = port.take();
            let app = tokio::spawn(async move {
                let cfgsrc = wmd::ConfigSource::File("./wmd.example.toml");
                wmd::start_app(port, &[cfgsrc]).await.unwrap();
            });
            tokio::time::sleep(Duration::from_millis(100)).await;
            ready_tx.send(port).unwrap();
            app.await
        });
    };
}

macro_rules! request_once {
    ($path:expr) => {{
        launch_server!(ready_rx);
        let port = ready_rx.await.unwrap();
        let client = reqwest::Client::builder()
            .redirect(Policy::none())
            .build()
            .expect("failed to create HTTP client");
        let response = client
            .get(format!("http://127.0.0.1:{}{}", port, $path))
            .send()
            .await
            .unwrap();
        response
    }};
}

macro_rules! assert_string_response {
    ($res:expr, $status:expr, $mime:expr, $assert_body:expr) => {
        let status = $res.status();
        assert_ne!(status, StatusCode::NOT_FOUND);
        let resp_mime: mime::Mime = {
            let response_mime = $res
                .headers()
                .get("content-type")
                .expect("missing header `content-type`")
                .to_str()
                .unwrap();
            response_mime.parse().unwrap()
        };
        let body = $res.text().await.unwrap();
        $assert_body(body);
        assert_eq!(resp_mime, $mime);
        assert_eq!(status, StatusCode::OK);
    };
}

#[tokio::test]
async fn test_get_index() {
    let response = request_once!("/");

    assert_string_response!(response, StatusCode::OK, mime::TEXT_HTML_UTF_8, |body| {
        insta::assert_display_snapshot!(body);
    });
}

#[tokio::test]
async fn test_get_index_with_param() {
    let response = request_once!("/?k=hello");

    assert_string_response!(response, StatusCode::OK, mime::TEXT_HTML_UTF_8, |body| {
        insta::assert_display_snapshot!(body);
    });
}

#[tokio::test]
async fn test_do_search() {
    {
        // trivial case

        let response = request_once!("/search?q=drx%20tokio");

        assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
        assert_eq!(
            response.headers().get("location").unwrap(),
            "https://docs.rs/tokio"
        );
    };

    {
        // keywords should be trimmed
        let response = request_once!("/search?q=drx%20tokio%20");

        assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
        assert_eq!(
            response.headers().get("location").unwrap(),
            "https://docs.rs/tokio"
        );
    };
}

#[tokio::test]
async fn test_do_search_with_missing_prefix() {
    let response = request_once!("/search?q=tokio");

    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get("location").unwrap(),
        "http://127.0.0.1:3000/?k=tokio"
    );
}

#[tokio::test]
async fn test_get_prefixes() {
    let response = request_once!("/prefixes");

    assert_string_response!(response, StatusCode::OK, mime::TEXT_HTML_UTF_8, |body| {
        insta::assert_display_snapshot!(body);
    });
}

#[tokio::test]
async fn test_get_opensearch_xml() {
    let response = request_once!("/opensearch.xml");

    let mime: mime::Mime = r"application/opensearchdescription+xml; charset=utf-8"
        .parse()
        .unwrap();
    assert_string_response!(response, StatusCode::OK, mime, |body| {
        insta::assert_display_snapshot!(body);
    });
}

#[tokio::test]
async fn test_get_robots_txt() {
    let response = request_once!("/robots.txt");

    assert_string_response!(response, StatusCode::OK, mime::TEXT_PLAIN_UTF_8, |body| {
        insta::assert_display_snapshot!(body);
    });
}

// todo(kfj): test ripm
