use axum::{extract::Query, routing::post,routing::get, Json, Router};
use lazy_static::lazy_static;
use tower_http::services::ServeDir;
use serde::Deserialize;

lazy_static! {
    static ref client_id: String = std::env::var("clientId").unwrap();
    static ref client_secret: String = std::env::var("clientSecret").unwrap();
    static ref redirect_uri: String = std::env::var("redirectUri").unwrap();
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app: Router = Router::new()
        .route("/feed", get(get_feed))
        .route("/feed", post(post_feed))
        .fallback_service(ServeDir::new("public"));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000").await?;
    axum::serve(listener, app).await?;
    Ok(())
}

#[derive(Deserialize)]
struct UrlParam {
    code : String 
}

async fn get_feed(query : Query<UrlParam>) -> Json<String> {
    let body = match get_access_token(query.code.clone()).await {
        Ok(rust) => rust,
        Err(_) => {
            return Json("Reqwest failed".to_string());
        }
    };

    Json(body)
}

async fn post_feed(Json(code): Json<String>) -> Json<String> {
    let body = match get_access_token(code).await {
        Ok(rust) => rust,
        Err(_) => {
            return Json("Reqwest failed".to_string());
        }
    };

    Json(body)
}

async fn get_access_token(code: String) -> Result<String, reqwest::Error> {
    let params = [
        ("code", code),
        ("grant_type", "authorization_code".to_string()),
        ("client_id", client_id.to_string()),
        ("client_secret", client_secret.to_string()),
        ("redirect_uri", redirect_uri.to_string()),
    ];
    let client = reqwest::Client::new();
    client
        .post("https://www.patreon.com/api/oauth2/token")
        .form(&params)
        .send()
        .await?
        .text()
        .await
}
