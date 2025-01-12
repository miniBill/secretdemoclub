use axum::{routing::post, Json, Router};
use tokio::io;
use tower_http::services::ServeDir;

#[tokio::main]
async fn main() -> io::Result<()> {
    let app: Router = Router::new()
        .route("/feed", post(get_feed))
        .fallback_service(ServeDir::new("public"));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000").await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn get_feed(
    // this argument tells axum to parse the request body
    // as JSON into a `CreateUser` type
    Json(payload): Json<String>,
) -> Json<String> {
    let body = match rust_request(payload).await {
        Ok(rust) => rust,
        Err(_) => {
            return Json("Reqwest failed".to_string());
        }
    };

    /*
        let client = reqwest::Client::new();
    let res = client.post("http://httpbin.org/post")
        .body("the exact body that is sent")
        .send()
        .await?;
    */

    Json(body)
}

async fn rust_request(payload: String) -> Result<String, reqwest::Error> {
    return reqwest::get("https://www.rust-lang.org")
        .await?
        .text()
        .await;
}
