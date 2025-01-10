use axum::{http::StatusCode, routing::post, Json, Router};
use tower_http::services::ServeDir;

#[tokio::main]
async fn main() {
    // build our application with a route
    let app: Router = Router::new()
        .route("/feed", post(get_feed))
        // `GET /` goes to `root`
        .fallback_service(ServeDir::new("public"));

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

async fn get_feed(
    // this argument tells axum to parse the request body
    // as JSON into a `CreateUser` type
    Json(payload): Json<String>,
) -> Json<String> {
    let body = reqwest::get("https://www.rust-lang.org")
        .await?
        .text()
        .await?;

    /*
        let client = reqwest::Client::new();
    let res = client.post("http://httpbin.org/post")
        .body("the exact body that is sent")
        .send()
        .await?;
    */

    Json(body)
}
