use anyhow::anyhow;
use axum::{
    extract::Query,
    http::{HeaderMap, HeaderValue},
    routing::{get, post},
    Json, Router,
};
use lazy_static::lazy_static;
use serde::Deserialize;
use tower_http::services::ServeDir;

lazy_static! {
    static ref client_id: String = std::env::var("clientId").unwrap();
    static ref client_secret: String = std::env::var("clientSecret").unwrap();
    static ref redirect_uri: String = std::env::var("redirectUri").unwrap();
    static ref orla_campaign_id: String = std::env::var("orlaCampaignId").unwrap();
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
    code: String,
}

async fn get_feed(query: Query<UrlParam>) -> Json<String> {
    let body = match get_access_token(query.code.clone()).await {
        Ok(rust) => rust,
        Err(_) => {
            return Json("Reqwest failed".to_string());
        }
    };

    Json(body)
}

async fn post_feed(Json(code): Json<String>) -> Json<String> {
    let access_token = match get_access_token(code).await {
        Ok(access_token) => access_token,
        Err(_) => {
            return Json("Token request failed".to_string());
        }
    };

    let tier = match get_tier(access_token).await {
        Ok(tier) => tier,
        Err(_) => return Json("Tier request failed".to_string()),
    };

    Json(format!("{:?}", tier))
}

#[derive(Deserialize)]
struct AccessToken {
    access_token: String,
}

async fn get_access_token(code: String) -> Result<String, reqwest::Error> {
    let params = [
        ("code", code),
        ("grant_type", "authorization_code".to_string()),
        ("client_id", client_id.to_string()),
        ("client_secret", client_secret.to_string()),
        ("redirect_uri", redirect_uri.to_string()),
    ];
    let access_token = reqwest::Client::new()
        .post("https://www.patreon.com/api/oauth2/token")
        .form(&params)
        .send()
        .await?
        .json::<AccessToken>()
        .await?
        .access_token;

    Ok(access_token)
}

#[derive(Deserialize)]
struct Identity {
    included: Vec<Included>,
}

#[derive(Deserialize)]
struct Included {
    relationships: Relationships,
    attributes: Attributes,
}

#[derive(Deserialize)]
struct Relationships {
    campaign: Campaign,
}

#[derive(Deserialize)]
struct Campaign {
    data: Data,
}

#[derive(Deserialize)]
struct Data {
    id: String,
}

#[derive(Deserialize)]
struct Attributes {
    title: String,
}

#[derive(Debug)]
enum Membership {
    Bronze,
    Silver,
    Gold,
}

async fn get_tier(access_token: String) -> anyhow::Result<Membership> {
    let mut headers = HeaderMap::new();
    headers.insert(
        "Authorization",
        HeaderValue::from_str(&format!("Bearer {}", access_token)).unwrap(),
    );

    let tier : Identity = reqwest::Client::new()
        .get("https://www.patreon.com/api/oauth2/v2/identity?include=memberships.currently_entitled_tiers.campaign&fields[tier]=title")
        .headers(headers)
        .send()
        .await?
        .json::<Identity>()
        .await?;
    for include in tier.included {
        if include.relationships.campaign.data.id == *orla_campaign_id {
            return match include.attributes.title.trim() {
                "Bronze membership" => Ok(Membership::Bronze),
                "Silver membership" => Ok(Membership::Silver),
                "Gold membership" => Ok(Membership::Gold),
                trimmed => Err(anyhow!("Unknown tier: {}", trimmed)),
            };
        }
    }

    Err(anyhow!("No tier found"))
}
