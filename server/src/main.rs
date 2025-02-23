use anyhow::anyhow;
use axum::{
    http::{HeaderMap, HeaderValue},
    routing::post,
    Json, Router,
};
use lazy_static::lazy_static;
use tower_http::services::ServeDir;
use types::{AccessToken, Identity, Included};

mod types;

lazy_static! {
    static ref client_id: String = std::env::var("clientId").unwrap();
    static ref client_secret: String = std::env::var("clientSecret").unwrap();
    static ref redirect_uri: String = std::env::var("redirectUri").unwrap();
    static ref orla_campaign_id: String = std::env::var("orlaCampaignId").unwrap();
    static ref bronze_tier: String = std::env::var("bronzeTier").unwrap();
    static ref silver_tier: String = std::env::var("silverTier").unwrap();
    static ref gold_tier: String = std::env::var("goldTier").unwrap();
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app: Router = Router::new()
        .route("/feed", post(post_feed))
        .fallback_service(ServeDir::new("public"));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000").await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn post_feed(Json(code): Json<String>) -> Json<String> {
    let access_token = match get_access_token(code).await {
        Ok(access_token) => access_token,
        Err(e) => {
            dbg!("{}", e);
            return Json("Token request failed".to_string());
        }
    };

    let tier: Tier = match get_tier(access_token.clone()).await {
        Ok(tier) => tier,
        Err(e) => {
            dbg!("{}", e);
            return Json("Tier request failed".to_string());
        }
    };

    Json(match tier {
        Tier::Bronze => bronze_tier.clone(),
        Tier::Silver => silver_tier.clone(),
        Tier::Gold => gold_tier.clone(),
    })
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

#[derive(Debug)]
enum Tier {
    Bronze,
    Silver,
    Gold,
}

async fn get_tier(access_token: String) -> anyhow::Result<Tier> {
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
        if let Included::Membership(membership) = include {
            if membership.relationships.campaign.data.id == *orla_campaign_id {
                return match membership.attributes.title.trim() {
                    "Bronze membership" => Ok(Tier::Bronze),
                    "Bronze membership (old/unpublished tier)" => Ok(Tier::Bronze),
                    "Silver membership" => Ok(Tier::Silver),
                    "Gold membership" => Ok(Tier::Gold),
                    trimmed => Err(anyhow!("Unknown tier: {}", trimmed)),
                };
            }
        }
    }

    Err(anyhow!("No tier found"))
}
