use anyhow::anyhow;
use axum::{
    http::{HeaderMap, HeaderValue, StatusCode},
    routing::post,
    Router,
};
use lazy_static::lazy_static;
use tower_http::services::{ServeDir, ServeFile};
use types::{AccessToken, Identity, Included};

mod types;

lazy_static! {
    static ref client_id: String =
        std::env::var("clientId").expect("Missing env variable: clientId");
    static ref client_secret: String =
        std::env::var("clientSecret").expect("Missing env variable: clientSecret");
    static ref redirect_uri: String =
        std::env::var("redirectUri").expect("Missing env variable: redirectUri");
    static ref orla_campaign_id: String =
        std::env::var("orlaCampaignId").expect("Missing env variable: orlaCampaignId");
    static ref bronze_tier: String =
        std::env::var("bronzeTier").expect("Missing env variable: bronzeTier");
    static ref silver_tier: String =
        std::env::var("silverTier").expect("Missing env variable: silverTier");
    static ref gold_tier: String =
        std::env::var("goldTier").expect("Missing env variable: goldTier");
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app: Router = Router::new()
        .route("/api", post(post_api))
        .fallback_service(ServeDir::new("public").fallback(ServeFile::new("public/index.html")));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000").await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn post_api(code: String) -> (StatusCode, String) {
    println!("POST {code}");
    let access_token = match get_access_token(code).await {
        Ok(access_token) => access_token,
        Err(e) => {
            dbg!(e);
            return (StatusCode::UNAUTHORIZED, "Token request failed".to_string());
        }
    };

    let tier: Tier = match get_tier(access_token.clone()).await {
        Ok(tier) => tier,
        Err(e) => {
            dbg!(e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Tier request failed".to_string(),
            );
        }
    };

    match tier {
        Tier::Bronze => (StatusCode::OK, bronze_tier.clone()),
        Tier::Silver => (StatusCode::OK, silver_tier.clone()),
        Tier::Gold => (StatusCode::OK, gold_tier.clone()),
    }
}

async fn get_access_token(code: String) -> Result<String, reqwest::Error> {
    let params = [
        ("code", code),
        ("grant_type", "authorization_code".to_string()),
        ("client_id", client_id.to_string()),
        ("client_secret", client_secret.to_string()),
        ("redirect_uri", redirect_uri.to_string()),
    ];
    let response = reqwest::Client::new()
        .post("https://www.patreon.com/api/oauth2/token")
        .form(&params)
        .send()
        .await?;

    // dbg!(response.text().await?);
    // Ok("TODO".to_string())

    let access_token = response.json::<AccessToken>().await?.access_token;

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
        HeaderValue::from_str(&format!("Bearer {}", access_token))?,
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
