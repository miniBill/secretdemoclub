use std::sync::{Arc, RwLock};

use anyhow::anyhow;
use axum::{
    extract::{FromRef, State},
    http::{HeaderMap, HeaderValue, StatusCode},
    routing::post,
    Router,
};
use clap::Parser;
use serde::Deserialize;
use tower_http::services::{ServeDir, ServeFile};
use types::{AccessToken, Identity, Included};

mod types;

#[derive(Clone)]
struct AppState {
    config: Arc<RwLock<AppConfig>>,
}

#[derive(Deserialize, Clone)]
struct AppConfig {
    client_id: String,
    client_secret: String,
    redirect_uri: String,
    orla_campaign_id: String,
    bronze_tier: String,
    silver_tier: String,
    gold_tier: String,
}

impl FromRef<AppState> for AppConfig {
    fn from_ref(state: &AppState) -> Self {
        (*state.config.read().unwrap()).clone()
    }
}

#[derive(Parser, Debug, Clone)]
#[command(version, about)]
struct Args {
    #[arg(short, long, default_value_t = 3000)]
    port: u16,

    #[arg(short, long, default_value = "config.toml")]
    config: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    println!("🚀 SDC HQ server starting");

    let config_file: String = args.config;

    println!("📖 Reading config file");
    let raw_config = tokio::fs::read_to_string(&config_file)
        .await
        .expect("Failed to open config file");
    let parsed_config: AppConfig =
        toml::from_str(&raw_config).expect("Failed to parse config file");

    let app_state = AppState {
        config: Arc::new(RwLock::new(parsed_config)),
    };

    let app: Router = Router::new()
        .route("/api", post(post_api))
        .fallback_service(ServeDir::new("public").fallback(ServeFile::new("public/index.html")))
        .with_state(app_state.clone());

    println!("👂 Spawning task listening for SIGHUP for config update");
    tokio::spawn(reload_config_on_sighup(config_file, app_state));

    println!("🚪 Listening on port {}", args.port);
    let listener = tokio::net::TcpListener::bind(("localhost", args.port)).await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn reload_config_on_sighup(config_file: String, app_state: AppState) -> anyhow::Result<()> {
    let mut stream = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::user_defined1())?;

    // Print whenever a HUP signal is received
    loop {
        stream.recv().await;
        println!("‼️ Got SIGHUP, reloading config");

        let raw_config = match tokio::fs::read_to_string(&config_file).await {
            Ok(raw_config) => raw_config,
            Err(e) => {
                println!("Failed to open config file: {}", e);
                continue;
            }
        };

        let parsed_config: AppConfig = match toml::from_str(&raw_config) {
            Ok(parsed_config) => parsed_config,
            Err(e) => {
                println!("Failed to parse config file: {}", e);
                continue;
            }
        };

        *app_state.config.write().unwrap() = parsed_config;
    }
}

async fn post_api(State(app_config): State<AppConfig>, code: String) -> (StatusCode, String) {
    println!("POST /api");
    let access_token = match get_access_token(&app_config, code).await {
        Ok(access_token) => access_token,
        Err(e) => {
            dbg!(e);
            return (StatusCode::UNAUTHORIZED, "Token request failed".to_string());
        }
    };

    let tier: Tier = match get_tier(&app_config, access_token.clone()).await {
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
        Tier::Bronze => (StatusCode::OK, app_config.bronze_tier.clone()),
        Tier::Silver => (StatusCode::OK, app_config.silver_tier.clone()),
        Tier::Gold => (StatusCode::OK, app_config.gold_tier.clone()),
    }
}

async fn get_access_token(app_config: &AppConfig, code: String) -> Result<String, reqwest::Error> {
    let params = [
        ("code", code),
        ("grant_type", "authorization_code".to_string()),
        ("client_id", app_config.client_id.clone()),
        ("client_secret", app_config.client_secret.clone()),
        ("redirect_uri", app_config.redirect_uri.clone()),
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

async fn get_tier(app_config: &AppConfig, access_token: String) -> anyhow::Result<Tier> {
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
            if membership.relationships.campaign.data.id == *app_config.orla_campaign_id {
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
