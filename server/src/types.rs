use serde::Deserialize;

#[derive(Deserialize)]
pub struct AccessToken {
    pub access_token: String,
}

#[derive(Deserialize)]
pub struct Identity {
    pub included: Vec<Included>,
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum Included {
    Membership(Membership),
    Ignored(Ignored),
}

#[derive(Deserialize)]
pub struct Ignored {}

#[derive(Deserialize)]
pub struct Membership {
    pub relationships: Relationships,
    pub attributes: Attributes,
}

#[derive(Deserialize)]
pub struct Relationships {
    pub campaign: Campaign,
}

#[derive(Deserialize)]
pub struct Campaign {
    pub data: Data,
}

#[derive(Deserialize)]
pub struct Data {
    pub id: String,
}

#[derive(Deserialize)]
pub struct Attributes {
    pub title: String,
}
