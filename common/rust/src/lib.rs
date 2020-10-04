use serde::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TodoResponse {
    pub tr_id: i32,
    pub tr_url: String,
    pub tr_title: String,
    pub tr_completed: bool,
    pub tr_order: i32,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TodoAction {
    pub tact_title: Option<String>,
    pub tact_completed: Option<bool>,
    pub tact_order: Option<i32>,
}
