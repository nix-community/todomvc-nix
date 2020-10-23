use serde::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TodoResponse {
    pub id: i32,
    // pub url: String,
    pub title: String,
    pub completed: bool,
    pub orderx: i32,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TodoAction {
    pub title: String,
    pub completed: bool,
    pub orderx: i32,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TodoCompletion {
    pub all_complete: bool
}
