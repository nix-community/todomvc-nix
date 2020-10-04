use tide::Request;
use crate::{ AppState, model::{ Todo, todos_to_tactions, }};
use common::TodoAction;
use http_types::{StatusCode, Method, Body};
use serde::*;
use sqlx::postgres::PgQueryAs;

pub async fn get_todos(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    let s: Vec<Todo> = sqlx::query_as("SELECT * FROM todos")
        .fetch_all(&req.state().db_pool).await?;
    let d: Vec<TodoAction> = todos_to_tactions(s).await?;
    res.set_body(Body::from_json(&d)?);
    Ok(res)
}

pub async fn delete_todos(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    Ok(res)
}

pub async fn post_todo(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    Ok(res)
}

pub async fn get_todo(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    Ok(res)
}

pub async fn delete_todo(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    Ok(res)
}

pub async fn patch_todo(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    Ok(res)
}
