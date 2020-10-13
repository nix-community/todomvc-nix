use tide::Request;
use crate::{ AppState, model::{ Todo, todos_to_tresponses, todo_to_tresponse}};
use common::{TodoAction, TodoResponse};
use http_types::{StatusCode, Method, Body};
use serde::*;
use sqlx::postgres::PgQueryAs;

pub async fn get_todos(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    let s: Vec<Todo> = sqlx::query_as("SELECT * FROM todos")
        .fetch_all(&req.state().db_pool).await?;
    let d: Vec<TodoResponse> = todos_to_tresponses(s).await?;
    res.set_body(Body::from_json(&d)?);
    Ok(res)
}

pub async fn delete_todos(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    let _del = sqlx::query("DELETE FROM todos")
        .execute(&req.state().db_pool).await?;
    Ok(res)
}

pub async fn post_todo(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    let form = req.body_json::<TodoAction>().await?;
    log::trace!("form: {:?}", form.clone());
    //let todo =  taction_to_todo(form).await?;
    let add_todo: Result<Todo, _> = sqlx::query_as("INSERT INTO todos (title, completed, orderx) values($1,$2, $3) returning id, title, completed, orderx")
        .bind(&form.title)
        .bind(form.completed)
        .bind(form.orderx)
        .fetch_one(&req.state().db_pool).await;
    match add_todo {
        Ok(todo) => {
            let s: Todo = sqlx::query_as("SELECT * FROM todos WHERE id = $1")
                .bind(todo.id)
                .fetch_one(&req.state().db_pool).await?;
            res.set_body(Body::from_json(&todo_to_tresponse(s).await?)?);
            Ok(res)
        }
        Err(_e) => {
            res.set_body(Body::from_json(&"Failed to insert todo")?);
            Ok(res)
        }
    }
}

pub async fn get_todo(mut req: Request<AppState>) -> tide::Result{
    let mut res = tide::Response::new(StatusCode::Ok);
    let todo_id = req.param::<i32>("todo_id")?;
    let s: Todo = sqlx::query_as("SELECT * FROM todos WHERE id = $1")
        .bind(todo_id)
        .fetch_one(&req.state().db_pool).await?;
    res.set_body(Body::from_json(&todo_to_tresponse(s).await?)?);
    Ok(res)
}

pub async fn delete_todo(mut req: Request<AppState>) -> tide::Result {
    let mut res = tide::Response::new(StatusCode::Ok);
    let todo_id = req.param::<i32>("todo_id")?;
    let delete_todo = sqlx::query("DELETE FROM Todo WHERE id = $1")
        .bind(todo_id)
        .execute(&req.state().db_pool).await;
    match delete_todo {
        Ok(_todo) => {
            res.set_body(Body::from_json(&"Delete success")?);
            Ok(res)
        }
        Err(_e) => {
            res.set_body(Body::from_json(&"Failed to delete todo")?);
            Ok(res)
        }
    }
}

pub async fn patch_todo(mut req: Request<AppState>) -> tide::Result {
    let mut res = tide::Response::new(StatusCode::Ok);
    let form = req.body_json::<TodoAction>().await?;
    let todo_id = req.param::<i32>("todo_id")?;
    let update_todo = sqlx::query("UPDATE todos set title = $1, completed = $2, orderx = $3 WHERE id = $4")
        .bind(&form.title)
        .bind(form.completed)
        .bind(form.orderx)
        .bind(todo_id)
        .execute(&req.state().db_pool).await;
    match update_todo {
        Ok(_todo) => {
            res.set_body(Body::from_json(&"Update successful")?);
            Ok(res)
        }
        Err(_e) => {
            res.set_body(Body::from_json(&"Cannot update todo")?);
            Ok(res)
        }
    }
}
