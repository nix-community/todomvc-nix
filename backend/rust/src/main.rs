extern crate tide;
extern crate async_std;
extern crate serde;
extern crate uuid;

mod ructe_tide;

use ructe_tide::{RenderBuilder};
use async_std::task;
use tide::{Request, Response, StatusCode};
use sqlx::postgres::PgPool;
use std::env;

mod model;
mod routes;
mod api;

#[derive(Debug, Clone)]
pub struct AppState {
    pub db_pool: PgPool
}

fn main() -> anyhow::Result<()> {
    #[cfg(debug_assertions)]
    tide::log::start();

    // #[cfg(debug_assertions)]
    // tide::log::with_level(tide::log::max_level());
    tide::log::info!("Hello world");

    task::block_on(async {
        let pool = PgPool::new(&env::var("DATABASE_URL")?).await?;
        let state = AppState {
            db_pool: pool.clone()
        };
        let app = crate::routes::routes(state.clone());
        // app.at("/static").serve_dir("./client/pkg/")?;
        app.listen("127.0.0.1:8183").await?;
        Ok(())
    })
}
async fn index(_req: Request<AppState>)->tide::Result{
    Ok(Response::builder(StatusCode::Ok)
    .render_html(|o| templates::index(o, String::from("todomvc-nix")))
    .build())
}

include!(concat!(env!("OUT_DIR"), "/templates.rs"));
