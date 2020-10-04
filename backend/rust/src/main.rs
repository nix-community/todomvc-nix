extern crate tide;
extern crate async_std;
extern crate serde;
extern crate env_logger;
extern crate uuid;
extern crate log;
use async_std::task;
use tide::Request;
use tide::StatusCode;
use sqlx::postgres::PgPool;
use std::env;
use dotenv::dotenv;

#[async_std::main]
async fn main() -> Result<(), std::io::Error> {
    tide::log::start();
    let mut app = tide::new();
    app.at("/").get(|_| async { Ok("Hello, world!") });
    app.listen("127.0.0.1:8183").await?;
    Ok(())
}
