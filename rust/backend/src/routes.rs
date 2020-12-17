use crate::{AppState, index, api};

use tide::{
    http::headers::HeaderValue, security::CorsMiddleware, security::Origin,
};

pub fn routes(state: AppState)->tide::Server<AppState>{
    let mut app = tide::with_state(state.clone());

    let cors = CorsMiddleware::new()
    .allow_methods("GET, POST, PATCH, DELETE, OPTIONS".parse::<HeaderValue>().unwrap())
    .allow_origin(Origin::from("*"))
    .allow_credentials(false);

    app.with(cors.clone()).at("/").all(index);
    app.with(cors.clone()).at("/rstodos").get(api::get_todos);
    app.with(cors.clone()).at("/rstodos").delete(api::delete_todos);
    app.with(cors.clone()).at("/rstodos").post(api::post_todo);
    app.with(cors.clone()).at("/rstodos/allcomplete").patch(api::patch_all_complete);
    app.with(cors.clone()).at("/rstodos/:todo_id").get(api::get_todo);
    app.with(cors.clone()).at("/rstodos/:todo_id").delete(api::delete_todo);
    app.with(cors.clone()).at("/rstodos/:todo_id").patch(api::patch_todo);
    app.with(tide::log::LogMiddleware::new());
    app
}
