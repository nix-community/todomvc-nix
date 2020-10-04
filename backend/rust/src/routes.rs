use crate::{AppState, index, api};

pub fn routes(state: AppState)->tide::Server<AppState>{
    let mut app = tide::with_state(state.clone());
    app.at("/").all(index);
    app.at("/rstodos").get(api::get_todos);
    app.at("/rstodos").delete(api::delete_todos);
    app.at("/rstodos").post(api::post_todo);
    app.at("/rstodos/:todo_id").get(api::get_todo);
    app.at("/rstodos/:todo_id").delete(api::delete_todo);
    app.at("/rstodos/:todo_id").patch(api::patch_todo);
    app
}
