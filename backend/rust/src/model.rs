use common::TodoAction;
use sqlx;

#[derive(Clone, Debug, sqlx::FromRow)]
pub struct Todo {
    pub act_id: i32,
    pub act_title: Option<String>,
    pub act_completed: Option<bool>,
    pub act_order: Option<i32>,
}

// pub async fn taction_to_todo(taction: TodoAction) -> Todo {
//     Todo {
//        act_title: taction.tact_title,
//        act_completed: taction.tact_completed,
//        act_order: taction.tact_order,
//     }
// }

pub async fn todos_to_tactions(todos: Vec<Todo>) -> anyhow::Result<Vec<TodoAction>> {
    let mut new_vec = Vec::new();
    for todo in todos {
        let todo_val = TodoAction {
            tact_title: todo.act_title,
            tact_completed: todo.act_completed,
            tact_order: todo.act_order,
         };
        new_vec.push(todo_val);
    }
    Ok(new_vec)
}
