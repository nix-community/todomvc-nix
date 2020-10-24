use std::rc::Rc;
use std::cell::Cell;

use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::{JsFuture, spawn_local};
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;

use futures_signals::signal::{Signal, SignalExt, Mutable};
use futures_signals::signal_vec::{SignalVec, SignalVecExt, MutableVec};
use dominator::{Dom, text_signal, html, clone, events, link};

use js_sys::Error;
use web_sys::{window, Response, RequestInit, Headers, RequestMode};

use common::{TodoResponse, TodoAction, TodoCompletion};

use crate::todo::{Todo, response_to_todo};
use crate::routing::Route;

#[derive(Debug)]
pub struct App {
    todo_id: Cell<i32>,

    new_todo_title: Mutable<String>,

    pub todo_list: MutableVec<Rc<Todo>>,
}

const BASE_URL: &str = "http://localhost:8183";

impl App {
    pub fn new() -> Rc<Self> {
        Rc::new(App {
            todo_id: Cell::new(0),
            new_todo_title: Mutable::new("".to_owned()),
            todo_list: MutableVec::new(),
        })
    }

    pub async fn deserialize() -> Rc<Self> {
        let app = match App::get_todos().await {
            Ok(res) => res,
            Err(_) => App::new(),
        };
        app
    }

    pub async fn get_todos() -> Result<Rc<Self>, JsValue> {
        let url = format!("{}/{}", BASE_URL,"rstodos");
        let headers = Headers::new()?;
        headers.set("Accept", "application/json")?;

        let future = window()
            .unwrap_throw()
            .fetch_with_str_and_init(
                &url,
                RequestInit::new()
                    .headers(&headers),
            );

        let response = JsFuture::from(future)
            .await?
            .unchecked_into::<Response>();

        if !response.ok() {
            return Err(Error::new("Fetch failed").into());
        }

        let values: Vec<TodoResponse> = JsFuture::from(response.json()?)
        .await?
        .into_serde()
        .unwrap_throw();

        let list_todos: Vec<Rc<Todo>> = {
            let mut new_vec = Vec::new();
            for value in values {
                let todo_val = response_to_todo(value);
                new_vec.push(todo_val);
            }
            new_vec
        };

        let last_id = match list_todos.clone().pop() {
            Some(todo) => todo.id,
            None => 0
        };

        Ok( Rc::new(App {
            todo_id: Cell::new(last_id + 1),
            new_todo_title: Mutable::new("".to_owned()),
            todo_list: MutableVec::new_with_values(list_todos),
        }))
    }

    pub async fn post_todo(&self, ntitle: String) -> Result<TodoResponse, JsValue> {
        let url = format!("{}/{}", BASE_URL,"rstodos");
        let headers = Headers::new()?;
        headers.append("Accept", "application/json")?;
        headers.append("Content-Type", "application/json;charset=utf-8")?;
        headers.append("Control-Allow-Origin", "*")?;

        let body = TodoAction {
            title: ntitle,
            completed: false,
            orderx: self.todo_id.get(),
        };

        let body_json = match serde_json::to_string(&body){
            Ok(str) => str,
            Err(err) => return Err(Error::new(&format!("Serialize error in POST: {}", err)).into())
        };

        let jsval = JsValue::from_str(&body_json);

        let future = window()
            .unwrap_throw()
            .fetch_with_str_and_init(
                &url,
                RequestInit::new()
                    .headers(&headers)
                    .method("POST")
                    .mode(RequestMode::Cors)
                    .body(Some(&jsval)),
            );

        let response = JsFuture::from(future)
            .await?
            .unchecked_into::<Response>();
        if !response.ok() {
            return Err(Error::new("Fetch failed").into());
        }

        let value: TodoResponse = JsFuture::from(response.json()?)
        .await?
        .into_serde()
        .unwrap_throw();

        Ok(value)
    }

    pub async fn delete_todo(&self, todo: Rc<Todo>) -> Result<Vec<TodoResponse>, JsValue>  {
        let url = format!("{}/{}/{}", BASE_URL,"rstodos", todo.id);
        let headers = Headers::new()?;
        headers.append("Accept", "application/json")?;
        headers.append("Control-Allow-Origin", "*")?;

        let future = window()
            .unwrap_throw()
            .fetch_with_str_and_init(
                &url,
                RequestInit::new()
                    .headers(&headers)
                    .method("DELETE"),
            );

        let response = JsFuture::from(future)
            .await?
            .unchecked_into::<Response>();

        if !response.ok() {
            return Err(Error::new("Fetch failed").into());
        }

        let values: Vec<TodoResponse> = JsFuture::from(response.json()?)
        .await?
        .into_serde()
        .unwrap_throw();

        Ok(values)
    }

    pub async fn delete_todos(&self) -> Result<(), JsValue>{
        let url = format!("{}/{}", BASE_URL,"rstodos");
        let headers = Headers::new()?;
        headers.append("Accept", "application/json")?;
        headers.append("Control-Allow-Origin", "*")?;

        let future = window()
            .unwrap_throw()
            .fetch_with_str_and_init(
                &url,
                RequestInit::new()
                    .headers(&headers)
                    .method("DELETE")
            );

        let response = JsFuture::from(future)
            .await?
            .unchecked_into::<Response>();
        if !response.ok() {
            return Err(Error::new("Fetch failed").into());
        }

        let _value: TodoResponse = JsFuture::from(response.json()?)
        .await?
        .into_serde()
        .unwrap_throw();

        Ok(())
    }

    pub async fn patch_todo(self: Rc<Self>, todo: Rc<Todo>) -> Result<Todo, JsValue> {
        let url = format!("{}/{}/{}", BASE_URL,"rstodos", todo.id);
        let headers = Headers::new()?;
        headers.append("Accept", "application/json")?;
        headers.append("Content-Type", "application/json;charset=utf-8")?;
        headers.append("Control-Allow-Origin", "*")?;

        let body = TodoAction {
            title: todo.title.get_cloned(),
            completed: todo.completed.get(),
            orderx: todo.orderx.get(),
        };

        let body_json = match serde_json::to_string(&body){
            Ok(str) => str,
            Err(err) => return Err(Error::new(&format!("Serialize error in PATCH: {}", err)).into())
        };

        let jsval = JsValue::from_str(&body_json);

        let future = window()
            .unwrap_throw()
            .fetch_with_str_and_init(
                &url,
                RequestInit::new()
                    .headers(&headers)
                    .method("PATCH")
                    .body(Some(&jsval)),
            );

        let response = JsFuture::from(future)
            .await?
            .unchecked_into::<Response>();

        if !response.ok() {
            return Err(Error::new("Fetch failed").into());
        }

        let tr: TodoResponse = JsFuture::from(response.json()?)
        .await?
        .into_serde()
        .unwrap_throw();

        let new_todo = Todo {
            id: tr.id,
            title: Mutable::new(tr.title),
            completed: Mutable::new(tr.completed),
            editing: Mutable::new(None),
            orderx: Mutable::new(tr.orderx)
        };

        Ok(new_todo)
    }

    pub async fn patch_all_completion(&self, checked: bool) -> Result<Vec<TodoResponse>, JsValue> {
        let url = format!("{}/{}", BASE_URL,"rstodos/allcomplete");
        let headers = Headers::new()?;
        headers.append("Accept", "application/json")?;
        headers.append("Content-Type", "application/json;charset=utf-8")?;
        headers.append("Control-Allow-Origin", "*")?;

        let body = TodoCompletion {
            all_complete: checked,
        };

        let body_json = match serde_json::to_string(&body){
            Ok(str) => str,
            Err(err) => return Err(Error::new(&format!("Serialize error in PATCH: {}", err)).into())
        };

        let jsval = JsValue::from_str(&body_json);

        let future = window()
            .unwrap_throw()
            .fetch_with_str_and_init(
                &url,
                RequestInit::new()
                    .headers(&headers)
                    .method("PATCH")
                    .body(Some(&jsval)),
            );

        let response = JsFuture::from(future)
            .await?
            .unchecked_into::<Response>();

        if !response.ok() {
            return Err(Error::new("Fetch failed").into());
        }

        let values: Vec<TodoResponse> = JsFuture::from(response.json()?)
        .await?
        .into_serde()
        .unwrap_throw();

        Ok(values)

    }

    async fn create_new_todo(self: Rc<Self>) {
        let trimmed = trim(&self.new_todo_title.lock_ref());

        // Only create a new Todo if the text box is not empty
        if let Some(title) = trimmed {
            self.new_todo_title.set_neq("".to_owned());
            let id = self.todo_id.get();
            self.todo_id.set(id);

            let res = match self.post_todo(title).await {
                Ok(tr) => tr,
                Err(_) => return (),
            };

            let new_todo = response_to_todo(res);

            self.todo_list.lock_mut().push_cloned(new_todo);
        }
    }

    pub async fn remove_todo(self: Rc<Self>, todo: Rc<Todo>) {
        let values = self.delete_todo(todo).await.ok().unwrap_throw();
        let list_todos: Vec<Rc<Todo>> = {
            let mut new_vec = Vec::new();
            for value in values {
                let todo_val = response_to_todo(value);
                new_vec.push(todo_val);
            }
            new_vec
        };
        self.todo_list.lock_mut().replace_cloned(list_todos);
    }

    async fn remove_all_completed_todos(self: Rc<Self>) {
        let _x = self.delete_todos().await.ok().unwrap_throw();
        self.todo_list.lock_mut().retain(|x| x.completed.get() == false);
    }

    async fn set_all_todos_completed(self: Rc<Self>, checked: bool) {
        let values = self.patch_all_completion(checked).await.ok().unwrap_throw();

        let list_todos: Vec<Rc<Todo>> = {
            let mut new_vec = Vec::new();
            for value in values {
                let todo_val = response_to_todo(value);
                new_vec.push(todo_val);
            }
            new_vec
        };

        self.todo_list.lock_mut().replace_cloned(list_todos);
    }


    fn completed(&self) -> impl SignalVec<Item = bool> {
        self.todo_list.signal_vec_cloned()
            .map_signal(|todo| todo.completed.signal())
    }

    fn completed_len(&self) -> impl Signal<Item = usize> {
        self.completed()
            .filter(|completed| *completed)
            .len()
    }

    fn not_completed_len(&self) -> impl Signal<Item = usize> {
        self.completed()
            .filter(|completed| !completed)
            .len()
    }

    fn render_header(app: Rc<Self>) -> Dom {
        html!("header", {
            .class(["header","m-6","d-inline-flex","flex-row","width-auto","flex-justify-between","flex-wrap"])
            .children(&mut [
                html!("h1", {
                    .class(["h1","m-6","p-6","text-bold","text-center","my-lg-2","width-full"])
                    .text("TodoMVC - Rust")
                }),

                html!("input", {
                    .focused(true)
                    .class(["new-todo","form-control","width-full"])
                    .attribute("placeholder", "What needs to be done?")
                    .property_signal("value", app.new_todo_title.signal_cloned())

                    .event(clone!(app => move |event: events::Input| {
                        app.new_todo_title.set_neq(event.value().unwrap_throw());
                    }))

                    .event_preventable(clone!(app => move |event: events::KeyDown| {
                        if event.key() == "Enter" {
                            event.prevent_default();
                            spawn_local(app.clone().create_new_todo());
                        }
                    }))
                }),
            ])
        })
    }

    fn render_main(app: Rc<Self>) -> Dom {
        html!("section", {
            .class(["main","my-2","mx-6","d-inline-flex","flex-column"])
            // Hide if it doesn't have any todos.
            .visible_signal(app.todo_list.signal_vec_cloned()
                .len()
                .map(|len| len > 0))

            .children(&mut [
                html!("div", {
                    .class(["d-inline-flex","flex-row","flex-items-center"])
                    .children(&mut [
                        html!("input", {
                            .class(["toggle-all", "form-checkbox", "m-2"])
                            .attribute("id", "toggle-all")
                            .attribute("type", "checkbox")
                            .property_signal("checked", app.not_completed_len().map(|len| len == 0))

                            .event(clone!(app => move |event: events::Change| {
                                let checked = event.checked().unwrap_throw();
                                spawn_local(app.clone().set_all_todos_completed(checked));
                            }))
                        }),

                        html!("label", {
                            .class(["h3","m-1"])
                            .attribute("for", "toggle-all")
                            .text("Mark all as complete")
                        }),
                    ])
                }),
                html!("ul", {
                    .class(["todo-list", "list-style-none"])
                    .children_signal_vec(app.todo_list.signal_vec_cloned()
                        .map(clone!(app => move |todo| Todo::render(todo, app.clone()))))
                }),
            ])
        })
    }

    fn render_button(text: &str, route: Route) -> Dom {
        html!("li", {
            .children(&mut [
                link!(route.url(), {
                    .text(text)
                    .class_signal("selected", Route::signal().map(move |x| x == route))
                })
            ])
        })
    }

    fn render_footer(app: Rc<Self>) -> Dom {
        html!("footer", {
            .class(["footer","p-2","my-2","mx-6","d-inline-flex","flex-column"])
            // Hide if it doesn't have any todos.
            .visible_signal(app.todo_list.signal_vec_cloned()
                .len()
                .map(|len| len > 0))

            .children(&mut [
                html!("span", {
                    .class("todo-count")

                    .children(&mut [
                        html!("strong", {
                            .text_signal(app.not_completed_len().map(|len| len.to_string()))
                        }),

                        text_signal(app.not_completed_len().map(|len| {
                            if len == 1 {
                                " item left"
                            } else {
                                " items left"
                            }
                        })),
                    ])
                }),

                html!("ul", {
                    .class(["filters","list-style-none"])
                    .children(&mut [
                        Self::render_button("All", Route::All),
                        Self::render_button("Active", Route::Active),
                        Self::render_button("Completed", Route::Completed),
                    ])
                }),

                html!("button", {
                    .class("clear-completed")

                    // Show if there is at least one completed item.
                    .visible_signal(app.completed_len().map(|len| len > 0))

                    .event(clone!(app => move |_: events::Click| {
                        // app.todo_list.lock_mut().retain(|x| x.completed.get() == false);
                        spawn_local(app.clone().remove_all_completed_todos());
                    }))

                    .text("Clear completed")
                }),
            ])
        })
    }

    pub fn render(app: Rc<Self>) -> Dom {
        html!("section", {
            .class(["todoapp","bg-gray-light","height-full","d-inline-flex","flex-column","width-full"])
            .children(&mut [
                Self::render_header(app.clone()),
                Self::render_main(app.clone()),
                Self::render_footer(app.clone()),
            ])
        })
    }
}

#[inline]
pub fn trim(input: &str) -> Option<String> {
    let trimmed = input.trim();

    if trimmed.is_empty() {
        None

    } else {
        Some(trimmed.to_owned())
    }
}
