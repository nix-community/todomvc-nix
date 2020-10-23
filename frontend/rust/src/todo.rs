use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::{spawn_local};

use serde::{Serialize, Deserialize};
use futures_signals::map_ref;
use futures_signals::signal::{SignalExt, Mutable};
use dominator::{Dom, html, clone, events, with_node};

use common::{TodoResponse};

use crate::app::{App, trim};
use crate::routing::Route;

#[derive(Debug, Serialize, Deserialize)]
pub struct Todo {
    pub id: i32,
    pub title: Mutable<String>,
    pub completed: Mutable<bool>,

    #[serde(skip)]
    pub editing: Mutable<Option<String>>,
    pub orderx: Mutable<i32>
}


pub fn response_to_todo(tr: TodoResponse) -> Rc<Todo> {
    let todo = Todo::new(tr.id, tr.title, tr.orderx);
    todo.completed.set(tr.completed);
    todo
}

impl Todo {
    pub fn new(id: i32, title: String, orderx: i32) -> Rc<Self> {
        Rc::new(Self {
            id: id,
            title: Mutable::new(title),
            completed: Mutable::new(false),
            editing: Mutable::new(None),
            orderx: Mutable::new(orderx)
        })
    }

    async fn set_completed(self: Rc<Self>, app: Rc<App>, completed: bool) {
        self.completed.set_neq(completed);
        app.patch_todo(self).await.ok();
    }

    async fn remove(self: Rc<Self>, app: Rc<App>) {
        app.remove_todo(self).await;
    }

    fn cancel_editing(&self) {
        self.editing.set_neq(None);
    }

    async fn done_editing(self: Rc<Self>, app: Rc<App>) {
        if let Some(title) = self.editing.replace(None) {
            if let Some(title) = trim(&title) {
                self.title.set_neq(title);
            } else {
                app.clone().remove_todo(self.clone()).await;
                return ()
            }
            app.clone().patch_todo(self.clone()).await.ok().unwrap_throw();
        }
    }

    pub fn render(todo: Rc<Self>, app: Rc<App>) -> Dom {
        html!("li", {
            .class_signal("editing", todo.editing.signal_cloned().map(|x| x.is_some()))
            .class_signal("completed", todo.completed.signal())

            .visible_signal(map_ref!(
                    let route = Route::signal(),
                    let completed = todo.completed.signal() =>
                    match *route {
                        Route::Active => !completed,
                        Route::Completed => *completed,
                        Route::All => true,
                    }
                )
                .dedupe())

            .children(&mut [
                html!("div", {
                    .class("view")
                    .children(&mut [
                        html!("input", {
                            .attribute("type", "checkbox")
                            .class("toggle")

                            .property_signal("checked", todo.completed.signal())

                            .event(clone!(todo, app => move |event: events::Change| {
                                spawn_local(todo.clone().set_completed(app.clone(), event.checked().unwrap_throw()));
                            }))
                        }),

                        html!("label", {
                            .event(clone!(todo => move |_: events::DoubleClick| {
                                todo.editing.set_neq(Some(todo.title.get_cloned()));
                            }))

                            .text_signal(todo.title.signal_cloned())
                        }),

                        html!("button", {
                            .class("destroy")
                            .event(clone!(todo, app => move |_: events::Click| {
                                spawn_local(todo.clone().remove(app.clone()));
                            }))
                        }),
                    ])
                }),

                html!("input", {
                    .class("edit")

                    .property_signal("value", todo.editing.signal_cloned()
                        .map(|x| x.unwrap_or_else(|| "".to_owned())))

                    .visible_signal(todo.editing.signal_cloned()
                        .map(|x| x.is_some()))

                    // TODO dedupe this somehow ?
                    .focused_signal(todo.editing.signal_cloned()
                        .map(|x| x.is_some()))

                    .with_node!(element => {
                        .event(clone!(todo => move |event: events::KeyDown| {
                            match event.key().as_str() {
                                "Enter" => {
                                    element.blur().unwrap_throw();
                                },
                                "Escape" => {
                                    todo.cancel_editing();
                                },
                                _ => {}
                            }
                        }))
                    })

                    .event(clone!(todo => move |event: events::Input| {
                        todo.editing.set_neq(Some(event.value().unwrap_throw()));
                    }))

                    // TODO global_event ?
                    .event(clone!(todo, app => move |_: events::Blur| {
                        spawn_local(todo.clone().done_editing(app.clone()));
                    }))
                }),
            ])
        })
    }
}

impl PartialEq<Todo> for Todo {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
