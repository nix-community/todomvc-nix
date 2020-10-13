use std::rc::Rc;
use std::cell::Cell;

use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};
use futures_signals::signal::{Signal, SignalExt, Mutable};
use futures_signals::signal_vec::{SignalVec, SignalVecExt, MutableVec};
use dominator::{Dom, text_signal, html, clone, events, link};

use crate::todo::Todo;
use crate::routing::Route;

#[derive(Debug, Serialize, Deserialize)]
pub struct App {
    todo_id: Cell<u32>,

    #[serde(skip)]
    new_todo_title: Mutable<String>,

    todo_list: MutableVec<Rc<Todo>>,
}
