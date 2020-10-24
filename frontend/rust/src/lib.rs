mod routing;
mod todo;
mod app;

use wasm_bindgen::prelude::*;
use crate::app::App;

// use std::sync::Arc;
use cfg_if::cfg_if;

#[wasm_bindgen(start)]
pub async fn main_js() -> Result<(), JsValue>{
    #[cfg(debug_assertions)]
    setup_logger();
    console_error_panic_hook::set_once();

    dominator::append_dom(&dominator::get_id("app"), App::render(App::deserialize().await));

    Ok(())
}

cfg_if! {
    if #[cfg(all(feature = "wasm-logger", feature = "console_error_panic_hook", debug_assertions))] {
        fn setup_logger() {
            wasm_logger::init(wasm_logger::Config::default());
            console_error_panic_hook::set_once();
            std::panic::set_hook(Box::new(console_error_panic_hook::hook));
            log::info!("rust logging enabled!!!");
        }
    } else {
        fn setup_logger() {
            log::info!("rust logging disabled!"); //<-- won't be seen
        }
    }
}
