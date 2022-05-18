mod app;
mod config;
mod http;
mod percent_encoding;
mod search_query;
mod target_url_maker;
mod templating;

#[cfg(debug_assertions)]
#[doc(inline)]
pub use std;

pub use app::start as start_app;
pub use config::{Source as ConfigSource, DEFAULT_SOURCES as DEFAULT_CONFIG_SOURCES};