mod app;
mod config;
mod http;
mod percent_encoding;
mod ripm;
mod search_query;
mod search_targets;
mod url_maker;
mod templating;

#[cfg(debug_assertions)]
#[doc(inline)]
pub use std;

pub use app::start as start_app;
pub use config::{Source as ConfigSource, DEFAULT_SOURCES as DEFAULT_CONFIG_SOURCES};
