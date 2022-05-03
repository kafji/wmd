/*!
Defines handlers for HTTP requests.
*/

use crate::app::App;

pub trait Response {}

impl Response for () {}

/// Handles `GET /`.
pub async fn get_index(app: &App) -> impl Response {
    todo!()
}
