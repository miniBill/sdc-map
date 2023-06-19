use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::post,
    Json, Router,
};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use std::sync::{Mutex, MutexGuard};
use tower_http::services::ServeDir;

lazy_static! {
    static ref SQLITE_CONNECTION: Mutex<sqlite::Connection> = {
        let args: Vec<String> = std::env::args().collect();
        let len = args.len();
        if len < 4 {
            Mutex::new(sqlite::open(":memory:").expect("Failed to create DB in memory"))
        } else {
            Mutex::new(sqlite::open(&args[3]).expect("Failed open DB"))
        }
    };
}

#[tokio::main]
async fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let args_len = args.len();

    if args_len < 2 {
        return Err(format!("Usage: {} <path> [port] [db]", args[0]));
    }
    let serve_path: &String = &args[1];

    let port: u16 = if args_len < 3 {
        3000
    } else {
        args[2]
            .parse::<u16>()
            .unwrap_or_else(|_| panic!("Cannot parse {} as a port number", args[2]))
    };

    let query = "
        CREATE TABLE IF NOT EXISTS answers (
            encrypted TEXT,
            captcha TEXT
        )
    ";
    SQLITE_CONNECTION
        .lock()
        .expect("Cannot acquire SQL connection")
        .execute(query)
        .expect("Failed to create the `answers` table");

    // build our application with a route
    let app: Router = router(serve_path);

    // run our app with hyper
    // `axum::Server` is a re-export of `hyper::Server`
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    let builder = axum::Server::bind(&addr);

    builder
        .serve(app.into_make_service())
        .await
        .expect("Failed to start server");

    Ok(())
}

fn router(serve_path: &String) -> Router {
    Router::new()
        .route(
            "/submit",
            post(|payload| async { handle_sqlite_error(store_form(payload).await) }),
        )
        .fallback_service(ServeDir::new(serve_path))
}

fn handle_sqlite_error(
    value: sqlite::Result<(StatusCode, impl IntoResponse)>,
) -> (StatusCode, Response) {
    match value {
        Ok((code, msg)) => (code, msg.into_response()),
        Err(e) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("{}", e).into_response(),
        ),
    }
}

async fn store_form(Json(payload): Json<Input>) -> sqlite::Result<(StatusCode, &'static str)> {
    let insert_query = "
        INSERT INTO answers(encrypted, captcha)
        VALUES (:encrypted, :captcha)
    ";
    let connection: MutexGuard<sqlite::Connection> = SQLITE_CONNECTION
        .lock()
        .expect("Cannot acquire SQL connection");
    let mut statement: sqlite::Statement = connection.prepare(insert_query)?;
    statement.bind::<&[(_, sqlite::Value)]>(&[
        (":encrypted", payload.encrypted.into()),
        (":captcha", payload.captcha.into()),
    ])?;
    while statement.next()? != sqlite::State::Done {}

    Result::Ok((StatusCode::CREATED, "Saved!"))
}

// the input to our `create_user` handler
#[derive(Serialize, Deserialize, Debug)]
struct Input {
    encrypted: String,
    captcha: String,
}
