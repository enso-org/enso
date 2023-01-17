use time::format_description::well_known::Rfc3339;
use time::OffsetDateTime;



// =================
// === Constants ===
// =================

pub static MESSAGE_BINARY: &str = "<binary>";

static INIT_REQUEST_SENT: &str = "wstest sent init request";
static BENCH_REQUEST_SENT: &str = "wstest sent bench request";
static WARMUP_REQUEST_SENT: &str = "wstest sent warmup request";
static RESPONSE_HANDLED: &str = "wstest handled response";
static RESPONSE_IGNORED: &str = "wstest ignored response";

static FMT_LEVEL: &str = "info";
static FMT_MODULE: &str = "main";



// ==========================
// === Message Formatting ===
// ==========================

/// Message for logging the initialization request
pub fn init_request(message: &str) -> String {
    fmt(format!("{INIT_REQUEST_SENT} [{message}]").as_str())
}

/// Message for logging the warmup request
pub fn warmup_request(message: &str) -> String {
    fmt(format!("{WARMUP_REQUEST_SENT} [{message}]").as_str())
}

/// Message for logging the benchmarking request
pub fn bench_request(message: &str) -> String {
    fmt(format!("{BENCH_REQUEST_SENT} [{message}]").as_str())
}

/// Message for logging the text response
pub fn response_text(message: &str) -> String {
    fmt(format!("{RESPONSE_HANDLED} [{message}]").as_str())
}

/// Message for logging the binary response
pub fn response_binary() -> String {
    fmt(format!("{RESPONSE_HANDLED} [{MESSAGE_BINARY}]").as_str())
}

/// Message for logging the ignored response
pub fn response_ignored(message: &str) -> String {
    fmt(format!("{RESPONSE_IGNORED} [{message}]").as_str())
}

fn fmt(message: &str) -> String {
    let time_now = OffsetDateTime::now_utc();
    format!(
        "[{level}] [{timestamp}] [{module}] {message}",
        level = FMT_LEVEL,
        timestamp = time_now.format(&Rfc3339).unwrap(),
        module = FMT_MODULE,
        message = message,
    )
}
