use time::format_description::well_known::Rfc3339;
use time::OffsetDateTime;

static REQUEST_SENT: &str = "wstest sent request";
static RESPONSE_RECEIVED: &str = "wstest received response";
static MESSAGE_UNKNOWN: &str = "<message>";

pub fn request(message: &str) -> String {
    fmt(format!("{} [{}]", REQUEST_SENT, message).as_str())
}

pub fn response(message: Option<&str>) -> String {
    fmt(format!("{} [{}]", RESPONSE_RECEIVED, message.unwrap_or(MESSAGE_UNKNOWN)).as_str())
}

fn fmt(message: &str) -> String {
    let time_now = OffsetDateTime::now_utc();
    format!(
        "[{level}] [{timestamp}] [{module}] {message}",
        level = "info",
        timestamp = time_now.format(&Rfc3339).unwrap(),
        module = "main",
        message = message,
    )
}
