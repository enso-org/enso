use ide_ci::prelude::*;



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    info!("Enso Formatter running in {}", ide_ci::env::current_dir()?.display());
    enso_formatter::process_path(".", enso_formatter::Action::Format).await
}
