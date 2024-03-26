use enso_build::prelude::*;

use ide_ci::actions::workflow::MessageLevel;



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;

    ide_ci::actions::workflow::debug("Debug");
    ide_ci::actions::workflow::message(MessageLevel::Debug, "Debug2");
    ide_ci::actions::workflow::message(MessageLevel::Notice, "Notice");
    ide_ci::actions::workflow::message(MessageLevel::Warning, "Warning");
    ide_ci::actions::workflow::message(MessageLevel::Error, "Error");

    println!("Hello");
    trace!("Hello");
    debug!("Hello");
    info!("Hello");
    warn!("Hello");
    error!("Hello");
    Ok(())
}
