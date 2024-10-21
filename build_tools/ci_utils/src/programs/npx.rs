use crate::prelude::*;



#[derive(Clone, Copy, Debug, Default)]
pub struct Npx;

impl Program for Npx {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "npx"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn call_npx() -> Result {
        setup_logging().ok();
        Npx.cmd()?.run_ok().await?;
        Ok(())
    }
}
