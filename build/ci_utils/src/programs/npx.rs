use crate::prelude::*;



#[derive(Clone, Copy, Debug, Default)]
pub struct Npx;

impl Program for Npx {
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
        setup_logging()?;
        Npx.cmd()?.run_ok().await?;
        Ok(())
    }
}
