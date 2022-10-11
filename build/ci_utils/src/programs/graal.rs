use crate::prelude::*;



#[derive(Clone, Copy, Debug, Default)]
pub struct Gu;

impl Program for Gu {
    fn executable_name(&self) -> &'static str {
        "gu"
    }
}


pub fn take_until_whitespace(text: &str) -> &str {
    text.split_whitespace().next().unwrap_or(text)
}

/// Support for sulong has not been implemented for Windows yet.
///
/// See: https://github.com/oracle/graal/issues/1160
pub fn sulong_supported() -> bool {
    TARGET_OS != OS::Windows
}

pub async fn list_components() -> Result<HashSet<Component>> {
    let output = Gu.cmd()?.arg("list").output_ok().await?;
    let lines = std::str::from_utf8(&output.stdout)?.lines();
    let lines = lines.skip(2); // We drop header and table dash separator lines.
    Ok(lines
        .filter_map(|line| {
            let name = take_until_whitespace(line);
            match Component::from_str(name) {
                Ok(component) => Some(component),
                Err(e) => {
                    warn!("Unrecognized component name '{name}'. Error: {e}");
                    None
                }
            }
        })
        .collect())
}

pub async fn install_missing_components(components: impl IntoIterator<Item = Component>) -> Result {
    let already_installed = list_components().await?;
    let missing_components =
        components.into_iter().filter(|c| !already_installed.contains(c)).collect_vec();
    // We want to avoid running `gu install` when all required components are already installed,
    // as this command might require root privileges in some environments.
    if !missing_components.is_empty() {
        let mut cmd = Gu.cmd()?;
        cmd.arg("install");
        for missing_component in missing_components {
            cmd.arg(missing_component.as_ref());
        }
        cmd.run_ok().await?;
    } else {
        debug!("All required components are installed.");
    }
    Ok(())
}

#[derive(
    Clone,
    Copy,
    Hash,
    PartialEq,
    Eq,
    Debug,
    strum::Display,
    strum::AsRefStr,
    strum::EnumString
)]
#[strum(serialize_all = "kebab-case")]
pub enum Component {
    #[strum(serialize = "graalvm")]
    GraalVM,
    JS,
    NativeImage,
    Python,
    #[strum(serialize = "R")]
    R,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::log::setup_logging;

    #[tokio::test]
    #[ignore]
    async fn gu_list() -> Result {
        setup_logging()?;
        // let output = Gu.cmd()?.arg("list").output_ok().await?;
        // println!("{:?}", std::str::from_utf8(&output.stdout)?);
        dbg!(list_components().await)?;
        Ok(())
    }
}
