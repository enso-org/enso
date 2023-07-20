use crate::prelude::*;

use serde::Deserializer;
use serde::Serializer;



/// GraalVM Updater, `gu`, is a command-line tool for installing and managing optional GraalVM
/// language runtimes and utilities.
#[derive(Clone, Copy, Debug, Default)]
pub struct Gu;

impl Program for Gu {
    fn executable_name(&self) -> &'static str {
        "gu"
    }
}

/// Get installed GraalVM components.
pub async fn list_installed_components() -> Result<HashSet<ComponentId>> {
    let output = Gu.cmd()?.args(["list", "--json"]).output_ok().await?;
    let components = serde_json::from_slice::<Root>(&output.stdout)?;
    Ok(components.components.into_iter().map(|c| c.id).collect())
}

/// Get available GraalVM components (i.e. the components that can be installed in this
/// environment).
pub async fn list_available_components() -> Result<HashSet<ComponentId>> {
    let output = Gu.cmd()?.args(["list", "--catalog", "--json"]).output_ok().await?;
    let components = serde_json::from_slice::<Root>(&output.stdout)?;
    Ok(components.components.into_iter().map(|c| c.id).collect())
}

/// Install given GraalVM components.
pub async fn install_components(components: impl IntoIterator<Item = &ComponentId>) -> Result {
    let components = components.into_iter().map(|c| c.as_ref()).collect_vec();
    if !components.is_empty() {
        debug!(
            "Will install {} GraalVM component(s): {}.",
            components.len(),
            components.join(", ")
        );
        // `gu install` dos not like being invoked with no arguments. Well, sensible.
        let mut cmd = Gu.cmd()?;
        cmd.arg("install");
        // Add force flag. Sometimes, this seems to be needed to install a component that was
        // previously installed. Otherwise, we get error like:
        // Installation of Native Image failed: Existing file contents differ:
        // .../lib/svm/bin/native-image. Run with -f to force overwrite. Error: Existing
        // file contents differ: .../lib/svm/bin/native-image. Run with -f to force overwrite.
        cmd.arg("-f");
        cmd.args(components);
        cmd.run_ok().await?;
    } else {
        debug!("No GraalVM components to install.");
    }
    Ok(())
}

/// Install given GraalVM components if they are not already installed.
///
/// # Arguments
/// * `required_components` - the components that must be installed. If any of them fails to
///   install, the whole operation fails.
/// * `optional_components` - the components that should be installed if they are not already
///   installed, but if they fail to install, the whole operation does not fail.
pub async fn install_missing_components(
    required_components: impl IntoIterator<Item = ComponentId>,
    optional_components: impl IntoIterator<Item = ComponentId>,
) -> Result {
    let required_components = required_components.into_iter().collect::<HashSet<_>>();
    let optional_components = optional_components.into_iter().collect::<HashSet<_>>();
    let installed_components = list_installed_components().await?;
    let available_components = list_available_components().await?;

    let missing_required_components =
        required_components.difference(&installed_components).collect::<HashSet<_>>();
    for required_component in missing_required_components.iter() {
        if !available_components.contains(required_component) {
            bail!("Required component {} is not available in the catalog.", required_component);
        }
    }

    let missing_optional_components =
        optional_components.difference(&installed_components).cloned().collect::<HashSet<_>>();
    let available_missing_optional_components =
        missing_optional_components.intersection(&available_components);
    let components_to_install =
        missing_required_components.into_iter().chain(available_missing_optional_components);
    install_components(components_to_install).await
}

/// Known components of GraalVM.
#[derive(Clone, Hash, PartialEq, Eq, Debug, strum::Display, strum::AsRefStr, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum ComponentId {
    /// Java on Truffle
    Espresso,
    #[strum(serialize = "graalvm")]
    GraalVM,
    /// Graal.js
    JS,
    /// LLVM Runtime Core
    Llvm,
    /// LLVM.org toolchain
    LlvmToolchain,
    /// Native Image
    NativeImage,
    /// Graal.nodejs
    #[strum(serialize = "nodejs")]
    NodeJs,
    /// Graal.Python
    Python,
    /// TruffleRuby
    Ruby,
    #[strum(serialize = "visualvm")]
    /// VisualVM
    VisualVm,
    /// GraalWasm
    Wasm,
    #[strum(serialize = "icu4j")]
    Icu4J,
    Regex,
    Jipher,
    #[strum(disabled)]
    Unrecognized(String),
}

impl<'de> Deserialize<'de> for ComponentId {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where D: Deserializer<'de> {
        let s = String::deserialize(deserializer)?;
        ComponentId::from_str(&s).or_else(|_| {
            warn!("Unrecognized component name '{s}'.");
            Ok(ComponentId::Unrecognized(s))
        })
    }
}

impl Serialize for ComponentId {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where S: Serializer {
        crate::serde::via_string::serialize(self, serializer)
    }
}

/// Wrapper over a single GraalVM component as listed by `gu` in JSON format.
///
/// Note that we omit most of the fields, as we only need the `id` field.
#[derive(Debug, Serialize, Deserialize)]
struct Component {
    /// Identifier of the component.
    pub id: ComponentId,
    // There are more fields, but we don't need them. We don't want to fail if they are changed.
    // Also, there are significant format differences between `gu list` and `gu list --catalog`.
}

/// Representation of the output of JSON-formatted `gu list` and `gu list --catalog`.
#[derive(Debug, Serialize, Deserialize)]
struct Root {
    /// Components listed by `gu`.
    pub components: Vec<Component>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::log::setup_logging;

    #[tokio::test]
    #[ignore]
    async fn gu_list() -> Result {
        setup_logging()?;
        dbg!(list_installed_components().await)?;
        dbg!(list_available_components().await)?;
        Ok(())
    }

    #[test]
    fn deserialize_component_catalogue() -> Result {
        setup_logging()?;
        // Obtained by calling `gu.cmd list --catalog --json` on Windows machine.
        let json_text = r#"{"components":[{"graalvm":"22.3.0","origin":"https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/espresso-installable-svm-java11-windows-amd64-22.3.0.jar","name":"Java on Truffle","id":"espresso","version":"22.3.0","stability":"Experimental"},{"graalvm":"22.3.0","origin":"https://github.com/oracle/graaljs/releases/download/vm-22.3.0/js-installable-svm-java11-windows-amd64-22.3.0.jar","name":"Graal.js","id":"js","version":"22.3.0","stability":"Supported"},{"graalvm":"22.3.0","origin":"https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/llvm-installable-svm-java11-windows-amd64-22.3.0.jar","name":"LLVM Runtime Core","id":"llvm","version":"22.3.0","stability":"Experimental"},{"graalvm":"22.3.0","origin":"https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/llvm-toolchain-installable-java11-windows-amd64-22.3.0.jar","name":"LLVM.org toolchain","id":"llvm-toolchain","version":"22.3.0","stability":"Supported"},{"graalvm":"22.3.0","origin":"https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/native-image-installable-svm-java11-windows-amd64-22.3.0.jar","name":"Native Image","id":"native-image","version":"22.3.0","stability":"Early adopter"},{"graalvm":"22.3.0","origin":"https://github.com/oracle/graaljs/releases/download/vm-22.3.0/nodejs-installable-svm-java11-windows-amd64-22.3.0.jar","name":"Graal.nodejs","id":"nodejs","version":"22.3.0","stability":"Supported"},{"graalvm":"22.3.0","origin":"https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/visualvm-installable-ce-java11-windows-amd64-22.3.0.jar","name":"VisualVM","id":"visualvm","version":"22.3.0","stability":"Experimental"},{"graalvm":"22.3.0","origin":"https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/wasm-installable-svm-java11-windows-amd64-22.3.0.jar","name":"GraalWasm","id":"wasm","version":"22.3.0","stability":"Experimental"}]}"#;
        let catalogue: Root = serde_json::from_str(json_text)?;
        dbg!(&catalogue);

        // Obtained by calling `gu.cmd list --json` on Windows machine.
        let json_text = r#"{"components":[{"graalvm":"n/a","origin":"","name":"GraalVM Core","id":"graalvm","version":"22.3.0","stability":"Supported"}]}"#;
        let catalogue: Root = serde_json::from_str(json_text)?;
        dbg!(&catalogue);
        Ok(())
    }
}
