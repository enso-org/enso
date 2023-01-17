use crate::prelude::*;

use crate::fs::copy_to;
use crate::programs::vs;

use dependency_runner::executable::Executables;
use unicase::UniCase;



/// List of dynamically loaded dependencies.
///
/// While the code in itself is portable, it supports only Windows binaries (PE files).
pub async fn pe_dependencies(binary: impl AsRef<Path>) -> Result<Executables> {
    let binary = binary.as_ref().to_path_buf();
    tokio::task::spawn_blocking(move || {
        let mut query =
            dependency_runner::query::LookupQuery::deduce_from_executable_location(&binary)
                .with_context(|| {
                    format!("Failed to prepare dependency query for binary {}.", binary.display())
                })?;
        query.parameters.extract_symbols = true;
        let lookup_path = dependency_runner::path::LookupPath::deduce(&query);
        dependency_runner::runner::run(&query, &lookup_path).with_context(|| {
            format!("Failed to run dependency query for binary {}.", binary.display())
        })
    })
    .await?
}

/// Place all the required MSVC CRT dependencies next to the binary.
#[context("Failed to copy MSVC CRT dependencies for binary `{}`.", binary.display())]
pub async fn add_msvc_redist_dependencies(binary: &Path) -> Result {
    let binary = binary.absolutize()?;
    let binary_dir = binary.try_parent()?;
    let dependencies = pe_dependencies(&binary).await?;
    let msvc_redist_dlls = vs::redist::list_crt_dlls(vs::Platforms::local()?).await?;
    // map filename -> full path of redist dll
    // Be careful about casing! Windows is case-insensitive.
    let msvc_redist_dlls: HashMap<_, _> = msvc_redist_dlls
        .iter()
        .flat_map(|path| path.file_name().map(|filename| (UniCase::new(filename.as_str()), path)))
        .collect();
    for dependency in dependencies.sorted_by_first_appearance() {
        let filename = dependency.dllname.as_str();
        if let Some(redist_path) = msvc_redist_dlls.get(&UniCase::new(filename)) {
            trace!("{} is a redist dll: {}.", filename, redist_path.display());
            copy_to(redist_path, binary_dir)?;
        } else {
            trace!("{} is not a redist dll.", filename);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::programs::vs;

    #[tokio::test]
    #[ignore]
    async fn lookup_dependencies() -> Result {
        setup_logging()?;
        vs::apply_dev_environment().await?;
        let binary = Path::new(
            r"H:\NBO\enso5\built-distribution\enso-project-manager-2022.1.1-dev-windows-amd64\enso\bin\project-manager.exe",
        );
        add_msvc_redist_dependencies(binary).await?;
        Ok(())
    }
}
