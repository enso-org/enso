//! Visualization controller.
//!
//! Ths Visualization Controller is Responsible identifying all the available visualizations
//! natively embedded in IDE and available within the project's `visualization` folder.

use crate::prelude::*;

use crate::constants::VISUALIZATION_DIRECTORY;

use engine_protocol::language_server;
use ide_view::graph_editor::component::visualization;
use ide_view::graph_editor::component::visualization::definition;
use ide_view::graph_editor::component::visualization::java_script::Sources;
use std::rc::Rc;


// ==============
// === Export ===
// ==============

pub mod manager;



// =============
// === Error ===
// =============

/// Enumeration of errors used in `Visualization Controller`.
#[derive(Debug, Fail)]
#[allow(missing_docs)]
pub enum Error {
    #[fail(display = "Visualization \"{}\" not found.", identifier)]
    NotFound { identifier: VisualizationPath },
    #[fail(display = "JavaScript visualization \"{}\" failed to be prepared.", identifier)]
    Preparation {
        identifier: VisualizationPath,
        #[cause]
        cause:      failure::Error,
    },
    #[fail(display = "JavaScript visualization \"{}\" failed to be instantiated.", identifier)]
    Instantiation { identifier: VisualizationPath },
}

impl Error {
    /// Construct a new error regarding visualization preparation.
    pub fn js_preparation_error(
        identifier: VisualizationPath,
        error: visualization::foreign::java_script::definition::Error,
    ) -> Self {
        let cause = failure::format_err!("{}", error);
        Self::Preparation { identifier, cause }
    }
}



// =========================
// === VisualizationPath ===
// =========================

/// This enum is used to provide a path to visualization either in the project folder or natively
/// embedded in IDE.
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum VisualizationPath {
    Embedded(String),
    File(language_server::Path),
}



// ==============================
// === EmbeddedVisualizations ===
// ==============================

#[allow(missing_docs)]
pub type EmbeddedVisualizationName = String;

/// Embedded visualizations mapped from name to source code.
#[derive(Debug, Clone, Default, Deref, DerefMut)]
pub struct EmbeddedVisualizations {
    #[allow(missing_docs)]
    pub map: HashMap<EmbeddedVisualizationName, definition::Definition>,
}



// ==============
// === Handle ===
// ==============

/// Visualization Controller is responsible for listing and loading all the available
/// visualizations on the project and the native ones embedded on IDE.
#[derive(Debug, Clone, CloneRef)]
pub struct Handle {
    language_server_rpc:     Rc<language_server::Connection>,
    embedded_visualizations: Rc<RefCell<EmbeddedVisualizations>>,
}

impl Handle {
    /// Creates a new visualization controller.
    pub fn new(
        language_server_rpc: Rc<language_server::Connection>,
        embedded_visualizations: EmbeddedVisualizations,
    ) -> Self {
        let embedded_visualizations = Rc::new(RefCell::new(embedded_visualizations));
        Self { language_server_rpc, embedded_visualizations }
    }

    async fn list_project_specific_visualizations(&self) -> FallibleResult<Vec<VisualizationPath>> {
        let root_id = self.language_server_rpc.project_root().id();
        let path = language_server::Path::new(root_id, &[VISUALIZATION_DIRECTORY]);
        let folder = self.language_server_rpc.file_exists(&path).await?;
        let file_list = if folder.exists {
            self.language_server_rpc.file_list(&path).await?.paths
        } else {
            default()
        };
        let result = file_list
            .iter()
            .filter_map(|object| {
                if let language_server::FileSystemObject::File { .. } = object {
                    Some(VisualizationPath::File(object.into()))
                } else {
                    None
                }
            })
            .collect();
        Ok(result)
    }

    fn list_embedded_visualizations(&self) -> Vec<VisualizationPath> {
        let embedded_visualizations = self.embedded_visualizations.borrow();
        let result = embedded_visualizations.keys().cloned();
        let result = result.map(VisualizationPath::Embedded);
        result.collect()
    }

    /// Get a list of all available visualizations.
    pub async fn list_visualizations(&self) -> FallibleResult<Vec<VisualizationPath>> {
        let mut visualizations = self.list_embedded_visualizations();
        visualizations.extend_from_slice(&self.list_project_specific_visualizations().await?);
        Ok(visualizations)
    }

    /// Load the source code of the specified visualization.
    pub async fn load_visualization(
        &self,
        visualization: &VisualizationPath,
    ) -> FallibleResult<definition::Definition> {
        match visualization {
            VisualizationPath::Embedded(identifier) => {
                let embedded_visualizations = self.embedded_visualizations.borrow();
                let result = embedded_visualizations.get(identifier);
                let identifier = visualization.clone();
                let error = || Error::NotFound { identifier }.into();
                result.cloned().ok_or_else(error)
            }
            VisualizationPath::File(path) => {
                let project = visualization::path::Project::CurrentProject;
                let js_code = self.language_server_rpc.read_file(path).await?.contents;
                let wrap_error =
                    |err| Error::js_preparation_error(visualization.clone(), err).into();
                let sources = if let Some(file_name) = path.file_name() {
                    let sources: &[(&str, &str)] = &[(file_name, &js_code)];
                    Sources::from_files(sources)
                } else {
                    warn!(
                        "Unable to get a file name from {path}. Visualization source map will not be provided."
                    );
                    Sources::empty()
                };
                visualization::java_script::Definition::new(project, sources)
                    .map(Into::into)
                    .map_err(wrap_error)
            }
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use engine_protocol::language_server::FileSystemObject;
    use engine_protocol::language_server::Path;
    use ide_view::graph_editor::builtin;
    use ide_view::graph_editor::component::visualization;
    use ide_view::graph_editor::component::visualization::java_script as js_vis;
    use json_rpc::expect_call;

    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    async fn list_and_load() {
        let mock_client = language_server::MockClient::default();

        let root_id = uuid::Uuid::default();
        let path = Path::new(root_id, &["visualization"]);
        let path0 = Path::new(root_id, &["visualization", "histogram.js"]);
        let path1 = Path::new(root_id, &["visualization", "graph.js"]);

        let paths = vec![
            FileSystemObject::new_file(path0.clone()).unwrap(),
            FileSystemObject::new_file(path1.clone()).unwrap(),
        ];
        let file_list_result = language_server::response::FileList { paths };
        expect_call!(mock_client.file_list(path=path.clone()) => Ok(file_list_result));

        let file_content0 = r#"
            class Vis0 {
                static inputTypes = ["Float"]
                onDataReceived(root,data) {}
                setSize(root,size) {}
            }
            return Vis0
        "#
        .to_string();
        let file_content1 = r#"
            class Vis1 {
                static inputTypes = ["Float"]
                onDataReceived(root,data) {}
                setSize(root,size) {}
            }
            return Vis1
        "#
        .to_string();
        let read_result0 = language_server::response::Read { contents: file_content0.clone() };
        let read_result1 = language_server::response::Read { contents: file_content1.clone() };
        let exists_result0 = language_server::response::FileExists { exists: true };
        let exists_result1 = language_server::response::FileExists { exists: true };
        expect_call!(mock_client.file_exists(path=path.clone()) => Ok(exists_result0));
        expect_call!(mock_client.file_exists(path=path.clone()) => Ok(exists_result1));
        expect_call!(mock_client.read_file(path=path0.clone())  => Ok(read_result0));
        expect_call!(mock_client.read_file(path=path1.clone())  => Ok(read_result1));

        let language_server = language_server::Connection::new_mock_rc(mock_client);
        let mut embedded_visualizations = EmbeddedVisualizations::default();
        let embedded_visualization = builtin::visualization::native::BubbleChart::definition();
        embedded_visualizations
            .insert("[Demo] Bubble Visualization".to_string(), embedded_visualization.clone());
        let vis_controller = Handle::new(language_server, embedded_visualizations);

        let visualizations = vis_controller.list_visualizations().await;
        let visualizations = visualizations.expect("Couldn't list visualizations.");

        assert_eq!(
            visualizations[0],
            VisualizationPath::Embedded("[Demo] Bubble Visualization".to_string())
        );
        assert_eq!(visualizations[1], VisualizationPath::File(path0));
        assert_eq!(visualizations[2], VisualizationPath::File(path1));
        assert_eq!(visualizations.len(), 3);

        let owner = visualization::Project::CurrentProject;
        let sources_vis0 = Sources::from_files(&[("file0.js", &file_content0)]);
        let javascript_vis0 = js_vis::Definition::new(owner.clone_ref(), sources_vis0);
        let sources_vis1 = Sources::from_files(&[("file0.js", &file_content1)]);
        let javascript_vis1 = js_vis::Definition::new(owner, sources_vis1);
        let javascript_vis0 = javascript_vis0.expect("Couldn't create visualization class.");
        let javascript_vis1 = javascript_vis1.expect("Couldn't create visualization class.");
        let javascript_vis0: visualization::Definition = javascript_vis0.into();
        let javascript_vis1: visualization::Definition = javascript_vis1.into();

        let expected_visualizations =
            vec![embedded_visualization, javascript_vis0, javascript_vis1];
        let zipped = visualizations.iter().zip(expected_visualizations.iter());
        for (visualization, expected_definition) in zipped {
            let loaded_definition = vis_controller.load_visualization(visualization).await;
            let loaded_definition =
                loaded_definition.expect("Couldn't load visualization's content.");
            let loaded_signature = &loaded_definition.signature;
            let expected_signature = &expected_definition.signature;
            assert_eq!(loaded_signature, expected_signature);
        }
    }
}
