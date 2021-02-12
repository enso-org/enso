//! This module contains the IDE object implementation.
pub mod initializer;
pub mod integration;

use crate::prelude::*;

use crate::controller::FilePath;
use crate::model::module::Path as ModulePath;
use crate::ide::integration::Integration;

use ensogl::application::Application;
use enso_protocol::language_server::MethodPointer;
use parser::Parser;

pub use initializer::Initializer;



// =================
// === Constants ===
// =================

/// The name of the module initially opened in the project view.
///
/// Currently this name is hardcoded in the engine services and is populated for each project
/// created using engine's Project Picker service.
///
/// TODO [mwu] Name of the module that will be initially opened in the text editor.
///      Provisionally the Project View is hardcoded to open with a single text
///      editor and it will be connected with a file with module of this name.
///      To be replaced with better mechanism once we decide how to describe
///      default initial layout for the project.
pub const INITIAL_MODULE_NAME:&str = "Main";

/// Name of the main definition.
///
/// This is the definition whose graph will be opened on IDE start.
pub const MAIN_DEFINITION_NAME:&str = "main";

/// The code with definition of the default `main` method.
pub fn default_main_method_code() -> String {
    format!(r#"{} = "Hello, World!""#, MAIN_DEFINITION_NAME)
}

/// The default content of the newly created initial main module file.
pub fn default_main_module_code() -> String {
    default_main_method_code()
}

/// Method pointer that described the main method, i.e. the method that project view wants to open
/// and which presence is currently required.
pub fn main_method_ptr(project_name:impl Str, module_path:&model::module::Path) -> MethodPointer {
    module_path.method_pointer(project_name,MAIN_DEFINITION_NAME)
}



// ===========
// === Ide ===
// ===========

/// The main Ide structure.
///
/// This structure is a root of all objects in our application. It includes both layers:
/// Controllers and Views, and an integration between them.
#[derive(Debug)]
pub struct Ide {
    application : Application,
    integration : Integration,
}

impl Ide {
    /// Constructor.
    pub async fn new(application:Application, view:ide_view::project::View, project:model::Project) -> FallibleResult<Self> {
        let logger      = Logger::new("Ide");
        let module_path = initial_module_path(&project)?;
        let file_path   = module_path.file_path().clone();
        // TODO [mwu] This solution to recreate missing main file should be considered provisional
        //   until proper decision is made. See: https://github.com/enso-org/enso/issues/1050
        recreate_if_missing(&project, &file_path, default_main_method_code()).await?;

        let method = main_method_ptr(project.name(),&module_path);
        let module = project.module(module_path).await?;
        add_main_if_missing(project.name().as_ref(),&module,&method,&project.parser())?;

        // Here, we should be relatively certain (except race conditions in case of multiple clients
        // that we currently do not support) that main module exists and contains main method.
        // Thus, we should be able to successfully create a graph controller for it.
        let graph         = controller::ExecutedGraph::new(&logger,project.clone_ref(),method).await?;
        let text          = controller::Text::new(&logger, &project, file_path).await?;
        let visualization = project.visualization().clone();

        let integration = Integration::new(view,graph,text,visualization,project);
        Ok(Ide {application,integration})
    }
}


/// Returns the path to the initially opened module in the given project.
pub fn initial_module_path(project:&model::Project) -> FallibleResult<ModulePath> {
    model::module::Path::from_name_segments(project.content_root_id(),&[INITIAL_MODULE_NAME])
}

/// Create a file with default content if it does not already exist.
pub async fn recreate_if_missing(project:&model::Project, path:&FilePath, default_content:String)
-> FallibleResult {
    let rpc = project.json_rpc();
    if !rpc.file_exists(path).await?.exists {
        rpc.write_file(path,&default_content).await?;
    }
    Ok(())
}

/// Add main method definition to the given module, if the method is not already defined.
///
/// The lookup will be done using the given `main_ptr` value.
pub fn add_main_if_missing
(project_name:&str, module:&model::Module, main_ptr:&MethodPointer, parser:&Parser)
-> FallibleResult {
    if module.lookup_method(project_name,main_ptr).is_err() {
        let mut info  = module.info();
        let main_code = default_main_method_code();
        let main_ast  = parser.parse_line(main_code)?;
        info.add_ast(main_ast,double_representation::module::Placement::End)?;
        module.update_ast(info.ast)?;
    }
    Ok(())
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    #[wasm_bindgen_test]
    fn adding_missing_main() {
        let _ctx        = TestWithLocalPoolExecutor::set_up();
        let parser      = parser::Parser::new_or_panic();
        let mut data    = crate::test::mock::Unified::new();
        let module_name = data.module_path.module_name();
        let main_ptr    = main_method_ptr(&data.project_name,&data.module_path);

        // Check that module without main gets it after the call.
        let empty_module_code = "";
        data.set_code(empty_module_code);
        let module = data.module();
        assert!(module.lookup_method(&data.project_name,&main_ptr).is_err());
        add_main_if_missing(&data.project_name,&module,&main_ptr,&parser).unwrap();
        assert!(module.lookup_method(&data.project_name,&main_ptr).is_ok());

        // Now check that modules that have main already defined won't get modified.
        let mut expect_intact = move |code:&str| {
            data.set_code(code);
            let module = data.module();
            add_main_if_missing(&data.project_name,&module,&main_ptr,&parser).unwrap();
            assert_eq!(code,module.ast().repr());
        };
        expect_intact("main = 5");
        expect_intact("here.main = 5");
        expect_intact(&format!("{}.main = 5",module_name));
    }
}
