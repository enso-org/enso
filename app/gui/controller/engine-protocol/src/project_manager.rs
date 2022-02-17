//! Client library for the JSON-RPC-based Project Manager service.
//!
//! The all methods and types are derived from Engine RPC API described
//! here https://dev.enso.org/docs/enso/language-server/protocol-project-manager.html

//FIXME: We need to review the structures' names in Enso Protocol specification
// https://github.com/enso-org/enso/issues/708

use crate::prelude::*;

use crate::types::UTCDateTime;
use json_rpc::api::Result;
use json_rpc::make_rpc_methods;
use json_rpc::Handler;
use serde::Deserialize;
use serde::Serialize;
use std::future::Future;
use uuid::Uuid;



// =============
// === Event ===
// =============

// Project Manager has no notifications, so we create a dummy Notification type for it.
type Notification = ();

/// Event emitted by the Project Manager `Client`.
pub type Event = json_rpc::handler::Event<Notification>;



// ===================
// === RPC Methods ===
// ===================

make_rpc_methods! {
/// An interface containing all the available project management operations.
trait API {
    /// Request the project manager to open a specified project. This operation also
    /// includes spawning an instance of the language server open on the specified project.
    ///
    /// If the opened project uses Enso version not installed yet, this method outcome is defined
    /// by `missing_component_action` argument.
    #[MethodInput=OpenProjectInput,rpc_name="project/open"]
    fn open_project
    (&self, project_id:Uuid, missing_component_action:MissingComponentAction)
    -> response::OpenProject;

    /// Request the project manager to close a specified project. This operation
    /// includes shutting down the language server gracefully so that it can persist state to disk
    /// as needed.
    #[MethodInput=CloseProjectInput,rpc_name="project/close"]
    fn close_project(&self, project_id:Uuid) -> ();

    /// Request the project manager to lists all user's projects. The list of projects is sorted by
    /// the open time.
    #[MethodInput=ListRecentProjectsInput,rpc_name="project/list"]
    fn list_projects(&self, number_of_projects:Option<u32>) -> response::ProjectList;

    /// Request the creation of a new project.
    #[MethodInput=CreateProjectInput,rpc_name="project/create"]
    fn create_project
    ( &self
    , name                     : ProjectName
    , project_template         : Option<String>
    , version                  : Option<String>
    , missing_component_action : MissingComponentAction
    ) -> response::CreateProject;

    /// Request project renaming.
    #[MethodInput=RenameProject,rpc_name="project/rename"]
    fn rename_project(&self, project_id:Uuid, name:ProjectName) -> ();

    /// Request the deletion of a project.
    #[MethodInput=DeleteProjectInput,rpc_name="project/delete"]
    fn delete_project(&self, project_id:Uuid) -> ();

    /// Request a list of sample projects that are available to the user.
    #[MethodInput=ListSamplesInput,rpc_name="project/listSample"]
    fn list_samples(&self, num_projects:u32) -> response::ProjectList;
}}



// =============
// === Types ===
// =============

/// Address consisting of host and port.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct IpWithSocket {
    /// Host name.
    pub host: String,
    /// Port number.
    pub port: u16,
}

impl Display for IpWithSocket {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ws://{}:{}", self.host, self.port)
    }
}

/// Project name.
#[derive(Clone, Debug, Deserialize, Display, Eq, From, Hash, PartialEq, Serialize)]
pub struct ProjectName(String);

impl ProjectName {
    /// Create new ProjectName without any validation.
    ///
    /// The caller is responsible for making sure that provided string is a valid project name
    /// (e.g. not empty and starts with a capital letter).
    pub fn new_unchecked(name: impl Str) -> Self {
        Self(name.into())
    }
}

impl AsRef<str> for ProjectName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<ProjectName> for String {
    fn from(name: ProjectName) -> Self {
        name.0
    }
}

/// Project information, such as name, its id and last time it was opened.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ProjectMetadata {
    /// Project's name.
    pub name:           ProjectName,
    /// Project's namespace,
    pub namespace:      String,
    /// Project's uuid.
    pub id:             Uuid,
    /// Engine version to use for the project, represented by a semver version string.
    pub engine_version: Option<String>,
    /// Last time the project was opened.
    pub last_opened:    Option<UTCDateTime>,
}

/// This type specifies what action should be taken if an Engine's component required to complete
/// the Project Manager operation (like project/open) is missing.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum MissingComponentAction {
    /// Will make the operation fail if any components are missing.
    Fail,
    /// Will try to install any missing components, unless they are marked as broken.
    Install,
    /// Will try to install all missing components, even if some of them are marked as broken.
    ForceInstallBroken,
}


/// Wrappers for RPC method responses.
pub mod response {
    use super::*;

    /// Response of `list_projects` and `list_samples`.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct ProjectList {
        /// List of projects.
        pub projects: Vec<ProjectMetadata>,
    }

    /// Response of `create_project`.
    #[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct CreateProject {
        /// Created project uuid.
        pub project_id: Uuid,
    }

    /// Response of `open_project`.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct OpenProject {
        /// The version of the started language server represented by a semver version string.
        pub engine_version:                 String,
        /// Address of the endpoint for JSON-RPC communication.
        pub language_server_json_address:   IpWithSocket,
        /// Address of the endpoint for binary FlatBuffers communication.
        pub language_server_binary_address: IpWithSocket,
        /// The name of the project as it is opened.
        pub project_name:                   ProjectName,
        /// The namespace of the project.
        pub project_namespace:              String,
    }
}



// ========================
// === MockClient tests ===
// ========================

#[cfg(test)]
mod mock_client_tests {
    use super::*;

    use chrono::DateTime;
    use json_rpc::error::RpcError;
    use json_rpc::expect_call;
    use json_rpc::messages::Error;
    use json_rpc::Result;
    use std::future::Future;
    use uuid::Uuid;

    fn error<T>(message: &str) -> Result<T> {
        let code = 1;
        let data = None;
        let message = message.to_string();
        let error = Error { code, data, message };
        Err(RpcError::RemoteError(error))
    }

    fn result<T, F: Future<Output = Result<T>>>(fut: F) -> Result<T> {
        let mut fut = Box::pin(fut);
        fut.expect_ready()
    }

    #[test]
    fn project_life_cycle() {
        let mock_client = MockClient::default();
        let expected_uuid = Uuid::default();
        let creation_response = response::CreateProject { project_id: expected_uuid };
        let host = "localhost".to_string();
        let port = 30500;
        let language_server_address = IpWithSocket { host, port };
        let expected_open_result = response::OpenProject {
            engine_version:                 "0.2.1".to_owned(),
            language_server_json_address:   language_server_address.clone(),
            language_server_binary_address: language_server_address,
            project_name:                   ProjectName::new_unchecked("Test"),
            project_namespace:              "local".to_owned(),
        };
        let open_result = Ok(expected_open_result.clone());
        let missing_component_action = MissingComponentAction::Fail;
        expect_call!(mock_client.create_project(
            name                     = ProjectName::new_unchecked("HelloWorld"),
            project_template         = None,
            version                  = None,
            missing_component_action = missing_component_action
        ) => Ok(creation_response));
        expect_call!(mock_client.open_project(expected_uuid,missing_component_action) => open_result);
        expect_call!(mock_client.close_project(expected_uuid) => error("Project isn't open."));
        expect_call!(mock_client.delete_project(expected_uuid) => error("Project doesn't exist."));

        let delete_result = mock_client.delete_project(&expected_uuid);
        result(delete_result).expect_err("Project shouldn't exist.");

        let name = ProjectName::new_unchecked("HelloWorld");
        let response = mock_client.create_project(&name, &None, &None, &missing_component_action);
        let uuid = result(response).expect("Couldn't create project").project_id;
        assert_eq!(uuid, expected_uuid);

        let close_result = result(mock_client.close_project(&uuid));
        close_result.expect_err("Project shouldn't be open.");

        let ip_with_socket = result(mock_client.open_project(&uuid, &missing_component_action));
        let ip_with_socket = ip_with_socket.expect("Couldn't open project");
        assert_eq!(ip_with_socket, expected_open_result);

        expect_call!(mock_client.close_project(expected_uuid) => Ok(()));
        result(mock_client.close_project(&uuid)).expect("Couldn't close project.");

        expect_call!(mock_client.delete_project(expected_uuid) => Ok(()));
        result(mock_client.delete_project(&uuid)).expect("Couldn't delete project.");
    }

    #[test]
    fn list_projects() {
        let mock_client = MockClient::default();
        let project1 = ProjectMetadata {
            name:           ProjectName::new_unchecked("project1"),
            id:             Uuid::default(),
            last_opened:    Some(DateTime::parse_from_rfc3339("2020-01-07T21:25:26Z").unwrap()),
            engine_version: Some("0.2.21".to_owned()),
            namespace:      "local".to_owned(),
        };
        let project2 = ProjectMetadata {
            name:           ProjectName::new_unchecked("project2"),
            id:             Uuid::default(),
            last_opened:    Some(DateTime::parse_from_rfc3339("2020-02-02T13:15:20Z").unwrap()),
            engine_version: Some("0.2.22".to_owned()),
            namespace:      "local".to_owned(),
        };
        let expected_recent_projects = response::ProjectList { projects: vec![project1, project2] };
        let sample1 = ProjectMetadata {
            name:           ProjectName::new_unchecked("sample1"),
            id:             Uuid::default(),
            last_opened:    Some(DateTime::parse_from_rfc3339("2019-11-23T05:30:12Z").unwrap()),
            engine_version: Some("0.2.21".to_owned()),
            namespace:      "test".to_owned(),
        };
        let sample2 = ProjectMetadata {
            name:           ProjectName::new_unchecked("sample2"),
            id:             Uuid::default(),
            last_opened:    Some(DateTime::parse_from_rfc3339("2019-12-25T00:10:58Z").unwrap()),
            engine_version: Some("0.2.21".to_owned()),
            namespace:      "test".to_owned(),
        };
        let expected_sample_projects = response::ProjectList { projects: vec![sample1, sample2] };
        expect_call!(mock_client.list_projects(count=Some(2)) =>
           Ok(expected_recent_projects.clone()));
        expect_call!(mock_client.list_samples(count=2) => Ok(expected_sample_projects.clone()));

        let list_recent_error = "Couldn't get recent projects.";
        let list_sample_error = "Couldn't get sample projects.";
        let count_limit = Some(2);
        let recent_projects = result(mock_client.list_projects(&count_limit));
        let recent_projects = recent_projects.expect(list_recent_error);
        assert_eq!(recent_projects, expected_recent_projects);
        let sample_projects = result(mock_client.list_samples(&2)).expect(list_sample_error);
        assert_eq!(sample_projects, expected_sample_projects);
    }
}



// ====================
// === Client tests ===
// ====================

#[cfg(test)]
mod remote_client_tests {
    use super::*;

    use chrono::DateTime;
    use futures::task::LocalSpawnExt;
    use json_rpc::messages::Message;
    use json_rpc::messages::RequestMessage;
    use json_rpc::test_util::transport::mock::MockTransport;
    use serde_json::json;
    use serde_json::Value;
    use std::future::Future;

    struct Fixture {
        transport: MockTransport,
        client:    Client,
        executor:  futures::executor::LocalPool,
    }

    fn setup_fm() -> Fixture {
        let transport = MockTransport::new();
        let client = Client::new(transport.clone());
        let executor = futures::executor::LocalPool::new();
        executor.spawner().spawn_local(client.runner()).unwrap();
        Fixture { transport, client, executor }
    }

    /// Tests making a request using project manager:
    /// * creates PM client and uses `make_request` to make a request
    /// * checks that request is made for `expected_method`
    /// * checks that request input is `expected_input`
    /// * mocks receiving a response from server with `result`
    /// * checks that FM-returned Future yields `expected_output`
    fn test_request<Fun, Fut, T>(
        make_request: Fun,
        expected_method: &str,
        expected_input: &Value,
        result: &Value,
        expected_output: &T,
    ) where
        Fun: FnOnce(&mut Client) -> Fut,
        Fut: Future<Output = Result<T>>,
        T: Debug + PartialEq,
    {
        let mut fixture = setup_fm();
        let mut fut = Box::pin(make_request(&mut fixture.client));

        let request = fixture.transport.expect_json_message::<RequestMessage<Value>>();
        assert_eq!(request.method, *expected_method);
        assert_eq!(request.params, *expected_input);

        let response = Message::new_success(request.id, result);
        fixture.transport.mock_peer_json_message(response);
        fixture.executor.run_until_stalled();
        let output = fut.expect_ok();
        assert_eq!(output, *expected_output);
    }

    #[test]
    fn test_requests() {
        let unit_json = json!(null);
        let project_id = Uuid::default();
        let missing_component_action = MissingComponentAction::Install;
        let engine_version = "1.0.0".to_owned();
        let engine_version_opt = Some(engine_version);
        let create_project_response = response::CreateProject { project_id };
        let project_id_json = json!({"projectId":"00000000-0000-0000-0000-000000000000"});
        let project_id_and_mca = json!({
            "projectId"              : "00000000-0000-0000-0000-000000000000",
            "missingComponentAction" : "Install"
        });

        let engine_version = "0.2.1".to_owned();
        let language_server_json_address =
            IpWithSocket { host: "localhost".to_string(), port: 27015 };
        let language_server_binary_address =
            IpWithSocket { host: "localhost".to_string(), port: 27016 };
        let project_name = ProjectName::new_unchecked("Test");
        let project_namespace = "test_ns".to_owned();
        let open_result = response::OpenProject {
            engine_version,
            language_server_json_address,
            language_server_binary_address,
            project_name,
            project_namespace,
        };
        let open_result_json = json!({
            "engineVersion" : "0.2.1",
            "languageServerJsonAddress" : {
                "host" : "localhost",
                "port" : 27015
            },
            "languageServerBinaryAddress" : {
                "host" : "localhost",
                "port" : 27016
            },
            "projectName"      : "Test",
            "projectNamespace" : "test_ns",
        });
        let project_name = ProjectName::new_unchecked("HelloWorld");
        let project_template = Some(String::from("template"));
        let project_create_json = json!({
            "name"                   : serde_json::to_value(&project_name).unwrap(),
            "projectTemplate"        : serde_json::to_value(&project_template).unwrap(),
            "missingComponentAction" : "Install",
            "version"                : "1.0.0",
        });
        let number_of_projects = 2;
        let number_of_projects_json = json!({ "numberOfProjects": number_of_projects });
        let num_projects_json = json!({ "numProjects": number_of_projects });
        let project1 = ProjectMetadata {
            name:           ProjectName::new_unchecked("project1"),
            id:             Uuid::default(),
            last_opened:    Some(DateTime::parse_from_rfc3339("2020-01-07T21:25:26Z").unwrap()),
            engine_version: Some("0.2.21".to_owned()),
            namespace:      "local".to_owned(),
        };
        let project2 = ProjectMetadata {
            name:           ProjectName::new_unchecked("project2"),
            id:             Uuid::default(),
            last_opened:    Some(DateTime::parse_from_rfc3339("2020-02-02T13:15:20Z").unwrap()),
            engine_version: Some("0.2.22".to_owned()),
            namespace:      "local".to_owned(),
        };
        let project_list = response::ProjectList { projects: vec![project1, project2] };
        let project_list_json = json!({
            "projects" : [
                {
                    "id"            : "00000000-0000-0000-0000-000000000000",
                    "lastOpened"    : "2020-01-07T21:25:26+00:00",
                    "name"          : "project1",
                    "engineVersion" : "0.2.21",
                    "namespace"     : "local"
                },
                {
                    "id"            : "00000000-0000-0000-0000-000000000000",
                    "lastOpened"    : "2020-02-02T13:15:20+00:00",
                    "name"          : "project2",
                    "engineVersion" : "0.2.22",
                    "namespace"     : "local"
                }
            ]
        });

        test_request(
            |client| client.list_projects(&Some(number_of_projects)),
            "project/list",
            &number_of_projects_json,
            &project_list_json,
            &project_list,
        );
        test_request(
            |client| client.list_samples(&number_of_projects),
            "project/listSample",
            &num_projects_json,
            &project_list_json,
            &project_list,
        );
        test_request(
            |client| client.open_project(&project_id, &missing_component_action),
            "project/open",
            &project_id_and_mca,
            &open_result_json,
            &open_result,
        );
        test_request(
            |client| client.close_project(&project_id),
            "project/close",
            &project_id_json,
            &unit_json,
            &(),
        );
        test_request(
            |client| client.delete_project(&project_id),
            "project/delete",
            &project_id_json,
            &unit_json,
            &(),
        );
        test_request(
            |client| {
                client.create_project(
                    &project_name,
                    &project_template,
                    &engine_version_opt,
                    &missing_component_action,
                )
            },
            "project/create",
            &project_create_json,
            &project_id_json,
            &create_project_response,
        );
    }
}
