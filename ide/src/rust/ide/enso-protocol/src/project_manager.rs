//! Client library for the JSON-RPC-based Project Manager service.

//FIXME: We need to review the structures' names in Enso Protocol specification
// https://github.com/luna/enso/issues/708

use crate::prelude::*;

use crate::types::UTCDateTime;
use json_rpc::api::Result;
use json_rpc::Handler;
use json_rpc::make_rpc_methods;
use futures::Stream;
use serde::Serialize;
use serde::Deserialize;
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
    /// Request the project picker to open a specified project. This operation also
    /// includes spawning an instance of the language server open on the specified project.
    #[MethodInput=OpenProjectInput,rpc_name="project/open",result=open_project_result,
    set_result=set_open_project_result]
    fn open_project(&self, project_id:Uuid) -> response::OpenProject;

    /// Request the project picker to close a specified project. This operation
    /// includes shutting down the language server gracefully so that it can persist state to disk
    /// as needed.
    #[MethodInput=CloseProjectInput,rpc_name="project/close",result=close_project_result,
    set_result=set_close_project_result]
    fn close_project(&self, project_id:Uuid) -> ();

    /// Request the project picker to list the user's most recently opened projects.
    #[MethodInput=ListRecentProjectsInput,rpc_name="project/listRecent",
    result=list_recent_projects_result,set_result=set_list_recent_projects_result]
    fn list_recent_projects(&self, number_of_projects:u32) -> response::ProjectList;

    /// Request the creation of a new project.
    #[MethodInput=CreateProjectInput,rpc_name="project/create",result=create_project_result,
    set_result=set_create_project_result]
    fn create_project(&self, name:String) -> response::CreateProject;

    /// Request the deletion of a project.
    #[MethodInput=DeleteProjectInput,rpc_name="project/delete",result=delete_project_result,
    set_result=set_delete_project_result]
    fn delete_project(&self, project_id:Uuid) -> ();

    /// Request a list of sample projects that are available to the user.
    #[MethodInput=ListSamplesInput,rpc_name="project/listSample",result=list_samples_result,
    set_result=set_list_samples_result]
    fn list_samples(&self, num_projects:u32) -> response::ProjectList;
}}



// =============
// === Types ===
// =============

/// Address consisting of host and port.
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq)]
pub struct IpWithSocket {
    /// Host name.
    pub host : String,
    /// Port number.
    pub port : u16
}

/// Project name.
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq,Shrinkwrap)]
pub struct ProjectName {
    #[allow(missing_docs)]
    pub name : String
}

/// Project information, such as name, its id and last time it was opened.
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq)]
pub struct ProjectMetadata {
    /// Project's name.
    #[serde(flatten)]
    pub name : ProjectName,
    /// Project's uuid.
    pub id : Uuid,
    /// Last time the project was opened.
    pub last_opened : Option<UTCDateTime>
}


/// Wrappers for RPC method responses.
pub mod response {
    use super::*;

    /// Response of `list_recent_projects` and `list_samples`.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct ProjectList {
        /// List of projects.
        pub projects: Vec<ProjectMetadata>
    }

    /// Response of `create_project`.
    #[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct CreateProject {
        /// Created project uuid.
        pub project_id: Uuid
    }

    /// Response of `open_project`.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    #[serde(rename_all = "camelCase")]
    pub struct OpenProject {
        /// Address of the endpoint for JSON-RPC communication.
        pub language_server_rpc_address  : IpWithSocket,
        /// Address of the endpoint for binary FlatBuffers communication.
        pub language_server_data_address : IpWithSocket,
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
    use json_rpc::messages::Error;
    use json_rpc::Result;
    use std::future::Future;
    use utils::test::poll_future_output;
    use uuid::Uuid;

    fn error<T>(message:&str) -> Result<T> {
        let code    = 1;
        let data    = None;
        let message = message.to_string();
        let error   = Error {code,data,message};
        Err(RpcError::RemoteError(error))
    }

    fn result<T,F:Future<Output = Result<T>>>(fut:F) -> Result<T> {
        let mut fut = Box::pin(fut);
        poll_future_output(&mut fut).expect("Promise isn't ready")
    }

    #[test]
    fn project_life_cycle() {
        let mock_client             = MockClient::default();
        let expected_uuid           = Uuid::default();
        let creation_response       = response::CreateProject {project_id : expected_uuid.clone()};
        let host                    = "localhost".to_string();
        let port                    = 30500;
        let language_server_address = IpWithSocket {host,port};
        let expected_ip_with_socket = response::OpenProject {
            language_server_rpc_address  : language_server_address.clone(),
            language_server_data_address : language_server_address,
        };
        let open_result             = Ok(expected_ip_with_socket.clone());
        mock_client.set_create_project_result("HelloWorld".into(),Ok(creation_response));
        mock_client.set_open_project_result(expected_uuid.clone(),open_result);
        mock_client.set_close_project_result(expected_uuid.clone(),error("Project isn't open."));
        mock_client.set_delete_project_result(expected_uuid.clone(),error("Project doesn't exist."));

        let delete_result = mock_client.delete_project(&expected_uuid);
        result(delete_result).expect_err("Project shouldn't exist.");

        let creation_response = mock_client.create_project(&"HelloWorld".to_string());
        let uuid = result(creation_response).expect("Couldn't create project").project_id;
        assert_eq!(uuid, expected_uuid);

        let close_result = result(mock_client.close_project(&uuid));
        close_result.expect_err("Project shouldn't be open.");

        let ip_with_socket = result(mock_client.open_project(&uuid));
        let ip_with_socket = ip_with_socket.expect("Couldn't open project");
        assert_eq!(ip_with_socket, expected_ip_with_socket);

        mock_client.set_close_project_result(expected_uuid.clone(), Ok(()));
        result(mock_client.close_project(&uuid)).expect("Couldn't close project.");

        mock_client.set_delete_project_result(expected_uuid.clone(), Ok(()));
        result(mock_client.delete_project(&uuid)).expect("Couldn't delete project.");
    }

    #[test]
    fn list_recent_projects() {
        let mock_client = MockClient::default();
        let project1    = ProjectMetadata {
            name        : ProjectName { name : "project1".to_string() },
            id          : Uuid::default(),
            last_opened : Some(DateTime::parse_from_rfc3339("2020-01-07T21:25:26Z").unwrap())
        };
        let project2 = ProjectMetadata {
            name        : ProjectName { name : "project2".to_string() },
            id          : Uuid::default(),
            last_opened : Some(DateTime::parse_from_rfc3339("2020-02-02T13:15:20Z").unwrap())
        };
        let expected_recent_projects = response::ProjectList { projects : vec![project1,project2] };
        let sample1 = ProjectMetadata {
            name        : ProjectName { name : "sample1".to_string() },
            id          : Uuid::default(),
            last_opened : Some(DateTime::parse_from_rfc3339("2019-11-23T05:30:12Z").unwrap())
        };
        let sample2 = ProjectMetadata {
            name        : ProjectName { name : "sample2".to_string() },
            id          : Uuid::default(),
            last_opened : Some(DateTime::parse_from_rfc3339("2019-12-25T00:10:58Z").unwrap())
        };
        let expected_sample_projects = response::ProjectList { projects : vec![sample1,sample2] };
        mock_client.set_list_recent_projects_result(2,Ok(expected_recent_projects.clone()));
        mock_client.set_list_samples_result(2,Ok(expected_sample_projects.clone()));

        let list_recent_error = "Couldn't get recent projects.";
        let list_sample_error = "Couldn't get sample projects.";
        let recent_projects = result(mock_client.list_recent_projects(&2)).expect(list_recent_error);
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
    use json_rpc::messages::Message;
    use json_rpc::messages::RequestMessage;
    use json_rpc::test_util::transport::mock::MockTransport;
    use serde_json::json;
    use serde_json::Value;
    use std::future::Future;
    use utils::test::poll_future_output;
    use futures::task::LocalSpawnExt;

    struct Fixture {
        transport : MockTransport,
        client    : Client,
        executor  : futures::executor::LocalPool,
    }

    fn setup_fm() -> Fixture {
        let transport = MockTransport::new();
        let client    = Client::new(transport.clone());
        let executor  = futures::executor::LocalPool::new();
        executor.spawner().spawn_local(client.runner()).unwrap();
        Fixture {transport,client,executor}
    }

    /// Tests making a request using project manager:
    /// * creates PM client and uses `make_request` to make a request
    /// * checks that request is made for `expected_method`
    /// * checks that request input is `expected_input`
    /// * mocks receiving a response from server with `result`
    /// * checks that FM-returned Future yields `expected_output`
    fn test_request<Fun, Fut, T>
    ( make_request    : Fun
    , expected_method : &str
    , expected_input  : &Value
    , result          : &Value
    , expected_output : &T
    ) where Fun : FnOnce(&mut Client) -> Fut,
            Fut : Future<Output = Result<T>>,
              T : Debug + PartialEq {
        let mut fixture = setup_fm();
        let mut fut     = Box::pin(make_request(&mut fixture.client));

        let request = fixture.transport.expect_json_message::<RequestMessage<Value>>();
        assert_eq!(request.method, *expected_method);
        assert_eq!(request.params, *expected_input);

        let response = Message::new_success(request.id, result);
        fixture.transport.mock_peer_json_message(response);
        fixture.executor.run_until_stalled();
        let output = poll_future_output(&mut fut).unwrap().unwrap();
        assert_eq!(output, *expected_output);
    }

    #[test]
    fn test_requests() {
        let unit_json               = json!(null);
        let project_id              = Uuid::default();
        let create_project_response = response::CreateProject { project_id };
        let project_id_json         = json!({"projectId":"00000000-0000-0000-0000-000000000000"});

        let language_server_rpc_address  = IpWithSocket{host:"localhost".to_string(),port:27015};
        let language_server_data_address = IpWithSocket{host:"localhost".to_string(),port:27016};
        let ip_with_address              = response::OpenProject {language_server_rpc_address,
            language_server_data_address};
        let ip_with_address_json = json!({
            "languageServerRpcAddress" : {
                "host" : "localhost",
                "port" : 27015
            },
            "languageServerDataAddress" : {
                "host" : "localhost",
                "port" : 27016
            }
        });
        let project_name            = String::from("HelloWorld");
        let project_name_json       = json!({"name":serde_json::to_value(&project_name).unwrap()});
        let number_of_projects      = 2;
        let number_of_projects_json = json!({"numberOfProjects":number_of_projects});
        let num_projects_json       = json!({"numProjects":number_of_projects});
        let project1                = ProjectMetadata {
            name        : ProjectName { name : "project1".to_string() },
            id          : Uuid::default(),
            last_opened : Some(DateTime::parse_from_rfc3339("2020-01-07T21:25:26Z").unwrap())
        };
        let project2 = ProjectMetadata {
            name        : ProjectName { name : "project2".to_string() },
            id          : Uuid::default(),
            last_opened : Some(DateTime::parse_from_rfc3339("2020-02-02T13:15:20Z").unwrap())
        };
        let project_list      = response::ProjectList { projects : vec![project1,project2] };
        let project_list_json = json!({
            "projects" : [
                {
                    "id"          : "00000000-0000-0000-0000-000000000000",
                    "last_opened" : "2020-01-07T21:25:26+00:00",
                    "name"        : "project1"
                },
                {
                    "id"          : "00000000-0000-0000-0000-000000000000",
                    "last_opened" : "2020-02-02T13:15:20+00:00",
                    "name"        : "project2"
                }
            ]
        });

        test_request(
            |client| client.list_recent_projects(&number_of_projects),
            "project/listRecent",
            &number_of_projects_json,
            &project_list_json,
            &project_list
        );
        test_request(
            |client| client.list_samples(&number_of_projects),
            "project/listSample",
            &num_projects_json,
            &project_list_json,
            &project_list
        );
        test_request(
            |client| client.open_project(&project_id),
            "project/open",
            &project_id_json,
            &ip_with_address_json,
            &ip_with_address
        );
        test_request(
            |client| client.close_project(&project_id),
            "project/close",
            &project_id_json,
            &unit_json,
            &()
        );
        test_request(
            |client| client.delete_project(&project_id),
            "project/delete",
            &project_id_json,
            &unit_json,
            &()
        );
        test_request(
            |client| client.create_project(&project_name),
            "project/create",
            &project_name_json,
            &project_id_json,
            &create_project_response
        );
    }
}
