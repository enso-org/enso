//! The module with all handlers required by uploading dragged and dropped files on IDE.

use crate::prelude::*;

use crate::controller::graph::LocationHint;
use crate::controller::graph::NewNodeInfo;
use crate::model::module::NodeMetadata;
use crate::model::module::Position;
use crate::model::module::UploadingFile;
use crate::model::undo_redo;
use crate::model::undo_redo::Repository;

use engine_protocol::binary;
use engine_protocol::common::error::code;
use engine_protocol::language_server;
use engine_protocol::language_server::FileSystemObject;
use engine_protocol::language_server::Path;
use engine_protocol::types::Sha3_224;
use json_rpc::error::RpcError;
use sha3::Digest;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
#[fail(display = "Wrong checksum of uploaded file: {}, local checksum is {}.", remote, local)]
pub struct ChecksumMismatch {
    pub remote: Sha3_224,
    pub local:  Sha3_224,
}



// =================
// === Constants ===
// =================

const DATA_DIR_NAME: &str = "data";



// ====================
// === DataProvider ===
// ====================

/// Trait allowing reading specific file content chunk by chunk.
pub trait DataProvider {
    /// Return a future providing the next chunk of file data.
    ///
    /// Returns [`None`] if the whole file's content has been read. The upload handlers defined in
    /// this module ([`NodeFromDroppedFileHandler`] and [`FileUploadProcess`]) will not call this
    /// method before fully uploading and freeing the previously read chunk.
    fn next_chunk(&mut self) -> BoxFuture<FallibleResult<Option<Vec<u8>>>>;
}



// =========================
// === FileUploadProcess ===
// =========================

/// Information about file-to-upload.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct FileToUpload<DataProvider> {
    pub name: String,
    pub size: u64,
    pub data: DataProvider,
}

/// The handler of uploading a given file to the specific location using the Language Server's file
/// API.
#[derive(Clone, Debug)]
pub struct FileUploadProcess<DataProvider> {
    bin_connection:  Rc<binary::Connection>,
    json_connection: Rc<language_server::Connection>,
    file:            FileToUpload<DataProvider>,
    remote_path:     Path,
    bytes_uploaded:  u64,
    checksum:        sha3::Sha3_224,
}

/// The information if the uploading is finished or not, returned from
/// [`FileUploadProcess::upload_chunk`].
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UploadingState {
    Finished,
    NotFinished,
}

impl<DP: DataProvider> FileUploadProcess<DP> {
    /// Constructor.
    pub fn new(
        file: FileToUpload<DP>,
        bin_connection: Rc<binary::Connection>,
        json_connection: Rc<language_server::Connection>,
        remote_path: Path,
    ) -> Self {
        let bytes_uploaded = 0;
        let checksum = sha3::Sha3_224::new();
        Self { bin_connection, json_connection, file, remote_path, bytes_uploaded, checksum }
    }

    /// Upload next chunk. Returns information if all data has been uploaded.
    ///
    /// After uploading, the checksum of the uploaded file is compared with the file content digest,
    /// and an error is returned if they do not match.
    ///
    /// The outcome of this function when uploading is finished (the `upload_chunk` have returned
    /// [`UploadingState::Finished`] before) is undefined.
    pub async fn upload_chunk(&mut self) -> FallibleResult<UploadingState> {
        match self.file.data.next_chunk().await {
            Ok(Some(data)) => {
                debug!(
                    "Received chunk of {} of size {} uploading to {:?}: {:?}",
                    self.file.name,
                    data.len(),
                    self.remote_path,
                    data
                );
                let offset = self.bytes_uploaded;
                self.bin_connection.write_bytes(&self.remote_path, offset, false, &data).await?;
                self.checksum.input(&data);
                self.bytes_uploaded += data.len() as u64;
                Ok(UploadingState::NotFinished)
            }
            Ok(None) => {
                // If we haven't got any content, we need to create the file.
                if self.bytes_uploaded == 0 {
                    self.bin_connection.write_file(&self.remote_path, &[]).await?;
                }
                if self.bytes_uploaded != self.file.size {
                    error!(
                        "The promised file size ({}) and uploaded data length ({}) do not match. \
                        Leaving as much data as received.",
                        self.file.size, self.bytes_uploaded
                    );
                    self.bytes_uploaded = self.file.size;
                }
                self.check_checksum().await?;
                Ok(UploadingState::Finished)
            }
            Err(err) => Err(err),
        }
    }

    async fn check_checksum(&mut self) -> FallibleResult {
        let remote = self.json_connection.file_checksum(&self.remote_path).await?.checksum;
        let local = Into::<Sha3_224>::into(std::mem::take(&mut self.checksum));
        if remote != local {
            Err(ChecksumMismatch { remote, local }.into())
        } else {
            Ok(())
        }
    }
}



// ==================================
// === NodeFromDroppedFileHandler ===
// ==================================

/// The handler for nodes created by dragging and dropping files into IDE.
///
/// It is responsible for creating node, uploading file and updating the node's metadata.
#[derive(Clone, CloneRef, Debug)]
pub struct NodeFromDroppedFileHandler {
    logger:  Logger,
    project: model::Project,
    graph:   controller::Graph,
}

impl NodeFromDroppedFileHandler {
    /// Constructor
    pub fn new(parent: impl AnyLogger, project: model::Project, graph: controller::Graph) -> Self {
        let logger = Logger::new_sub(parent, "NodeFromDroppedFileHandler");
        Self { logger, project, graph }
    }

    /// Create a node from dropped file and start uploading file.
    ///
    /// The function returns once the node is created; the uploading process is scheduled in the
    /// global executor. The node's metadata will be updated with the uploading progress and
    /// error messages if any.
    pub fn create_node_and_start_uploading(
        &self,
        file: FileToUpload<impl DataProvider + 'static>,
        position: Position,
    ) -> FallibleResult {
        let node = self.graph.add_node(Self::new_node_info(&file, position))?;
        let this = self.clone_ref();
        executor::global::spawn(async move {
            if let Err(err) = this.upload_file(node, file).await {
                error!("Error while uploading file: {err}");
                this.update_metadata(node, |md| md.error = Some(err.to_string()));
            }
        });
        Ok(())
    }

    fn new_node_info<DP>(file: &FileToUpload<DP>, position: Position) -> NewNodeInfo {
        NewNodeInfo {
            expression:        Self::uploading_node_expression(&file.name),
            doc_comment:       None,
            metadata:          Some(Self::metadata_of_new_node(file, position)),
            id:                None,
            location_hint:     LocationHint::End,
            introduce_pattern: true,
        }
    }

    fn metadata_of_new_node<DP>(file: &FileToUpload<DP>, position: Position) -> NodeMetadata {
        let uploading_metadata = UploadingFile {
            name:           file.name.clone(),
            remote_name:    None,
            size:           file.size,
            bytes_uploaded: 0,
            error:          None,
        };
        NodeMetadata {
            position: Some(position),
            uploading_file: Some(uploading_metadata),
            ..default()
        }
    }

    async fn upload_file(
        &self,
        node: ast::Id,
        file: FileToUpload<impl DataProvider>,
    ) -> FallibleResult {
        self.ensure_data_directory_exists().await?;
        let remote_name = self.establish_remote_file_name(&file.name).await?;
        self.update_metadata(node, |md| md.remote_name = Some(remote_name.clone()));
        self.update_expression(node, Self::uploading_node_expression(&remote_name))?;
        let remote_path = self.data_path().append_im(&remote_name);
        let bin_connection = self.project.binary_rpc();
        let json_connection = self.project.json_rpc();
        let mut process =
            FileUploadProcess::new(file, bin_connection, json_connection, remote_path);

        while process.upload_chunk().await? == UploadingState::NotFinished {
            self.update_metadata(node, |md| md.bytes_uploaded = process.bytes_uploaded);
        }
        self.update_expression(node, Self::uploaded_node_expression(&remote_name))?;
        if let Err(err) =
            self.graph.module.with_node_metadata(node, Box::new(|md| md.uploading_file = None))
        {
            warning!(self.logger, "Cannot remove uploading metadata: {err}");
        }
        Ok(())
    }

    fn update_metadata(&self, node: ast::Id, f: impl FnOnce(&mut UploadingFile)) {
        //TODO[ao] see the TODO comment in update_expression.
        let _tr = self.undo_redo_repository().open_ignored_transaction("Upload Metadata Update");
        let update_md = Box::new(|md: &mut NodeMetadata| {
            if let Some(uploading_md) = &mut md.uploading_file {
                f(uploading_md)
            } else {
                warning!(
                    self.logger,
                    "Cannot update upload progress on {node:?}: Metadata are \
                    missing."
                );
            }
        });
        if let Err(err) = self.graph.module.with_node_metadata(node, update_md) {
            warning!(self.logger, "Cannot update upload progress: {err}");
        }
    }

    fn update_expression(&self, node: ast::Id, expression: String) -> FallibleResult {
        //TODO[ao]: Despite ignoring transaction, this will still not play well with undo-redo.
        //    Because UR keeps the whole AST in its stack, the transactions created during uploading
        //    process will keep trail of old metadata and expressions.
        //    Tracked in https://github.com/enso-org/ide/issues/1623
        let _tr =
            self.undo_redo_repository().open_transaction("Update Uploading Node's Expression");
        self.graph.set_expression(node, expression)
    }

    async fn establish_remote_file_name(&self, original_name: &str) -> FallibleResult<String> {
        pick_non_colliding_name(&*self.project.json_rpc(), &self.data_path(), original_name).await
    }

    async fn ensure_data_directory_exists(&self) -> FallibleResult {
        if !self.data_directory_exists().await? {
            let to_create = FileSystemObject::Directory {
                name: DATA_DIR_NAME.to_owned(),
                path: Path::new_root(self.project.project_content_root_id()),
            };
            self.project.json_rpc().create_file(&to_create).await?
        }
        Ok(())
    }

    async fn data_directory_exists(&self) -> FallibleResult<bool> {
        let path = self.data_path();
        let dir_info = self.project.json_rpc().file_info(&path).await;
        match dir_info {
            Ok(info) => Ok(matches!(info.attributes.kind, FileSystemObject::Directory { .. })),
            Err(RpcError::RemoteError(err))
                if err.code == code::FILE_NOT_FOUND || err.code == code::CONTENT_ROOT_NOT_FOUND =>
                Ok(false),
            Err(other_error) => Err(other_error.into()),
        }
    }

    fn uploading_node_expression(name: &str) -> String {
        format!("File_Uploading.file_uploading Enso_Project.data/\"{}\"", name)
    }

    fn uploaded_node_expression(name: &str) -> String {
        format!("File.read Enso_Project.data/\"{}\"", name)
    }

    fn data_path(&self) -> Path {
        Path::new(self.project.project_content_root_id(), &[DATA_DIR_NAME])
    }
}

impl undo_redo::Aware for NodeFromDroppedFileHandler {
    fn undo_redo_repository(&self) -> Rc<Repository> {
        self.graph.undo_redo_repository()
    }
}



// ======================================
// === File Name Collisions Resolving ===
// ======================================

/// Return the name derived from `original_name` which does not collide with any other file in
/// directory under `path`.
///
/// The directory content is retrieve from the Engine. If there is no colliding file, this function
/// will return the orignal name. Otherwise it adds to its stem a first non-colliding numeric
/// prefix.
pub async fn pick_non_colliding_name(
    json_rpc: &language_server::Connection,
    path: &Path,
    original_name: &str,
) -> FallibleResult<String> {
    let list_response = json_rpc.client.file_list(path).await?;
    let files_list = list_response.paths.into_iter().map(|f| f.take_name());
    let files_in_data_dir = files_list.collect::<HashSet<String>>();
    let extension_sep = original_name.rfind('.').filter(|i| *i > 0);
    let name_stem = extension_sep.map_or(original_name, |i| &original_name[0..i]);
    let name_ext = extension_sep.map_or("", |i| &original_name[i..]);
    let first_candidate = std::iter::once(original_name.to_owned());
    let next_candidates = (1..).map(|num| iformat!("{name_stem}_{num}{name_ext}"));
    let mut candidates = first_candidate.chain(next_candidates);
    Ok(candidates.find(|name| !files_in_data_dir.contains(name)).unwrap())
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::test::mock;

    use engine_protocol::language_server::response;
    use engine_protocol::language_server::FileAttributes;
    use engine_protocol::types::UTCDateTime;
    use futures::future;
    use futures::SinkExt;
    use mockall::Sequence;


    // === Test Providers ===

    type TestProvider = Box<dyn Iterator<Item = Vec<u8>>>;
    type TestAsyncProvider = futures::channel::mpsc::Receiver<FallibleResult<Vec<u8>>>;
    type TestAsyncProviderSink = futures::channel::mpsc::Sender<FallibleResult<Vec<u8>>>;

    impl DataProvider for TestProvider {
        fn next_chunk(&mut self) -> BoxFuture<FallibleResult<Option<Vec<u8>>>> {
            futures::future::ready(Ok(self.next())).boxed_local()
        }
    }

    impl DataProvider for TestAsyncProvider {
        fn next_chunk(&mut self) -> BoxFuture<FallibleResult<Option<Vec<u8>>>> {
            self.next()
                .map(|chunk| match chunk {
                    Some(Ok(chunk)) => Ok(Some(chunk)),
                    Some(Err(err)) => Err(err),
                    None => Ok(None),
                })
                .boxed_local()
        }
    }


    // === Data ===

    const TEST_FILE: &str = "file";

    struct TestData {
        chunks:    Vec<Vec<u8>>,
        file_size: usize,
        file_name: String,
        checksum:  Sha3_224,
        path:      Path,
    }

    impl TestData {
        fn new_with_file_name(
            chunks: Vec<Vec<u8>>,
            file_name: String,
            remote_name: String,
        ) -> Self {
            let entire_file = chunks.iter().flatten().copied().collect_vec();
            let file_size = entire_file.len();
            let checksum = Sha3_224::new(&entire_file);
            let path = Path::new(mock::data::ROOT_ID, &[DATA_DIR_NAME, &remote_name]);
            Self { chunks, file_size, file_name, checksum, path }
        }

        fn new(chunks: Vec<Vec<u8>>) -> Self {
            Self::new_with_file_name(chunks, TEST_FILE.to_owned(), TEST_FILE.to_owned())
        }

        fn setup_uploading_expectations(
            &self,
            json_client: &language_server::MockClient,
            binary_client: &mut binary::MockClient,
        ) {
            let mut write_seq = Sequence::new();
            let mut offset = 0;
            for chunk in self.chunks.iter().cloned() {
                let path = self.path.clone();
                let checksum = Sha3_224::new(&chunk);
                let chunk_len = chunk.len();
                DEBUG!("Setting expectation {path:?} {chunk:?}");
                binary_client
                    .expect_write_bytes()
                    .withf(move |p, off, ow, ch| *p == path && ch == chunk && *off == offset && !ow)
                    .times(1)
                    .in_sequence(&mut write_seq)
                    .returning(move |_, _, _, _| future::ready(Ok(checksum.clone())).boxed_local());
                offset += chunk_len as u64;
            }
            let checksum = self.checksum.clone();
            json_client.expect.file_checksum(enclose!((self.path => path) move |p| {
                assert_eq!(*p,path);
                Ok(response::FileChecksum{checksum})
            }));
        }

        fn file_to_upload(&self) -> FileToUpload<TestProvider> {
            FileToUpload {
                name: self.file_name.clone(),
                size: self.file_size as u64,
                data: Box::new(self.chunks.clone().into_iter()),
            }
        }

        fn file_to_upload_async(&self) -> (FileToUpload<TestAsyncProvider>, TestAsyncProviderSink) {
            let (sender, receiver) = futures::channel::mpsc::channel(5);
            let file_to_upload = FileToUpload {
                name: self.file_name.clone(),
                size: self.file_size as u64,
                data: receiver,
            };
            (file_to_upload, sender)
        }
    }


    // === FileUploadProcess Tests ===

    struct UploadingFixture {
        test:          TestWithLocalPoolExecutor,
        chunks:        <Vec<Vec<u8>> as IntoIterator>::IntoIter,
        process:       FileUploadProcess<TestAsyncProvider>,
        provider_sink: Option<TestAsyncProviderSink>,
    }

    impl UploadingFixture {
        fn new(data: TestData) -> Self {
            let mut binary_cli = binary::MockClient::new();
            let json_cli = language_server::MockClient::default();
            json_cli.require_all_calls();
            data.setup_uploading_expectations(&json_cli, &mut binary_cli);
            let (file, provider_sink) = data.file_to_upload_async();
            let bin_con = Rc::new(binary::Connection::new_mock(binary_cli));
            let json_con = Rc::new(language_server::Connection::new_mock(json_cli));

            Self {
                test:          TestWithLocalPoolExecutor::set_up(),
                chunks:        data.chunks.into_iter(),
                process:       FileUploadProcess::new(file, bin_con, json_con, data.path),
                provider_sink: Some(provider_sink),
            }
        }

        fn next_chunk_result(&mut self) -> FallibleResult<UploadingState> {
            let mut future = self.process.upload_chunk().boxed_local();

            if let Some(mut sink) = std::mem::take(&mut self.provider_sink) {
                // If the stream is still open, we shall wait for a new value
                self.test.run_until_stalled();
                future.expect_pending();

                if let Some(chunk) = self.chunks.next() {
                    sink.send(Ok(chunk)).boxed_local().expect_ok();
                    self.provider_sink = Some(sink);
                }
            }

            self.test.run_until_stalled();
            future.expect_ready()
        }
    }

    #[test]
    fn uploading_file() {
        let data = TestData::new(vec![vec![1, 2, 3, 4, 5], vec![3, 4, 5, 6, 7, 8]]);
        let mut test = UploadingFixture::new(data);

        assert_eq!(test.next_chunk_result().unwrap(), UploadingState::NotFinished);
        assert_eq!(test.next_chunk_result().unwrap(), UploadingState::NotFinished);
        assert_eq!(test.next_chunk_result().unwrap(), UploadingState::Finished);
    }

    #[test]
    fn checksum_mismatch_should_cause_an_error() {
        let mut data = TestData::new(vec![vec![1, 2, 3, 4, 5]]);
        data.checksum = Sha3_224::new(&[3, 4, 5, 6, 7, 8]);
        let mut test = UploadingFixture::new(data);

        assert_eq!(test.next_chunk_result().unwrap(), UploadingState::NotFinished);
        assert!(test.next_chunk_result().is_err());
    }


    // === NodeFromDroppedFileHandler Tests ===

    #[wasm_bindgen_test]
    fn creating_node_from_dropped_file() {
        let logger = Logger::new("test::creating_node_from_dropped_file");
        let data = TestData::new(vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8]]);
        let mut fixture = mock::Unified::new().fixture_customize(|_, json_rpc, binary_rpc| {
            json_rpc.expect.file_info(|path| {
                assert_eq!(*path, data_path());
                Ok(response::FileInfo { attributes: data_dir_attributes() })
            });
            json_rpc.expect.file_list(|path| {
                assert_eq!(*path, data_path());
                let other_file =
                    FileSystemObject::File { name: "other".to_owned(), path: data_path() };
                Ok(response::FileList { paths: vec![other_file] })
            });
            data.setup_uploading_expectations(json_rpc, binary_rpc);
        });

        let handler = NodeFromDroppedFileHandler::new(logger, fixture.project, fixture.graph);
        let position = model::module::Position::new(45.0, 70.0);
        let file = data.file_to_upload();
        handler.create_node_and_start_uploading(file, position).unwrap();
        assert_eq!(fixture.module.ast().repr(), module_code_uploading(TEST_FILE));
        fixture.executor.run_until_stalled();
        assert_eq!(fixture.module.ast().repr(), module_code_uploaded(TEST_FILE));
    }

    #[wasm_bindgen_test]
    fn recreating_data_directory() {
        let logger = Logger::new("test::recreating_data_directory");
        let mut fixture = mock::Unified::new().fixture_customize(|_, json_rpc, _| {
            json_rpc.expect.file_info(|path| {
                assert_eq!(*path, data_path());
                Err(RpcError::new_remote_error(code::FILE_NOT_FOUND, "FileNotFound"))
            });
            json_rpc.expect.create_file(|fs_object| {
                let root_path = Path::new_root(mock::data::ROOT_ID);
                assert!(matches!(fs_object,FileSystemObject::Directory {name,path}
                    if *path == root_path && name == DATA_DIR_NAME));
                Ok(())
            });
            json_rpc.expect.file_list(|path| {
                assert_eq!(*path, data_path());
                Ok(response::FileList { paths: vec![] })
            });
        });
        let handler = NodeFromDroppedFileHandler::new(logger, fixture.project, fixture.graph);
        fixture.executor.expect_completion(handler.ensure_data_directory_exists()).unwrap();
    }

    #[wasm_bindgen_test]
    fn name_collisions_are_avoided() {
        struct Case {
            file_name:            String,
            other_collision_name: String,
            expected_remote_name: String,
        }

        impl Case {
            fn new(
                file_name: impl Str,
                other_collision_name: impl Str,
                expected_remote_name: impl Str,
            ) -> Self {
                Case {
                    file_name:            file_name.into(),
                    other_collision_name: other_collision_name.into(),
                    expected_remote_name: expected_remote_name.into(),
                }
            }

            fn run(self) {
                let Case { file_name, other_collision_name, expected_remote_name } = self;

                let logger = Logger::new("test::creating_node_from_dropped_file");
                let data = TestData::new_with_file_name(
                    vec![vec![1, 2, 3, 4]],
                    file_name.clone(),
                    expected_remote_name.clone(),
                );
                let mut fixture =
                    mock::Unified::new().fixture_customize(|_, json_rpc, binary_rpc| {
                        json_rpc.expect.file_info(|path| {
                            assert_eq!(*path, Path::new(mock::data::ROOT_ID, &[DATA_DIR_NAME]));
                            Ok(response::FileInfo { attributes: data_dir_attributes() })
                        });
                        let file_name = file_name.clone();
                        json_rpc.expect.file_list(|path| {
                            assert_eq!(*path, Path::new(mock::data::ROOT_ID, &[DATA_DIR_NAME]));
                            use FileSystemObject::File;
                            let file_1 = File { name: file_name, path: data_path() };
                            let file_2 = File { name: other_collision_name, path: data_path() };
                            Ok(response::FileList { paths: vec![file_1, file_2] })
                        });
                        data.setup_uploading_expectations(json_rpc, binary_rpc);
                    });

                let project = fixture.project;
                let graph = fixture.graph;
                let handler = NodeFromDroppedFileHandler::new(logger, project, graph);
                let position = model::module::Position::new(45.0, 70.0);
                let (file, mut provider_sink) = data.file_to_upload_async();
                handler.create_node_and_start_uploading(file, position).unwrap();
                assert_eq!(fixture.module.ast().repr(), module_code_uploading(&file_name));
                fixture.executor.run_until_stalled();
                assert_eq!(
                    fixture.module.ast().repr(),
                    module_code_uploading(&expected_remote_name)
                );
                fixture
                    .executor
                    .expect_completion(provider_sink.send(Ok(vec![1, 2, 3, 4])))
                    .unwrap();
                drop(provider_sink);
                fixture.executor.run_until_stalled();
                assert_eq!(
                    fixture.module.ast().repr(),
                    module_code_uploaded(&expected_remote_name)
                );
            }
        }

        let without_extension = Case::new("test", "test_1", "test_2");
        let with_extension = Case::new("text.file.txt", "text.file_1.txt", "text.file_2.txt");
        let starts_with_dot = Case::new(".gitignore", ".gitignore_1", ".gitignore_2");
        for case in vec![without_extension, with_extension, starts_with_dot] {
            case.run()
        }
    }

    fn data_path() -> Path {
        Path::new(mock::data::ROOT_ID, &[DATA_DIR_NAME])
    }

    fn data_dir_attributes() -> FileAttributes {
        let dummy_time = UTCDateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00").unwrap();
        FileAttributes {
            creation_time:      dummy_time,
            last_access_time:   dummy_time,
            last_modified_time: dummy_time,
            kind:               FileSystemObject::Directory {
                name: DATA_DIR_NAME.to_owned(),
                path: Path::new_root(mock::data::ROOT_ID),
            },
            byte_size:          0,
        }
    }

    fn module_code_uploading(file_name: &str) -> String {
        format!(
            "{}\n    operator1 = File_Uploading.file_uploading Enso_Project.data/\"{}\"",
            mock::data::CODE,
            file_name
        )
    }

    fn module_code_uploaded(file_name: &str) -> String {
        format!(
            "{}\n    operator1 = File.read Enso_Project.data/\"{}\"",
            mock::data::CODE,
            file_name
        )
    }
}
