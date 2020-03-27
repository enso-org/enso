//! Graph Controller.
//!
//! This controller provides access to a specific graph. It lives under a module controller, as
//! each graph belongs to some module.


use crate::prelude::*;

pub use crate::double_representation::graph::Id;
pub use crate::double_representation::graph::LocationHint;
use crate::controller::module::NodeMetadata;
use crate::double_representation::graph::GraphInfo;
use crate::double_representation::definition;
use crate::double_representation::node;

use flo_stream::MessagePublisher;
use flo_stream::Subscriber;
use utils::channel::process_stream_with_handle;
use parser::api::IsParser;



// ==============
// === Errors ===
// ==============

/// Error raised when node with given Id was not found in the graph's body.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Node with Id {} was not found.", _0)]
pub struct NodeNotFound(ast::Id);

/// Error raised when an attempt to set node's expression to a binding has been made.
#[derive(Clone,Debug,Fail)]
#[fail(display="Illegal string `{}` given for node expression. It must not be a binding.", _0)]
pub struct BindingExpressionNotAllowed(String);

/// Expression AST cannot be used to produce a node. Means a bug in parser and id-giving code.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Internal error: failed to create a new node.")]
pub struct FailedToCreateNode;



// ============
// === Node ===
// ============

/// Description of the node with all information available to the graph controller.
#[derive(Clone,Debug)]
pub struct Node {
    /// Information based on AST, from double_representation module.
    pub info : double_representation::node::NodeInfo,
    /// Information about this node stored in the module's metadata.
    pub metadata : Option<NodeMetadata>,
}



// ===================
// === NewNodeInfo ===
// ===================

/// Describes the node to be added.
#[derive(Clone,Debug)]
pub struct NewNodeInfo {
    /// Expression to be placed on the node
    pub expression : String,
    /// Visual node position in the graph scene.
    pub metadata : Option<NodeMetadata>,
    /// ID to be given to the node.
    pub id : Option<ast::Id>,
    /// Where line created by adding this node should appear.
    pub location_hint : LocationHint
}

impl NewNodeInfo {
    /// New node with given expression added at the end of the graph's blocks.
    pub fn new_pushed_back(expression:impl Str) -> NewNodeInfo {
        NewNodeInfo {
            expression    : expression.into(),
            metadata      : default(),
            id            : default(),
            location_hint : LocationHint::End,
        }
    }
}



// ==================
// === Controller ===
// ==================

/// Handle providing graph controller interface.
#[derive(Clone,Debug)]
pub struct Handle {
    /// Controller of the module which this graph belongs to.
    module    : controller::module::Handle,
    id        : Id,
    /// Publisher. When creating a controller, it sets up task to emit notifications through this
    /// publisher to relay changes from the module controller.
    // TODO: [mwu] Remove in favor of streams mapping over centralized module-scope publisher
    publisher : Rc<RefCell<controller::notification::Publisher<controller::notification::Graph>>>,
    logger : Logger
}

impl Handle {
    /// Gets a handle to a controller of the module that this definition belongs to.
    pub fn module(&self) -> controller::module::Handle {
        self.module.clone_ref()
    }

    /// Gets a handle to a controller of the module that this definition belongs to.
    pub fn id(&self) -> Id {
        self.id.clone()
    }

    /// Creates a new controller. Does not check if id is valid.
    ///
    /// Requires global executor to spawn the events relay task.
    pub fn new_unchecked(module:controller::module::Handle, id:Id) -> Handle {
        let graphs_notifications = module.subscribe_graph_notifications();
        let publisher = default();
        let logger    = Logger::new(format!("Graph Controller {}", id));
        let ret       = Handle {module,id,publisher,logger};
        let weak      = Rc::downgrade(&ret.publisher);
        let relay_notifications = process_stream_with_handle(graphs_notifications,weak,
            |notification,this| {
                match notification {
                    controller::notification::Graphs::Invalidate =>
                    this.borrow_mut().publish(controller::notification::Graph::Invalidate),
            }
        });
        executor::global::spawn(relay_notifications);
        ret
    }

    /// Creates a new graph controller. Given ID should uniquely identify a definition in the
    /// module. Fails if ID cannot be resolved.
    ///
    /// Requires global executor to spawn the events relay task.
    pub fn new(module:controller::module::Handle, id:Id) -> FallibleResult<Handle> {
        let ret = Self::new_unchecked(module,id);
        // Get and discard definition info, we are just making sure it can be obtained.
        let _ = ret.graph_definition_info()?;
        Ok(ret)
    }

    /// Retrieves double rep information about definition providing this graph.
    pub fn graph_definition_info
    (&self) -> FallibleResult<double_representation::definition::DefinitionInfo> {
        let module = self.module();
        let id     = self.id();
        module.find_definition(&id)
    }

    /// Returns double rep information about all nodes in the graph.
    pub fn all_node_infos
    (&self) -> FallibleResult<Vec<double_representation::node::NodeInfo>> {
        let definition = self.graph_definition_info()?;
        let graph      = double_representation::graph::GraphInfo::from_definition(definition);
        Ok(graph.nodes())
    }

    /// Retrieves double rep information about node with given ID.
    pub fn node_info
    (&self, id:ast::Id) -> FallibleResult<double_representation::node::NodeInfo> {
        let nodes = self.all_node_infos()?;
        let node  = nodes.into_iter().find(|node_info| node_info.id() == id);
        node.ok_or_else(|| NodeNotFound(id).into())
    }

    /// Gets information about node with given id.
    ///
    /// Note that it is more efficient to use `get_nodes` to obtain all information at once,
    /// rather then repeatedly call this method.
    pub fn node(&self, id:ast::Id) -> FallibleResult<Node> {
        let info     = self.node_info(id)?;
        let metadata = self.node_metadata(id).ok();
        Ok(Node {info,metadata})
    }

    /// Returns information about all the nodes currently present in this graph.
    pub fn nodes(&self) -> FallibleResult<Vec<Node>> {
        let node_infos = self.all_node_infos()?;
        let mut nodes  = Vec::new();
        for info in node_infos {
            let metadata = self.node_metadata(info.id()).ok();
            nodes.push(Node {info,metadata})
        }
        Ok(nodes)
    }

    /// Updates the AST of the definition of this graph.
    pub fn update_definition_ast<F>(&self, f:F) -> FallibleResult<()>
    where F:FnOnce(definition::DefinitionInfo) -> FallibleResult<definition::DefinitionInfo> {
        let ast_so_far     = self.module.ast()?;
        let definition     = definition::locate(&ast_so_far, &self.id)?;
        let new_definition = f(definition.item)?;
        trace!(self.logger, "Applying graph changes onto definition");
        let new_ast    = new_definition.ast.into();
        let new_module = ast_so_far.set_traversing(&definition.crumbs,new_ast)?;
        self.module.update_ast(new_module);
        Ok(())
    }

    /// Parses given text as a node expression.
    pub fn parse_node_expression
    (&self, expression_text:impl Str) -> FallibleResult<Ast> {
        let mut parser    = self.module.parser();
        let node_ast      = parser.parse_line(expression_text.as_ref())?;
        if ast::opr::is_assignment(&node_ast) {
            Err(BindingExpressionNotAllowed(expression_text.into()).into())
        } else {
            Ok(node_ast)
        }
    }

    /// Adds a new node to the graph and returns information about created node.
    pub fn add_node(&self, node:NewNodeInfo) -> FallibleResult<ast::Id> {
        trace!(self.logger, "Adding node with expression `{node.expression}`");
        let ast           = self.parse_node_expression(&node.expression)?;
        let mut node_info = node::NodeInfo::from_line_ast(&ast).ok_or(FailedToCreateNode)?;
        if let Some(desired_id) = node.id {
            node_info.set_id(desired_id)
        }

        self.update_definition_ast(|definition| {
            let mut graph = GraphInfo::from_definition(definition);
            let node_ast  = node_info.ast().clone();
            graph.add_node(node_ast,node.location_hint)?;
            Ok(graph.source)
        })?;

        if let Some(initial_metadata) = node.metadata {
            self.with_node_metadata(node_info.id(),|metadata| {
                *metadata = initial_metadata;
            })
        }

        Ok(node_info.id())
    }

    /// Removes the node with given Id.
    pub fn remove_node(&self, id:ast::Id) -> FallibleResult<()> {
        trace!(self.logger, "Removing node {id}");
        self.update_definition_ast(|definition| {
            let mut graph = GraphInfo::from_definition(definition);
            graph.remove_node(id)?;
            Ok(graph.source)
        })?;

        // It's fine if there were no metadata.
        let _ = self.module().pop_node_metadata(id);
        Ok(())
    }

    /// Sets the given's node expression.
    pub fn set_expression(&self, id:ast::Id, expression_text:impl Str) -> FallibleResult<()> {
        trace!(self.logger, "Setting node {id} expression to `{expression_text.as_ref()}`");
        let new_expression_ast = self.parse_node_expression(expression_text)?;
        self.update_definition_ast(|definition| {
            let mut graph = GraphInfo::from_definition(definition);
            graph.edit_node(id,new_expression_ast)?;
            Ok(graph.source)
        })?;
        Ok(())
    }

    /// Subscribe to updates about changes in this graph.
    pub fn subscribe(&self) -> Subscriber<controller::notification::Graph> {
        self.publisher.borrow_mut().0.subscribe()
    }

    /// Retrieves metadata for the given node.
    pub fn node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata> {
        self.module().node_metadata(id)
    }

    /// Modify metadata of given node.
    /// If ID doesn't have metadata, empty (default) metadata is inserted.
    pub fn with_node_metadata(&self, id:ast::Id, fun:impl FnOnce(&mut NodeMetadata)) {
        let     module = self.module();
        let mut data   = module.pop_node_metadata(id).unwrap_or_default();
        fun(&mut data);
        module.set_node_metadata(id,data);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::double_representation::definition::DefinitionName;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::controller::module;
    use crate::controller::graph;
    use crate::controller::notification;

    use ast::HasRepr;
    use data::text::Index;
    use data::text::TextChange;
    use json_rpc::test_util::transport::mock::MockTransport;
    use parser::Parser;
    use utils::test::ExpectTuple;
    use wasm_bindgen_test::wasm_bindgen_test;
    use ast::test_utils::expect_shape;

    struct GraphControllerFixture(TestWithLocalPoolExecutor);
    impl GraphControllerFixture {
        pub fn set_up() -> GraphControllerFixture {
            let nested = TestWithLocalPoolExecutor::set_up();
            Self(nested)
        }

        pub fn run_graph_for_main<Test,Fut>(&mut self, code:impl Str, function_name:impl Str, test:Test)
        where Test : FnOnce(module::Handle,Handle) -> Fut + 'static,
              Fut  : Future<Output=()> {
            let fm       = file_manager_client::Handle::new(MockTransport::new());
            let loc      = controller::module::Location("Main".to_string());
            let parser   = Parser::new_or_panic();
            let module   = controller::module::Handle::new_mock(loc,code.as_ref(),default(),fm,parser).unwrap();
            let graph_id = Id::new_single_crumb(DefinitionName::new_plain(function_name.into()));
            let graph    = module.get_graph_controller(graph_id).unwrap();
            self.0.run_test(async move {
                test(module,graph).await
            })
        }

        pub fn run_graph_for<Test,Fut>(&mut self, code:impl Str, graph_id:Id, test:Test)
            where Test : FnOnce(module::Handle,Handle) -> Fut + 'static,
                  Fut  : Future<Output=()> {
            let fm       = file_manager_client::Handle::new(MockTransport::new());
            let loc      = controller::module::Location("Main".to_string());
            let parser   = Parser::new_or_panic();
            let module   = controller::module::Handle::new_mock(loc,code.as_ref(),default(),fm,parser).unwrap();
            let graph    = module.get_graph_controller(graph_id).unwrap();
            self.0.run_test(async move {
                test(module,graph).await
            })
        }

        pub fn run_inline_graph<Test,Fut>(&mut self, definition_body:impl Str, test:Test)
        where Test : FnOnce(module::Handle,Handle) -> Fut + 'static,
              Fut  : Future<Output=()> {
            assert_eq!(definition_body.as_ref().contains('\n'), false);
            let code = format!("main = {}", definition_body.as_ref());
            let name = "main";
            self.run_graph_for_main(code, name, test)
        }
    }

    #[wasm_bindgen_test]
    fn node_operations() {
        TestWithLocalPoolExecutor::set_up().run_test(async {
            let transport    = MockTransport::new();
            let file_manager = file_manager_client::Handle::new(transport);
            let parser       = Parser::new().unwrap();
            let location     = module::Location("Test".to_string());
            let code         = "main = Hello World";
            let idmap        = default();
            let module       = module::Handle::new_mock
                (location,code,idmap,file_manager,parser).unwrap();
            let pos          = module::Position {vector:Vector2::new(0.0,0.0)};
            let crumbs       = vec![DefinitionName::new_plain("main")];
            let graph        = graph::Handle::new(module, Id {crumbs}).unwrap();

            let uid          = graph.all_node_infos().unwrap()[0].id();

            graph.with_node_metadata(uid, |data| data.position = Some(pos));

            assert_eq!(graph.node_metadata(uid).unwrap().position, Some(pos));
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_notification_relay() {
        let mut test = GraphControllerFixture::set_up();
        test.run_graph_for_main("main = 2 + 2", "main", |module, graph| async move {
            let text_change = TextChange::insert(Index::new(12), "2".into());
            module.apply_code_change(&text_change).unwrap();

            let mut sub = graph.subscribe();
            module.apply_code_change(&TextChange::insert(Index::new(1),"2".to_string())).unwrap();
            assert_eq!(Some(notification::Graph::Invalidate), sub.next().await);
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_inline_definition() {
        let mut test = GraphControllerFixture::set_up();
        const EXPRESSION: &str = "2+2";
        test.run_inline_graph(EXPRESSION, |_,graph| async move {
            let nodes   = graph.nodes().unwrap();
            let (node,) = nodes.expect_tuple();
            assert_eq!(node.info.expression().repr(), EXPRESSION);
            let id   = node.info.id();
            let node = graph.node(id).unwrap();
            assert_eq!(node.info.expression().repr(), EXPRESSION);
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_block_definition() {
        let mut test  = GraphControllerFixture::set_up();
        let program = r"
main =
    foo = 2
    print foo";
        test.run_graph_for_main(program, "main", |_, graph| async move {
            let nodes   = graph.nodes().unwrap();
            let (node1,node2) = nodes.expect_tuple();
            assert_eq!(node1.info.expression().repr(), "2");
            assert_eq!(node2.info.expression().repr(), "print foo");
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_parse_expression() {
        let mut test  = GraphControllerFixture::set_up();
        let program = r"main = 0";
        test.run_graph_for_main(program, "main", |_, graph| async move {
            let foo = graph.parse_node_expression("foo").unwrap();
            assert_eq!(expect_shape::<ast::Var>(&foo), &ast::Var {name:"foo".into()});

            assert!(graph.parse_node_expression("Vec").is_ok());
            assert!(graph.parse_node_expression("5").is_ok());
            assert!(graph.parse_node_expression("5+5").is_ok());
            assert!(graph.parse_node_expression("a+5").is_ok());
            assert!(graph.parse_node_expression("a=5").is_err());
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_nested_definition() {
        let mut test  = GraphControllerFixture::set_up();
        const PROGRAM:&str = r"main =
    foo a =
        bar b = 5
    print foo";
        let definition = definition::Id::new_plain_names(vec!["main","foo"]);
        test.run_graph_for(PROGRAM, definition, |module, graph| async move {
            let expression = "new_node";
            graph.add_node(NewNodeInfo::new_pushed_back(expression)).unwrap();
            let expected_program = r"main =
    foo a =
        bar b = 5
        new_node
    print foo";
            module.expect_code(expected_program);
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_doubly_nested_definition() {
        // Tests editing nested definition that requires transforming inline expression into
        // into a new block.
        let mut test  = GraphControllerFixture::set_up();
        // Not using multi-line raw string literals, as we don't want IntelliJ to automatically
        // strip the trailing whitespace in the lines.
        const PROGRAM:&str = "main =\n    foo a =\n        bar b = 5\n    print foo";
        let definition = definition::Id::new_plain_names(vec!["main","foo","bar"]);
        test.run_graph_for(PROGRAM, definition, |module, graph| async move {
            let expression = "new_node";
            graph.add_node(NewNodeInfo::new_pushed_back(expression)).unwrap();
            let expected_program = "main =\n    foo a =\n        bar b = \
                                    \n            5\n            new_node\n    print foo";
            module.expect_code(expected_program);
        })
    }

    #[wasm_bindgen_test]
    fn graph_controller_node_operations_node() {
        let mut test  = GraphControllerFixture::set_up();
        const PROGRAM:&str = r"
main =
    foo = 2
    print foo";
        test.run_graph_for_main(PROGRAM, "main", |module, graph| async move {
            // === Initial nodes ===
            let nodes   = graph.nodes().unwrap();
            let (node1,node2) = nodes.expect_tuple();
            assert_eq!(node1.info.expression().repr(), "2");
            assert_eq!(node2.info.expression().repr(), "print foo");


            // === Add node ===
            let id       = ast::Id::new_v4();
            let position = Some(controller::module::Position::new(10.0,20.0));
            let metadata = NodeMetadata {position};
            let info     = NewNodeInfo {
                expression    : "a+b".into(),
                metadata      : Some(metadata),
                id            : Some(id),
                location_hint : LocationHint::End,
            };
            graph.add_node(info).unwrap();
            let expected_program = r"
main =
    foo = 2
    print foo
    a+b";
            assert_eq!(module.code(), expected_program);
            let nodes = graph.nodes().unwrap();
            let (_,_,node3) = nodes.expect_tuple();
            assert_eq!(node3.info.id(),id);
            assert_eq!(node3.info.expression().repr(), "a+b");
            let pos = node3.metadata.unwrap().position;
            assert_eq!(pos, position);
            assert!(graph.node_metadata(id).is_ok());


            // === Edit node ===
            graph.set_expression(id, "bar baz").unwrap();
            let (_,_,node3) = graph.nodes().unwrap().expect_tuple();
            assert_eq!(node3.info.id(),id);
            assert_eq!(node3.info.expression().repr(), "bar baz");
            assert_eq!(node3.metadata.unwrap().position, position);


            // === Remove node ===
            graph.remove_node(node3.info.id()).unwrap();
            let nodes = graph.nodes().unwrap();
            let (node1,node2) = nodes.expect_tuple();
            assert_eq!(node1.info.expression().repr(), "2");
            assert_eq!(node2.info.expression().repr(), "print foo");
            assert!(graph.node_metadata(id).is_err());

            assert_eq!(module.code(), PROGRAM);
        })
    }
}
