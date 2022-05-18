//! Support for IDE Undo-Redo functionality.

use crate::prelude::*;

use crate::controller;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, Fail, PartialEq)]
#[fail(display = "Cannot undo because there is an ongoing transaction '{}'.", transaction_name)]
pub struct CannotUndoDuringTransaction {
    transaction_name: String,
}

#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Eq, Fail, PartialEq)]
#[fail(display = "There is no action stored that can be undone.")]
pub struct NoActionToUndo;

#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Eq, Fail, PartialEq)]
#[fail(display = "The faux undo transaction was leaked.")]
pub struct FauxTransactionLeaked;

#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Eq, Fail, PartialEq)]
#[fail(display = "No frame to pop, {} stack is empty.", _0)]
pub struct NoFrameToPop(Stack);

#[allow(missing_docs)]
#[derive(Debug, Clone, Eq, Fail, PartialEq)]
#[fail(display = "The module {} is not accessible.", _0)]
pub struct MissingModuleHandle(model::module::Id);



// ==============
// === Traits ===
// ==============

/// Trait represents undo-aware type that is able to access undo-redo repository.
///
/// It allows to open transactions and check state of the repository.
/// It does not allow however to execute undo/redo itself, this is done through [`Manager`].
pub trait Aware {
    /// Get handle to undo-redo [`Repository`].
    fn undo_redo_repository(&self) -> Rc<Repository>;

    /// Get current ongoing transaction. If there is no ongoing transaction, create a one.
    #[profile(Debug)]
    #[must_use]
    fn get_or_open_transaction(&self, name: &str) -> Rc<Transaction> {
        self.undo_redo_repository().transaction(name)
    }
}



// ===================
// === Transaction ===
// ===================

/// Transaction is a RAII-style object used to group a number of actions into a single undoable
/// operation.
///
/// When the transaction is dropped, it adds itself to the undo stack, unless it was ignored.
#[derive(Debug)]
pub struct Transaction {
    #[allow(missing_docs)]
    pub logger: Logger,
    frame:      RefCell<Frame>,
    urm:        Weak<Repository>,
    ignored:    Cell<bool>,
}

impl Transaction {
    /// Create a new transaction, that will add to the given's repository undo stack on destruction.
    pub fn new(urm: &Rc<Repository>, name: String) -> Self {
        Self {
            logger:  Logger::new_sub(&urm.logger, "Transaction"),
            frame:   RefCell::new(Frame { name, ..default() }),
            urm:     Rc::downgrade(urm),
            ignored: default(),
        }
    }

    /// Get the transaction name.
    ///
    /// Currently the name serves only debugging purposes.
    pub fn name(&self) -> String {
        self.frame.borrow().name.clone()
    }

    /// Stores the state of given module.
    ///
    /// This is the state that will be restored, when the transaction is undone. As such is should
    /// be the state "from before" the undoable action.
    ///
    /// This method stores content only once for given module. Thus it is safe to call this on
    /// the current transaction in context where it is not clear whether transaction was already set
    /// up or not.
    pub fn fill_content(&self, id: model::module::Id, content: model::module::Content) {
        with(self.frame.borrow_mut(), |mut data| {
            debug!(
                self.logger,
                "Filling transaction '{data.name}' with snapshot of module '{id}':\
            \n{content}"
            );
            if data.snapshots.try_insert(id, content).is_err() {
                debug!(self.logger, "Skipping this snapshot, as module's state was already saved.")
            }
        })
    }

    /// Ignore the transaction.
    ///
    /// Ignored transaction when dropped is discarded, rather than being put on top of "Redo" stack.
    /// It does not affect the actions belonging to transaction in any way.
    pub fn ignore(&self) {
        debug!(self.logger, "Marking transaction '{self.frame.borrow().name}' as ignored.");
        self.ignored.set(true)
    }
}

impl Drop for Transaction {
    fn drop(&mut self) {
        if let Some(urm) = self.urm.upgrade() {
            if !self.ignored.get() {
                info!(self.logger, "Transaction '{self.name()}' will create a new frame.");
                urm.push_to(Stack::Undo, self.frame.borrow().clone());
                urm.clear(Stack::Redo);
            } else {
                info!(
                    self.logger,
                    "Dropping the ignored transaction '{self.name()}' without \
                pushing a frame to repository."
                )
            }
        }
    }
}



// =============
// === Frame ===
// =============

/// Frame represents a state stored on undo or redo stack.
///
/// [`Manager`] is able to restore project's state to a given `Frame`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Frame {
    /// Name of the transaction that created this frame.
    pub name:      String,
    /// Context module where the change was made.
    pub module:    Option<model::module::Id>,
    /// Context graph where the change was made.
    pub graph:     Option<controller::graph::Id>,
    /// Snapshots of content for all edited modules.
    pub snapshots: BTreeMap<model::module::Id, model::module::Content>,
}

impl Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name: {}; ", self.name)?;
        if let Some(m) = &self.module {
            write!(f, "Module: {}; ", m)?;
        }
        if let Some(g) = &self.graph {
            write!(f, "Graph: {}; ", g)?;
        }
        for (id, code) in &self.snapshots {
            write!(f, "Code for {}: {}; ", id, code)?;
        }
        Ok(())
    }
}



// ==================
// === Repository ===
// ==================

/// Identifies a stack in Undo-Redo repository.
#[derive(Clone, Copy, Debug, Display, Ord, PartialOrd, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum Stack {
    Undo,
    Redo,
}

/// The inner state of the Und-Redo repository.
#[derive(Debug, Default)]
pub struct Data {
    /// Undo stack.
    pub undo:                Vec<Frame>,
    /// Redo stack.
    pub redo:                Vec<Frame>,
    /// Currently open transaction (if `Some` and alive).
    pub current_transaction: Option<Weak<Transaction>>,
}

/// `Repository` stores undo and redo stacks and provides transaction support.
///
/// This is the primary type meant to be exposed to entities that want their actions to be
/// undoable. They can group edits together by keeping a [`Transaction`] handle alive.
///
/// `Repository`, unlike [`Manager`] does not keep any modules (or other model entities) alive and
/// can be shared with no consequence on project state.
#[derive(Debug)]
pub struct Repository {
    logger: Logger,
    data:   RefCell<Data>,
}


impl Default for Repository {
    fn default() -> Self {
        Self::new(Logger::new(""))
    }
}

impl Repository {
    /// Create a new repository.
    pub fn new(parent: impl AnyLogger) -> Self {
        Self { logger: Logger::new_sub(parent, "Repository"), data: default() }
    }

    /// Get the currently open transaction. [`None`] if there is none.
    pub fn current_transaction(&self) -> Option<Rc<Transaction>> {
        self.data.borrow().current_transaction.as_ref().and_then(Weak::upgrade)
    }

    /// Open a new transaction.
    ///
    /// If there is already an opened transaction, it will be returned as [`Err`].
    pub fn open_transaction(
        self: &Rc<Self>,
        name: impl Into<String>,
    ) -> Result<Rc<Transaction>, Rc<Transaction>> {
        if let Some(ongoing_transaction) = self.current_transaction() {
            Err(ongoing_transaction)
        } else {
            let name = name.into();
            debug!(self.logger, "Creating a new transaction `{name}`");
            let new_transaction = Rc::new(Transaction::new(self, name));
            self.data.borrow_mut().current_transaction = Some(Rc::downgrade(&new_transaction));
            Ok(new_transaction)
        }
    }

    /// Open an ignored transaction
    ///
    /// This function should be used when we want to do some changes in module which should not be
    /// tracked in undo redo (when they are not a result of user activity). If the transaction is
    /// already opened, it will **not** be ignored, and will be returned in [`Err`] variant.
    ///
    /// See also [`Repository::open_transaction`], [`Transaction::ignore`].
    pub fn open_ignored_transaction(
        self: &Rc<Self>,
        name: impl Into<String>,
    ) -> Result<Rc<Transaction>, Rc<Transaction>> {
        let transaction = self.open_transaction(name);
        if let Ok(new) = &transaction {
            new.ignore();
        }
        transaction
    }


    /// Get currently opened transaction. If there is none, open a new one.
    pub fn transaction(self: &Rc<Self>, name: impl Into<String>) -> Rc<Transaction> {
        self.open_transaction(name).into_ok_or_err()
    }

    /// Borrow given stack.
    fn borrow(&self, stack: Stack) -> Ref<Vec<Frame>> {
        let data = self.data.borrow();
        match stack {
            Stack::Undo => Ref::map(data, |d| &d.undo),
            Stack::Redo => Ref::map(data, |d| &d.redo),
        }
    }

    /// Borrow given stack mutably.
    fn borrow_mut(&self, stack: Stack) -> RefMut<Vec<Frame>> {
        let data = self.data.borrow_mut();
        match stack {
            Stack::Undo => RefMut::map(data, |d| &mut d.undo),
            Stack::Redo => RefMut::map(data, |d| &mut d.redo),
        }
    }

    /// Push a new frame to the given stack.
    fn push_to(&self, stack: Stack, frame: Frame) {
        debug!(self.logger, "Pushing to {stack} stack a new frame: {frame}");
        self.borrow_mut(stack).push(frame);
    }

    /// Clear all frames from the given stack.
    fn clear(&self, stack: Stack) {
        debug!(self.logger, "Clearing {stack} stack.");
        self.borrow_mut(stack).clear();
    }

    /// Clear all frames from both undo and redo stacks.
    pub fn clear_all(&self) {
        for stack in [Stack::Undo, Stack::Redo] {
            self.clear(stack)
        }
    }

    /// Get the top frame from a given stack. [`Err`] if the stack is empty.
    ///
    /// Does *not* pop.
    pub fn last(&self, stack: Stack) -> FallibleResult<Frame> {
        self.borrow(stack).last().cloned().ok_or_else(|| NoActionToUndo.into())
    }

    /// Pop the top frame from a given stack. [`Err`] if there are no frames to pop.
    fn pop(&self, stack: Stack) -> FallibleResult<Frame> {
        let frame = self.borrow_mut(stack).pop().ok_or(NoFrameToPop(stack))?;
        debug!(
            self.logger,
            "Popping a frame from {stack}. Remaining length: {self.len(stack)}. \
        Frame: {frame}"
        );
        Ok(frame)
    }

    /// Get number of frames on a given stack.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self, stack: Stack) -> usize {
        self.borrow(stack).len()
    }
}



// ===============
// === Manager ===
// ===============

/// Undo-Redo manager. Allows undoing or redoing recent actions.
///
/// Owns [`Repository`] and keeps track of open modules.
#[derive(Debug)]
pub struct Manager {
    #[allow(missing_docs)]
    pub logger:     Logger,
    /// Repository with undo and redo stacks.
    pub repository: Rc<Repository>,
    /// Currently available modules.
    modules:        RefCell<BTreeMap<model::module::Id, model::Module>>,
}

impl Aware for Manager {
    fn undo_redo_repository(&self) -> Rc<Repository> {
        self.repository.clone()
    }
}

impl Manager {
    /// Create a new undo-redo manager.
    pub fn new(parent: impl AnyLogger) -> Self {
        let logger = Logger::new_sub(parent, "URM");
        Self { repository: Rc::new(Repository::new(&logger)), modules: default(), logger }
    }

    /// Register a new opened module in the manager.
    ///
    /// Only a modules registered as open can be subject of undo-redo operations.
    pub fn module_opened(&self, module: model::Module) {
        self.modules.borrow_mut().insert(module.id(), module);
    }

    /// Unregisters a previously opened module.
    pub fn module_closed(&self, module: model::Module) {
        self.modules.borrow_mut().remove(&module.id());
    }

    /// Undo last operation.
    pub fn undo(&self) -> FallibleResult {
        debug!(self.logger, "Undo requested, stack size is {self.repository.len(Stack::Undo)}.");
        let frame = self.repository.last(Stack::Undo)?;

        // Before applying undo we create a special transaction. The purpose it two-fold:
        // 1) We want to prevent any undo attempt if there is already an ongoing transaction;
        // 2) We want to make sure that any of undo consequences won't create a new transaction,
        //    leading to a situation when undoing would re-add itself onto the undo stack.
        // We mark transaction as ignored right after creating, as it is never intended to create a
        // new undo frame. Instead, frame will be pushed to the redo stack manually.
        let undo_transaction = self.repository.open_transaction("Undo faux transaction").map_err(
            |ongoing_transaction| {
                let transaction_name = ongoing_transaction.name();
                CannotUndoDuringTransaction { transaction_name }
            },
        )?;
        undo_transaction.ignore();
        self.reset_to(&frame)?;
        let popped = self.repository.pop(Stack::Undo);

        // Sanity check the we popped the same frame as we have just undone. What was on top is
        // supposed to stay on top, as we maintain an open transaction while undoing.
        if !popped.contains(&frame) {
            // No reason to stop the world but should catch our eye in logs.
            error!(self.logger, "Undone frame mismatch!");
            debug_assert!(false, "Undone frame mismatch!");
        }

        let undo_transaction =
            Rc::try_unwrap(undo_transaction).map_err(|_| FauxTransactionLeaked)?;
        self.repository.data.borrow_mut().redo.push(undo_transaction.frame.borrow().clone());
        Ok(())
    }

    /// Redo the last undone operation.
    pub fn redo(&self) -> FallibleResult {
        let frame = self.repository.data.borrow_mut().redo.pop().ok_or(NoActionToUndo)?;
        let redo_transaction = self.get_or_open_transaction(&frame.name);
        redo_transaction.ignore();
        self.reset_to(&frame)?;
        self.repository.push_to(Stack::Undo, redo_transaction.frame.borrow().clone());
        Ok(())
    }

    /// Restore all modules affected by the [`Frame`] to their stored state.
    fn reset_to(&self, frame: &Frame) -> FallibleResult {
        info!(self.logger, "Resetting to initial state on frame {frame}");

        // First we must have all modules resolved. Only then we can start applying changes.
        // Otherwise, if one of the modules could not be retrieved, we'd risk ending up with
        // a partially undone operation and inconsistent state.
        //
        // In general this should never happen, as we store strong references to all opened modules
        // and don't allow getting snapshots of modules that are not opened.
        let module_and_content = with(self.modules.borrow(), |modules| {
            frame
                .snapshots
                .iter()
                .map(|(id, content)| -> FallibleResult<_> {
                    let err = || MissingModuleHandle(id.clone());
                    let module = modules.get(id).cloned().ok_or_else(err)?;
                    Ok((module, content.clone()))
                })
                .collect::<FallibleResult<Vec<_>>>()
        })?;

        for (module, content) in module_and_content {
            info!(self.logger, "Undoing on module {module.path()}");
            // The below should never fail, because it can fail only if serialization to code fails.
            // And it cannot fail, as it already underwent this procedure successfully in the past
            // (we are copying an old state, so it must ba a representable state).
            module.update_whole(content.clone())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    //use utils::test::traits::*;
    use super::*;
    use crate::test::mock::Fixture;
    use crate::test::mock::Unified;
    use span_tree::SpanTree;

    fn check_atomic_undo(fixture: &Fixture, action: impl FnOnce()) {
        let Fixture { project, module, .. } = &fixture;
        let urm = project.urm();

        assert_eq!(urm.repository.len(Stack::Undo), 0);
        assert_eq!(urm.repository.len(Stack::Redo), 0);

        let before_action = module.serialized_content().unwrap();
        action();
        let after_action = module.serialized_content().unwrap();

        assert_eq!(urm.repository.len(Stack::Undo), 1);
        assert_eq!(urm.repository.len(Stack::Redo), 0);

        // After single undo everything should be as before.
        urm.undo().unwrap();
        assert_eq!(module.serialized_content().unwrap(), before_action);
        assert_eq!(urm.repository.len(Stack::Undo), 0);
        assert_eq!(urm.repository.len(Stack::Redo), 1);

        // After redo - as right after connecting.
        urm.redo().unwrap();
        assert_eq!(module.serialized_content().unwrap(), after_action);
        assert_eq!(urm.repository.len(Stack::Undo), 1);
        assert_eq!(urm.repository.len(Stack::Redo), 0);
    }

    fn check_atomic_graph_action(code: &str, action: impl FnOnce(&controller::graph::Handle)) {
        let mut data = Unified::new();
        data.set_code(code);

        let fixture = data.fixture();
        let graph = &fixture.graph;
        check_atomic_undo(&fixture, move || action(graph));
    }

    // Collapse two middle nodes.
    #[wasm_bindgen_test]
    fn collapse_nodes_atomic() {
        let code = r#"
main =
    foo = 2
    bar = foo + 6
    baz = 2 + foo + bar
    caz = baz / 2 * baz
"#;
        check_atomic_graph_action(code, |graph| {
            let nodes = graph.nodes().unwrap();
            assert_eq!(nodes.len(), 4);
            graph.collapse(vec![nodes[1].id(), nodes[2].id()], "extracted").unwrap();
        });
    }

    // A complex operation: involves introducing variable name, reordering lines and
    // replacing an argument.
    #[wasm_bindgen_test]
    fn connect_nodes_atomic() {
        let code = r#"
main =
    2 + 2
    5 * 5
"#;
        check_atomic_graph_action(code, |graph| {
            let nodes = graph.nodes().unwrap();
            let sum_node = &nodes[0];
            let product_node = &nodes[1];

            assert_eq!(sum_node.expression().to_string(), "2 + 2");
            assert_eq!(product_node.expression().to_string(), "5 * 5");

            let sum_tree = SpanTree::<()>::new(sum_node.expression(), graph).unwrap();
            let sum_input =
                sum_tree.root_ref().leaf_iter().find(|n| n.is_argument()).unwrap().crumbs;
            let connection = controller::graph::Connection {
                source:      controller::graph::Endpoint::new(product_node.id(), []),
                destination: controller::graph::Endpoint::new(sum_node.id(), sum_input),
            };

            graph.connect(&connection, &span_tree::generate::context::Empty).unwrap();
        });
    }


    // Check that node position is properly updated.
    #[wasm_bindgen_test]
    fn move_node() {
        use model::module::Position;

        let mut fixture = crate::test::mock::Unified::new().fixture();
        let Fixture { executed_graph, graph, project, logger, .. } = &mut fixture;
        let logger: &Logger = logger;

        let urm = project.urm();
        let nodes = executed_graph.graph().nodes().unwrap();
        let node = &nodes[0];

        debug!(logger, "{node.position():?}");
        let pos1 = Position::new(500.0, 250.0);
        let pos2 = Position::new(300.0, 150.0);

        graph.set_node_position(node.id(), pos1).unwrap();
        graph.set_node_position(node.id(), pos2).unwrap();

        assert_eq!(graph.node(node.id()).unwrap().position(), Some(pos2));
        urm.undo().unwrap();
        assert_eq!(graph.node(node.id()).unwrap().position(), Some(pos1));
        urm.undo().unwrap();
        assert_eq!(graph.node(node.id()).unwrap().position(), None);
        urm.redo().unwrap();
        assert_eq!(graph.node(node.id()).unwrap().position(), Some(pos1));
        urm.redo().unwrap();
        assert_eq!(graph.node(node.id()).unwrap().position(), Some(pos2));
    }

    #[wasm_bindgen_test]
    fn undo_redo() {
        use crate::test::mock::Fixture;
        // Setup the controller.
        let mut fixture = crate::test::mock::Unified::new().fixture();
        let Fixture { executed_graph, project, module, .. } = &mut fixture;

        let urm = project.urm();
        let nodes = executed_graph.graph().nodes().unwrap();
        let node = &nodes[0];

        // Check initial state.
        assert_eq!(urm.repository.len(Stack::Undo), 0);
        assert_eq!(module.ast().to_string(), "main = \n    2 + 2");

        // Perform an action.
        executed_graph.graph().set_expression(node.info.id(), "5 * 20").unwrap();

        // We can undo action.
        assert_eq!(urm.repository.len(Stack::Undo), 1);
        assert_eq!(module.ast().to_string(), "main = \n    5 * 20");
        urm.undo().unwrap();
        assert_eq!(module.ast().to_string(), "main = \n    2 + 2");

        // We cannot undo more actions than we made.
        assert_eq!(urm.repository.len(Stack::Undo), 0);
        assert!(urm.undo().is_err());
        assert_eq!(module.ast().to_string(), "main = \n    2 + 2");

        // We can redo since we undid.
        urm.redo().unwrap();
        assert_eq!(module.ast().to_string(), "main = \n    5 * 20");

        // And we can undo once more.
        urm.undo().unwrap();
        assert_eq!(module.ast().to_string(), "main = \n    2 + 2");

        //We cannot redo after edit has been made.
        executed_graph.graph().set_expression(node.info.id(), "4 * 20").unwrap();
        assert!(urm.redo().is_err());
    }
}
