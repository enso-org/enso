//! A ExecutionContext model which is synchronized with LanguageServer state.

use crate::prelude::*;

use crate::double_representation::definition::DefinitionName;
use crate::model::execution_context::LocalCall;

use enso_protocol::language_server;
use json_rpc::error::RpcError;



// ==========================
// === Synchronized Model ===
// ==========================

/// An ExecutionContext model synchronized with LanguageServer. It will be automatically removed
/// from LS once dropped.
#[derive(Debug)]
pub struct ExecutionContext {
    id              : model::execution_context::Id,
    model           : model::ExecutionContext,
    module_path     : Rc<controller::module::Path>,
    language_server : Rc<language_server::Connection>,
    logger          : Logger,
}

impl ExecutionContext {
    /// Create new ExecutionContext. It will be created in LanguageServer and the ExplicitCall
    /// stack frame will be pushed.
    pub async fn create
    ( parent          : Logger
    , language_server : Rc<language_server::Connection>
    , module_path     : Rc<controller::module::Path>
    , root_definition : DefinitionName
    ) -> FallibleResult<Self> {
        let logger = parent.sub("ExecutionContext");
        let model  = model::ExecutionContext::new(root_definition);
        info!(logger,"Creating.");
        let id = language_server.client.create_execution_context().await?.context_id;
        info!(logger,"Created. Id:{id}");
        let this  = Self {id,module_path,model,language_server,logger};
        this.push_root_frame().await?;
        info!(this.logger,"Pushed root frame");
        Ok(this)
    }

    fn push_root_frame(&self) -> impl Future<Output=FallibleResult<()>> {
        let method_pointer = language_server::MethodPointer {
            file            : self.module_path.file_path().clone(),
            defined_on_type : self.module_path.module_name().to_string(),
            name            : self.model.entry_point.name.item.clone(),
        };
        let this_argument_expression         = default();
        let positional_arguments_expressions = default();

        let call = language_server::ExplicitCall {method_pointer,this_argument_expression,
            positional_arguments_expressions};
        let frame  = language_server::StackItem::ExplicitCall(call);
        let result = self.language_server.push_to_execution_context(&self.id,&frame);
        result.map(|res| res.map_err(|err| err.into()))
    }

    /// Push a new stack item to execution context.
    pub fn push(&self, stack_item: LocalCall) -> impl Future<Output=Result<(),RpcError>> {
        let expression_id = stack_item.call;
        let call          = language_server::LocalCall{expression_id};
        let frame         = language_server::StackItem::LocalCall(call);
        self.model.push(stack_item);
        self.language_server.push_to_execution_context(&self.id,&frame)
    }

    /// Pop the last stack item from this context. It returns error when only root call
    /// remains.
    pub async fn pop(&self) -> FallibleResult<()> {
        self.model.pop()?;
        self.language_server.pop_from_execution_context(&self.id).await?;
        Ok(())
    }

    /// Create a mock which does no call on `language_server` during construction.
    #[cfg(test)]
    pub fn new_mock
    ( id              : model::execution_context::Id
    , path            : controller::module::Path
    , model           : model::ExecutionContext
    , language_server : language_server::MockClient
    ) -> Self {
        let module_path     = Rc::new(path);
        let language_server = language_server::Connection::new_mock_rc(language_server);
        let logger          = Logger::new("ExecuctionContext mock");
        ExecutionContext {id,model,module_path,language_server,logger}
    }
}

impl Drop for ExecutionContext {
    fn drop(&mut self) {
        let id     = self.id;
        let ls     = self.language_server.clone_ref();
        let logger = self.logger.clone_ref();
        executor::global::spawn(async move {
            let result = ls.client.destroy_execution_context(&id).await;
            if result.is_err() {
                error!(logger,"Error when destroying Execution Context: {result:?}.");
            }
        });
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use json_rpc::expect_call;
    use language_server::response;
    use utils::test::ExpectTuple;
    use enso_protocol::language_server::CapabilityRegistration;


    #[test]
    fn creating_context() {
        let path       = Rc::new(controller::module::Path::from_module_name("Test"));
        let context_id = model::execution_context::Id::new_v4();
        let root_def   = DefinitionName::new_plain("main");
        let ls_client  = language_server::MockClient::default();
        let can_modify =
            CapabilityRegistration::create_can_modify_execution_context(context_id);
        let receives_updates =
            CapabilityRegistration::create_receives_execution_context_updates(context_id);
        ls_client.expect.create_execution_context(move || Ok(response::CreateExecutionContext {
            context_id,can_modify,receives_updates,
        }));
        let method = language_server::MethodPointer {
            file            : path.file_path().clone(),
            defined_on_type : "Test".to_string(),
            name            : "main".to_string(),
        };
        let root_frame = language_server::ExplicitCall {
            method_pointer                   : method,
            this_argument_expression         : None,
            positional_arguments_expressions : vec![]
        };
        let stack_item = language_server::StackItem::ExplicitCall(root_frame);
        expect_call!(ls_client.push_to_execution_context(context_id,stack_item) => Ok(()));
        expect_call!(ls_client.destroy_execution_context(context_id) => Ok(()));
        ls_client.require_all_calls();
        let connection = language_server::Connection::new_mock_rc(ls_client);

        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let context = ExecutionContext::create(default(),connection,path.clone(),root_def);
            let context = context.await.unwrap();
            assert_eq!(context_id             , context.id);
            assert_eq!(path                   , context.module_path);
            assert_eq!(Vec::<LocalCall>::new(), context.model.stack_items().collect_vec());
        })
    }

    #[test]
    fn pushing_stack_item() {
        let id                  = model::execution_context::Id::new_v4();
        let definition          = model::execution_context::DefinitionId::new_plain_name("foo");
        let expression_id       = model::execution_context::ExpressionId::new_v4();
        let path                = controller::module::Path::from_module_name("Test");
        let root_def            = DefinitionName::new_plain("main");
        let model               = model::ExecutionContext::new(root_def);
        let ls                  = language_server::MockClient::default();
        let expected_call_frame = language_server::LocalCall{expression_id};
        let expected_stack_item = language_server::StackItem::LocalCall(expected_call_frame);
        
        expect_call!(ls.push_to_execution_context(id,expected_stack_item) => Ok(()));
        expect_call!(ls.destroy_execution_context(id) => Ok(()));
        let context  = ExecutionContext::new_mock(id,path.clone(),model,ls);

        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let item = LocalCall {
                call       : expression_id,
                definition : definition.clone()
            };
            context.push(item.clone()).await.unwrap();
            assert_eq!((item,), context.model.stack_items().expect_tuple());
        })
    }

    #[test]
    fn popping_stack_item() {
        let id   = model::execution_context::Id::new_v4();
        let item = LocalCall {
            call       : model::execution_context::ExpressionId::new_v4(),
            definition : model::execution_context::DefinitionId::new_plain_name("foo"),
        };
        let path          = controller::module::Path::from_module_name("Test");
        let root_def      = DefinitionName::new_plain("main");
        let ls            = language_server::MockClient::default();
        let model         = model::ExecutionContext::new(root_def);
        expect_call!(ls.pop_from_execution_context(id) => Ok(()));
        expect_call!(ls.destroy_execution_context(id) => Ok(()));
        model.push(item);
        let context  = ExecutionContext::new_mock(id,path.clone(),model,ls);

        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            context.pop().await.unwrap();
            assert_eq!(Vec::<LocalCall>::new(), context.model.stack_items().collect_vec());
            // Pop on empty stack.
            assert!(context.pop().await.is_err());
        })
    }
}
