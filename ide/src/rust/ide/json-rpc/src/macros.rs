//! Helper macros to generate RemoteClient and MockClient.

// FIXME[dg]: https://github.com/luna/ide/issues/401 We want to make the generated methods to
// take references instead of ownership.
/// This macro reads a `trait API` item and generates asynchronous methods for RPCs. Each method
/// should be signed with `MethodInput`, `rpc_name`, `result` and `set_result` attributes. e.g.:
/// ```rust,compile_fail
/// make_rpc_method!{
///     trait API {
///         #[MethodInput=CallMePleaseInput,camelCase=callMePlease,result=call_me_please_result,
///         set_result=set_call_me_please_result]
///         fn call_me_please(&self, my_number_is:String) -> ();
///     }
/// }
/// ```
///
/// This macro generates an `API` trait and creates two structs implementing `API`
/// called `Client`, with the actual RPC methods, and `MockClient`, with mocked methods with
/// return types setup by:
/// ```rust,compile_fail
///     fn set_call_me_please_result
///     (&mut self, my_number_is:String,result:json_rpc::api::Result<()>) { /* impl */ }
/// ```
#[macro_export]
macro_rules! make_rpc_methods {
    (
        $(#[doc = $impl_doc:expr])+
        trait API {
            $($(#[doc = $doc:expr])+
            #[MethodInput=$method_input:ident,rpc_name=$rpc_name:expr,result=$method_result:ident,
            set_result=$set_result:ident]
            fn $method:ident(&self $(,$param_name:ident:$param_ty:ty)*) -> $result:ty;
            )*
        }
    ) => {
        // ===========
        // === API ===
        // ===========

        $(#[doc = $impl_doc])+
        pub trait API {
            $(
                $(#[doc = $doc])+
                fn $method(&self $(,$param_name:$param_ty)*)
                -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>>;
            )*
        }



        // ==============
        // === Client ===
        // ==============

        $(#[doc = $impl_doc])+
        #[derive(Debug)]
        pub struct Client {
            /// JSON-RPC protocol handler.
            handler : RefCell<Handler<Notification>>,
        }

        impl Client {
                /// Create a new client that will use given transport.
                pub fn new(transport:impl json_rpc::Transport + 'static) -> Self {
                    let handler = RefCell::new(Handler::new(transport));
                    Self { handler }
                }

                /// Asynchronous event stream with notification and errors.
                ///
                /// On a repeated call, previous stream is closed.
                pub fn events(&self) -> impl Stream<Item = Event> {
                    self.handler.borrow_mut().handler_event_stream()
                }

                /// Returns a future that performs any background, asynchronous work needed
                /// for this Client to correctly work. Should be continually run while the
                /// `Client` is used. Will end once `Client` is dropped.
                pub fn runner(&self) -> impl Future<Output = ()> {
                    self.handler.borrow_mut().runner()
                }
        }

        impl API for Client {
            $(fn $method(&self, $($param_name:$param_ty),*)
            -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>> {
                let input = $method_input { $($param_name:$param_name),* };
                Box::pin(self.handler.borrow().open_request(input))
            })*
        }

        $(
            /// Structure transporting method arguments.
            #[derive(Serialize,Deserialize,Debug,PartialEq)]
            #[serde(rename_all = "camelCase")]
            struct $method_input {
                $($param_name : $param_ty),*
            }

            impl json_rpc::RemoteMethodCall for $method_input {
                const NAME:&'static str = $rpc_name;
                type Returned = $result;
            }
        )*



        // ==================
        // === MockClient ===
        // ==================

        /// Mock used for tests.
        #[derive(Debug,Default)]
        pub struct MockClient {
            $($method_result : RefCell<HashMap<($($param_ty),*),Result<$result>>>,)*
        }

        impl API for MockClient {
            $(fn $method(&self $(,$param_name:$param_ty)*)
            -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>> {
                let mut result = self.$method_result.borrow_mut();
                let result     = result.remove(&($($param_name),*)).unwrap();
                Box::pin(async move { result })
            })*
        }

        impl MockClient {
            $(
                /// Sets `$method`'s result to be returned when it is called.
                pub fn $set_result(&self $(,$param_name:$param_ty)*, result:Result<$result>) {
                    self.$method_result.borrow_mut().insert(($($param_name),*),result);
                }
            )*
        }
    }
}
