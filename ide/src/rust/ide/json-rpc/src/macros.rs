//! Helper macros to generate RemoteClient and MockClient.

// TODO[dg]: Make it possible to create an API which accepts pass both references and ownership.
/// This macro reads a `trait API` item and generates asynchronous methods for RPCs. Each method
/// should be signed with `MethodInput`, `rpc_name`, `result` and `set_result` attributes. e.g.:
/// ```rust,compile_fail
/// make_rpc_method!{
///     trait API {
///         #[MethodInput=CallMePleaseInput,camelCase=callMePlease,result=call_me_please_result,
///         set_result=set_call_me_please_result]
///         fn call_me_please<'a>(&'a self, my_number_is:&'a String) -> ();
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
        #[allow(clippy::ptr_arg)]
        pub trait API {
            $(
                $(#[doc = $doc])+
                fn $method<'a>(&'a self $(,$param_name:&'a $param_ty)*)
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
            $(fn $method<'a>(&'a self, $($param_name:&'a $param_ty),*)
            -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>> {
                use json_rpc::api::RemoteMethodCall;
                let phantom    = std::marker::PhantomData;
                let input      = $method_input { phantom, $($param_name:&$param_name),* };
                let input_json = serde_json::to_value(input).unwrap();
                let name       = $method_input::NAME;
                let result_fut = self.handler.borrow().open_request_with_json(name,&input_json);
                Box::pin(result_fut)
            })*
        }

        $(
            /// Structure transporting method arguments.
            #[derive(Serialize,Debug,PartialEq)]
            #[serde(rename_all = "camelCase")]
            struct $method_input<'a> {
                #[serde(skip)]
                phantom : std::marker::PhantomData<&'a()>,
                $($param_name : &'a $param_ty),*
            }

            impl json_rpc::RemoteMethodCall for $method_input<'_> {
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
            expect_all_calls : Cell<bool>,
            $($method_result : RefCell<HashMap<($($param_ty),*),Vec<Result<$result>>>>,)*
        }

        impl API for MockClient {
            $(fn $method<'a>(&'a self $(,$param_name:&'a $param_ty)*)
            -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>> {
                let mut results = self.$method_result.borrow_mut();
                let params      = ($($param_name.clone()),*);
                let result      = results.get_mut(&params).and_then(|res| res.pop());
                let err         = format!("Unrecognized call {} with params {:?}",$rpc_name,params);
                Box::pin(futures::future::ready(result.expect(err.as_str())))
            })*
        }

        impl MockClient {
            $(
                /// Sets `$method`'s result to be returned when it is called.
                pub fn $set_result(&self $(,$param_name:$param_ty)*, result:Result<$result>) {
                    let mut results = self.$method_result.borrow_mut();
                    let mut entry   = results.entry(($($param_name),*));
                    entry.or_default().push(result);
                }
            )*

            /// Mark all calls defined by `set_$method_result` as required. If client will be
            /// dropped without calling the test will fail.
            pub fn expect_all_calls(&self) {
                self.expect_all_calls.set(true);
            }
        }

        impl Drop for MockClient {
            fn drop(&mut self) {
                if self.expect_all_calls.get() {
                    $(
                        for (params,results) in self.$method_result.borrow().iter() {
                            assert!(results.is_empty(), "Didn't make expected call {} with \
                            parameters {:?}",$rpc_name,params);
                        }
                    )*
                }
            }
        }
    }
}
