//! Helper macros to generate RemoteClient and MockClient.



// TODO[dg]: Make it possible to create an API which accepts pass both references and ownership.
/// This macro reads a `trait API` item and generates asynchronous methods for RPCs. Each method
/// should be signed with `MethodInput`, `rpc_name`, `result` and `set_result` attributes. e.g.:
/// ```rust,compile_fail
/// make_rpc_method!{
///     trait API {
///         #[MethodInput=CallMePleaseInput,camelCase=callMePlease,result=call_me_please_result,
///         expect_result=set_call_me_please]
///         fn call_me_please<'a>(&'a self, my_number_is:&'a String) -> ();
///     }
/// }
/// ```
///
/// This macro generates an `API` trait and creates two structs implementing `API`
/// called `Client`, with the actual RPC methods, and `MockClient`, with mocked methods with
/// return types setup by:
/// ```rust,compile_fail
///     fn expect_call_me_please
///     (&mut self, my_number_is:String,result:json_rpc::api::Result<()>) { /* impl */ }
/// ```
#[macro_export]
macro_rules! make_rpc_methods {
    (
        $(#[doc = $impl_doc:expr])+
        trait API {
            $($(#[doc = $doc:expr])+
            #[MethodInput=$method_input:ident,rpc_name=$rpc_name:expr]
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

            /// Asynchronous event stream with notification and errors.
            ///
            /// On a repeated call, previous stream is closed.
            fn events(&self) -> futures::stream::LocalBoxStream<'static,Event>;
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

            /// Returns a future that performs any background, asynchronous work needed
            /// for this Client to correctly work. Should be continually run while the
            /// `Client` is used. Will end once `Client` is dropped.
            pub fn runner(&self) -> impl Future<Output = ()> {
                self.handler.borrow_mut().runner()
            }

            /// Set new timeout for future requests. Pending requests are not affected.
            pub fn set_timeout(&mut self, timeout:std::time::Duration) {
                self.handler.borrow().set_timeout(timeout);
            }
        }

        impl API for Client {
            $(fn $method<'a>(&'a self, $($param_name:&'a $param_ty),*)
            -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>> {
                use json_rpc::api::RemoteMethodCall;
                use $crate::enso_profiler as profiler;
                use $crate::enso_profiler::internal::StartState;
                use $crate::enso_profiler::internal::Profiler;

                let label = profiler::internal::Label(stringify!($method));
                let parent = profiler::internal::EventId::implicit();
                let now = Some(profiler::internal::Timestamp::now());
                let profiler = profiler::Task::start(parent, label, now, StartState::Active);

                json_rpc::log::rpc_request(stringify!($method));

                let phantom    = std::marker::PhantomData;
                let input      = $method_input { phantom, $($param_name:&$param_name),* };
                let input_json = serde_json::to_value(input).unwrap();
                let name       = $method_input::NAME;
                let result_fut = self.handler.borrow().open_request_with_json(name,&input_json);

                profiler.pause();

                let result_fut = result_fut.map(move |value| {
                    profiler.resume();
                    profiler.finish();
                    value
                });
                Box::pin(result_fut)
            })*

            fn events(&self) -> futures::stream::LocalBoxStream<'static,Event> {
                self.handler.borrow_mut().handler_event_stream().boxed_local()
            }
        }

        $(
            /// Structure transporting method arguments.
            #[derive(Serialize,Debug,PartialEq)]
            #[serde(rename_all="camelCase")]
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

        /// Utilities for mocking client.
        // TODO[ao] This whole environment should be replaced with mock-all crate.
        pub mod mock {
            use super::*;

            /// Mock used for tests.
            ///
            /// You may specify expected calls and their return values by setting appropriate call
            /// handler as in the following example:
            /// ```rust,compile_fail
            ///     let mock = MockClient::default();
            ///     mock.expect.some_method(|param1, param2| result);
            /// ```
            #[derive(Debug,Default)]
            pub struct Client {
                require_all_calls : Cell<bool>,
                /// Expected calls handlers.
                pub expect : ExpectedCalls,
                events     : RefCell<Option<futures::channel::mpsc::UnboundedReceiver<Event>>>,
            }

            impl API for Client {
                $(fn $method<'a>(&'a self $(,$param_name:&'a $param_ty)*)
                -> std::pin::Pin<Box<dyn Future<Output=Result<$result>>>> {
                    let mut handlers = self.expect.$method.borrow_mut();
                    assert!(!handlers.is_empty(),"Unexpected call {}",$rpc_name);
                    let handler      = handlers.remove(0);
                    let result       = handler($($param_name),*);
                    Box::pin(futures::future::ready(result))
                })*

                fn events(&self) -> futures::stream::LocalBoxStream<'static,Event> {
                    if let Some(receiver) = self.events.borrow_mut().take() {
                        receiver.boxed_local()
                    } else {
                        futures::stream::empty().boxed_local()
                    }
                }
            }

            impl Client {
                /// Mark all calls defined by `expect.$method` as required. If client will be
                /// dropped without calling the test will fail.
                pub fn require_all_calls(&self) {
                    self.require_all_calls.set(true);
                }

                /// Set up a channel that will feed `events` stream with events.
                pub fn setup_events(&self) -> futures::channel::mpsc::UnboundedSender<Event> {
                    let (sender,receiver) = futures::channel::mpsc::unbounded();
                    *self.events.borrow_mut() = Some(receiver);
                    sender
                }
            }

            impl Drop for Client {
                fn drop(&mut self) {
                    if self.require_all_calls.get() && !std::thread::panicking() {
                        $(
                            let method = stringify!($method);
                            let msg = iformat!("An expected call to {method} was not made.");
                            assert!(self.expect.$method.borrow().is_empty(), "{}", msg);
                        )*
                    }
                }
            }

            /// A set of handlers of expected Mock Client calls. Handlers get call's parameters and
            /// returns the value to be returned.
            #[derive(Default)]
            pub struct ExpectedCalls {
                $($method : RefCell<Vec<Box<dyn FnOnce($(&$param_ty),*) -> Result<$result>>>>,)*
            }

            impl ExpectedCalls {
                $(
                    /// Adds handler to the next expected call of `$method`. Each handle will be
                    /// called once and removed. The handlers will be called in the same order as
                    /// set by this function.
                    pub fn $method(&self, handler:impl FnOnce($(&$param_ty),*) -> Result<$result> + 'static) {
                        self.$method.borrow_mut().push(Box::new(handler));
                    }
                )*
            }

            impl Debug for ExpectedCalls {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(f, "Expected calls for Mock Client")
                }
            }
        }
        pub use mock::Client as MockClient;
    }
}

/// A shortcut for creating call's handlers for client mocks; when you want to just check if call's
/// parameters are equal to some expected values and then return specific value, you can call:
/// ```rust,compile_fail
/// expect_call!(client.method(param1=value1,param2=value2) => Ok(result));
/// ```
#[macro_export]
macro_rules! expect_call {
    ($mock:ident.$method:ident() => $result:expr) => {
        let result = $result;
        $mock.expect.$method(move || result)
    };
    ($mock:ident.$method:ident($($param:ident),*) => $result:expr) => {
        expect_call!($mock.$method($($param=$param),*) => $result)
    };
    ($mock:ident.$method:ident($($param:ident=$value:expr),*) => $result:expr) => {
        let result          = $result;
        let expected_params = ($($value,)*);
        $mock.expect.$method(move |$($param),*| {
            let expected_params_refs = {
                let ($($param,)*) = &expected_params;
                ($($param,)*)
            };
            assert_eq!(($($param,)*),expected_params_refs);
            result
        })
    };
}
