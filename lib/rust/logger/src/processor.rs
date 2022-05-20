//! Logger processor implementation.

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::entry::level::DefaultLevels;
use crate::entry::Entry;


// ==============
// === Export ===
// ==============

pub mod consumer;
pub mod formatter;



// ===========================
// === JavaScript Bindings ===
// ===========================

mod js {
    use super::*;
    #[wasm_bindgen(inline_js = "
        export function setup_logs_flush(fn) {
            let oldShowLogs = window.showLogs
            window.showLogs = () => {
                if (oldShowLogs) { oldShowLogs() }
                fn()
            }
        }

        export function show_logs() {
            window.showLogs()
        }

        export function check_auto_flush() {
            return (console.autoFlush === true)
        }
    ")]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn setup_logs_flush(closure: &Closure<dyn Fn()>);

        #[allow(unsafe_code)]
        pub fn show_logs();

        // When the `showLogs` function is evaluated, the `autoFlush` flag is set to true. This
        // may happen even before the WASM file is loaded, so it's worth checking whether it
        // happened on startup.
        #[allow(unsafe_code)]
        pub fn check_auto_flush() -> bool;
    }
}



// =================
// === Processor ===
// =================

/// The most primitive building block of a logger. Processor takes some input and returns some
/// output, forming a message processing pipeline.
///
/// Processors can be chained together with the use of the `Seq` processor. They can perform both
/// simple actions like formatting the logs or outputting them to console, as well as more complex
/// ones, like buffering them globally and dumping them on demand only. There are a lot of sample
/// processors defined in this module and its sub-modules.
///
/// Processors always implement the `Default` trait, so it's sufficient to construct them using
/// type-level mechanisms only. For example, one of the simplest usages of processors would be a
/// processor defined as `Seq<Formatter<formatter::JsConsole>,Consumer<consumer::JsConsole>>`,
/// which for each input message first formats it and then prints it to the JavaScript console.
#[allow(missing_docs)]
pub trait Processor<Input> {
    type Output;
    fn submit(&mut self, input: Input) -> Self::Output;
}



// ==================================
// === Processors Implementations ===
// ==================================

// === Seq ===

/// A seq processor builder. It allows defining connected processors in a linear fashion. The macro
/// below generates a special type `Seq` which can accept two or more processors to be connected
/// together. Because it uses default arguments, you are allowed to use it like `Seq<P1,P2>`,
/// or `Seq<P1,P2,P3,P4>`.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct SeqBuilder<First, Second> {
    pub first:  First,
    pub second: Second,
}

impl<Input, First, Second> Processor<Input> for SeqBuilder<First, Second>
where
    First: Processor<Input>,
    Second: Processor<First::Output>,
{
    type Output = Second::Output;
    #[inline(always)]
    fn submit(&mut self, input: Input) -> Self::Output {
        self.second.submit(self.first.submit(input))
    }
}


// === Multi-args Seqs ===

macro_rules! define_seqs {
    ($arg:tt,$($args:tt),*) => {
        define_sub_seqs!{$arg,$($args),*}
        /// A generic seq implementation. See docs of `SeqBuilder` to learn more.
        pub type Seq<T=Identity,$($args=Identity),*> = $arg<T,$($args),*>;
    };
}

macro_rules! define_sub_seqs {
    () => {};
    ($arg:tt) => {};
    ($arg:tt, $($args:tt),*) => {
        /// Nested seq. See docs of `SeqBuilder` to learn more.
        pub type $arg<$arg,$($args),*> = define_seq_type!{$arg,$($args),*};
        define_sub_seqs! {$($args),*}
    };
}

macro_rules! define_seq_type {
    ($arg1:tt, $arg2:tt) => {
        SeqBuilder<$arg1,$arg2>
    };
    ($arg:tt $(,$args:tt)*) => {
        SeqBuilder<$arg,define_seq_type!{$($args),*}>
    };
}

define_seqs!(Seq5, Seq4, Seq3, Seq2, Seq1);


// === Branch ===

/// A branch processor builder. It passes the incoming input to all of its children. The macro
/// below generates a special type `Branch` which can accept two or more processors to be connected
/// together. Because it uses default arguments, you are allowed to use it like `Branch<P1,P2>`,
/// or `Branch<P1,P2,P3,P4>`.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct BranchBuilder<First, Second> {
    pub first:  First,
    pub second: Second,
}

impl<Input, First, Second> Processor<Input> for BranchBuilder<First, Second>
where
    First: Processor<Input>,
    Second: Processor<Input>,
    Input: Clone,
{
    type Output = ();
    #[inline(always)]
    fn submit(&mut self, input: Input) -> Self::Output {
        self.first.submit(input.clone());
        self.second.submit(input);
    }
}


// === Multi-args Branches ===

macro_rules! define_branches {
    ($arg:tt,$($args:tt),*) => {
        define_sub_branches!{$arg,$($args),*}
        /// A generic seq implementation. See docs of `SeqBuilder` to learn more.
        pub type Branch<T=Drop,$($args=Drop),*> = $arg<T,$($args),*>;
    };
}

macro_rules! define_sub_branches {
    () => {};
    ($arg:tt) => {};
    ($arg:tt, $($args:tt),*) => {
        /// Nested seq. See docs of `SeqBuilder` to learn more.
        pub type $arg<$arg,$($args),*> = define_branch_type!{$arg,$($args),*};
        define_sub_branches! {$($args),*}
    };
}

macro_rules! define_branch_type {
    ($arg1:tt, $arg2:tt) => {
        SeqBuilder<$arg1,$arg2>
    };
    ($arg:tt $(,$args:tt)*) => {
        SeqBuilder<$arg,define_branch_type!{$($args),*}>
    };
}

define_branches!(Branch5, Branch4, Branch3, Branch2, Branch1);


// === Drop Processor ===

/// Drop processor. Does nothing, just drops the input.
#[derive(Clone, Copy, Debug, Default)]
pub struct Drop;

impl<Input> Processor<Input> for Drop {
    type Output = ();
    #[inline(always)]
    fn submit(&mut self, _input: Input) {}
}


// === Identity Processor ===

/// Identity processor. It passes its input to output without performing any modification.
#[derive(Clone, Copy, Debug, Default)]
pub struct Identity;

impl<Input> Processor<Input> for Identity {
    type Output = Input;
    #[inline(always)]
    fn submit(&mut self, input: Input) -> Self::Output {
        input
    }
}


// === Formatter ===

/// Formatter processor. It uses the provided formatter to format its input.
#[derive(Debug, Default)]
pub struct Formatter<T> {
    formatter: PhantomData<T>,
}

impl<Fmt, Lvl> Processor<Entry<Lvl>> for Formatter<Fmt>
where Fmt: formatter::GenericDefinition<Lvl>
{
    type Output = (Entry<Lvl>, Option<Fmt::Output>);
    #[inline(always)]
    fn submit(&mut self, entry: Entry<Lvl>) -> Self::Output {
        let out = <Fmt>::generic_format(&entry);
        (entry, out)
    }
}


// === Consumer ===

/// Consumer processor. It uses the provided consumer to consume the results, and probably print
/// them on the screen or write to a file.
#[derive(Debug, Default)]
pub struct Consumer<T> {
    consumer: T,
}

impl<C, Levels, Message> Processor<(Entry<Levels>, Option<Message>)> for Consumer<C>
where C: consumer::Definition<Levels, Message>
{
    type Output = ();
    #[inline(always)]
    fn submit(&mut self, (entry, message): (Entry<Levels>, Option<Message>)) -> Self::Output {
        self.consumer.consume(entry, message)
    }
}


// === Buffer ===

#[derive(Debug, Derivative)]
#[allow(missing_docs)]
pub struct Buffer<Input, Next> {
    model:    Rc<RefCell<BufferModel<Input, Next>>>,
    _closure: Closure<dyn Fn()>,
}

impl<Input, Next> Default for Buffer<Input, Next>
where
    Input: 'static,
    Next: 'static + Default + Processor<Input>,
{
    fn default() -> Self {
        let model = Rc::new(RefCell::new(BufferModel::<Input, Next>::default()));
        let closure = Closure::new(f!(model.borrow_mut().flush_and_enable_auto_flush()));
        js::setup_logs_flush(&closure);
        if cfg!(debug_assertions) {
            println!("Debug mode. Logs will be enabled automatically.");
            js::show_logs();
        }
        Self { model, _closure: closure }
    }
}

impl<Input, Next> Processor<Input> for Buffer<Input, Next>
where Next: Processor<Input>
{
    type Output = ();
    #[inline(always)]
    fn submit(&mut self, input: Input) {
        self.model.borrow_mut().submit(input);
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
pub struct BufferModel<Input, Next> {
    buffer:     Vec<Input>,
    auto_flush: bool,
    next:       Next,
}

impl<Input, Next> BufferModel<Input, Next>
where Next: Processor<Input>
{
    /// Constructor.
    pub fn new() -> Self
    where Next: Default {
        let auto_flush = js::check_auto_flush();
        let buffer = default();
        let next = default();
        Self { buffer, auto_flush, next }
    }

    /// Submit the input to the buffer or the subsequent processor in case the `auto_flush` is
    /// enabled.
    pub fn submit(&mut self, input: Input) {
        if self.auto_flush {
            self.next.submit(input);
        } else {
            self.buffer.push(input);
        }
    }

    /// Pass all buffered entries to the subsequent processor.
    pub fn flush(&mut self) {
        for input in mem::take(&mut self.buffer) {
            self.next.submit(input);
        }
    }

    /// Pass all buffered entries to the subsequent processor and set the `auto_flush` flag to on.
    pub fn flush_and_enable_auto_flush(&mut self) {
        self.flush();
        self.auto_flush = true;
    }
}

impl<Input, Next> Default for BufferModel<Input, Next>
where Next: Processor<Input> + Default
{
    fn default() -> Self {
        Self::new()
    }
}


// === Global ===

#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct Global<Processor> {
    processor: PhantomData<Processor>,
}

impl<P, Input> Processor<Input> for Global<P>
where
    P: GlobalProcessor,
    P::Processor: 'static + Processor<Input>,
{
    type Output = <<P as GlobalProcessor>::Processor as Processor<Input>>::Output;
    #[inline(always)]
    fn submit(&mut self, entry: Input) -> Self::Output {
        global_processor::<P>().submit(entry)
    }
}

/// Abstraction for global processors. Global processors may be insanely useful to optimize the
/// logging performance. You can, for example, define a single global processor and redirect all
/// loggers to it. The single global processor can have a buffer layer, which will buffer messages
/// without formatting them and will format all of them and print them to the screen on-demand only.
#[allow(missing_docs)]
pub trait GlobalProcessor {
    type Processor;
    fn get_mut() -> &'static mut Self::Processor;
}

/// Get a reference to a global processor. Read docs of `GlobalProcessor` to learn more.
pub fn global_processor<T: GlobalProcessor>() -> &'static mut T::Processor {
    T::get_mut()
}

/// Define a global processor based on the provided type. Read the docs of `GlobalProcessor` to
/// learn more.
#[macro_export]
macro_rules! define_global_processor {
    ($name:ident = $tp:ty;) => {
        /// Global processor definition.
        #[derive(Copy, Clone, Debug, Default)]
        pub struct $name;
        paste! {
            #[allow(non_upper_case_globals)]
            static mut [<$name _STATIC_MUT>]: Option<$tp> = None;
        }
        impl GlobalProcessor for $name {
            type Processor = $tp;
            paste! {
                #[allow(unsafe_code)]
                fn get_mut() -> &'static mut Self::Processor {
                    unsafe {
                        match &mut [<$name _STATIC_MUT>] {
                            Some(t) => t,
                            None    => {
                                let processor = default();
                                [<$name _STATIC_MUT>] = Some(processor);
                                [<$name _STATIC_MUT>].as_mut().unwrap()
                            }
                        }
                    }
                }
            }
        }
    };
}



// ========================
// === DefaultProcessor ===
// ========================

/// Default processor implementation.
#[cfg(target_arch = "wasm32")]
pub type DefaultProcessor = DefaultJsProcessor;

/// Default processor implementation.
#[cfg(not(target_arch = "wasm32"))]
pub type DefaultProcessor = DefaultNativeProcessor;

#[allow(dead_code)]
type DefaultJsProcessor = Global<DefaultGlobalJsProcessor>;

#[allow(dead_code)]
type DefaultNativeProcessor =
    Seq<Formatter<formatter::NativeConsole>, Consumer<consumer::NativeConsole>>;

define_global_processor! {
    DefaultGlobalJsProcessor =
        Buffer<Entry<DefaultLevels>,
            Seq <
                Formatter<formatter::JsConsole>,
                Consumer<consumer::JsConsole>
            >
        >;
}
