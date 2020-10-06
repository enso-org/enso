//! View part of the text editor.

use crate::prelude::*;

pub mod movement;
pub mod selection;
pub mod word;

pub use movement::*;
pub use selection::Selection;

use crate::buffer::Buffer;
use crate::buffer::DefaultSetter;
use crate::buffer::Setter;
use crate::buffer::data::Text;
use crate::buffer::data::text::BoundsError;
use crate::buffer::data::unit::*;
use crate::buffer::style::Style;
use crate::buffer::style;
use crate::buffer;

use ensogl_core::data::color;
use enso_frp as frp;



// ==================
// === Frp Macros ===
// ==================

// FIXME[WD]: This should be refactored to the right place as part of the cleaning PR:
// https://github.com/enso-org/ide/issues/670

/// Generate a set of structures allowing for nice management of FRP inputs, outputs, and commands.
///
/// Given the definition:
///
/// ```compile_fail
/// define_endpoints! {
///     Commands { Commands }
///     Input {
///         input1 (f32),
///         input2 (),
///     }
///     Output {
///         output1 (String),
///         output2 (),
///     }
/// }
/// ```
///
/// There would be generated a bunch of structures, the main structure contains FRP network and
/// another struct with all the endpoints, to which it dereferences to:
///
/// ```compile_fail
/// #[derive(Debug,Clone,CloneRef)]
/// pub struct Frp {
///     pub network : frp::Network,
///     output      : FrpEndpoints,
/// }
///
/// impl Frp {
///     pub fn new(network:frp::Network, output:FrpEndpoints) -> Self {
///         Self {network,output}
///     }
/// }
///
/// impl Deref for Frp {
///     type Target = FrpEndpoints;
///     fn deref(&self) -> &Self::Target {
///         &self.output
///     }
/// }
/// ```
///
/// The target structure contains all outputs as `frp::Stream` values. As a reminder, you are
/// allowed to read from streams but you are not allowed to write to them, which makes perfect
/// sense in the context of output values.
///
/// ```compile_fail
/// #[derive(Debug, Clone, CloneRef)]
/// #[allow(missing_docs)]
/// pub struct FrpEndpoints {
///     pub input         : FrpInputs,
///     pub(crate) source : FrpOutputsSource,
///     pub output1       : frp::Stream<String>,
///     pub output2       : frp::Stream<>,
/// }
/// impl Deref for FrpEndpoints {
///     type Target = FrpInputs;
///     fn deref(&self) -> &Self::Target {
///         &self.input
///     }
/// }
/// impl FrpEndpoints {
///     pub fn new(network: &frp::Network, input: FrpInputs) -> Self {
///         let source = FrpOutputsSource::new(network);
///         let output1 = source.output1.clone_ref().into();
///         let output2 = source.output2.clone_ref().into();
///         Self { source, input, output1, output2 }
///     }
/// }
/// ```
///
/// However, as the owner of this FRP network, you want to emit values to the output streams.
/// Exactly for this purpose there is `FrpOutputsSource` struct defined, which is private and cannot
/// be accessed from outside of this crate. The structure contains the same FRP endpoints but in a
/// form allowing for both emiting and attaching values (see FRP documentation to learn more):
///
/// ```compile_fail
/// #[derive(Debug,Clone,CloneRef)]
/// pub(crate) struct FrpOutputsSource {
///     output1: frp::Any<String>,
///     output2: frp::Any<>,
/// }
///
/// impl FrpOutputsSource {
///     pub fn new(network: &frp::Network) -> Self {
///         frp::extend! { network
///             output1  <- any(...);
///             output2  <- any(...);
///         }
///         Self {output1,output2}
///     }
/// }
/// ```
///
/// Moreover, the above presented `FrpEndpoints` structure contains the `input` field which
/// describes all FRP input endpoints (input API of this FRP network). The struct contains all the
/// fields declared in the macro usage in a form of `frp::Source` values (allowing emiting values
/// on demand).
///
/// ```compile_fail
/// #[derive(Debug, Clone, CloneRef)]
/// #[allow(missing_docs)]
/// #[allow(unused_parens)]
/// pub struct FrpInputs {
///     pub command: Commands,
///     pub input1: frp::Source<(   f32   )>,
///     pub input2: frp::Source<()>,
/// }
/// impl Deref for FrpInputs {
///     type Target = Commands;
///     fn deref(&self) -> &Self::Target {
///         &self.command
///     }
/// }
///
/// impl FrpInputs {
///     pub fn new(network: &frp::Network) -> Self {
///         let command = Commands::new(network);
///         frp::extend! { network
///             input1 <- source();
///             input2 <- source();
///         }
///         Self {command,input1,input2}
///     }
/// ```
///
/// Moreover, for each input value, there is a sugar-method generated which allows for nice call
/// syntax. Thanks to that and all the derefs defined above, if your component dereferences to the
/// generated FRP struct (and it should!), then instead of calling
/// `my_comp.frp.input.clear_all.emit(())`, you can just call `my_comp.clear_all()`.
///
/// ```compile_fail
/// impl FrpInputs {
///     #[allow(missing_docs)]
///     pub fn input1(&self, t1:impl IntoParam<f32>) {
///         self.input1.emit(t1);
///     }
///     #[allow(missing_docs)]
///     pub fn input2(&self) {
///         self.input2.emit(());
///     }
/// }
/// ```
///
/// Last thing to note here is that if you declared the usage of commands in the usage of the macro,
/// the `FrpInputs` struct will contain commands as a field and will dereference to it. To learn
/// more how to define commands, see the docs of the `def_command_api` macro and example usages
/// in components defined by using of this API.
#[macro_export]
macro_rules! define_endpoints {
    (
        $(Commands {$commands_name : ident})?
        Input  { $($(#[doc=$($in_doc :tt)*])? $in_field  : ident ($($in_field_type  : tt)*)),* $(,)? }
        Output { $($(#[doc=$($out_doc:tt)*])? $out_field : ident ($($out_field_type : tt)*)),* $(,)? }
    ) => {
        use enso_frp::IntoParam;

        /// Frp network and endpoints.
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        pub struct Frp {
            pub network : frp::Network,
            output      : FrpEndpoints,
        }

        impl Frp {
            /// Constructor.
            pub fn new(network:frp::Network, output:FrpEndpoints) -> Self {
                Self {network,output}
            }

            /// Create Frp with network, inputs and outputs.
            pub fn new_network() -> Self {
                let network       = frp::Network::new();
                let frp_inputs    = FrpInputs::new(&network);
                let frp_endpoints = FrpEndpoints::new(&network,frp_inputs.clone_ref());
                Self::new(network,frp_endpoints)
            }
        }

        impl Deref for Frp {
            type Target = FrpEndpoints;
            fn deref(&self) -> &Self::Target {
                &self.output
            }
        }

        /// Frp inputs.
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        #[allow(unused_parens)]
        pub struct FrpInputs {
            $(pub command : $commands_name,)?
            $( $(#[doc=$($in_doc)*])? pub $in_field : frp::Source<($($in_field_type)*)>),*
        }

        $(impl Deref for FrpInputs {
            type Target = $commands_name;
            fn deref(&self) -> &Self::Target {
                &self.command
            }
        })?

        #[allow(unused_parens)]
        impl FrpInputs {
            /// Constructor.
            pub fn new(network:&frp::Network) -> Self {
                $(
                    #[allow(non_snake_case)]
                    let $commands_name = $commands_name::new(network);
                )?
                frp::extend! { network
                    $($in_field <- source();)*
                }
                Self { $(command:$commands_name,)? $($in_field),* }
            }

            $($crate::define_endpoints_emit_alias!{$in_field ($($in_field_type)*)})*
        }

        /// Frp outputs.
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        pub struct FrpEndpoints {
            pub input         : FrpInputs,
            pub(crate) source : FrpOutputsSource,
            $($(#[doc=$($out_doc)*])? pub $out_field  : frp::Stream<$($out_field_type)*>),*
        }

        impl Deref for FrpEndpoints {
            type Target = FrpInputs;
            fn deref(&self) -> &Self::Target {
                &self.input
            }
        }

        impl FrpEndpoints {
            /// Constructor.
            pub fn new(network:&frp::Network, input:FrpInputs) -> Self {
                let source = FrpOutputsSource::new(network);
                $(let $out_field = source.$out_field.clone_ref().into();)*
                Self {source,input,$($out_field),*}
            }
        }

        /// Frp output setters.
        #[derive(Debug,Clone,CloneRef)]
        pub(crate) struct FrpOutputsSource {
            $($out_field : frp::Any<$($out_field_type)*>),*
        }

        impl FrpOutputsSource {
            /// Constructor.
            pub fn new(network:&frp::Network) -> Self {
                frp::extend! { network
                    $($out_field <- any(...);)*
                }
                Self {$($out_field),*}
            }
        }
    };
}

/// Defines a method which is an alias to FRP emit method. Used internally by the `define_endpoints`
/// macro.
#[macro_export]
macro_rules! define_endpoints_emit_alias {
    ($field:ident ()) => {
        #[allow(missing_docs)]
        pub fn $field(&self) {
            self.$field.emit(());
        }
    };

    ($field:ident ($t1:ty,$t2:ty)) => {
        #[allow(missing_docs)]
        pub fn $field
        ( &self
        , t1:impl IntoParam<$t1>
        , t2:impl IntoParam<$t2>
        ) {
            let t1 = t1.into_param();
            let t2 = t2.into_param();
            self.$field.emit((t1,t2));
        }
    };

    ($field:ident ($t1:ty,$t2:ty,$t3:ty)) => {
        #[allow(missing_docs)]
        pub fn $field
        ( &self
        , t1:impl IntoParam<$t1>
        , t2:impl IntoParam<$t2>
        , t3:impl IntoParam<$t3>
        ) {
            let t1 = t1.into_param();
            let t2 = t2.into_param();
            let t3 = t3.into_param();
            self.$field.emit((t1,t2,t3));
        }
    };

    ($field:ident ($t1:ty,$t2:ty,$t3:ty,$t4:ty)) => {
        #[allow(missing_docs)]
        pub fn $field
        ( &self
        , t1:impl IntoParam<$t1>
        , t2:impl IntoParam<$t2>
        , t3:impl IntoParam<$t3>
        , t4:impl IntoParam<$t4>
        ) {
            let t1 = t1.into_param();
            let t2 = t2.into_param();
            let t3 = t3.into_param();
            let t4 = t4.into_param();
            self.$field.emit((t1,t2,t3,t4));
        }
    };

    ($field:ident ($t1:ty,$t2:ty,$t3:ty,$t4:ty,$t5:ty)) => {
        #[allow(missing_docs)]
        pub fn $field
        ( &self
        , t1:impl IntoParam<$t1>
        , t2:impl IntoParam<$t2>
        , t3:impl IntoParam<$t3>
        , t4:impl IntoParam<$t4>
        , t5:impl IntoParam<$t5>
        ) {
            let t1 = t1.into_param();
            let t2 = t2.into_param();
            let t3 = t3.into_param();
            let t4 = t4.into_param();
            let t5 = t5.into_param();
            self.$field.emit((t1,t2,t3,t4,t5));
        }
    };

    ($field:ident $t1:ty) => {
        #[allow(missing_docs)]
        pub fn $field(&self,t1:impl IntoParam<$t1>) {
            self.$field.emit(t1);
        }
    };
}


// =================
// === Constants ===
// =================

/// Default visible line count in a new buffer view.
const DEFAULT_LINE_COUNT : usize = 10;



// ===============
// === History ===
// ===============

/// Modifications history. Contains data used by undo / redo mechanism.
#[derive(Debug,Clone,CloneRef,Default)]
pub struct History {
    data : Rc<RefCell<HistoryData>>
}

/// Internal representation of `History`.
#[derive(Debug,Clone,Default)]
pub struct HistoryData {
    undo_stack : Vec<(Text,Style,selection::Group)>,
    redo_stack : Vec<(Text,Style,selection::Group)>,
}



// ==================
// === ViewBuffer ===
// ==================

/// Specialized form of `Buffer` with view-related information, such as selection and undo redo
/// history (containing also cursor movement history). This form of buffer is mainly used by `View`,
/// but can also be combined with other `ViewBuffer`s to display cursors, selections, and edits of
/// several users at the same time.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct ViewBuffer {
    pub buffer            : Buffer,
    pub selection         : Rc<RefCell<selection::Group>>,
    pub next_selection_id : Rc<Cell<usize>>,
    pub history           : History,
}

impl Deref for ViewBuffer {
    type Target = Buffer;
    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl From<Buffer> for ViewBuffer {
    fn from(buffer:Buffer) -> Self {
        let selection         = default();
        let next_selection_id = default();
        let history           = default();
        Self {buffer,selection,next_selection_id,history}
    }
}

impl From<&Buffer> for ViewBuffer {
    fn from(buffer:&Buffer) -> Self {
        buffer.clone_ref().into()
    }
}

impl Default for ViewBuffer {
    fn default() -> Self {
        Buffer::default().into()
    }
}

impl ViewBuffer {
    fn commit_history(&self) {
        let text      = self.buffer.text();
        let style     = self.buffer.style();
        let selection = self.selection.borrow().clone();
        self.history.data.borrow_mut().undo_stack.push((text,style,selection));
    }

    fn undo(&self) -> Option<selection::Group> {
        let item      = self.history.data.borrow_mut().undo_stack.pop();
        item.map(|(text,style,selection)| {
            self.buffer.set_text(text);
            self.buffer.set_style(style);
            selection
        })
    }

    fn first_selection(&self) -> selection::Group {
        self.selection.borrow().first().cloned().into()
    }

    fn last_selection(&self) -> selection::Group {
        self.selection.borrow().last().cloned().into()
    }

    fn first_cursor(&self) -> selection::Group {
        self.first_selection().snap_selections_to_start()
    }

    fn last_cursor(&self) -> selection::Group {
        self.last_selection().snap_selections_to_start()
    }

    fn newest_selection(&self) -> selection::Group {
        self.selection.borrow().newest().cloned().into()
    }

    fn oldest_selection(&self) -> selection::Group {
        self.selection.borrow().oldest().cloned().into()
    }

    fn newest_cursor(&self) -> selection::Group {
        self.newest_selection().snap_selections_to_start()
    }

    fn oldest_cursor(&self) -> selection::Group {
        self.oldest_selection().snap_selections_to_start()
    }

    fn new_cursor(&self, location:Location) -> Selection {
        let id = self.next_selection_id.get();
        self.next_selection_id.set(id+1);
        Selection::new_cursor(location,id)
    }

    fn add_cursor(&self, location:Location) -> selection::Group {
        let mut selection = self.selection.borrow().clone();
        let new_selection = self.new_cursor(location);
        selection.merge(new_selection);
        selection
    }

    fn set_newest_selection_end(&self, location:Location) -> selection::Group {
        let mut group = self.selection.borrow().clone();
        group.newest_mut().for_each(|s| s.end=location);
        group
    }

    fn set_oldest_selection_end(&self, location:Location) -> selection::Group {
        let mut group = self.selection.borrow().clone();
        group.oldest_mut().for_each(|s| s.end=location);
        group
    }

    /// Insert new text in the place of current selections / cursors.
    fn insert(&self, text:impl Into<Text>) -> selection::Group {
        self.modify(text,None)
    }

    /// Paste new text in the place of current selections / cursors. In case of pasting multiple
    /// chunks (e.g. after copying multiple selections), the chunks will be pasted into subsequent
    /// selections. In case there are more chunks than selections, end chunks will be dropped. In
    /// case there is more selections than chunks, end selections will be replaced with empty
    /// strings.
    fn paste(&self, text:&[String]) -> selection::Group {
        self.modify_iter(text.iter(),None)
    }

    // TODO
    // Delete left should first delete the vowel (if any) and do not move cursor. After pressing
    // backspace second time, the consonant should be removed. Please read this topic to learn
    // more: https://phabricator.wikimedia.org/T53472
    fn delete_left(&self) -> selection::Group {
        self.modify("", Some(Transform::Left))
    }

    fn delete_right(&self) -> selection::Group {
        self.modify("", Some(Transform::Right))
    }

    fn delete_word_left(&self) -> selection::Group {
        self.modify("", Some(Transform::LeftWord))
    }

    fn delete_word_right(&self) -> selection::Group {
        self.modify("", Some(Transform::RightWord))
    }

    /// Generic buffer modify utility. It replaces each selection range with the provided `text`.
    ///
    /// If `transform` is provided, it will modify the selections being a simple cursor before
    /// applying modification, what is useful when handling delete operations.
    ///
    /// ## Implementation details.
    /// This function converts all selections to byte-based ones first, and then applies all
    /// modification rules. This way, it can work in an 1D byte-based space (as opposed to 2D
    /// location-based space), which makes handling multiple cursors much easier.
    fn modify(&self, text:impl Into<Text>, transform:Option<Transform>) -> selection::Group {
        self.commit_history();
        let text                    = text.into();
        let mut new_selection_group = selection::Group::new();
        let mut byte_offset         = 0.bytes();
        for rel_byte_selection in self.byte_selections() {
            let byte_selection         = rel_byte_selection.map(|t|t+byte_offset);
            let selection              = self.to_location_selection(byte_selection);
            let (new_selection,offset) = self.modify_selection(selection,&text,transform);
            byte_offset                += offset;
            new_selection_group.merge(new_selection);
        }
        new_selection_group
    }

    /// Generic buffer modify utility. It replaces each selection range with next iterator item.
    ///
    /// If `transform` is provided, it will modify the selections being a simple cursor before
    /// applying modification, what is useful when handling delete operations.
    fn modify_iter<I,S>(&self, mut iter:I, transform:Option<Transform>) -> selection::Group
    where I:Iterator<Item=S>, S:Into<Text> {
        self.commit_history();
        let mut new_selection_group = selection::Group::new();
        let mut byte_offset         = 0.bytes();
        for rel_byte_selection in self.byte_selections() {
            let text                   = iter.next().map(|t|t.into()).unwrap_or_default();
            let byte_selection         = rel_byte_selection.map(|t|t+byte_offset);
            let selection              = self.to_location_selection(byte_selection);
            let (new_selection,offset) = self.modify_selection(selection,&text,transform);
            byte_offset               += offset;
            new_selection_group.merge(new_selection);
        }
        new_selection_group
    }

    /// Generic selection modify utility. It replaces selection range with given text.
    ///
    /// If `transform` is provided and selection is a simple cursor, it will modify it before
    /// applying modification, what is useful when handling delete operations.
    ///
    /// It returns selection after modification and byte offset of the next selection ranges.
    fn modify_selection
    (&self, selection:Selection, text:&Text, transform:Option<Transform>) -> (Selection,Bytes) {
        let text_byte_size = text.byte_size();
        let transformed    = match transform {
            Some(t) if selection.is_cursor() => self.moved_selection_region(t,selection,true),
            _                                => selection
        };
        let byte_selection = self.to_bytes_selection(transformed);
        let byte_range     = byte_selection.range();
        let byte_offset    = text_byte_size - byte_range.size();
        self.buffer.replace(byte_range,text);
        let new_byte_cursor_pos = byte_range.start + text_byte_size;
        let new_byte_selection  = Selection::new_cursor(new_byte_cursor_pos,selection.id);
        (self.to_location_selection(new_byte_selection),byte_offset)
    }

    fn byte_selections(&self) -> Vec<Selection<Bytes>> {
        self.selection.borrow().iter().map(|s|self.to_bytes_selection(*s)).collect()
    }

    fn to_bytes_selection(&self, selection:Selection) -> Selection<Bytes> {
        let start = self.byte_offset_of_location_snapped(selection.start);
        let end   = self.byte_offset_of_location_snapped(selection.end);
        let id    = selection.id;
        Selection::new(start,end,id)
    }

    fn to_location_selection(&self, selection:Selection<Bytes>) -> Selection {
        let start = self.offset_to_location(selection.start);
        let end   = self.offset_to_location(selection.end);
        let id    = selection.id;
        Selection::new(start,end,id)
    }

    fn offset_to_location(&self, offset:Bytes) -> Location {
        let line = self.line_index_of_byte_offset_snapped(offset);
        let line_offset = offset - self.byte_offset_of_line_index(line).unwrap();
        let column = self.column_of_line_index_and_in_line_byte_offset_snapped(line,line_offset);
        Location(line,column)
    }
}



// ===========
// === FRP ===
// ===========

define_endpoints! {
    Input {
        cursors_move               (Option<Transform>),
        cursors_select             (Option<Transform>),
        set_cursor                 (Location),
        add_cursor                 (Location),
        set_newest_selection_end   (Location),
        set_oldest_selection_end   (Location),
        insert                     (String),
        paste                      (Vec<String>),
        remove_all_cursors         (),
        delete_left                (),
        delete_right               (),
        delete_word_left           (),
        delete_word_right          (),
        clear_selection            (),
        keep_first_selection_only  (),
        keep_last_selection_only   (),
        keep_first_cursor_only     (),
        keep_last_cursor_only      (),
        keep_oldest_selection_only (),
        keep_newest_selection_only (),
        keep_oldest_cursor_only    (),
        keep_newest_cursor_only    (),
        undo                       (),
        redo                       (),
        set_default_color          (color::Rgba),
        set_default_text_size      (style::Size),
        set_color_bytes            (buffer::Range<Bytes>,color::Rgba),
    }

    Output {
        selection_edit_mode     (selection::Group),
        selection_non_edit_mode (selection::Group),
        text_changed            (),
    }
}



// ============
// === View ===
// ============

/// View for a region of a buffer. There are several cases where multiple views share the same
/// buffer, including displaying the buffer in separate tabs or displaying multiple users in the
/// same file (keeping a view per user and merging them visually).
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct View {
    model   : ViewModel,
    pub frp : Frp,
}

impl Deref for View {
    type Target = ViewModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl View {
    /// Constructor.
    pub fn new(view_buffer:impl Into<ViewBuffer>) -> Self {
        let network = frp::Network::new();
        let model   = ViewModel::new(&network,view_buffer);
        let input   = model.frp.clone_ref();
        let output  = FrpEndpoints::new(&network,input.clone_ref());
        let m       = &model;

        frp::extend! { network
            sel_on_insert            <- input.insert.map(f!((s) m.insert(s)));
            sel_on_paste             <- input.paste.map(f!((s) m.paste(s)));
            sel_on_delete_left       <- input.delete_left.map(f_!(m.delete_left()));
            sel_on_delete_right      <- input.delete_right.map(f_!(m.delete_right()));
            sel_on_delete_word_left  <- input.delete_word_left.map(f_!(m.delete_word_left()));
            sel_on_delete_word_right <- input.delete_word_right.map(f_!(m.delete_word_right()));
            sel_on_delete            <- any(sel_on_delete_left,sel_on_delete_right
                ,sel_on_delete_word_left,sel_on_delete_word_right);
            sel_on_change              <- any(sel_on_insert,sel_on_paste,sel_on_delete);
            has_cursor                 <- sel_on_change.map(|sel| !sel.is_empty());
            output.source.text_changed <+ sel_on_change.gate(&has_cursor).constant(());

            sel_on_move           <- input.cursors_move.map(f!((t) m.moved_selection2(*t,false)));
            sel_on_mod            <- input.cursors_select.map(f!((t) m.moved_selection2(*t,true)));
            sel_on_clear          <- input.clear_selection.constant(default());
            sel_on_keep_last      <- input.keep_last_selection_only.map(f_!(m.last_selection()));
            sel_on_keep_first     <- input.keep_first_selection_only.map(f_!(m.first_selection()));
            sel_on_keep_lst_cursor <- input.keep_last_cursor_only.map(f_!(m.last_cursor()));
            sel_on_keep_fst_cursor <- input.keep_first_cursor_only.map(f_!(m.first_cursor()));

            sel_on_keep_newest       <- input.keep_newest_selection_only.map(f_!(m.newest_selection()));
            sel_on_keep_oldest       <- input.keep_oldest_selection_only.map(f_!(m.oldest_selection()));
            sel_on_keep_newest_cursor <- input.keep_newest_cursor_only.map(f_!(m.newest_cursor()));
            sel_on_keep_oldest_cursor <- input.keep_oldest_cursor_only.map(f_!(m.oldest_cursor()));

            sel_on_set_cursor        <- input.set_cursor.map(f!((t) m.new_cursor(*t).into()));
            sel_on_add_cursor        <- input.add_cursor.map(f!((t) m.add_cursor(*t)));
            sel_on_set_newest_end    <- input.set_newest_selection_end.map(f!((t) m.set_newest_selection_end(*t)));
            sel_on_set_oldest_end    <- input.set_oldest_selection_end.map(f!((t) m.set_oldest_selection_end(*t)));

            sel_on_remove_all <- input.remove_all_cursors.map(|_| default());
            sel_on_undo       <= input.undo.map(f_!(m.undo()));

            eval input.set_default_color     ((t) m.set_default(*t));
            eval input.set_default_text_size ((t) m.set_default(*t));
            eval input.set_color_bytes       (((range,color)) m.replace(range,*color));
            eval input.set_default_color     ((color) m.set_default(*color));

            output.source.selection_edit_mode     <+ sel_on_undo;
            output.source.selection_non_edit_mode <+ sel_on_move;
            output.source.selection_non_edit_mode <+ sel_on_mod;
            output.source.selection_edit_mode     <+ sel_on_clear;
            output.source.selection_non_edit_mode <+ sel_on_keep_last;
            output.source.selection_non_edit_mode <+ sel_on_keep_first;
            output.source.selection_non_edit_mode <+ sel_on_keep_newest;
            output.source.selection_non_edit_mode <+ sel_on_keep_oldest;
            output.source.selection_non_edit_mode <+ sel_on_keep_lst_cursor;
            output.source.selection_non_edit_mode <+ sel_on_keep_fst_cursor;
            output.source.selection_non_edit_mode <+ sel_on_keep_newest_cursor;
            output.source.selection_non_edit_mode <+ sel_on_keep_oldest_cursor;
            output.source.selection_non_edit_mode <+ sel_on_set_cursor;
            output.source.selection_non_edit_mode <+ sel_on_add_cursor;
            output.source.selection_non_edit_mode <+ sel_on_set_newest_end;
            output.source.selection_non_edit_mode <+ sel_on_set_oldest_end;
            output.source.selection_edit_mode     <+ sel_on_insert;
            output.source.selection_edit_mode     <+ sel_on_paste;
            output.source.selection_edit_mode     <+ sel_on_delete;
            output.source.selection_non_edit_mode <+ sel_on_remove_all;

            eval output.source.selection_edit_mode     ((t) m.set_selection(t));
            eval output.source.selection_non_edit_mode ((t) m.set_selection(t));
        }
        let frp = Frp::new(network,output);
        Self {frp,model}
    }
}

impl Default for View {
    fn default() -> Self {
        Self::new(ViewBuffer::default())
    }
}



// =================
// === ViewModel ===
// =================

/// Internal model for the `View`.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct ViewModel {
    pub frp               : FrpInputs,
    pub view_buffer       : ViewBuffer,
    first_view_line_index : Rc<Cell<Line>>,
    view_line_count       : Rc<Cell<usize>>,
}

impl Deref for ViewModel {
    type Target = ViewBuffer;
    fn deref(&self) -> &Self::Target {
        &self.view_buffer
    }
}

impl ViewModel {
    /// Constructor.
    pub fn new(network:&frp::Network, view_buffer:impl Into<ViewBuffer>) -> Self {
        let frp                   = FrpInputs::new(network);
        let view_buffer           = view_buffer.into();
        let first_view_line_index = default();
        let view_line_count       = Rc::new(Cell::new(DEFAULT_LINE_COUNT));
        Self {frp,view_buffer,first_view_line_index,view_line_count}
    }
}

impl ViewModel {
    /// Set the selection to a new value.
    pub fn set_selection(&self, selection:&selection::Group) {
        *self.selection.borrow_mut() = selection.clone();
    }

    /// Return all active selections.
    pub fn selections(&self) -> selection::Group {
        self.selection.borrow().clone()
    }

    /// Return all selections as vector of strings. For cursors, the string will be empty.
    pub fn selections_contents(&self) -> Vec<String> {
        let mut result = Vec::<String>::new();
        for selection in self.byte_selections() {
            result.push(self.buffer.text.sub(selection.range()).into())
        }
        result
    }

    // FIXME: rename
    fn moved_selection2(&self, movement:Option<Transform>, modify:bool) -> selection::Group {
        movement.map(|t| self.moved_selection(t,modify)).unwrap_or_default()
    }

    /// Index of the first line of this buffer view.
    pub fn first_view_line_index(&self) -> Line {
        self.first_view_line_index.get()
    }

    /// Index of the last line of this buffer view.
    pub fn last_view_line_index(&self) -> Line {
        let max_line          = self.last_line_index();
        let view_line_count : Line = self.view_line_count().into();
        max_line.min(self.first_view_line_index() + view_line_count)
    }

    /// Number of lines visible in this buffer view.
    pub fn view_line_count(&self) -> usize {
        self.view_line_count.get()
    }

    /// Range of line indexes of this buffer view.
    pub fn view_line_range(&self) -> Range<Line> {
        self.first_view_line_index() .. self.last_view_line_index()
    }

    /// Byte offset of the first line of this buffer view.
    pub fn first_view_line_byte_offset(&self) -> Bytes {
        self.byte_offset_of_line_index(self.first_view_line_index()).unwrap() // FIXME
    }

    /// Byte offset of the last line of this buffer view.
    pub fn last_view_line_byte_offset(&self) -> Bytes {
        self.byte_offset_of_line_index(self.last_view_line_index()).unwrap()
    }

    /// Byte offset range of lines visible in this buffer view.
    pub fn view_line_byte_offset_range(&self) -> Range<Bytes> {
        self.first_view_line_byte_offset() .. self.last_view_line_byte_offset()
    }

    /// Byte offset of the end of this buffer view. Snapped to the closest valid value.
    pub fn view_end_byte_offset_snapped(&self) -> Bytes {
        self.end_byte_offset_of_line_index_snapped(self.last_view_line_index())
    }

    /// Return the offset after the last character of a given view line if the line exists.
    pub fn end_offset_of_view_line(&self, line:Line) -> Option<Bytes> {
        self.end_byte_offset_of_line_index(line + self.first_view_line_index.get()).ok()
    }

    /// The byte range of this buffer view.
    pub fn view_byte_range(&self) -> Range<Bytes> {
        self.first_view_line_byte_offset() .. self.view_end_byte_offset_snapped()
    }

    /// The byte offset of the given buffer view line index.
    pub fn byte_offset_of_view_line_index(&self, view_line:Line) -> Result<Bytes,BoundsError> {
        let line = self.first_view_line_index() + view_line;
        self.byte_offset_of_line_index(line)
    }

    /// Byte range of the given view line.
    pub fn byte_range_of_view_line_index_snapped(&self, view_line:Line) -> Range<Bytes> {
        let line = view_line + self.first_view_line_index.get();
        self.byte_range_of_line_index_snapped(line)
    }

    /// Return all lines of this buffer view.
    pub fn view_lines(&self) -> Vec<String> {
        self.lines_vec(self.view_byte_range())
    }
}
