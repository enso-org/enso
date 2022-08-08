use crate::entry;
use crate::prelude::*;
use crate::Col;
use crate::Row;
use ensogl_core::data::color;


/// A subset of [`entry::FRP`] endpoints used by the specific highlight [`Handler`].
#[derive(Clone, CloneRef, Debug)]
pub struct EntryEndpoints {
    /// The "is_highlighted" input: may be `is_selected` or `is_hovered`.
    pub flag:     frp::Any<bool>,
    pub location: frp::Stream<(Row, Col)>,
    pub contour:  frp::Stream<entry::Contour>,
    pub color:    frp::Stream<color::Rgba>,
}


/// A guard managing the connections between highlighted entry and the [`Handler`] FRP outputs.
///
/// Until dropped, this structure keeps connected the entry endpoints declaring the highlight
/// appearance (`position`, `contour` and `color`) to the appropriate [`Handler`] endpoints.
/// Also, the entry's flag (`is_selected` or `is_hovered`) will be set to `true` on construction and
/// set back to `false` on drop.
#[derive(Debug)]
struct Guard {
    network:           frp::Network,
    /// An event emitted when we should drop this guard and try to create new with the same
    /// location, for example when the Entry instance is re-used in another location.
    should_be_dropped: frp::Stream,
    dropped:           frp::Source,
}

impl Guard {
    /// Create guard for entry FRP at given location.  
    fn new_for_entry(
        entry_frp: EntryEndpoints,
        row: Row,
        col: Col,
        highlight_frp: &Output,
    ) -> Self {
        let network = frp::Network::new("HighlightedEntryGuard");
        frp::extend! { network
            init <- source_();
            dropped <- source_();
            contour <- all(init, entry_frp.contour)._1();
            color <- all(init, entry_frp.color)._1();
            entry_frp.flag <+ init.constant(true);
            entry_frp.flag <+ dropped.constant(false);
            highlight_frp.contour <+ contour;
            highlight_frp.color <+ color;
            location_change <- entry_frp.location.filter(move |loc| *loc != (row, col));
            should_be_dropped <- location_change.constant(());
        }
        init.emit(());
        Self { network, dropped, should_be_dropped }
    }
}

impl Drop for Guard {
    fn drop(&mut self) {
        self.dropped.emit(());
    }
}


pub trait Getter = Fn(Row, Col) -> Option<EntryEndpoints> + 'static;

#[derive(CloneRef, Clone, Debug)]
pub struct Output {
    pub is_entry_connected: frp::Source<bool>,
    pub contour:            frp::Any<entry::Contour>,
    pub color:              frp::Any<color::Rgba>,
}

impl Output {
    pub fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            is_entry_connected <- source::<bool>();
            contour <- any(...);
            color <- any(...);
        }
        Self { is_entry_connected, contour, color }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct ConnectedEntry<EntryGetter> {
    #[derivative(Debug = "ignore")]
    entry_getter: EntryGetter,
    output:       Output,
    location:     Cell<Option<(Row, Col)>>,
    guard:        RefCell<Option<Guard>>,
}

impl<EntryGetter> ConnectedEntry<EntryGetter> {
    pub fn new(entry_getter: EntryGetter, output: Output) -> Rc<Self> {
        Rc::new(Self { entry_getter, output, location: default(), guard: default() })
    }

    /// Drop old [`ConnectedEntryGuard`] and create a new one for new highlighted entry.
    pub fn connect_new_highlighted_entry(self: &Rc<Self>, location: Option<(Row, Col)>)
    where EntryGetter: Getter {
        if let Some((row, col)) = location {
            let current_loc = self.location.get();
            let loc_changed = current_loc.map_or(true, |loc| loc != (row, col));
            if loc_changed {
                self.guard.take();
                let entry = (self.entry_getter)(row, col);
                if let Some(entry) = entry {
                    *self.guard.borrow_mut() =
                        Some(Guard::new_for_entry(entry, row, col, &self.output));
                    self.location.set(Some((row, col)));
                    self.output.is_entry_connected.emit(true);
                    self.set_up_guard_dropping();
                }
            }
        } else {
            self.drop_guard()
        }
    }

    pub fn drop_guard(&self) {
        self.location.set(None);
        self.guard.take();
        self.output.is_entry_connected.emit(false);
    }

    pub fn set_up_guard_dropping(self: &Rc<Self>)
    where EntryGetter: 'static {
        if let Some(guard) = &*self.guard.borrow() {
            let network = &guard.network;
            let this = self;
            frp::extend! { network
                eval_ guard.should_be_dropped (this.drop_guard());
            }
        }
    }
}
