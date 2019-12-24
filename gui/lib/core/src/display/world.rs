pub mod scene;
pub mod workspace;

use crate::prelude::*;

pub use crate::data::container::*;
pub use crate::display::world::workspace::SymbolId;

use crate::closure;
use crate::control::callback::CallbackHandle;
use crate::control::event_loop::EventLoop;
use crate::data::opt_vec::OptVec;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::promote_all;
use crate::promote_workspace_types;
use crate::promote;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::display::shape::text::font::Fonts;

use eval_tt::*;



// =============
// === World ===
// =============

// === Definition === 

/// World is the top-level structure managing several instances of `Workspace`.
/// It is responsible for updating the system on every animation frame.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct World {
    pub workspaces      : OptVec<Workspace>,
    pub workspace_dirty : WorkspaceDirty,
    pub logger          : Logger,
    pub event_loop      : EventLoop,
    pub fonts           : Fonts,
    pub update_handle   : Option<CallbackHandle>,
    pub self_reference  : Option<WorldRef>
}


// === Types ===

pub type WorkspaceID    = usize;
pub type WorkspaceDirty = dirty::SharedSet<WorkspaceID>;
promote_workspace_types!{ [[WorkspaceOnChange]] workspace }


// === Callbacks ===

closure! {
fn workspace_on_change(dirty:WorkspaceDirty, ix:WorkspaceID) -> WorkspaceOnChange {
    || dirty.set(ix)
}}


// === Implementation ===

impl World {
    /// Create and initialize new world instance.
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> WorldRef {
        println!("NOTICE! When profiling in Chrome check 'Disable JavaScript Samples' under the \
                  gear icon in the 'Performance' tab. It can drastically slow the reading.");
        let world_ref  = WorldRef::new(Self::new_uninitialized());
        let world_ref2 = world_ref.clone_rc();
        let world_ref3 = world_ref.clone_rc();
        with(world_ref.borrow_mut(), |mut data| {
            let update          = move || world_ref2.borrow_mut().update();
            let update_handle   = data.event_loop.add_callback(update);
            data.update_handle  = Some(update_handle);
            data.self_reference = Some(world_ref3);
        });
        world_ref
    }

    /// Create new uninitialized world instance. You should rather not need to
    /// call this function directly.
    fn new_uninitialized() -> Self {
        let workspaces       = default();
        let fonts            = Fonts::new();
        let logger           = Logger::new("world");
        let workspace_logger = logger.sub("workspace_dirty");
        let workspace_dirty  = WorkspaceDirty::new(workspace_logger,());
        let event_loop       = EventLoop::new();
        let update_handle    = default();
        let self_reference   = default();
        Self {workspaces,workspace_dirty,logger,event_loop,fonts,update_handle,self_reference}
    }

    /// Add new workspace and get its ID.
    pub fn add_workspace(&mut self, name: &str) -> WorkspaceID {
        let logger = &self.logger;
        let dirty  = &self.workspace_dirty;
        self.workspaces.insert_with_ix(|ix| {
            group!(logger, format!("Adding workspace {} ({}).", ix, name), {
                let on_change     = workspace_on_change(dirty.clone_ref(),ix);
                let wspace_logger = logger.sub(ix.to_string());
                Workspace::new(name,wspace_logger,on_change).unwrap() // FIXME
            })
        })
    }

    /// Dispose the workspace by the provided ID. In case of invalid ID, a 
    /// warning will be emitted.
    pub fn drop_workspace(&mut self, id: WorkspaceID) {
        let logger = &self.logger;
        let item   = self.workspaces.remove(id);
        match item {
            None => logger.warning("Trying to delete non-existing workspace."),
            Some(item) => group!(logger, "Dropping workspace {}.", id, {
                let _destruct_it_here = item;
            }),
        }
    }

    /// Run the provided callback on every frame. Returns a `CallbackHandle`,
    /// which when dropped will cancel the callback. If you want the function
    /// to run forever, you can use the `forget` method in the handle.
    pub fn on_frame<F:FnMut(&mut World)+'static>
    (&mut self, mut callback: F) -> CallbackHandle {
        let this = self.self_reference.as_ref().unwrap().clone_rc();
        let func = move || callback(&mut this.borrow_mut());
        self.event_loop.add_callback(func)
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        if self.workspace_dirty.check_all() {
            group!(self.logger, "Updating.", {
        // FIXME render only needed workspaces.
        self.workspace_dirty.unset_all();
        let fonts = &mut self.fonts;
        self.workspaces.iter_mut().for_each(|t| t.update(fonts));
            });
        }
    }

    // [Adam Obuchowicz]
    // So, I'm worried about this manual memory handling. For instance, I'm not sure that page
    // refresh clears all allocated wasm memory, because when I run examples/tests I got
    // performance drop and even crashes when I refresh browser tab many times.
    //
    // Normally the Rc for such object should be passed from one request_animation_frame to another
    // until the end of callback's life. Maybe we should do here a similar approach?
    /// Dispose the world object, cancel all handlers and events.
    pub fn dispose(&mut self) {
        self.self_reference = None;
        self.update_handle  = None;
    }
}

impl Add<workspace::WorkspaceBuilder> for World {
    type Result = WorkspaceID;
    /// Add new workspace to the world.
    fn add(&mut self, bldr:workspace::WorkspaceBuilder) -> Self::Result {
        let name   = bldr.name;
        let logger = &self.logger;
        let dirty  = &self.workspace_dirty;
        self.workspaces.insert_with_ix(|ix| {
            group!(logger, format!("Adding workspace {} ({}).", ix, name), {
                let on_change = workspace_on_change(dirty.clone(), ix);
                let wspace_logger = logger.sub(ix.to_string());
                Workspace::new(name, wspace_logger, on_change).unwrap() // FIXME
            })
        })
    }
}

impl Extend<workspace::WorkspaceBuilder> for World {
    fn extend<T: IntoIterator<Item=workspace::WorkspaceBuilder>>
    (&mut self, iter: T) {
        iter.into_iter().for_each(|t| { self.add(t); });
    }
}

impl Index<usize> for World {
    type Output = Workspace;
    fn index(&self, ix: usize) -> &Self::Output {
        self.workspaces.index(ix)
    }
}

impl IndexMut<usize> for World {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        self.workspaces.index_mut(ix)
    }
}

impl Drop for World {
    fn drop(&mut self) {
        self.logger.info("Dropping.");
    }
}



// ================
// === WorldRef ===
// ================

// === Definition ===

/// Shared reference to the `World` object.
#[derive(Shrinkwrap)]
#[derive(Debug)]
pub struct WorldRef {
    pub rc: Rc<RefCell<World>>,
}

impl WorldRef {
    /// Create new shared reference.
    pub fn new(world:World) -> Self {
        let rc = Rc::new(RefCell::new(world));
        Self {rc}
    }
    /// Dispose the world object, cancel all handlers and events.
    pub fn dispose(&self) {
        self.borrow_mut().dispose()
    }
}

impl<T> Add<T> for WorldRef where World: Add<T> {
    type Result = AddResult<World,T>;
    /// Add a new element to the world.
    fn add(&mut self, t:T) -> Self::Result {
        self.borrow_mut().add(t)
    }
}


// === Instances ===

impl From<Rc<RefCell<World>>> for WorldRef {
    fn from(rc: Rc<RefCell<World>>) -> Self {
        Self {rc}
    }
}
