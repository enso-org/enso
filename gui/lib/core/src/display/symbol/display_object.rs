use crate::prelude::*;

use crate::data::function::callback::*;
use crate::data::opt_vec::OptVec;
use crate::closure;
use crate::dirty;
use crate::dirty::traits::*;
use crate::system::web::group;

use nalgebra::{Vector3, Vector4, Matrix4, Perspective3};
use basegl_system_web::Logger;



// TODO: Refactor and describe

// === CloneRef ===

pub trait CloneRef {
    fn clone_ref(&self) -> Self;
}




// =================
// === AxisOrder ===
// =================

/// Defines the order in which particular axis coordinates are processed. Used
/// for example to define the rotation order in `DisplayObject`.
#[derive(Clone,Debug)]
pub enum AxisOrder { XYZ, XZY, YXZ, YZX, ZXY, ZYX }

impl Default for AxisOrder {
    fn default() -> Self { Self::XYZ }
}



// ======================
// === TransformOrder ===
// ======================

/// Defines the order in which transformations (scale, rotate, translate) are
/// applied to a particular object.
#[derive(Clone,Debug)]
pub enum TransformOrder {
    ScaleRotateTranslate,
    ScaleTranslateRotate,
    RotateScaleTranslate,
    RotateTranslateScale,
    TranslateRotateScale,
    TranslateScaleRotate
}

impl Default for TransformOrder {
    fn default() -> Self { Self::ScaleRotateTranslate }
}



// =================
// === Transform ===
// =================

/// Structure describing transform of an object, in particular its position, scale, and rotation.
/// You can use methods like `matrix` to get a combined transformation matrix. Bear in mind that
/// the matrix will always be recomputed from scratch. This structure does not contain any caching
/// mechanisms.
#[derive(Clone,Debug)]
pub struct Transform {
    pub position        : Vector3<f32>,
    pub scale           : Vector3<f32>,
    pub rotation        : Vector3<f32>,
    pub transform_order : TransformOrder,
    pub rotation_order  : AxisOrder,
}

impl Default for Transform {
    fn default() -> Self {
        let position        = Vector3::new(0.0,0.0,0.0);
        let scale           = Vector3::new(1.0,1.0,1.0);
        let rotation        = Vector3::new(0.0,0.0,0.0);
        let transform_order = default();
        let rotation_order  = default();
        Self {position,scale,rotation,transform_order,rotation_order}
    }
}

impl Transform {
    /// Creates a new transformation object.
    pub fn new() -> Self { default() }

    /// Computes transformation matrix from the provided scale, rotation, and
    /// translation components, based on the transformation and rotation orders.
    pub fn matrix(&self) -> Matrix4<f32> {
        let mut matrix = Matrix4::identity();
        let matrix_ref = &mut matrix;
        match self.transform_order {
            TransformOrder::ScaleRotateTranslate => {
                self.append_scale       (matrix_ref);
                self.append_rotation    (matrix_ref);
                self.append_translation (matrix_ref);
            }
            TransformOrder::ScaleTranslateRotate => {
                self.append_scale       (matrix_ref);
                self.append_translation (matrix_ref);
                self.append_rotation    (matrix_ref);
            }
            TransformOrder::RotateScaleTranslate => {
                self.append_rotation    (matrix_ref);
                self.append_scale       (matrix_ref);
                self.append_translation (matrix_ref);
            }
            TransformOrder::RotateTranslateScale => {
                self.append_rotation    (matrix_ref);
                self.append_translation (matrix_ref);
                self.append_scale       (matrix_ref);
            }
            TransformOrder::TranslateRotateScale => {
                self.append_translation (matrix_ref);
                self.append_rotation    (matrix_ref);
                self.append_scale       (matrix_ref);
            }
            TransformOrder::TranslateScaleRotate => {
                self.append_translation (matrix_ref);
                self.append_scale       (matrix_ref);
                self.append_rotation    (matrix_ref);
            }
        }
        matrix
    }

    /// Computes a rotation matrix from the provided rotation values based on
    /// the rotation order.
    pub fn rotation_matrix(&self) -> Matrix4<f32> {
        let rx = Matrix4::from_scaled_axis(Vector3::x() * self.rotation.x);
        let ry = Matrix4::from_scaled_axis(Vector3::y() * self.rotation.y);
        let rz = Matrix4::from_scaled_axis(Vector3::z() * self.rotation.z);
        match self.rotation_order {
            AxisOrder::XYZ => rz * ry * rx,
            AxisOrder::XZY => ry * rz * rx,
            AxisOrder::YXZ => rz * rx * ry,
            AxisOrder::YZX => rx * rz * ry,
            AxisOrder::ZXY => ry * rx * rz,
            AxisOrder::ZYX => rx * ry * rz,
        }
    }

    fn append_translation(&self, m:&mut Matrix4<f32>) {
        m.append_translation_mut(&self.position);
    }

    fn append_rotation(&self, m:&mut Matrix4<f32>) {
        *m = self.rotation_matrix() * (*m);
    }

    fn append_scale(&self, m:&mut Matrix4<f32>) {
        m.append_nonuniform_scaling_mut(&self.scale);
    }
}



// =======================
// === CachedTransform ===
// =======================

/// The same as `Transform` but with caching. It contains cached transformation matrix and dirty
/// flags which are set after fields are modified. You can use the `update` function to recompute
/// the matrix.
#[derive(Derivative)]
#[derivative(Clone(bound=""))]
#[derivative(Debug(bound=""))]
pub struct CachedTransform<OnChange> {
    transform        : Transform,
    transform_matrix : Matrix4<f32>,
    origin           : Matrix4<f32>,
    matrix           : Matrix4<f32>,
    pub dirty        : dirty::SharedBool<OnChange>,
    pub logger       : Logger,
}

impl<OnChange> CachedTransform<OnChange> {
    pub fn new(logger:Logger, on_change:OnChange) -> Self {
        let logger_dirty     = logger.sub("dirty");
        let transform        = default();
        let transform_matrix = Matrix4::identity();
        let origin           = Matrix4::identity();
        let matrix           = Matrix4::identity();
        let dirty            = dirty::SharedBool::new(logger_dirty,on_change);
        Self {transform,transform_matrix,origin,matrix,dirty,logger}
    }

    /// Update the transformation matrix and return information if the data was really updated.
    pub fn update(&mut self, new_origin:Option<&Matrix4<f32>>) -> bool {
        let is_dirty       = self.dirty.check_all();
        let origin_changed = new_origin.is_some();
        let changed        = is_dirty || origin_changed;
        if changed {
            group!(self.logger, "Update.", {
                if is_dirty {
                    self.transform_matrix = self.transform.matrix();
                    self.dirty.unset_all();
                }
                new_origin.iter().for_each(|t| self.origin = **t);
                self.matrix = self.origin * self.transform_matrix;
            })
        }
        changed
    }
}


// === Getters ===

impl<OnChange> CachedTransform<OnChange> {
    pub fn position(&self) -> &Vector3<f32> {
        &self.transform.position
    }

    pub fn rotation(&self) -> &Vector3<f32> {
        &self.transform.rotation
    }

    pub fn scale(&self) -> &Vector3<f32> {
        &self.transform.scale
    }

    pub fn matrix(&self) -> &Matrix4<f32> {
        &self.matrix
    }

    pub fn global_position(&self) -> Vector3<f32> {
        (self.matrix * Vector4::new(0.0,0.0,0.0,1.0)).xyz()
    }
}


// === Setters ===

impl<OnChange:Callback0> CachedTransform<OnChange> {
    pub fn position_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.position
    }

    pub fn rotation_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.rotation
    }

    pub fn scale_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.scale
    }

    pub fn set_position(&mut self, t:Vector3<f32>) {
        *self.position_mut() = t;
    }

    pub fn set_rotation(&mut self, t:Vector3<f32>) {
        *self.rotation_mut() = t;
    }

    pub fn set_scale(&mut self, t:Vector3<f32>) {
        *self.scale_mut() = t;
    }

    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        f(self.position_mut())
    }

    pub fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        f(self.rotation_mut())
    }

    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        f(self.scale_mut())
    }
}



// ==================
// === ParentBind ===
// ==================

/// Description of parent-child relation. It contains reference to parent node and information
/// about the child index there. It is used when a child is reconnected to different parent to
/// update the old parent with the information that the child was removed.
#[derive(Clone,Debug)]
pub struct ParentBind {
    pub parent : DisplayObjectData,
    pub index  : usize
}

impl ParentBind {
    pub fn dispose(&self) {
        self.parent.remove_child_by_index(self.index);
    }
}



// =====================================
// === HierarchicalObjectDescription ===
// =====================================

///
#[derive(Clone,Debug)]
pub struct HierarchicalObjectData {
    pub parent_bind : Option<ParentBind>,
    pub children    : OptVec<DisplayObjectData>,
    pub logger      : Logger,
}

// === Public API ===

impl HierarchicalObjectData {
    pub fn new(logger:Logger) -> Self {
        let parent_bind = default();
        let children    = default();
        Self {parent_bind,children,logger}
    }
}

// === Private API ===

impl HierarchicalObjectData {
    fn take_parent_bind(&mut self) -> Option<ParentBind> {
        self.parent_bind.take()
    }

    fn set_parent_bind(&mut self, bind:ParentBind) {
        self.parent_bind = Some(bind);
    }

    fn register_child<T:DisplayObject>(&mut self, child:T) -> usize {
        let child = child.display_object_description();
        self.children.insert(child)
    }

    pub fn remove_child_by_index(&mut self, index:usize) {
        let opt_child = self.children.remove(index);
        opt_child.for_each(|t| t.raw_unset_parent());
    }
}

// === Getters ===

impl DisplayObjectDataMut {
    pub fn parent(&self) -> Option<&DisplayObjectData> {
        self.parent_bind.as_ref().map(|ref t| &t.parent)
    }
}


// ======================================
// === LazyTransformObjectDescription ===
// ======================================

/// Internal representation of `ObjectData`. See it's documentation for more information.
#[derive(Derivative,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Debug)]
pub struct DisplayObjectDataMut {
    #[shrinkwrap(main_field)]
    pub wrapped          : HierarchicalObjectData,
    pub transform        : CachedTransform<Option<OnChange>>,
    pub child_dirty      : ChildDirty,
    pub new_parent_dirty : NewParentDirty,
    #[derivative(Debug="ignore")]
    pub on_updated       : Option<Box<dyn Fn(&DisplayObjectDataMut)>>,
}


// === Types ===

pub type ChildDirty     = dirty::SharedSet<usize,Option<OnChange>>;
pub type NewParentDirty = dirty::SharedBool<()>;
pub type TransformDirty = dirty::SharedBool<Option<OnChange>>;


// === Callbacks ===

closure! {
fn fn_on_change(dirty:ChildDirty, ix:usize) -> OnChange { || dirty.set(ix) }
}


// === API ===

impl DisplayObjectDataMut {
    pub fn new(logger:Logger) -> Self {
        let transform        = CachedTransform :: new(logger.sub("transform")       ,None);
        let child_dirty      = ChildDirty      :: new(logger.sub("child_dirty")     ,None);
        let new_parent_dirty = NewParentDirty  :: new(logger.sub("new_parent_dirty"),());
        let wrapped          = HierarchicalObjectData::new(logger);
        let on_updated       = None;
        Self {wrapped,transform,child_dirty,new_parent_dirty,on_updated}
    }

    pub fn update(&mut self) {
        let origin0 = Matrix4::identity();
        self.update_with(&origin0,false)
    }

    pub fn update_with(&mut self, parent_origin:&Matrix4<f32>, force:bool) {
        let use_origin = force || self.new_parent_dirty.check();
        let new_origin = use_origin.as_some(parent_origin);
        let msg        = match new_origin {
            Some(_) => "Update with new parent origin.",
            None    => "Update with old parent origin."
        };
        group!(self.logger, msg, {
            let origin_changed = self.transform.update(new_origin);
            let origin         = &self.transform.matrix;
            if origin_changed {
                self.logger.info("Self origin changed.");
                if !self.children.is_empty() {
                    group!(self.logger, "Updating all children.", {
                        self.children.iter().for_each(|child| {
                            child.update_with(origin,true);
                        });
                    })
                }
            } else {
                self.logger.info("Self origin did not change.");
                if self.child_dirty.check_all() {
                    group!(self.logger, "Updating dirty children.", {
                        self.child_dirty.iter().for_each(|ix| {
                            self.children[*ix].update_with(origin,false)
                        });
                    })
                }
            }
            self.child_dirty.unset_all();
        });
        self.new_parent_dirty.unset();
        if let Some(f) = &self.on_updated { f(self) }
    }
}

// === Private API ===

impl DisplayObjectDataMut {
    fn register_child<T:DisplayObject>(&mut self, child:T) -> usize {
        let index = self.wrapped.register_child(child);
        self.child_dirty.set(index);
        index
    }

    fn remove_child_by_index(&mut self, index:usize) {
        self.wrapped.remove_child_by_index(index);
        self.child_dirty.unset(&index);
    }

    fn raw_unset_parent(&mut self) {
        self.logger.info("Removing parent bind.");
        self.transform.dirty.set_callback(None);
        self.child_dirty.set_callback(None);
        self.new_parent_dirty.set();
        self.wrapped.take_parent_bind();
    }

    fn set_parent_bind(&mut self, bind:ParentBind) {
        self.logger.info("Adding new parent bind.");
        let dirty     = bind.parent.rc.borrow().child_dirty.clone_rc();
        let on_change = fn_on_change(dirty, bind.index);
        self.transform.dirty.set_callback(Some(on_change.clone()));
        self.child_dirty.set_callback(Some(on_change));
        self.new_parent_dirty.set();
        self.wrapped.set_parent_bind(bind);
    }
}


// === Getters ===

impl DisplayObjectDataMut {
    pub fn global_position(&self) -> Vector3<f32> {
        self.transform.global_position()
    }

    pub fn position(&self) -> &Vector3<f32> {
        self.transform.position()
    }

    pub fn scale(&self) -> &Vector3<f32> {
        self.transform.scale()
    }

    pub fn rotation(&self) -> &Vector3<f32> {
        self.transform.rotation()
    }

    pub fn matrix(&self) -> &Matrix4<f32> {
        self.transform.matrix()
    }
}


// === Setters ===

impl DisplayObjectDataMut {
    pub fn position_mut(&mut self) -> &mut Vector3<f32> {
        self.transform.position_mut()
    }

    pub fn scale_mut(&mut self) -> &mut Vector3<f32> {
        self.transform.scale_mut()
    }

    pub fn rotation_mut(&mut self) -> &mut Vector3<f32> {
        self.transform.rotation_mut()
    }

    pub fn set_position(&mut self, t:Vector3<f32>) {
        self.transform.set_position(t);
    }

    pub fn set_scale(&mut self, t:Vector3<f32>) {
        self.transform.set_scale(t);
    }

    pub fn set_rotation(&mut self, t:Vector3<f32>) {
        self.transform.set_rotation(t);
    }

    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_position(f)
    }

    pub fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_rotation(f)
    }

    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_scale(f)
    }

    pub fn set_on_updated<F:Fn(&DisplayObjectDataMut)+'static>(&mut self, f:F) {
        self.on_updated = Some(Box::new(f))
    }
}



// ================================
// === DisplayObjectDescription ===
// ================================

/// A hierarchical representation of object containing a position, a scale and a rotation.
///
/// # Safety
/// Please note that you will get runtime crash when running the `update` function if your object
/// hierarchy forms a loop, for example, `obj2` is child of `obj1`, while `obj1` is child of `obj2`.
/// It is not easy to discover such situations, but maybe it will be worth to add some additional
/// safety on top of that in the future.
#[derive(Clone,Debug)]
pub struct DisplayObjectData {
    rc: Rc<RefCell<DisplayObjectDataMut>>,
}

// === Public API ==

impl DisplayObjectData {
    /// Creates a new object instance.
    pub fn new(logger:Logger) -> Self {
        let data = DisplayObjectDataMut::new(logger);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update(&self) {
        self.rc.borrow_mut().update();
    }

    /// Adds a new `DisplayObject` as a child to the current one.
    pub fn add_child<T:DisplayObject>(&self, child:T) {
        child.display_object_description().set_parent(self);
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    pub fn remove_child<T:DisplayObject>(&self, child:T) {
        let child = child.display_object_description();
        if self.has_child(&child) {
            child.unset_parent()
        }
    }

    /// Replaces the parent binding with a new parent.
    pub fn set_parent<T:DisplayObject>(&self, parent:T) {
        group!(self.rc.borrow().logger, "Setting new parent.", {
            self.unset_parent();
            let parent      = parent.display_object_description();
            let index       = parent.rc.borrow_mut().register_child(self);
            let parent_bind = ParentBind {parent,index};
            self.rc.borrow().logger.info(|| format!("Child index is {}.", index));
            self.set_parent_bind(parent_bind);
        })
    }

    /// Removes the current parent binding.
    pub fn unset_parent(&self) {
        self.take_parent_bind().for_each(|t| t.dispose());
    }

    /// Checks if the provided object is child of the current one.
    pub fn has_child<T:DisplayObject>(&self, child:T) -> bool {
        self.child_index(child).is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T:DisplayObject>(&self, child:T) -> Option<usize> {
        let child = child.display_object_description();
        child.parent_bind().and_then(|bind| {
            if self == &bind.parent { Some(bind.index) } else { None }
        })
    }
}


// === Private API ===

impl DisplayObjectData {

    /// Updates object transformations by providing a new origin location. See docs of `update` to
    /// learn more.
    fn update_with(&self, new_origin:&Matrix4<f32>, force:bool) {
        self.rc.borrow_mut().update_with(new_origin,force);
    }

    fn take_parent_bind(&self) -> Option<ParentBind> {
        self.rc.borrow_mut().take_parent_bind()
    }

    /// Gets a reference to a parent bind description, if exists.
    fn parent_bind(&self) -> Option<ParentBind> {
        self.rc.borrow().parent_bind.clone()
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, parent:ParentBind) {
        self.rc.borrow_mut().set_parent_bind(parent);
    }

    /// Removes the binding to the parent object.
    fn raw_unset_parent(&self) {
        self.rc.borrow_mut().raw_unset_parent();
    }

    /// Removes child by a given index. Does nothing if the index was incorrect. Please use the
    /// `remove_child` method unless you are 100% sure that the index is correct.
    fn remove_child_by_index(&self, index:usize) {
        self.rc.borrow_mut().remove_child_by_index(index);
    }
}


// === Getters ===

impl DisplayObjectData {
    pub fn parent(&self) -> Option<DisplayObjectData> {
        self.rc.borrow().parent().map(|t| t.clone_ref())
    }

    pub fn index(&self) -> Option<usize> {
        self.parent_bind().map(|t| t.index)
    }

    pub fn global_position(&self) -> Vector3<f32> {
        self.rc.borrow().global_position()
    }

    pub fn position(&self) -> Vector3<f32> {
        *self.rc.borrow().position()
    }

    pub fn scale(&self) -> Vector3<f32> {
        *self.rc.borrow().scale()
    }

    pub fn rotation(&self) -> Vector3<f32> {
        *self.rc.borrow().rotation()
    }

    pub fn matrix(&self) -> Matrix4<f32> {
        *self.rc.borrow().matrix()
    }
}


// === Setters ===

impl DisplayObjectData {
    pub fn set_position(&self, t:Vector3<f32>) {
        self.rc.borrow_mut().set_position(t);
    }

    pub fn set_scale(&self, t:Vector3<f32>) {
        self.rc.borrow_mut().set_scale(t);
    }

    pub fn set_rotation(&self, t:Vector3<f32>) {
        self.rc.borrow_mut().set_rotation(t);
    }

    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_position(f)
    }

    pub fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_rotation(f)
    }

    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_scale(f)
    }

    pub fn set_on_updated<F:Fn(&DisplayObjectDataMut)+'static>(&self, f:F) {
        self.rc.borrow_mut().set_on_updated(f)
    }
}


// === Instances ===

impl From<&DisplayObjectData> for DisplayObjectData {
    fn from(t:&DisplayObjectData) -> Self { t.clone_ref() }
}

impl From<Rc<RefCell<DisplayObjectDataMut>>> for DisplayObjectData {
    fn from(rc: Rc<RefCell<DisplayObjectDataMut>>) -> Self {
        Self {rc}
    }
}

impl PartialEq for DisplayObjectData {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc,&other.rc)
    }
}

impl CloneRef for DisplayObjectData {
    fn clone_ref(&self) -> Self {
        self.clone()
    }
}


// =====================
// === DisplayObject ===
// =====================

pub trait DisplayObject: Into<DisplayObjectData> {
    fn display_object_description(self) -> DisplayObjectData {
        self.into()
    }
}

impl<T:Into<DisplayObjectData>> DisplayObject for T {}


pub trait DisplayObjectOps where for<'t> &'t Self:DisplayObject {
    fn add_child<T:DisplayObject>(&self, child:T) {
        self.display_object_description().add_child(child)
    }
}

impl<T> DisplayObjectOps for T where for<'t> &'t Self:DisplayObject {}


// ==========================================================



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::{PI};


    #[test]
    fn hierarchy_test() {
        let obj1 = DisplayObjectData::new(Logger::new("obj1"));
        let obj2 = DisplayObjectData::new(Logger::new("obj2"));
        let obj3 = DisplayObjectData::new(Logger::new("obj3"));

        obj1.add_child(&obj2);
        assert_eq!(obj2.index(), Some(0));
        obj1.add_child(&obj2);
        assert_eq!(obj2.index(), Some(0));
        obj1.add_child(&obj3);
        assert_eq!(obj3.index(), Some(1));
        obj1.remove_child(&obj3);
        assert_eq!(obj3.index(), None);
    }

    #[test]
    fn transformation_test() {
        let obj1 = DisplayObjectData::new(Logger::new("obj1"));
        let obj2 = DisplayObjectData::new(Logger::new("obj2"));
        let obj3 = DisplayObjectData::new(Logger::new("obj3"));

        assert_eq!(obj1.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj1.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(0.0,0.0,0.0));
        obj1.mod_position(|t| t.x += 7.0);
        obj1.add_child(&obj2);
        obj2.add_child(&obj3);
        assert_eq!(obj1.position()        , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj1.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(0.0,0.0,0.0));
        obj1.update();
        assert_eq!(obj1.position()        , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(7.0,0.0,0.0));
        obj2.mod_position(|t| t.y += 5.0);
        obj1.update();
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(7.0,5.0,0.0));
        obj3.mod_position(|t| t.x += 1.0);
        obj1.update();
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(8.0,5.0,0.0));
        obj2.mod_rotation(|t| t.z += PI/2.0);
        obj1.update();
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(7.0,6.0,0.0));
        obj1.add_child(&obj3);
        obj1.update();
        assert_eq!(obj3.global_position() , Vector3::new(8.0,0.0,0.0));
        obj1.remove_child(&obj3);
        obj3.update();
        assert_eq!(obj3.global_position() , Vector3::new(1.0,0.0,0.0));
        obj2.add_child(&obj3);
        obj1.update();
        assert_eq!(obj3.global_position() , Vector3::new(7.0,6.0,0.0));
        obj1.remove_child(&obj3);
        obj1.update();
        obj2.update();
        obj3.update();
        assert_eq!(obj3.global_position() , Vector3::new(7.0,6.0,0.0));
    }
}


// =================
// === Alignment ===
// =================

#[derive(Clone,Debug)]
pub struct Alignment {
    pub horizontal : HorizontalAlignment,
    pub vertical   : VerticalAlignment,
}

#[derive(Clone,Debug)]
pub enum HorizontalAlignment {Left,Center,Right}

#[derive(Clone,Debug)]
pub enum VerticalAlignment {Top,Center,Bottom}

impl Default for HorizontalAlignment { fn default() -> Self { Self::Center } }
impl Default for VerticalAlignment   { fn default() -> Self { Self::Center } }
impl Default for Alignment {
    fn default() -> Self {
        let horizontal = default();
        let vertical   = default();
        Self {horizontal,vertical}
    }
}



// ==============
// === Screen ===
// ==============

#[derive(Clone,Debug)]
pub struct Screen {
    pub width  : f32,
    pub height : f32,
}

impl Default for Screen {
    fn default() -> Self {
        let width  = 100.0;
        let height = 100.0;
        Self {width,height}
    }
}


// ==================
// === Projection ===
// ==================

#[derive(Clone,Debug)]
pub enum Projection {
    Perspective {fov:f32},
    Orthographic
}

impl Default for Projection {
    fn default() -> Self {
        Self::Perspective {fov:45.0f32.to_radians()}
    }
}



// ================
// === Clipping ===
// ================

#[derive(Clone,Debug)]
pub struct Clipping {
    pub near : f32,
    pub far  : f32
}

impl Default for Clipping {
    fn default() -> Self {
        let near = 0.0;
        let far  = 1000.0;
        Self {near,far}
    }
}



// ====================
// === Camera2DData ===
// ====================

/// Internal `Camera2D` representation. Please see `Camera2D` for full documentation.
#[derive(Clone,Debug)]
pub struct Camera2DData {
    pub transform          : DisplayObjectData,
    screen                 : Screen,
    zoom                   : f32,
    native_z               : f32,
    alignment              : Alignment,
    projection             : Projection,
    clipping               : Clipping,
    view_matrix            : Matrix4<f32>,
    projection_matrix      : Matrix4<f32>,
    view_projection_matrix : Matrix4<f32>,
    projection_dirty       : ProjectionDirty,
    transform_dirty        : TransformDirty2
}

type ProjectionDirty = dirty::SharedBool<()>;
type TransformDirty2 = dirty::SharedBool<()>;

impl Camera2DData {
    pub fn new(logger: Logger) -> Self {
        let screen                 = default();
        let projection             = default();
        let clipping               = default();
        let alignment              = default();
        let zoom                   = 2.0;
        let native_z               = 1.0;
        let view_matrix            = Matrix4::identity();
        let projection_matrix      = Matrix4::identity();
        let view_projection_matrix = Matrix4::identity();
        let projection_dirty       = ProjectionDirty::new(logger.sub("projection_dirty"),());
        let transform_dirty        = TransformDirty2::new(logger.sub("transform_dirty"),());
        let transform_dirty_copy   = transform_dirty.clone_rc();
        let transform              = DisplayObjectData::new(logger);
        transform.set_on_updated(move |_| { transform_dirty_copy.set(); });
        transform.mod_position(|p| p.z = 1.0);
        projection_dirty.set();
        Self {transform,screen,projection,clipping,alignment,zoom,native_z,view_matrix
             ,projection_matrix,view_projection_matrix,projection_dirty,transform_dirty}
    }

    pub fn recompute_view_matrix(&mut self) {
        // TODO: Handle all alignments.
        let mut transform       = self.transform.matrix();
        let div                 = 2.0 * self.zoom;
        let alignment_transform = Vector3::new(self.screen.width/div, self.screen.height/div, 0.0);
        transform.append_translation_mut(&alignment_transform);
        self.view_matrix = transform.try_inverse().unwrap()
    }

    pub fn recompute_projection_matrix(&mut self) {
        self.projection_matrix = match &self.projection {
            Projection::Perspective {fov} => {
                let aspect = self.screen.width / self.screen.height;
                let near   = self.clipping.near;
                let far    = self.clipping.far;
                *Perspective3::new(aspect,*fov,near,far).as_matrix()
            }
            _ => unimplemented!()
        };
    }

    // https://github.com/rust-lang/rust-clippy/issues/4914
    #[allow(clippy::useless_let_if_seq)]
    pub fn update(&mut self) -> bool {
        self.transform.update();
        let mut changed = false;
        if self.transform_dirty.check() {
            self.recompute_view_matrix();
            self.transform_dirty.unset();
            changed = true;
        }
        if self.projection_dirty.check() {
            self.recompute_projection_matrix();
            self.projection_dirty.unset();
            changed = true;
        }
        if changed {
            self.view_projection_matrix = self.projection_matrix * self.view_matrix;
        }
        changed
    }
}

// === Getters ===

impl Camera2DData {
    pub fn view_projection_matrix (&self) -> &Matrix4<f32> { &self.view_projection_matrix }
}

// === Setters ===

impl Camera2DData {
    pub fn projection_mut(&mut self) -> &mut Projection {
        self.projection_dirty.set();
        &mut self.projection
    }

    pub fn clipping_mut(&mut self) -> &mut Clipping {
        self.projection_dirty.set();
        &mut self.clipping
    }

    pub fn set_screen(&mut self, width:f32, height:f32) {
        self.screen.width  = width;
        self.screen.height = height;
        self.projection_dirty.set();

        match &self.projection {
            Projection::Perspective {fov} => {
                let zoom       = self.zoom;
                let alpha      = fov / 2.0;
                let native_z  = height / (2.0 * alpha.tan());
                self.native_z = native_z;
                self.mod_only_position(|t| t.z = native_z / zoom);
            }
            _ => unimplemented!()
        };
    }
}

// === Transform Setters ===

impl Camera2DData {
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.mod_only_position(f);
        self.zoom = self.native_z / self.transform.position().z;
    }

    pub fn set_position(&mut self, value:Vector3<f32>) {
        self.mod_position(|p| *p = value);
    }
}

// === Private Transform Setters ===

impl Camera2DData {
    fn mod_only_position<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_position(f)
    }
}



// ================
// === Camera2D ===
// ================

/// Camera definition for 2D objects.
///
/// Although this camera implementation is defined in terms of 3D transformations under the hood,
/// it has several properties which make sense only in the context of a 2D projection:
/// - The `zoom` factor which correlates to the final image zoom. When the `zoom` parameter is set
///   to `1.0`, the units correspond 1:1 to pixels on the screen.
/// - The `native_z` value describes the z-axis distance at which the `zoom` value is `1.0`.
/// - When a new screen dimensions are provided, the camera automatically recomputes the z-axis
///   position to keep the `zoom` unchanged.
/// - The `alignment` describes where the origin is placed in the camera frustum. It is used for
///   drawing elements and scaling the view. By default, the `alignment` is set to center, which
///   defines the origin center at the center of the screen. When scaling the view, objects placed
///   in the center of the view will not move visually. If you set the alignment to bottom-left
///   corner, you will get a view which behaves like a window in window-based GUIs. When scaling
///   the window, the left-bottom corner will stay in place.
#[derive(Clone,Debug)]
pub struct Camera2D {
    rc: Rc<RefCell<Camera2DData>>
}

impl Camera2D {
    /// Creates new Camera instance.
    pub fn new(logger:Logger) -> Self {
        let data = Camera2DData::new(logger);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }
}

// === Modifiers ===

impl Camera2D {
    /// Sets screen dimensions.
    pub fn set_screen(&self, width:f32, height:f32) {
        self.rc.borrow_mut().set_screen(width,height)
    }

    /// Update all diry camera parameters and compute updated view-projection matrix.
    pub fn update(&self) -> bool {
        self.rc.borrow_mut().update()
    }
}


// === Getters ===

impl Camera2D {
    pub fn view_projection_matrix(&self) -> Matrix4<f32> {
        *self.rc.borrow().view_projection_matrix()
    }
}

// === Setters ===

impl Camera2D {
    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_position(f)
    }

    pub fn set_position(&self, value:Vector3<f32>) {
        self.rc.borrow_mut().set_position(value)
    }
}
