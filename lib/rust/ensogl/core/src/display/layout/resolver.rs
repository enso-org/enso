//! TODO



// use crate::prelude::*;
// use enso_data_structures::opt_vec::OptVec;
//
//
//
// // ============
// // === Resizing ===
// // ============
//
// #[derive(Clone, Copy, Debug, Default, PartialEq)]
// pub enum Resizing {
//     #[default]
//     HugContent,
//     FillParent,
//     Fixed(f32),
// }
//
//
//
// // =============
// // === Size ===
// // =============
//
// #[derive(Clone, Default)]
// pub struct Size {
//     dirty:      Cell<bool>,
//     resizing:   Cell<Resizing>,
//     last_value: Cell<f32>,
// }
//
// impl Debug for Size {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_struct("Size")
//             .field("dirty", &self.dirty.get())
//             .field("resizing", &self.resizing.get())
//             .field("last_value", &self.last_value.get())
//             .finish()
//     }
// }
//
//
// // =============
// // === Frame ===
// // =============
//
// #[derive(Clone, CloneRef, Default, Deref)]
// pub struct Frame {
//     model: Rc<FrameModel>,
// }
//
// #[derive(Clone, Default, Debug)]
// pub struct FrameModel {
//     index:                     Cell<usize>,
//     width:                     Size,
//     width_dirty:               Cell<bool>,
//     children_with_dirty_width: RefCell<HashSet<usize>>,
//     parent:                    RefCell<Option<WeakFrame>>,
//     children:                  RefCell<OptVec<WeakFrame>>,
// }
//
// impl Debug for Frame {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.debug_struct("Frame")
//             .field("index", &self.index.get())
//             .field("width", &self.width)
//             .field("width_dirty", &self.width_dirty.get())
//             .field("children_with_dirty_width", &*self.children_with_dirty_width.borrow())
//             .finish()
//     }
// }
//
// impl Frame {
//     pub fn new() -> Self {
//         default()
//     }
//
//     pub fn downgrade(&self) -> WeakFrame {
//         WeakFrame { model: Rc::downgrade(&self.model) }
//     }
//
//     pub fn set_width_resizing(&self, width: Resizing) {
//         self.model.width.resizing.set(width);
//         self.on_width_changed();
//     }
//
//     pub fn parent(&self) -> Option<Frame> {
//         self.model.parent.borrow().as_ref().and_then(|t| t.upgrade())
//     }
//
//     fn on_width_changed(&self) {
//         self.width_dirty.set(true);
//         if let Some(parent) = self.parent() {
//             match parent.width.resizing.get() {
//                 Resizing::HugContent => parent.on_width_changed(),
//                 _ => parent.on_child_width_changed(self.index.get()),
//             }
//         }
//     }
//
//     fn on_child_width_changed(&self, index: usize) {
//         self.children_with_dirty_width.borrow_mut().insert(index);
//         if let Some(parent) = self.parent() {
//             parent.on_child_width_changed(self.index.get());
//         }
//     }
//
//     pub fn add_child(&self, child: &Frame) {
//         self.children.borrow_mut().insert_with_ix_(|index| {
//             child.index.set(index);
//             child.downgrade()
//         });
//         child.parent.replace(Some(self.downgrade()));
//     }
//
//     pub fn update(&self) {
//         for child_index in mem::take(&mut *self.children_with_dirty_width.borrow_mut()) {
//             let children = self.children.borrow();
//             if let Some(child) = children[child_index].upgrade() {
//                 child.update();
//             }
//         }
//         if self.width_dirty.take() {
//             match self.width.resizing.get() {
//                 Resizing::HugContent => {
//                     let mut width = 0.0;
//                     for child in &*self.children.borrow() {
//                         if let Some(child) = child.upgrade() {
//                             child.update();
//                             width += child.width.last_value.get();
//                         }
//                     }
//                     self.width.last_value.set(width);
//                 }
//                 Resizing::Fixed(width) => {
//                     self.width.last_value.set(width);
//                 }
//                 _ => panic!("Not implemented"),
//             }
//         }
//     }
// }
//
//
// // =================
// // === WeakFrame ===
// // =================
//
// #[derive(Clone, CloneRef, Debug)]
// pub struct WeakFrame {
//     model: Weak<FrameModel>,
// }
//
// impl WeakFrame {
//     pub fn upgrade(&self) -> Option<Frame> {
//         self.model.upgrade().map(|model| Frame { model })
//     }
// }
//
//
//
// // =============
// // === Tests ===
// // =============
//
// #[cfg(test)]
// mod test {
//     use super::*;
//
//     #[test]
//     fn test() {
//         let x = Frame::new();
//         x.set_width_resizing(Resizing::Fixed(100.0));
//         let a = Frame::new();
//         let b1 = Frame::new();
//         let b2 = Frame::new();
//         x.add_child(&a);
//         a.add_child(&b1);
//         a.add_child(&b2);
//         b1.set_width_resizing(Resizing::Fixed(10.0));
//         b2.set_width_resizing(Resizing::Fixed(10.0));
//         x.update();
//         println!("x: {:#?}", x);
//         println!("a: {:#?}", a);
//         println!("b1: {:#?}", b1);
//         println!("b2: {:#?}", b2);
//         assert_eq!(1, 1);
//     }
// }
