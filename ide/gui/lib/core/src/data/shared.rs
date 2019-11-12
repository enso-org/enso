use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::fmt;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;
use std::rc::Weak;

// ==============
// === Shared ===
// ==============

#[derive(Clone)]
pub struct Shared<T> {
    v: Rc<RefCell<T>>
}

impl <T> Shared<T> {
    pub fn new(t: T)-> Shared<T> {
        Shared{v: Rc::new(RefCell::new(t))}
    }
}

impl <T> Shared<T> {
    pub fn clone_ref(&self) -> Self {
        Self { v: self.v.clone() }
    }

    pub fn borrow(&self) -> Ref<T> {
        self.v.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<T> {
        self.v.borrow_mut()
    }

    pub fn as_ptr(&self) -> *mut T {
        self.v.as_ptr()
    }

    pub fn downgrade(&self) -> WeakShared<T> {
        WeakShared { raw: Rc::downgrade(&self.v) }
    }
}


impl <T: fmt::Display> fmt::Display for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.deref())
    }
}

impl <T: fmt::Debug> fmt::Debug for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.deref())
    }
}

impl <T> Deref for Shared<T>{
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe {self.as_ptr().as_ref().unwrap()}
    }

}

impl <T> DerefMut for Shared<T> {   
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        unsafe {self.as_ptr().as_mut().unwrap()}
    }
}



// ==================
// === WeakShared ===
// ==================

pub struct WeakShared<T> {
    raw: Weak<RefCell<T>>
}

impl<T> WeakShared<T> {
    pub fn upgrade(&self) -> Option<Shared<T>> {
        let opt_raw = self.raw.upgrade();
        opt_raw.map(|v| Shared { v })
    }
}

impl <T: fmt::Debug> fmt::Debug for WeakShared<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.deref())
    }
}

// fn split (s: Shared<String>) -> Vec<String> {
//     s.split_whitespace().map(|s| s.to_string()).collect()
// }

// pub fn main() {
//     let s = Shared::new("hello".to_string());
//     let s2 = s.clone();
//     s2.borrow_mut().push('!');
//     println!("{:?}",s2);

//     // Deref kicking in...
//     let n = s2.len();

//     println!("{:?}", n);

//     // mutation has to be explicit
//     s2.borrow_mut().push_str(" dolly");

//     println!("{:?} {}",s2.borrow(), s);

//     println!("{:?}", split(s2.clone()));



// }


