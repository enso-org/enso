use crate::prelude::*;

#[derive(Clone, Debug)]
pub struct Node<T> {
    pub head: T,
    pub tail: List<T>,
}

impl<T> Node<T> {
    pub fn singleton(head: T) -> Self {
        let tail = default();
        Self { head, tail }
    }
}


#[derive(Derivative, Deref, Debug)]
#[derivative(Clone(bound = ""))]
pub struct NonEmpty<T> {
    node: Rc<Node<T>>,
}

#[derive(Derivative, Deref)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
pub struct List<T> {
    data: Option<NonEmpty<T>>,
}

impl<T> NonEmpty<T> {
    pub fn singleton(head: T) -> Self {
        let node = Rc::new(Node::singleton(head));
        Self { node }
    }

    pub fn into_list(self) -> List<T> {
        let data = Some(self);
        List { data }
    }

    pub fn with_head(self, head: T) -> Self {
        self.into_list().with_head(head)
    }

    pub fn head(&self) -> &T {
        &self.head
    }

    pub fn tail(&self) -> &List<T> {
        &self.tail
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    fn to_vec(&self) -> Vec<&T> {
        let mut out = vec![&self.head];
        let mut list = self.tail();
        loop {
            match list.head() {
                None => break,
                Some(head) => {
                    out.push(head);
                    match list.tail() {
                        None => break,
                        Some(tail) => list = tail,
                    }
                }
            }
        }
        out
    }
}
impl<T> List<T> {
    pub fn with_head(self, head: T) -> NonEmpty<T> {
        let tail = self;
        let node = Rc::new(Node { head, tail });
        NonEmpty { node }
    }

    pub fn head(&self) -> Option<&T> {
        self.as_ref().map(|t| t.head())
    }

    pub fn tail(&self) -> Option<&List<T>> {
        self.as_ref().map(|t| t.tail())
    }

    pub fn is_empty(&self) -> bool {
        self.is_none()
    }

    fn to_vec(&self) -> Vec<&T> {
        self.data.as_ref().map(|t| t.to_vec()).unwrap_or_default()
    }

    pub fn try_to_non_empty(&self) -> &Option<NonEmpty<T>> {
        &self.data
    }

    pub fn try_into_non_empty(self) -> Option<NonEmpty<T>> {
        self.data
    }
}

impl<T> From<NonEmpty<T>> for List<T> {
    fn from(list: NonEmpty<T>) -> Self {
        list.into_list()
    }
}

impl<T: Debug> Debug for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.to_vec(), f)
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type Item = &'a T;
    type IntoIter = std::vec::IntoIter<&'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a NonEmpty<T> {
    type Item = &'a T;
    type IntoIter = std::vec::IntoIter<&'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}
