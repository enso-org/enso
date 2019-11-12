// ===========
// === NOP ===
// ===========

pub trait NOP {
    fn nop() -> Self;
}

impl NOP for () {
    fn nop() -> Self {
        ()
    }
}

impl<T1> NOP for fn(T1) {
    fn nop() -> Self {
        |_| {}
    }
}
impl<T1, T2> NOP for fn(T1, T2) {
    fn nop() -> Self {
        |_, _| {}
    }
}
impl<T1, T2, T3> NOP for fn(T1, T2, T3) {
    fn nop() -> Self {
        |_, _, _| {}
    }
}
impl<T1, T2, T3, T4> NOP for fn(T1, T2, T3, T4) {
    fn nop() -> Self {
        |_, _, _, _| {}
    }
}
impl<T1, T2, T3, T4, T5> NOP for fn(T1, T2, T3, T4, T5) {
    fn nop() -> Self {
        |_, _, _, _, _| {}
    }
}
