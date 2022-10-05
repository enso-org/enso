use crate::prelude::*;



pub trait VersionExt {
    fn triple(&self) -> (u64, u64, u64);
    fn same_triple(&self, other: &Self) -> bool {
        self.triple() == other.triple()
    }
}

impl VersionExt for Version {
    fn triple(&self) -> (u64, u64, u64) {
        (self.major, self.minor, self.patch)
    }
}
