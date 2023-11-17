//! Code for handling globally-stateful temporary files operations.

use crate::prelude::*;

use std::sync::Mutex;

/// Current global temporary file handling policy.
static POLICY: Mutex<crate::temp::Policy> = Mutex::new(crate::temp::Policy::Drop);

/// Set global temporary file policy.
pub fn set_policy(policy: crate::temp::Policy) {
    // We assume that no-one panics while holding a lock. It is not public.
    *POLICY.lock().unwrap() = policy;
}

/// Get global temporary file policy.
pub fn get_policy() -> crate::temp::Policy {
    // We assume that no-one panics while holding a lock. It is not public.
    POLICY.lock().unwrap().clone()
}

pub fn directory() -> Result<crate::temp::Directory> {
    crate::temp::new_tempdir(get_policy())
}
