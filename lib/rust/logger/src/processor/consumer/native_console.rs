//! Native console consumer implementation.

use crate::entry::Entry;
use crate::entry;
use crate::processor::consumer;



// ===============================
// === Native Console Consumer ===
// ===============================

/// A simple consumer which uses `println!` to simulate hierarchical logging.
#[derive(Clone,Copy,Debug,Default)]
pub struct NativeConsole {
    depth           : usize,
    collapsed_depth : usize,
}

impl NativeConsole {
    fn print(&self, msg:String) {
        if self.collapsed_depth == 0 {
            if self.depth == 0 {
                println!("{}",msg)
            } else {
                let pfx = " ".repeat(4 * self.depth);
                println!("{}{}",pfx,msg)
            }
        }
    }
}

impl<Levels> consumer::Definition<Levels,String> for NativeConsole {
    fn consume(&mut self, event:Entry<Levels>, message:Option<String>) {
        match &event.content {
            entry::Content::Message(_) => {
                if let Some(msg) = message {
                    self.print(msg);
                }
            },
            entry::Content::GroupBegin(group) => {
                if let Some(msg) = message {
                    self.print(msg);
                }
                if group.collapsed {
                    self.collapsed_depth += 1
                } else {
                    self.depth += 1
                }
            },
            entry::Content::GroupEnd => {
                if self.collapsed_depth > 0 {
                    self.collapsed_depth -= 1
                } else {
                    self.depth -= 1
                }
            }
        }
    }
}
