use crate::prelude::*;

use std::fmt::Write;



pub trait CommandExt {
    fn as_std(&self) -> &std::process::Command;

    fn describe(&self) -> String {
        let mut ret = String::new();
        let pretty_printed = format!("{:?}", self.as_std());
        let _ = write!(ret, "Command:\n\t{pretty_printed}");
        if let Some(cwd) = self.as_std().get_current_dir() {
            let _ = write!(ret, "\n\twith working directory: {}", cwd.display());
        };
        let env = self.as_std().get_envs();
        if env.len() != 0 {
            let _ = write!(ret, "\n\twith environment overrides:");
        }
        for (name, val) in self.as_std().get_envs() {
            let _ = write!(
                ret,
                "\n\t\t{}={}",
                name.to_string_lossy(),
                val.map_or(default(), OsStr::to_string_lossy)
            );
        }
        ret
        // ?self.as_std().get_program()
    }
}

#[allow(unused_qualifications)]
impl CommandExt for crate::program::command::Command {
    fn as_std(&self) -> &std::process::Command {
        self.inner.as_std()
    }
}


impl CommandExt for std::process::Command {
    fn as_std(&self) -> &std::process::Command {
        self
    }
}

impl CommandExt for tokio::process::Command {
    fn as_std(&self) -> &std::process::Command {
        self.as_std()
    }
}
