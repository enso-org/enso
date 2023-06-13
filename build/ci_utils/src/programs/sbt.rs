use crate::prelude::*;



macro_rules! strong_string {
    ($name:ident($inner_ty:ty)) => {
        paste::paste! {
            #[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
            pub struct $name(pub <$inner_ty as ToOwned>::Owned);

            impl $name {
                pub fn new(inner: impl Into<<$inner_ty as ToOwned>::Owned>) -> Self {
                    Self(inner.into())
                }
            }

            #[derive(Debug, Serialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
            pub struct [<$name Ref>]<'a>(pub &'a $inner_ty);
        }
    };
}

strong_string!(Task(str));

#[derive(Clone, Copy, Debug, Default)]
pub struct Sbt;

impl Program for Sbt {
    fn executable_name(&self) -> &'static str {
        "sbt"
    }
}

impl Sbt {
    /// Format a string with a command that will execute all the given tasks concurrently.
    pub fn concurrent_tasks(tasks: impl IntoIterator<Item: AsRef<str>>) -> String {
        let mut ret = String::from("all");
        for task in tasks {
            ret.push(' ');
            ret.push_str(task.as_ref())
        }
        ret
    }

    /// Format a string with a command that will execute all the given tasks sequentially.
    pub fn sequential_tasks<'a>(tasks: impl IntoIterator<Item = &'a str>) -> String {
        tasks.into_iter().collect::<Vec<_>>().join("; ")
    }
}

#[derive(Clone, Debug)]
pub struct SystemProperty {
    pub name:  String,
    pub value: String,
}

impl SystemProperty {
    pub fn new(name: impl Into<String>, value: impl Into<String>) -> Self {
        Self { name: name.into(), value: value.into() }
    }
}

impl<'a> IntoIterator for &'a SystemProperty {
    type Item = String;
    type IntoIter = std::iter::Once<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        once(format!("-D{}={}", self.name, self.value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_concurrent_tasks() {
        let tasks = ["test", "syntaxJS/fullOptJS"];
        assert_eq!(Sbt::concurrent_tasks(tasks), "all test syntaxJS/fullOptJS");
    }
}
