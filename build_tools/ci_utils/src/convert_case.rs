use convert_case::Case;
use convert_case::Casing;



/// Describes items that can be converted into the kebab case.
pub trait ToKebabCase<T: AsRef<str>> {
    /// Convert string into the kebab case.
    fn to_kebab_case(&self) -> String;
}

impl<T: AsRef<str>> ToKebabCase<T> for T
where String: PartialEq<T>
{
    fn to_kebab_case(&self) -> String {
        self.to_case(Case::Kebab).replace("::", "-").replace("(", "").replace(")", "")
    }
}
