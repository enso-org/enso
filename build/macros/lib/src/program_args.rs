use crate::prelude::*;



#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Policy {
    pub flag_case: Case,
    pub format:    Option<String>,
}

impl const Default for Policy {
    fn default() -> Self {
        Self { flag_case: Case::Kebab, format: None }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Generator<'a> {
    pub input:  &'a syn::DeriveInput,
    pub policy: Vec<Policy>,
}

impl<'a> Generator<'a> {
    pub fn current_policy(&self) -> &Policy {
        static DEFAULT_POLICY: Policy = Policy::default();
        self.policy.last().unwrap_or(&DEFAULT_POLICY)
    }

    pub fn new(input: &'a syn::DeriveInput) -> Self {
        Self { input, policy: vec![Default::default()] }
    }

    pub fn format_flag(&mut self, name: impl ToString) -> String {
        format!("--{}", name.to_string().to_case(self.current_policy().flag_case))
    }

    pub fn generate(self) -> TokenStream {
        // let name = &self.input.ident;
        match &self.input.data {
            Data::Enum(e) => EnumGenerator::new(self, e).generate(),
            _ => unimplemented!(),
            // Data::Struct(_) => {}
            // Data::Union(_) => {}
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deref, DerefMut)]
pub struct EnumGenerator<'a> {
    #[deref]
    #[deref_mut]
    pub generator: Generator<'a>,
    pub enum_data: &'a syn::DataEnum,
}

impl<'a> EnumGenerator<'a> {
    pub fn new(generator: Generator<'a>, enum_data: &'a syn::DataEnum) -> Self {
        Self { generator, enum_data }
    }

    /// Generate output for enum where all variants are units.
    ///
    /// In such case every variant can be converted to OsStr.
    /// An iterator is just a single occurrence of the string.
    pub fn generate_plain(&mut self) -> TokenStream {
        let name = &self.generator.input.ident;
        let variant_names =
            self.enum_data.variants.iter().map(|variant| &variant.ident).collect_vec();
        let flags = variant_names.iter().map(|v| self.format_flag(v)).collect_vec();
        quote! {
            impl AsRef<std::ffi::OsStr> for #name {
                fn as_ref(&self) -> &std::ffi::OsStr {
                    match self {
                        #( #name::#variant_names => #flags, )*
                    }.as_ref()
                }
            }

            impl IntoIterator for #name {
                type Item = std::ffi::OsString;
                type IntoIter = std::iter::Once<std::ffi::OsString>;

                fn into_iter(self) -> Self::IntoIter {
                    std::iter::once(self.as_ref().to_owned())
                }
            }
        }
    }

    /// Generate arm that matches a variant with zero or one field and outputs `Vec<OsString>`.
    pub fn generate_arm_with_field(&mut self, variant: &syn::Variant) -> TokenStream {
        let relevant_attrs = variant
            .attrs
            .iter()
            .filter_map(|attr| attr.path.is_ident("arg").then_some(&attr.tokens))
            .collect_vec();
        // dbg!(&relevant_attrs.iter().map(|t| t.to_string()).collect_vec());
        let _relevant_attrs_as_expr = relevant_attrs
            .iter()
            .filter_map(|tokens| syn::parse2::<syn::ExprAssign>((*tokens).clone()).ok())
            .collect_vec();
        // dbg!(relevant_attrs_as_expr);

        let name = &self.generator.input.ident;
        let variant_name = &variant.ident;
        let flag = self.format_flag(variant_name);
        if let Some(_field) = variant.fields.iter().next() {
            // let field_type = &field.ty;
            quote! {
                #name::#variant_name(field) => {
                    let mut result = Vec::new();
                    result.push(#flag.into());
                    let os_str: &OsStr = field.as_ref();
                    result.push(os_str.into());
                    result.into_iter()
                }
            }
        } else {
            quote! {
                #name::#variant_name => vec![#flag.into()].into_iter()
            }
        }
    }

    /// Generate output for enum where variant can have fields.
    pub fn generate_with_fields(&mut self) -> TokenStream {
        let name = &self.generator.input.ident;
        let arms = self.enum_data.variants.iter().map(|v| self.generate_arm_with_field(v));
        quote! {
            impl IntoIterator for #name {
                type Item = std::ffi::OsString;
                type IntoIter = std::vec::IntoIter<std::ffi::OsString>;

                fn into_iter(self) -> Self::IntoIter {
                    match self {
                        #( #arms, )*
                    }
                }
            }
        }
    }

    pub fn generate(&mut self) -> TokenStream {
        // If all variants are unit variants, we just pretty print their names.
        if self.enum_data.variants.iter().all(|v| v.fields.is_empty()) {
            self.generate_plain()
        } else {
            self.generate_with_fields()
        }
    }
}

pub fn derive(input: DeriveInput) -> Result<TokenStream> {
    let generator = Generator::new(&input);
    Ok(generator.generate())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn foo() -> Result {
        let code = "enum Foo {
            #[arg]
            Bar,
            #[arg]
            Baz,
            #[arg]
            Quux,
        }";
        let _token_stream = syn::parse_str::<TokenStream>(code)?;
        Ok(())
    }

    /// Structure with AST of parenthesized sequence of assignments.
    ///
    /// For example, `(a = 1, b = ToString::to_string)`.
    #[derive(Debug, Clone)]
    pub struct Assignments {
        pub paren_token: syn::token::Paren,
        pub assignments: syn::punctuated::Punctuated<syn::ExprAssign, syn::Token![,]>,
    }

    impl Parse for Assignments {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let content;
            let paren_token = syn::parenthesized!(content in input);
            let assignments = content.parse_terminated(syn::ExprAssign::parse)?;
            Ok(Self { paren_token, assignments })
        }
    }

    #[test]
    #[ignore]
    fn parse_attribute() -> Result {
        let attribute = r#"(format = ToString :: to_string)"#;
        let token_stream = syn::parse_str::<TokenStream>(attribute)?;
        dbg!(&token_stream);
        let foo = syn::parse2::<Assignments>(token_stream)?;
        dbg!(foo);
        // let attribute = syn::parse2::<syn::Attribute>(token_stream)?;
        // dbg!(attribute);
        Ok(())
    }
}
