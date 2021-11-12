//! A module with structures related to examples found in Suggestion Database.
use crate::prelude::*;

use double_representation::definition;
use double_representation::definition::DefinitionName;
use double_representation::module;
use parser::Parser;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "Invalid example code.")]
pub struct InvalidExample;



// ===============
// === Example ===
// ===============

/// Example is a labeled piece of code user can put into their graph to see and learn how to use
/// the language.
///
/// If a user picks an example, its `code` should became a body of a new method defined in current
/// module. On the scene the node calling this method should appear.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Example {
    pub name:               String,
    pub code:               String,
    pub imports:            Vec<String>,
    pub documentation_html: String,
}

impl Example {
    /// Return the example name converted in such way, that it will be a valid identifier.
    ///
    /// #### Example
    /// ```
    /// use enso_prelude::*;
    ///
    /// use enso_gui::model::suggestion_database::Example;
    ///
    /// let name = "With Spaces and Strange $ąę#%^& Characters.".to_owned();
    /// let example = Example { name, ..default() };
    /// assert_eq!(example.function_name(), "with_spaces_and_strange__characters");
    /// ```
    pub fn function_name(&self) -> String {
        self.name
            .chars()
            .filter_map(|c| {
                if c == ' ' {
                    Some('_')
                } else if !c.is_ascii_alphanumeric() {
                    None
                } else {
                    Some(c.to_ascii_lowercase())
                }
            })
            .collect()
    }

    /// Returns the function definition containing the example code.
    pub fn definition_to_add(
        &self,
        module: &module::Info,
        parser: &Parser,
    ) -> FallibleResult<definition::ToAdd> {
        let base_name = self.function_name();
        let name = DefinitionName::new_plain(module.generate_name(&base_name)?);
        let code_ast = parser.parse_module(self.code.clone(), default())?;
        let body_block = code_ast.shape().as_block(0).ok_or(InvalidExample)?;
        let body_ast = Ast::new(body_block, None);
        Ok(definition::ToAdd::new_with_body(name, default(), body_ast))
    }
}

/// Creates a pretty documentation from hardcoded inner text.
pub fn documentation_html_from(inner: &str) -> String {
    return format!("<div class=\"doc\" style=\"font-size: 13px;\"><p>{}</p></div>", inner);
}

// =========================
// === Embedded Examples ===
// =========================

lazy_static! {
    /// The hard-coded examples to be used until the proper solution
    /// (described in https://github.com/enso-org/ide/issues/1011) will be implemented.
    //TODO[ao]: Remove once the issue will be implemented.
    pub static ref EXAMPLES:Vec<Example> = vec!
    [ Example
      { name               : "Parse JSON".to_owned()
      , code               : r#"Json.parse '{\"a\":10, \"b\": 20}'"#.to_owned()
      , imports            : default()
      , documentation_html : documentation_html_from("An example showing how to parse string to Json structure.")
      }
    , Example
      { name               : "Http GET".to_owned()
      , code               : "http1 = Http.new \n\
          response = http1.get \"http://enso.org/\"\n\
          body1    = response.body\n\
          body1.to_text".to_owned()
      , imports            : vec!["Standard.Base.Network.Http".to_owned()]
      , documentation_html : documentation_html_from("This snippet downloads the Enso main page."),
      }
    ];
}
