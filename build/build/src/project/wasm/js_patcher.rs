use crate::prelude::*;

use regex::Regex;



lazy_static! {
    pub static ref REPLACEMENTS: Vec<Replacement> = [
        Replacement::new(r"(?s)if \(typeof input === 'string'.*return wasm;", "return imports"),
        Replacement::new(
            r"(?s)if \(typeof input === 'undefined'.*const imports = \{\};",
            "const imports = {};",
        ),
        Replacement::new(r"(?s)export default init;", "export default init"),
    ]
    .into_iter()
    .try_collect_vec()
    // We parse literals that we know to be correct, as ensured by the tests.
    .unwrap();
}

#[derive(Clone, Debug)]
pub struct Replacement {
    pattern:     Regex,
    replacement: String,
}

impl Replacement {
    pub fn new(pattern: impl AsRef<str>, replacement: impl Into<String>) -> Result<Self> {
        Ok(Self { pattern: Regex::new(pattern.as_ref())?, replacement: replacement.into() })
    }

    pub fn replace_all<'a>(&'_ self, text: &'a str) -> Cow<'a, str> {
        self.pattern.replace_all(text, &self.replacement)
    }
}

pub fn multi_replace_all<'a, 'b>(
    text: impl Into<String>,
    replacements: impl IntoIterator<Item = &'b Replacement>,
) -> String {
    let init = text.into();
    replacements
        .into_iter()
        .fold(init, |text, replacement| replacement.replace_all(&text).to_string())
}

/// Workaround fix by wdanilo, see: <https://github.com/rustwasm/wasm-pack/issues/790>
pub fn js_workaround_patcher(code: impl Into<String>) -> Result<String> {
    let patched_code = multi_replace_all(code, REPLACEMENTS.as_slice());
    let epilogue = r"export function after_load(w,m) { wasm = w; init.__wbindgen_wasm_module = m;}";
    Ok(format!("{patched_code}\n{epilogue}"))
}

pub fn patch_js_glue(input_path: impl AsRef<Path>, output_path: impl AsRef<Path>) -> Result {
    debug!("Patching {}.", output_path.as_ref().display());
    let code = ide_ci::fs::read_to_string(&input_path)?;
    let patched_code = js_workaround_patcher(code)?;
    ide_ci::fs::write(output_path, patched_code)?;
    Ok(())
}

pub fn patch_js_glue_in_place(path: impl AsRef<Path>) -> Result {
    patch_js_glue(&path, &path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate_regexes() {
        for _replacement in REPLACEMENTS.iter() {}
    }
}
