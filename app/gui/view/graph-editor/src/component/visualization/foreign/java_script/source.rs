//! Contains [`Sources`] definition, a set of JS source code files with attached source map.
use crate::prelude::*;

use crate::visualization::Project;

use sourcemap::SourceMapBuilder;
use std::path::PathBuf;



// ============================
// === Visualizations paths ===
// ============================

/// Path prefixes for source files depending on `Project` enum.
/// These prefixes form a path to the source file displayed in the DevTools. We provide a specific
/// set of prefixes to make sources navigation in DevTools easier. As we have access to relative
/// file paths only, it would be hard to understand what relative path corresponds to otherwise.
mod path_prefix {
    use super::*;

    const BUILTIN: &str = "visualization/builtin";
    const CURRENT_PROJECT: &str = "visualization/project";
    const LIBRARY: &str = "visualization/library";

    /// Turn `Project` enum into a path prefix.
    pub fn from_project(project: &Project) -> PathBuf {
        match *project {
            Project::Builtin => PathBuf::from(BUILTIN),
            Project::CurrentProject => PathBuf::from(CURRENT_PROJECT),
            Project::Library(ref name) => {
                let name: &str = name.as_ref();
                PathBuf::from(LIBRARY).join(name)
            }
        }
    }
}



// ===============
// === Sources ===
// ===============

/// Internal identifier of the source file.
type SourceId = u32;

/// A set of JS source code files with attached
/// [source map](https://blog.teamtreehouse.com/introduction-source-maps).
///
/// It is used to concatenate several source files into a single one and preserve
/// the information about original file names and line numbers.
pub struct Sources {
    code:               String,
    lines_count:        u32,
    source_map_builder: SourceMapBuilder,
    files:              Vec<(PathBuf, SourceId)>,
}

impl Debug for Sources {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sources")
            .field("code", &self.code)
            .field("lines_count", &self.lines_count)
            .field("files", &self.files)
            .finish()
    }
}

impl Default for Sources {
    fn default() -> Self {
        Self::empty()
    }
}

impl Sources {
    /// Constructor. Typically [`Sources::from_files`] should be used instead.
    pub fn empty() -> Self {
        Self {
            code:               default(),
            lines_count:        0,
            source_map_builder: SourceMapBuilder::new(None),
            files:              default(),
        }
    }

    /// Constructor from a list of `(file_name, file_content)` pairs.
    ///
    /// `file_name` should be a file name or a relative path to the file.
    pub fn from_files(files: &[(&str, &str)]) -> Self {
        let mut sources = Self::empty();
        for (path, content) in files {
            sources.add_file(path, content);
        }
        sources
    }

    /// Add a new file to the sources set, concatenating it with other files and updating source
    /// map information.
    pub fn add_file(&mut self, path: &str, content: &str) {
        let source_id = self.source_map_builder.add_source(path);
        self.source_map_builder.set_source_contents(source_id, Some(content));
        let added_lines_count = content.lines().count() as u32;
        for line in 0..added_lines_count {
            // This offset is needed because of how DevTools handle source maps for
            // `eval`uated code. Our visualizations source code is turned into a JS object by
            // constructing a `Function` object
            // (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function).
            // DevTools basically wrap provided source code with `function anonimous(<args>) {
            // <sources> }`, so the actual line numbers in resulted code are off by 2. This was
            // calculated empirically, and apparently, this behavior of DevTools is undocumented.
            const OFFSET: u32 = 2;
            const LINE_START: u32 = 0;

            let dst_line = self.lines_count + line + OFFSET;
            let source = Some(source_id);
            self.source_map_builder.add_raw(dst_line, LINE_START, line, LINE_START, source, None);
        }
        self.lines_count += added_lines_count;
        self.files.push((PathBuf::from(path), source_id));
        self.code += content;
    }

    /// Get the final source string consisting of concatenated files and an inlined source map.
    pub fn to_string(mut self, project: &Project) -> String {
        self.set_file_names(project);
        let encoded_source_map = Self::source_map_to_base64(self.source_map_builder);
        const SOURCE_MAPPING_URL: &str =
            "\n//# sourceMappingURL=data:application/json;charset=utf-8;base64,";
        [&self.code, SOURCE_MAPPING_URL, &encoded_source_map].concat()
    }

    /// Set file names to follow `<path_prefix>/<original_file_name>` pattern. See `path_prefix`
    /// module documentation.
    fn set_file_names(&mut self, project: &Project) {
        let path_prefix = path_prefix::from_project(project);
        for (path, source_id) in &self.files {
            let source = path_prefix.join(path);
            self.source_map_builder.set_source(*source_id, &source.to_string_lossy());
        }
    }

    /// Helper function to convert source map into a base64-encoded JSON.
    fn source_map_to_base64(builder: SourceMapBuilder) -> String {
        let mut buf = Vec::new();
        let source_map = builder.into_sourcemap();
        source_map.to_writer(&mut buf).expect("Source map serialization failed.");
        base64::encode(&buf)
    }
}



// ====================
// === Helper macro ===
// ====================

/// Transform list of file paths `file1, file2, ...` into
/// `Sources::from_files(&[(file1, include_str!(file1)), (file2, include_str!(file2)), ...])`.
macro_rules! from_files {
    ($($file:literal),*) => {
        $crate::visualization::java_script::Sources::from_files(&[$(($file, include_str!($file))),*])
    }
}
pub(crate) use from_files;
