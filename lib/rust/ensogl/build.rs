use std::env;
use std::path;

/// A module with functions generating huge chunk of texts for text component benchmarks.
mod huge_text_generator {
    use std::collections::hash_map::DefaultHasher;
    use std::fs::File;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::io::Write;
    use std::path::Path;

    const MANY: usize = 100000;
    const NOT_SO_MANY: usize = 100;

    /// Create a file with many lines.
    pub fn make_long_text_file(name: &Path) {
        let mut file = File::create(name).unwrap();
        for i in (1..MANY).rev() {
            write_verse(&mut file, i);
            writeln!(file).unwrap();
        }
    }

    /// Create a file with not so many long lines
    pub fn make_wide_text_file(name: &Path) {
        let mut file = File::create(name).unwrap();
        let verses_in_line = MANY / NOT_SO_MANY;
        for i in (1..MANY).rev() {
            write_verse(&mut file, i);
            if i % verses_in_line == 0 {
                let line_index = i / verses_in_line;
                let offset = hash_from(line_index) % 32;
                let prefix = (0..offset).map(|_| '|').collect::<String>();
                writeln!(file).unwrap();
                write!(file, "{}", prefix).unwrap();
            }
        }
    }

    fn hash_from(i: usize) -> u64 {
        let mut hasher = DefaultHasher::new();
        i.hash(&mut hasher);
        hasher.finish()
    }

    fn write_verse(file: &mut File, i: usize) {
        write!(
            file,
            "{i} bottles of beer on the wall, {i} bottles of beer.\
            Take one down and pass it around, {j} bottles of beer on the wall. ",
            i = i,
            j = i - 1
        )
        .unwrap();
    }
}


fn main() {
    let out = env::var("OUT_DIR").unwrap();
    let out_dir = path::Path::new(&out);
    huge_text_generator::make_long_text_file(out_dir.join("long.txt").as_path());
    huge_text_generator::make_wide_text_file(out_dir.join("wide.txt").as_path());
    println!("cargo:rerun-if-changed=build.rs");
}
