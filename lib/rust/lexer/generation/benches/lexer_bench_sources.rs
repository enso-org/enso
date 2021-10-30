//! This file contains the sources that are replicated many times over for the purposes of
//! benchmarking the Enso lexer.

use criterion::{black_box, Criterion, Throughput};
use std::time::Duration;



// ===============================
// === Benchmark Configuration ===
// ===============================

/// Configures the benchmarking process.
pub fn bench_config() -> Criterion {
    Criterion::default()
        .measurement_time(Duration::from_secs(60))
        .warm_up_time(Duration::from_secs(3))
        .sample_size(25)
        .retain_baseline("EnsoLexer".to_string())
}



// =======================
// === Benchmark Setup ===
// =======================

/// The sizes of text to run the benchmarks over.
pub const SIZES:[(usize,&str);4] = [
    (1024         , "1KB"  ),
    (1024*100     , "100KB"),
    (1024*1024    , "1MB"  ),
    (1024*1024*10 , "10MB" ),
];



// ==============================
// === Benchmarking Utilities ===
// ==============================

/// Execute the provided benchmark for each of the [`SIZES`] above.
pub fn run_bench_sizes(name:&str, input:&str, add_newline:bool, c:&mut Criterion) {
    let mut group = c.benchmark_group(name);
    SIZES.iter().for_each(|(size,size_name)| {
        group.throughput(Throughput::Bytes(*size as u64));
        let input = replicate_to_size(input,*size,add_newline);
        group.bench_function(
            *size_name,
            |b| b.iter(|| {
                lexer::run(black_box(input.as_str()));
            })
        );
    })
}

/// This function replicates `input` until it reaches `size` (in bytes).
///
/// If this cannot be done exactly, it will err on the side of over-replication,
/// meaning that the output will be _larger_ than `size` bytes. If the size of
/// the input already exceeds `size`, it is returned unchanged.
pub fn replicate_to_size(input:&str, size:usize, add_newline:bool) -> String {
    let input_size        = input.len();
    let times             = 1 + (size / input_size);
    let mut input_newline = input.to_string();
    let to_add            = if add_newline { '\n' } else { ' ' };
    input_newline.push(to_add);
    input_newline.repeat(times)
}

/// Replace any windows-style line-endings in `input` with unix-style line-endings.
fn preprocess(input:&str) -> String {
    input.replace("\r\n","\n")
}



// ==============
// === Macros ===
// ==============

#[macro_export]
macro_rules! bench {
    (bench_name = $bench_name:literal; fun_name = $fun_name:ident; bench_input = $bench_input:expr;) => {
        pub fn $fun_name(c:&mut Criterion) {
            src::run_bench_sizes(
                $bench_name,
                $bench_input.as_str(),
                true,
                c
            )
        }
    }
}



// =================================
// === Literal Benchmark Sources ===
// =================================

#[allow(missing_docs)]
pub mod literal {
    use super::*;

    pub mod number {
        use super::*;

        pub fn integer() -> String {
            preprocess("12345")
        }

        pub fn integer_explicit_base() -> String {
            preprocess("16_a4fd31")
        }

        pub fn decimal() -> String {
            preprocess("1.3141")
        }

        pub fn decimal_explicit_base() -> String {
            preprocess("10_1.000999")
        }

        pub fn error_base() -> String {
            preprocess("10.2_2")
        }
    }

    pub mod text {
        use super::*;

        pub fn format_line() -> String {
            preprocess(r"'dearest creature in \n creation studying english pronunciation'")
        }

        pub fn format_inline_block() -> String {
            preprocess(r"''' An inline block. It's a very good inline block carl \u{AB}")
        }

        pub fn format_block() -> String {
            preprocess(
r#"''' Here is my block of format text. I can `interpolate + things` like that.
    It goes on and on and on for `times` times because I feel like it.

    Complex interpolated expression `x -> y ~> x | y` woo!
"#)
        }

        pub fn raw_line() -> String {
            preprocess(r#""dearest creature in '''' creation studying english pronunciation""#)
        }

        pub fn raw_inline_block() -> String {
            preprocess(r#"""" An inline block. It's a very good inline block carl ""#)
        }

        pub fn raw_block() -> String {
            preprocess(
r#"""" Here is my block of raw text. `Interpolations` are nothing special here.
    It goes on and on and on for I can escape \" though.

    It also supports blank lines!
"#)
        }
    }
}



// ==============================
// === Name Benchmark Sources ===
// ==============================

#[allow(missing_docs)]
pub mod name {
    use super::*;

    pub fn line_of() -> String {
        preprocess(
            "Referent_Ident var_ident JavaType _ @annotation ticked_ident' number_1"
        )
    }

    pub fn invalid_suffix() -> String {
        preprocess("some_var'iable some_varД")
    }
}



// ===================================
// === Operator Benchmarks Sources ===
// ===================================

#[allow(missing_docs)]
pub mod operator {
    use super::*;

    pub fn line_of() -> String {
        preprocess("+ - * -> ~> <~ <- ! & | /")
    }

    pub fn dot_call() -> String {
        preprocess(".== . != .<*> .*> .|>")
    }

    pub fn invalid_suffix() -> String {
        preprocess(".... +==")
    }
}



// ================================
// === Block Benchmarks Sources ===
// ================================

#[allow(missing_docs)]
pub mod block {
    use super::*;

    pub fn top_level() -> String {
        preprocess("foo\nbar\nbaz")
    }

    pub fn nested() -> String {
        preprocess("foo\nbar\n    baz\n    quux")
    }

    pub fn deeply_nested() -> String {
        preprocess(
r#"foo
bar
    baz
    quux
        bim
            bam
    oh
no
"#)
    }
}



// ===================================
// === Comments Benchmarks Sources ===
// ===================================

#[allow(missing_docs)]
pub mod comment {
    use super::*;

    pub fn line() -> String {
        preprocess("# foo bar baz I have a really long line comment here that goes on and on")
    }

    pub fn in_line() -> String {
        preprocess("a + b # A useless comment: add a to b")
    }

    pub fn doc() -> String {
        preprocess(
r#"##  I have a really big doc comment here
    That just keeps prattling on and on and on.

    With blank lines

    Forever

    and
    ever

    and




    ever
documented
"#)
    }
}



// ===========================
// === Combined Benchmarks ===
// ===========================

pub mod combined {
    use super::*;

    pub fn simple() -> String {
        preprocess(
r#"
import Base.Meta

##  Decompose the value using runtime reflection and print its decomposition.
Main.print_decomp a b =
    y      = a + b
    decomp = Meta.decompose y
    Io.println decomp
"#)
    }

    pub fn complex() -> String {
        preprocess(
r#"
import Base.Meta

##  Frobnicate the doodads by constructing a new type operator through runtime reflection such that
    it can be passed to another language.

    ! WARNING
    Type-checking code like this is virtually impossible, and it is treated as `Dynamic` inside
    Enso code.
Main.foo a b =
    y = x -> z ->
        ty = a.gen_type (~>) (<-) b
        ty (z x)
    decomp = Meta.decompose (y a b)
    Io.println decomp

##  Execute the main function of this project.
main =
    func = Meta.reify (here.foo "My_Name" "my_field")
    Io.println(func)
"#)
    }
}
