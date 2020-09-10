//! This file contains the time-based benchmarks for the Enso lexer.

mod lexer_bench_sources;

use criterion::{criterion_group, criterion_main, Criterion, Throughput, black_box};
use lexer_bench_sources as src;

// Performance Notes:
// - Block handling seems to contribute significantly to the runtime, on the order of ~30% in the
//   simple cases.
// - Performance is subpar. Hypotheses:
//   + Getting badly hit by cache misses.
//   + Getting badly hit by branch prediction.
//   + Allocation patterns are suboptimal.



// ==========================
// === Literal Benchmarks ===
// ==========================

fn bench_literal_number_integer(c:&mut Criterion) {
    src::run_bench_sizes(
        "Integer",
        src::literal::number::integer().as_str(),
        true,
        c
    );
}

fn bench_literal_number_integer_explicit_base(c:&mut Criterion) {
    src::run_bench_sizes(
        "Integer Explicit Base",
        src::literal::number::integer_explicit_base().as_str(),
        true,
        c
    );
}

fn bench_literal_number_decimal(c:&mut Criterion) {
    src::run_bench_sizes(
        "Decimal",
        src::literal::number::decimal().as_str(),
        true,
        c
    );
}

fn bench_literal_number_decimal_explicit_base(c:&mut Criterion) {
    src::run_bench_sizes(
        "Decimal Explicit Base",
        src::literal::number::decimal_explicit_base().as_str(),
        true,
        c
    );
}

fn bench_literal_number_error_base(c:&mut Criterion) {
    src::run_bench_sizes(
        "Number Error Base",
        src::literal::number::error_base().as_str(),
        true,
        c
    );
}

fn bench_literal_text_format_line(c:&mut Criterion) {
    src::run_bench_sizes(
        "Text Format Line",
        src::literal::text::format_line().as_str(),
        true,
        c
    );
}

fn bench_literal_text_format_inline_block(c:&mut Criterion) {
    src::run_bench_sizes(
        "Text Format Inline Block",
        src::literal::text::format_inline_block().as_str(),
        true,
        c
    );
}

fn bench_literal_text_format_block(c:&mut Criterion) {
    src::run_bench_sizes(
        "Text Format Block",
        src::literal::text::format_block().as_str(),
        true,
        c
    );
}

fn bench_literal_text_raw_line(c:&mut Criterion) {
    src::run_bench_sizes(
        "Text Raw Line",
        src::literal::text::raw_line().as_str(),
        true,
        c
    );
}

fn bench_literal_text_raw_inline_block(c:&mut Criterion) {
    src::run_bench_sizes(
        "Text Raw Inline Block",
        src::literal::text::raw_inline_block().as_str(),
        true,
        c
    );
}

fn bench_literal_text_raw_block(c:&mut Criterion) {
    src::run_bench_sizes(
        "Text Raw Block",
        src::literal::text::raw_block().as_str(),
        true,
        c
    );
}

criterion_group!{
    name    = literal_benchmarks;
    config  = src::bench_config();
    targets =
        bench_literal_number_integer,
        bench_literal_number_integer_explicit_base,
        bench_literal_number_decimal,
        bench_literal_number_decimal_explicit_base,
        bench_literal_number_error_base,
        bench_literal_text_format_line,
        bench_literal_text_format_inline_block,
        bench_literal_text_format_block,
        bench_literal_text_raw_line,
        bench_literal_text_raw_inline_block,
        bench_literal_text_raw_block,
}



// ========================
// === Names Benchmarks ===
// ========================

fn bench_names_line_of(c:&mut Criterion) {
    src::run_bench_sizes(
        "Line of Names",
        src::name::line_of().as_str(),
        true,
        c
    );
}

fn bench_names_invalid_suffix(c:&mut Criterion) {
    src::run_bench_sizes(
        "Names with Invalid Suffixes",
        src::name::invalid_suffix().as_str(),
        true,
        c
    );
}

criterion_group! {
    name    = name_benchmarks;
    config  = src::bench_config();
    targets =
        bench_names_line_of,
        bench_names_invalid_suffix,
}



// ===========================
// === Operator Benchmarks ===
// ===========================

fn bench_operator_line_of(c:&mut Criterion) {
    src::run_bench_sizes(
        "Line of Operators",
        src::operator::line_of().as_str(),
        true,
        c
    );
}

fn bench_operator_dot_call(c:&mut Criterion) {
    src::run_bench_sizes(
        "Dot Call Operators",
        src::operator::dot_call().as_str(),
        true,
        c
    );
}

fn bench_operator_invalid_suffix(c:&mut Criterion) {
    src::run_bench_sizes(
        "Operators with Invalid Suffixes",
        src::operator::invalid_suffix().as_str(),
        true,
        c
    );
}

criterion_group! {
    name    = operator_benchmarks;
    config  = src::bench_config();
    targets =
        bench_operator_line_of,
        bench_operator_dot_call,
        bench_operator_invalid_suffix
}



// ========================
// === Block Benchmarks ===
// ========================

fn bench_block_top_level(c:&mut Criterion) {
    src::run_bench_sizes(
        "Top Level Block",
        src::block::top_level().as_str(),
        true,
        c
    );
}

fn bench_block_nested(c:&mut Criterion) {
    src::run_bench_sizes(
        "Nested Block",
        src::block::nested().as_str(),
        true,
        c
    );
}

fn bench_block_deeply_nested(c:&mut Criterion) {
    src::run_bench_sizes(
        "Deeply Nested Blocks",
        src::block::deeply_nested().as_str(),
        true,
        c
    );
}

criterion_group! {
    name    = block_benchmarks;
    config  = src::bench_config();
    targets =
        bench_block_top_level,
        bench_block_nested,
        bench_block_deeply_nested,
}



// ==========================
// === Comment Benchmarks ===
// ==========================

fn bench_comment_line(c:&mut Criterion) {
    src::run_bench_sizes(
        "Line Comment",
        src::comment::line().as_str(),
        true,
        c
    );
}

fn bench_comment_in_line(c:&mut Criterion) {
    src::run_bench_sizes(
        "Comment in Line",
        src::comment::in_line().as_str(),
        true,
        c
    );
}

fn bench_comment_doc(c:&mut Criterion) {
    src::run_bench_sizes(
        "Doc Comment",
        src::comment::doc().as_str(),
        true,
        c
    );
}

criterion_group! {
    name    = comment_benchmarks;
    config  = src::bench_config();
    targets =
        bench_comment_line,
        bench_comment_in_line,
        bench_comment_doc,
}



// ===========================
// === Combined Benchmarks ===
// ===========================

fn bench_combined_simple(c:&mut Criterion) {
    src::run_bench_sizes(
        "Simple Combined Example",
        src::combined::simple().as_str(),
        true,
        c
    );
}

fn bench_combined_complex(c:&mut Criterion) {
    src::run_bench_sizes(
        "Complex Combined Example",
        src::combined::complex().as_str(),
        true,
        c
    );
}

criterion_group! {
    name    = combined_benchmarks;
    config  = src::bench_config();
    targets =
        bench_combined_simple,
        bench_combined_complex,
}



// ===================
// === Comparisons ===
// ===================

fn bench_rust_reader(c:&mut Criterion) {
    let mut group = c.benchmark_group("Rust Vector");
    src::SIZES.iter().for_each(|(size,name)| {
        group.throughput(Throughput::Bytes(*size as u64));
        let input = "abcdefghijklmnopqrstuvwxyz".repeat(1 + size / 26);
        group.bench_function(
            *name,
            |b| b.iter(|| {
                let mut counter = 0usize;
                for c in black_box(input.as_str()).chars() {
                    if c == 'f' {
                        counter += 1;
                    }
                }
                counter
            })
        );
    })
}

criterion_group! {
    name    = rust_comparison;
    config  = src::bench_config();
    targets =
        bench_rust_reader,
}



// ===================
// === The Harness ===
// ===================

criterion_main!(
    literal_benchmarks,
    name_benchmarks,
    operator_benchmarks,
    block_benchmarks,
    comment_benchmarks,
    combined_benchmarks,
    rust_comparison,
);
