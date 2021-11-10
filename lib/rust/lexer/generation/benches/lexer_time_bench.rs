//! This file contains the time-based benchmarks for the Enso lexer.

mod lexer_bench_sources;

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use criterion::Throughput;
use lexer_bench_sources as src;



// ==========================
// === Literal Benchmarks ===
// ==========================

bench! {
    bench_name  = "Integer";
    fun_name    = bench_literal_number_integer;
    bench_input = src::literal::number::integer();
}

bench! {
    bench_name  = "Integer Explicit Base";
    fun_name    = bench_literal_number_integer_explicit_base;
    bench_input = src::literal::number::integer_explicit_base();
}

bench! {
    bench_name  = "Decimal";
    fun_name    = bench_literal_number_decimal;
    bench_input = src::literal::number::decimal();
}

bench! {
    bench_name  = "Decimal Explicit Base";
    fun_name    = bench_literal_number_decimal_explicit_base;
    bench_input = src::literal::number::decimal_explicit_base();
}

bench! {
    bench_name  = "Number Error Base";
    fun_name    = bench_literal_number_error_base;
    bench_input = src::literal::number::error_base();
}

bench! {
    bench_name  = "Text Format Line";
    fun_name    = bench_literal_text_format_line;
    bench_input = src::literal::text::format_line();
}

bench! {
    bench_name  = "Text Format Inline Block";
    fun_name    = bench_literal_text_format_inline_block;
    bench_input = src::literal::text::format_inline_block();
}

bench! {
    bench_name  = "Text Format Block";
    fun_name    = bench_literal_text_format_block;
    bench_input = src::literal::text::format_block();
}

bench! {
    bench_name  = "Text Raw Line";
    fun_name    = bench_literal_text_raw_line;
    bench_input = src::literal::text::raw_line();
}

bench! {
    bench_name  = "Text Raw Inline Block";
    fun_name    = bench_literal_text_raw_inline_block;
    bench_input = src::literal::text::raw_inline_block();
}

bench! {
    bench_name  = "Text Raw Block";
    fun_name    = bench_literal_text_raw_block;
    bench_input = src::literal::text::raw_block();
}

criterion_group! {
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

bench! {
    bench_name  = "Line of Names";
    fun_name    = bench_names_line_of;
    bench_input = src::name::line_of();
}

bench! {
    bench_name  = "Names with invalid Suffixes";
    fun_name    = bench_names_invalid_suffix;
    bench_input = src::name::invalid_suffix();
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

bench! {
    bench_name  = "Line of Operators";
    fun_name    = bench_operator_line_of;
    bench_input = src::operator::line_of();
}

bench! {
    bench_name  = "Dot Call Operators";
    fun_name    = bench_operator_dot_call;
    bench_input = src::operator::dot_call();
}

bench! {
    bench_name  = "Operators with Invalid Suffixes";
    fun_name    = bench_operator_invalid_suffix;
    bench_input = src::operator::invalid_suffix();
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

bench! {
    bench_name  = "Top Level Block";
    fun_name    = bench_block_top_level;
    bench_input = src::block::top_level();
}

bench! {
    bench_name  = "Nested Block";
    fun_name    = bench_block_nested;
    bench_input = src::block::nested();
}

bench! {
    bench_name  = "Deeply Nested Blocks";
    fun_name    = bench_block_deeply_nested;
    bench_input = src::block::deeply_nested();
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

bench! {
    bench_name  = "Line Comment";
    fun_name    = bench_comment_line;
    bench_input = src::comment::line();
}

bench! {
    bench_name  = "Comment in Line";
    fun_name    = bench_comment_in_line;
    bench_input = src::comment::in_line();
}

bench! {
    bench_name  = "Doc Comment";
    fun_name    = bench_comment_doc;
    bench_input = src::comment::doc();
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

bench! {
    bench_name  = "Simple Combined Example";
    fun_name    = bench_combined_simple;
    bench_input = src::combined::simple();
}

bench! {
    bench_name  = "Complex Combined Example";
    fun_name    = bench_combined_complex;
    bench_input = src::combined::complex();
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

fn bench_rust_reader(c: &mut Criterion) {
    let mut group = c.benchmark_group("Rust Vector");
    src::SIZES.iter().for_each(|(size, name)| {
        group.throughput(Throughput::Bytes(*size as u64));
        let input = "abcdefghijklmnopqrstuvwxyz".repeat(1 + size / 26);
        group.bench_function(*name, |b| {
            b.iter(|| {
                let mut counter = 0usize;
                for c in black_box(input.as_str()).chars() {
                    if c == 'f' {
                        counter += 1;
                    }
                }
                counter
            })
        });
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
