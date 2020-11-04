//! This file contains benchmarks of the query performance for the tree structure used in the macro
//! resolver.

use parser::data::tree::*;
use itertools::*;

use criterion::{criterion_group, criterion_main, Criterion, black_box};
use std::time::Duration;



// =================
// === Utilities ===
// =================

/// The base configuration for the benchmarks.
fn bench_config() -> Criterion {
    Criterion::default()
        .measurement_time(Duration::from_secs(60))
        .warm_up_time(Duration::from_secs(3))
        .sample_size(25)
        .retain_baseline("TreeQuery".to_string())
}

/// Create a tree where each node has `width` branches, up to a maximum depth of `depth`.
fn gen_tree(width:usize, depth:usize) -> Tree<usize,usize> {
    let mut tree = Tree::<usize,usize>::empty();
    let paths    = (0..width).permutations(depth);
    paths.into_iter().for_each(|p| tree.insert(p.as_slice(),1));
    tree
}



// ==================
// === Benchmarks ===
// ==================

// === Query ===

fn wide_tree(c:&mut Criterion) {
    let tree  = gen_tree(10,5);
    let query = &[1,3,2,5,9];
    c.bench_function("Wide Tree",|b| b.iter(|| {
        tree.get_value(black_box(query))
    }));
}

fn deep_tree(c:&mut Criterion) {
    let tree  = gen_tree(5,10);
    let query = &[1,2,4,1,3];
    c.bench_function("Deep Tree",|b| b.iter(|| {
        tree.get_value(black_box(query))
    }));
}

criterion_group! {
    name    = tree_query_benchmarks;
    config  = bench_config();
    targets = wide_tree,deep_tree
}


// === Traversal ===

fn clone(c:&mut Criterion) {
    let tree = gen_tree(10,5);
    c.bench_function("Clone",|b| b.iter(|| {
        black_box(tree.clone());
    }));
}

fn map(c:&mut Criterion) {
    let tree = gen_tree(10,5);
    c.bench_function("Map",|b| b.iter(|| {
        let tree = tree.clone();
        black_box(tree.map(black_box(|v| v*2)));
    }));
}

fn map_in_place(c:&mut Criterion) {
    let tree = gen_tree(10,5);
    c.bench_function("Map in Place",|b| b.iter(|| {
        let mut tree = tree.clone();
        black_box(tree.map_in_place(black_box(|v:&mut usize| (*v)*2)));
    }));
}

criterion_group! {
    name    = tree_traversal_benchmarks;
    config  = bench_config();
    targets = clone,map,map_in_place
}

// ==============
// === Runner ===
// ==============

criterion_main!(tree_query_benchmarks,tree_traversal_benchmarks);
