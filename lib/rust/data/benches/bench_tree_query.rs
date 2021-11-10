//! This file contains benchmarks of the query performance for the HashTree structure.

use enso_data::hash_map_tree::HashMapTree;
use itertools::*;

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
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
        .retain_baseline("Tree".to_string())
}

/// Create a tree where each node has `width` branches, up to a maximum depth of `depth`.
fn gen_tree(width: usize, depth: usize) -> HashMapTree<usize, usize> {
    let mut tree = HashMapTree::<usize, usize>::default();
    let paths = (0..width).permutations(depth);
    paths.into_iter().for_each(|p| tree.set(p, 1));
    tree
}



// ==================
// === Benchmarks ===
// ==================


// === Query ===

/// A benchmark that tests querying a tree that has 10 branches at each node, and each node chain
/// goes five nodes deep.
fn wide_tree(c: &mut Criterion) {
    let tree = gen_tree(10, 5);
    let query: Vec<usize> = vec![1, 3, 2, 5, 9];
    c.bench_function("Wide Tree", |b| b.iter(|| tree.get(black_box(query.clone()))));
}

/// A benchmark that tests querying a tree that has 5 branches at each node, and each node chain
/// goes ten nodes deep.
fn deep_tree(c: &mut Criterion) {
    let tree = gen_tree(5, 10);
    let query: Vec<usize> = vec![1, 2, 4, 1, 3];
    c.bench_function("Deep Tree", |b| b.iter(|| tree.get(black_box(query.clone()))));
}

criterion_group! {
    name    = tree_query_benchmarks;
    config  = bench_config();
    targets = wide_tree,deep_tree
}


// === Traversal ===

fn clone(c: &mut Criterion) {
    let tree = gen_tree(10, 5);
    c.bench_function("Clone", |b| b.iter(|| black_box(tree.clone())));
}

fn map(c: &mut Criterion) {
    let tree = gen_tree(10, 5);
    c.bench_function("Map", |b| {
        b.iter(|| {
            let tree = tree.clone();
            black_box(
                tree.iter().map(black_box(|(k, v)| (k, v * 2))).collect::<HashMapTree<_, _>>(),
            );
        })
    });
}

fn map_in_place(c: &mut Criterion) {
    let tree = gen_tree(10, 5);
    c.bench_function("Map in Place", |b| {
        b.iter(|| {
            let mut tree = tree.clone();
            black_box(
                tree.iter_mut().for_each(black_box(|(_, v): (Vec<&usize>, &mut usize)| *v *= 2)),
            );
        })
    });
}

criterion_group! {
    name    = tree_traversal_benchmarks;
    config  = bench_config();
    targets = clone,map,map_in_place
}



// ==============
// === Runner ===
// ==============

criterion_main!(tree_query_benchmarks, tree_traversal_benchmarks);
