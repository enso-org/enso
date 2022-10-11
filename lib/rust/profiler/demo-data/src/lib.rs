//! Produces example profile data.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]

use enso_profiler as profiler;
use enso_profiler::profile;



// ==========================
// === Dummy Computations ===
// ==========================

/// Create example profiling data.
pub async fn create_data() -> String {
    start_project().await;
    profiler::internal::take_log()
}

/// A dummy computation that is intended to take some time based on input (where a higher number
///takes longer).
fn work(n: u32) {
    let mut m = n;
    for x in 0..n {
        for y in 0..n {
            for z in 0..n {
                m = m.wrapping_add(x * y * z)
            }
        }
    }
    // Create a side effect to avoid optimising away the computation.
    println!("{}", m % 7)
}

#[profile(Objective)]
async fn start_project() {
    wake_dragon().await;
    feed_troll();
    ride_rainbow();
}
#[profile(Objective)]
fn ride_rainbow() {
    work(333)
}
#[profile(Objective)]
fn feed_troll() {
    gather_herbs_and_spices();
    cook_troll_food();
    run_away();
}
#[profile(Objective)]
fn run_away() {
    work(100)
}
#[profile(Objective)]
fn cook_troll_food() {
    work(100)
}
#[profile(Objective)]
fn gather_herbs_and_spices() {
    walk_to_woods();
    search_stuff();
    find_stuff();
    gather_stuff();
}
#[profile(Objective)]
fn gather_stuff() {
    work(100)
}
#[profile(Objective)]
fn find_stuff() {
    work(100)
}
#[profile(Objective)]
fn search_stuff() {
    work(100)
}
#[profile(Objective)]
fn walk_to_woods() {
    work(100)
}
#[profile(Objective)]
async fn wake_dragon() {
    gather_gold().await;
    bake_gold_cake().await;
    start_tea_party().await;
}
#[profile(Objective)]
async fn start_tea_party() {
    work(100)
}
#[profile(Objective)]
async fn bake_gold_cake() {
    work(100)
}
#[profile(Objective)]
fn pick_coin() {
    work(75)
}
#[profile(Objective)]
async fn gather_gold() {
    for _ in 0..5 {
        pick_coin()
    }
}
