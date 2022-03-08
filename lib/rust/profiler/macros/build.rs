fn main() {
    println!("cargo:rerun-if-env-changed=ENSO_MAX_PROFILING_LEVEL");
    // This is a no-op assignment, except it makes cargo aware that the output depends on the env.
    let value = std::env::var("ENSO_MAX_PROFILING_LEVEL").unwrap_or(String::new());
    println!("cargo:rustc-env=ENSO_MAX_PROFILING_LEVEL={}", value);
}
