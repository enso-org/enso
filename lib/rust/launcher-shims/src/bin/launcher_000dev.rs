use launcher_shims::wrap_launcher;



// ===========================
// === EntryPoint0.0.0-dev ===
// ===========================

/// Runs the launcher wrapper overriding the version to 0.0.0-dev.
fn main() {
    wrap_launcher("0.0.0-dev")
}
