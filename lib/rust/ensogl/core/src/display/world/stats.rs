//! This module implements the stats monitor view, which can be visible on the screen in debug mode.

use crate::prelude::*;

use crate::debug;
use crate::debug::stats::Stats;



// ===============
// === Monitor ===
// ===============

shared! { Monitor

/// Visual panel showing performance-related methods.
#[derive(Debug)]
pub struct MonitorData {
    stats    : Stats,
    monitor  : debug::Monitor,
    panels   : Vec<debug::monitor::Panel>,
    snapshot : Vec<f64>,
}

impl {
    /// Constructor.
    pub fn new(stats:&Stats) -> Self {
        let stats       = stats.clone_ref();
        let mut monitor = debug::Monitor::new();
        let panels = vec![
            monitor.add( debug::monitor::FrameTime          :: new()       ),
            monitor.add( debug::monitor::Fps                :: new()       ),
            monitor.add( debug::monitor::WasmMemory         :: new()       ),
            monitor.add( debug::monitor::GpuMemoryUsage     :: new(&stats) ),
            monitor.add( debug::monitor::DrawCallCount      :: new(&stats) ),
            monitor.add( debug::monitor::DataUploadCount    :: new(&stats) ),
            monitor.add( debug::monitor::DataUploadSize     :: new(&stats) ),
            monitor.add( debug::monitor::BufferCount        :: new(&stats) ),
            monitor.add( debug::monitor::SymbolCount        :: new(&stats) ),
            monitor.add( debug::monitor::ShaderCount        :: new(&stats) ),
            monitor.add( debug::monitor::ShaderCompileCount :: new(&stats) ),
            monitor.add( debug::monitor::SpriteSystemCount  :: new(&stats) ),
            monitor.add( debug::monitor::SpriteCount        :: new(&stats) ),
        ];
        let snapshot = Vec::with_capacity(panels.len());
        Self {stats,monitor,panels,snapshot}
    }

    /// Start measuring data.
    pub fn begin(&mut self) {
        // FIXME: before, there was optimisation to only collect data if visible; how to do similar
        // optimization w.r.t. Profiling Framework collecting/not-collecting?
        for panel in &self.panels {
            panel.begin();
        }
    }

    /// Finish measuring data.
    pub fn end(&mut self) {
        // FIXME: before, there was optimisation to only collect data if visible; how to do similar
        // optimization w.r.t. Profiling Framework collecting/not-collecting?
        self.snapshot.clear();
        for panel in &self.panels {
            panel.end();
            self.snapshot.push(panel.raw_value());
        }
        if self.visible() {
            self.monitor.draw();
        }
        // This should be done even when hidden in order for the stats not to overflow limits.
        self.stats.reset_per_frame_statistics();
    }

    pub fn read_snapshot<F:FnOnce(&Vec<f64>)>(&self, f:F) {
        // NOTE: cannot just make it `fn snapshot(&self) -> &Vec<f64>` because of the `shared!`
        // macro
        f(&self.snapshot);
    }

    /// Checks if the monitor is visible.
    pub fn visible(&self) -> bool {
        self.monitor.visible()
    }

    /// Show the monitor.
    pub fn show(&mut self) {
        self.monitor.show()
    }

    /// Hide the monitor.
    pub fn hide(&mut self) {
        self.monitor.hide()
    }

    /// Toggle the visibility of the monitor.
    pub fn toggle(&mut self) {
        self.monitor.toggle()
    }
}}
