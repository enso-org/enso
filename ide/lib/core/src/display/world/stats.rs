//! This module implements the stats monitor view, which can be visible on the screen in debug mode.

use crate::prelude::*;

use crate::debug::monitor::Monitor;
use crate::debug::monitor::Panel;
use crate::debug::monitor;
use crate::debug::stats::Stats;



// ====================
// === StatsMonitor ===
// ====================

shared! { StatsMonitor

/// Visual panel showing performance-related methods.
#[derive(Debug)]
pub struct StatsMonitorData {
    stats   : Stats,
    monitor : Monitor,
    panels  : Vec<Panel>
}

impl {
    /// Constructor.
    pub fn new(stats:&Stats) -> Self {
        let stats       = stats.clone_ref();
        let mut monitor = Monitor::new();
        let panels = vec![
            monitor.add( monitor::FrameTime          :: new()       ),
            monitor.add( monitor::Fps                :: new()       ),
            monitor.add( monitor::WasmMemory         :: new()       ),
            monitor.add( monitor::GpuMemoryUsage     :: new(&stats) ),
            monitor.add( monitor::DrawCallCount      :: new(&stats) ),
            monitor.add( monitor::DataUploadCount    :: new(&stats) ),
            monitor.add( monitor::DataUploadSize     :: new(&stats) ),
            monitor.add( monitor::BufferCount        :: new(&stats) ),
            monitor.add( monitor::SymbolCount        :: new(&stats) ),
            monitor.add( monitor::ShaderCount        :: new(&stats) ),
            monitor.add( monitor::ShaderCompileCount :: new(&stats) ),
            monitor.add( monitor::SpriteSystemCount  :: new(&stats) ),
            monitor.add( monitor::SpriteCount        :: new(&stats) ),
        ];
        Self {stats,monitor,panels}
    }

    /// Start measuring data.
    pub fn begin(&mut self) {
        if self.visible() {
            for panel in &self.panels {
                panel.begin();
            }
        }
    }

    /// Finish measuring data.
    pub fn end(&mut self) {
        if self.visible() {
            for panel in &self.panels {
                panel.end();
            }
            self.monitor.draw();
            self.stats.reset_per_frame_statistics();
        }
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
