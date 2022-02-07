//! This module implements the stats monitor view, which can be visible on the screen in debug mode.

use crate::prelude::*;

use crate::debug;
use crate::debug::stats::Stats;
use crate::debug::stats::StatsData;
use crate::system::web;



// ===============
// === Monitor ===
// ===============

shared! { Monitor

/// Visual panel showing performance-related methods.
#[derive(Debug)]
pub struct MonitorData {
    stats            : Stats,
    performance      : web::Performance,
    monitor          : debug::Monitor,
    panels           : Vec<debug::monitor::Panel>
}

impl {
    /// Constructor.
    pub fn new(stats:&Stats) -> Self {
        let stats            = stats.clone_ref();
        let performance      = web::performance();
        let mut monitor      = debug::Monitor::new();
        let panels = vec![
            monitor.add( debug::monitor::FrameTime          :: default() ),
            monitor.add( debug::monitor::Fps                :: default() ),
            monitor.add( debug::monitor::WasmMemory         :: default() ),
            monitor.add( debug::monitor::GpuMemoryUsage     :: default() ),
            monitor.add( debug::monitor::DrawCallCount      :: default() ),
            monitor.add( debug::monitor::DataUploadCount    :: default() ),
            monitor.add( debug::monitor::DataUploadSize     :: default() ),
            monitor.add( debug::monitor::BufferCount        :: default() ),
            monitor.add( debug::monitor::SymbolCount        :: default() ),
            monitor.add( debug::monitor::ShaderCount        :: default() ),
            monitor.add( debug::monitor::ShaderCompileCount :: default() ),
            monitor.add( debug::monitor::SpriteSystemCount  :: default() ),
            monitor.add( debug::monitor::SpriteCount        :: default() ),
        ];
        Self {stats,performance,monitor,panels}
    }

    /// Start measuring data.
    pub fn begin(&mut self) -> Option<StatsData> {
        let stats_snapshot = if self.visible() {
            let time = self.performance.now();
            let previous_frame_stats = self.stats.begin_frame(time);
            for panel in &self.panels {
                panel.sample_and_postprocess(&previous_frame_stats);
            }
            self.monitor.draw();
            Some(previous_frame_stats)
        } else {
            None
        };
        // This should be done even when hidden in order for the stats not to overflow limits.
        self.stats.reset_per_frame_statistics();
        stats_snapshot
    }

    /// Finish measuring data.
    pub fn end(&mut self) {
        if self.visible() {
            let time = self.performance.now();
            self.stats.end_frame(time);
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
