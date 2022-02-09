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
    stats                     : Stats,
    previous_frame_stats      : Option<StatsData>,
    current_frame_measurement : FrameMeasurementState,
    performance               : web::Performance,
    monitor                   : debug::Monitor,
    panels                    : Vec<debug::monitor::Panel>
}

impl {
    /// Constructor.
    pub fn new(stats:&Stats) -> Self {
        let stats                     = stats.clone_ref();
        let previous_frame_stats      = None;
        let current_frame_measurement = FrameMeasurementState::Skipped;
        let performance               = web::performance();
        let mut monitor               = debug::Monitor::new();
        let panels = vec![
            monitor.add::<debug::monitor::FrameTime>(),
            monitor.add::<debug::monitor::Fps>(),
            monitor.add::<debug::monitor::WasmMemory>(),
            monitor.add::<debug::monitor::GpuMemoryUsage>(),
            monitor.add::<debug::monitor::DrawCallCount>(),
            monitor.add::<debug::monitor::DataUploadCount>(),
            monitor.add::<debug::monitor::DataUploadSize>(),
            monitor.add::<debug::monitor::BufferCount>(),
            monitor.add::<debug::monitor::SymbolCount>(),
            monitor.add::<debug::monitor::ShaderCount>(),
            monitor.add::<debug::monitor::ShaderCompileCount>(),
            monitor.add::<debug::monitor::SpriteSystemCount>(),
            monitor.add::<debug::monitor::SpriteCount>(),
        ];
        Self {
            stats,
            previous_frame_stats,
            current_frame_measurement,
            performance,
            monitor,
            panels,
        }
    }

    /// Start measuring data.
    pub fn begin(&mut self) {
        if self.visible() {
            let time = self.performance.now();
            if self.current_frame_measurement == FrameMeasurementState::Ended {
                let previous_frame_stats = self.stats.begin_frame(time);
                for panel in &self.panels {
                    panel.sample_and_postprocess(&previous_frame_stats);
                }
                self.monitor.draw();
                self.previous_frame_stats = Some(previous_frame_stats);
            } else {
                let _ = self.stats.begin_frame(time);
                self.previous_frame_stats = None;
            }
            self.current_frame_measurement = FrameMeasurementState::InProgress;
        } else {
            self.current_frame_measurement = FrameMeasurementState::Skipped;
            self.previous_frame_stats = None;
        }
        // This should be done even when hidden in order for the stats not to overflow limits.
        self.stats.reset_per_frame_statistics();
    }

    /// Finish measuring data.
    pub fn end(&mut self) {
        if self.visible() && self.current_frame_measurement == FrameMeasurementState::InProgress {
            let time = self.performance.now();
            self.stats.end_frame(time);
            self.current_frame_measurement = FrameMeasurementState::Ended;
        } else {
            self.current_frame_measurement = FrameMeasurementState::Skipped;
        }
    }

    /// Returns a snapshot of statistics data for the previous rendering frame, if available.
    /// A previous rendering frame is the one before the most recent call to [`begin()`].
    ///
    /// Note: a `None` result is returned if [`Monitor`] was not [`visible()`] during [`begin()`]
    /// or [`end()`] of the previous frame, or during [`begin()`] of the current frame. That's
    /// because (as a performance optimization) we're skipping stats tracking when not
    /// [`visible()`].
    /// For example, if [`Monitor`] is not [`visible()`] during a call to [`end()`], we skip a
    /// [`Stats::end_frame()`] call, which makes it impossible for [`Stats`] to correctly calculate
    /// the value of [`StatsData::frame_time`].
    pub fn previous_frame_stats(&self) -> Option<StatsData> {
        self.previous_frame_stats
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


// === FrameMeasurementState ===

#[derive(Debug, PartialEq)]
enum FrameMeasurementState {
    Skipped,
    InProgress,
    Ended,
}
