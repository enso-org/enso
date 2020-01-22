//! This module implements the stats monitor view, which can be visible on the screen in debug mode.

use crate::prelude::*;

use crate::debug::monitor::Monitor;
use crate::debug::monitor::Panel;
use crate::debug::monitor;
use crate::debug::stats::Stats;



// ====================
// === StatsMonitor ===
// ====================

#[derive(Clone,Debug)]
pub struct StatsMonitor {
    rc: Rc<RefCell<StatsMonitorData>>
}

impl StatsMonitor {
    pub fn new(stats:&Stats) -> Self {
        let rc = Rc::new(RefCell::new(StatsMonitorData::new(stats)));
        Self {rc}
    }

    pub fn begin(&self) {
        self.rc.borrow_mut().begin()
    }

    pub fn end(&self) {
        self.rc.borrow_mut().end()
    }
}


#[derive(Debug)]
pub struct StatsMonitorData {
    stats   : Stats,
    monitor : Monitor,
    panels  : Vec<Panel>
}

impl StatsMonitorData {
    fn new(stats:&Stats) -> Self {
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

    fn begin(&mut self) {
        for panel in &self.panels {
            panel.begin();
        }
    }

    fn end(&mut self) {
        for panel in &self.panels {
            panel.end();
        }
        self.monitor.draw();
        self.stats.reset_per_frame_statistics();
    }
}