//! A diagram type for illustrating timings of messages between processes.
//!
//! Like a UML timing diagram, this illustrates interactions between processes over time. However,
//! this diagram is simpler (the UML diagram type supports nonspecific times, process state changes,
//! and many other data complexities); as a result, it is better suited to display of large numbers
//! of events spread unevenly over a long time range than any UML renderers I[KW] am aware of.



// ===============
// === Diagram ===
// ===============

/// The data necessary to create a diagram of message timings.
#[derive(Debug, Default)]
pub struct Diagram<'a> {
    processes: Vec<&'a str>,
    messages:  Vec<Message>,
}

impl<'a> Diagram<'a> {
    /// Define a new process. Return a handle.
    pub fn process<'b: 'a>(&mut self, name: &'b str) -> Process {
        let id = self.processes.len();
        self.processes.push(name);
        Process { id }
    }

    /// Log a message between two processes.
    pub fn message(&mut self, sender: Process, recipient: Process, time: f64, label: String) {
        self.messages.push(Message { sender, recipient, time, label });
    }
}


// === Process ===

/// A process that may send and receive messages.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Process {
    id: usize,
}


// === Message ===

/// An event of communication between processes.
#[derive(Clone, Debug)]
pub struct Message {
    sender:    Process,
    recipient: Process,
    time:      f64,
    label:     String,
}



// ====================
// === Svg rendering ==
// ====================

/// Rendering a [`Diagram`] in SVG format.
pub mod svg {
    use super::*;

    // How far apart the poles should be.
    // Unit is the same size as 1 millisecond, but the X-axis is not quantitative.
    const POLE_SPACING: f64 = 500.0;

    const GRID_INTERVAL_MS: f64 = 100.0;

    // How much space to allow for headers, in millisecond-equivalent units.
    const HEADER_HEIGHT: f64 = 40.0;
    // Where to position the header text.
    const HEADER_BASELINE: f64 = HEADER_HEIGHT - 10.0;
    const FOOTER_HEIGHT: f64 = 40.0;

    // Y-offset of the point of the arrow from the text field's `y` attribute.
    // With the current hack of using unicode to get arrows, this must be experimentally
    // determined. Its exact value isn't important because it only affects placement
    // of everything relative to the grid, not differences between measurements, and the
    // grid doesn't need to be lined up to the millisecond.
    const Y_FUDGE: f64 = 4.0;

    const GRID_COLOR_RGB: &str = "bbbbbb";

    /// Write a SVG representation of the given [`Diagram`].
    pub fn write_diagram(dia: &Diagram, mut f: impl std::io::Write) -> std::io::Result<()> {
        writeln!(f, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")?;
        let xmlns = "xmlns=\"http://www.w3.org/2000/svg\"";
        let mut max_time = 0.0;
        for message in dia.messages.iter() {
            if message.time > max_time {
                max_time = message.time;
            }
        }
        let height = max_time;
        writeln!(f, "<svg height=\"{}\" {}>", height + HEADER_HEIGHT + FOOTER_HEIGHT, xmlns,)?;
        const LEFT: &str = "end";
        const MIDDLE: &str = "middle";
        const RIGHT: &str = "start";
        for (i, p) in dia.processes.iter().enumerate() {
            let offset_to_center = POLE_SPACING / 2.0;
            writeln!(
                f,
                "<text y=\"{}\" x=\"{}\" text-anchor=\"{}\">{}</text>",
                HEADER_BASELINE,
                POLE_SPACING * i as f64 + offset_to_center,
                MIDDLE,
                p,
            )?;
        }
        for i in 0..((height / GRID_INTERVAL_MS) as usize + 1) {
            writeln!(
                f,
                "<text y=\"{}\" x=\"{}\" text-anchor=\"{}\">{}ms</text>",
                HEADER_HEIGHT + GRID_INTERVAL_MS * i as f64,
                0,
                RIGHT,
                GRID_INTERVAL_MS * i as f64,
            )?;
            let y_pos = HEADER_HEIGHT + GRID_INTERVAL_MS * i as f64;
            let x_len = POLE_SPACING * dia.processes.len() as f64;
            let path = format!("M0,{} l{},0", y_pos, x_len);
            writeln!(f, "<path fill=\"none\" stroke=\"#{}\" d=\"{}\"/>", GRID_COLOR_RGB, path)?;
        }
        for i in 1..dia.processes.len() {
            let path = format!("M{},{} l0,{}", POLE_SPACING * i as f64, HEADER_HEIGHT, height);
            writeln!(f, "<path fill=\"none\" stroke=\"black\" d=\"{}\"/>", path)?;
        }
        let simple_only = "Drawing messages between non-adjacent processes is not implemented.";
        let mut pairs = std::collections::HashMap::new();
        struct Pair {
            index:   usize,
            forward: bool,
        }
        for index in 1..dia.processes.len() {
            let i0 = Process { id: index - 1 };
            let i1 = Process { id: index };
            pairs.insert((i0, i1), Pair { index, forward: true });
            pairs.insert((i1, i0), Pair { index, forward: false });
        }
        for m in &dia.messages {
            let pair = (m.sender, m.recipient);
            let Pair { index, forward } = *pairs.get(&pair).expect(simple_only);
            let x = index as f64 * POLE_SPACING;
            if forward {
                writeln!(
                    f,
                    "<text y=\"{}\" x=\"{}\" text-anchor=\"{}\">{}▶</text>",
                    HEADER_HEIGHT + m.time + Y_FUDGE,
                    x,
                    LEFT,
                    m.label,
                )?;
            } else {
                writeln!(
                    f,
                    "<text y=\"{}\" x=\"{}\" text-anchor=\"{}\">◀{}</text>",
                    HEADER_HEIGHT + m.time + Y_FUDGE,
                    x,
                    RIGHT,
                    m.label,
                )?;
            }
        }
        writeln!(f, "</svg>")?;
        Ok(())
    }
}
