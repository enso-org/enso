//! Library for working with simple uncompressed bitmaps.

// === Features ===
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



// ======================
// === Raw Image Data ===
// ======================

/// Raw image data.
#[derive(Debug)]
pub struct Image {
    /// Number of pixels.
    pub width:  usize,
    /// Number of rows.
    pub height: usize,
    /// Pixel data (RGB).
    pub data:   Vec<u8>,
}



// ==================
// === PPM Format ===
// ==================

impl Image {
    /// Encode in the PPM format.
    pub fn encode_ppm(&self) -> Vec<u8> {
        debug_assert_eq!(self.height * self.width * CHANNELS, self.data.len());
        let mut out = Vec::new();
        let width = self.width;
        let height = self.height;
        let header = format!("{PPM_MAGIC}\n{width} {height}\n255\n");
        out.extend(header.bytes());
        out.extend(self.data.iter().map(ppm_value));
        out
    }

    /// Decode from the PPM format.
    pub fn decode_ppm(data: &[u8]) -> Result<Self, Error> {
        const HEADER_LINES: usize = 3;
        const BODY_LINE: usize = 1;
        let mut lines = data.splitn(HEADER_LINES + BODY_LINE, |&x| x == b'\n');
        let magic = lines.next().ok_or(Error::Truncated)?;
        if magic != PPM_MAGIC.as_bytes() {
            return Err(Error::WrongFormat);
        }
        const DIMENSIONS: usize = 2;
        let dimensions = lines.next().ok_or(Error::Truncated)?;
        let dimensions = std::str::from_utf8(dimensions).map_err(|_| Error::Invalid)?;
        let (height, width) = dimensions.split_once(' ').ok_or(Error::Truncated)?;
        let height = height.parse().map_err(|_| Error::Invalid)?;
        let width = width.parse().map_err(|_| Error::Invalid)?;
        let num_shades = lines.next().ok_or(Error::Truncated)?;
        if num_shades != b"255" {
            return Err(Error::Invalid);
        }
        let data: Vec<_> = lines.next().ok_or(Error::Truncated)?.iter().map(ppm_value).collect();
        debug_assert_eq!(height * width * CHANNELS, data.len());
        Ok(Self { width, height, data })
    }
}

/// Map a channel value to or from PPM encoding, which is inverted from most formats.
fn ppm_value(x: &u8) -> u8 {
    255 - x
}

/// Encoding/decoding errors.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// The file does not appear to be in the expected format.
    WrongFormat,
    /// The file is invalid or uses unsupported features.
    Invalid,
    /// The file is invalid or may be truncated.
    Truncated,
}

/// The first bytes of a PPM file.
const PPM_MAGIC: &str = "P6";
const CHANNELS: usize = 3;
