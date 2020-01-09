#![allow(missing_docs)]

use nalgebra::clamp;
use std::ops::Mul;
use std::ops::Add;

pub fn linear_interpolation<T>(a:T, b:T, t:f32) -> T
    where T : Mul<f32, Output = T> + Add<T, Output = T> {
    let t = clamp(t, 0.0, 1.0);
    a * (1.0 - t) + b * t
}
