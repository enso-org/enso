//! Math utilities with focus on computer graphics.

#![feature(trait_alias)]
#![feature(clamp)]

pub mod algebra;
pub mod geometry;
pub mod topology;

pub use algebra::Vector2;
pub use algebra::Vector3;
pub use algebra::Vector4;

pub use algebra::Matrix2;
pub use algebra::Matrix3;
pub use algebra::Matrix4;

pub use algebra::Matrix2x3;
pub use algebra::Matrix2x4;
pub use algebra::Matrix3x2;
pub use algebra::Matrix3x4;
pub use algebra::Matrix4x2;
pub use algebra::Matrix4x3;

/// Common types.
pub mod types {
    pub use super::algebra::Vector2;
    pub use super::algebra::Vector3;
    pub use super::algebra::Vector4;

    pub use super::algebra::Matrix2;
    pub use super::algebra::Matrix3;
    pub use super::algebra::Matrix4;

    pub use super::algebra::Matrix2x3;
    pub use super::algebra::Matrix2x4;
    pub use super::algebra::Matrix3x2;
    pub use super::algebra::Matrix3x4;
    pub use super::algebra::Matrix4x2;
    pub use super::algebra::Matrix4x3;
    pub use super::algebra::MatrixMN;

    pub use super::algebra::Zero;
    pub use super::algebra::zero;
    pub use super::algebra::HasComponents;
    pub use super::algebra::Dim1;
    pub use super::algebra::Dim2;
    pub use super::algebra::Dim3;
    pub use super::algebra::Abs;
    pub use super::algebra::Magnitude;
    pub use super::algebra::Normalize;
}
