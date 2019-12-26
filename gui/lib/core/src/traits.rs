use nalgebra::Vector3;

pub trait HasPosition {
    /// Gets self's position.
    fn position(&self) -> Vector3<f32>;
    /// Sets self's position.
    fn set_position(&mut self, position:Vector3<f32>);
}
