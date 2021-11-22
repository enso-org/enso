//! Crate containing the Engine Services binary protocol interface.

pub mod client;
pub mod connection;
pub mod message;
pub mod serialization;
pub mod uuid;

pub use client::Client;
pub use client::Event;
pub use client::MockAPI as MockClient;
pub use client::Notification;
pub use client::API;
pub use connection::Connection;
