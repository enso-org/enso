//! Constants describing the target operating system and architecture.
//!
//! Based upon the [`platforms-2.0.0`](https://docs.rs/platforms/2.0.0/platforms/target/enum.OS.html) crate. Future crate versions dropped support for this in favor
//! of [`std::env::consts::OS`] and [`std::env::consts::ARCH`] -- these however are just string
//! literals, rather than enums (that are much more useful compile-time). Thus we put back this
//! functionality here.

use crate::prelude::*;



// Copied from platforms-2.0.0 crate
// https://github.com/rustsec/rustsec
//
// Copyright (c) 2018-2020 The Rust Secure Code Working Group
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.


// Detect and expose `target_os` as a constant
// Whether this is a good idea is somewhat debatable



#[cfg(target_arch = "aarch64")]
/// `target_arch` when building this crate: `x86_64`
pub const TARGET_ARCH: Arch = Arch::AArch64;

#[cfg(target_arch = "arm")]
/// `target_arch` when building this crate: `arm`
pub const TARGET_ARCH: Arch = Arch::Arm;

#[cfg(target_arch = "asmjs")]
/// `target_arch` when building this crate: `asmjs`
pub const TARGET_ARCH: Arch = Arch::AsmJs;

#[cfg(target_arch = "mips")]
/// `target_arch` when building this crate: `mips`
pub const TARGET_ARCH: Arch = Arch::Mips;

#[cfg(target_arch = "mips64")]
/// `target_arch` when building this crate: `mips64`
pub const TARGET_ARCH: Arch = Arch::Mips64;

#[cfg(target_arch = "msp430")]
/// `target_arch` when building this crate: `msp430`
pub const TARGET_ARCH: Arch = Arch::Msp430;

#[cfg(target_arch = "nvptx64")]
/// `target_arch` when building this crate: `nvptx64`
pub const TARGET_ARCH: Arch = Arch::Nvptx64;

#[cfg(target_arch = "powerpc")]
/// `target_arch` when building this crate: `powerpc`
pub const TARGET_ARCH: Arch = Arch::PowerPc;

#[cfg(target_arch = "powerpc64")]
/// `target_arch` when building this crate: `powerpc64`
pub const TARGET_ARCH: Arch = Arch::PowerPc64;

#[cfg(target_arch = "riscv")]
/// `target_arch` when building this crate: `riscv`
pub const TARGET_ARCH: Arch = Arch::RiscV;

#[cfg(target_arch = "s390x")]
/// `target_arch` when building this crate: `s390x`
pub const TARGET_ARCH: Arch = Arch::S390X;

#[cfg(target_arch = "sparc")]
/// `target_arch` when building this crate: `sparc`
pub const TARGET_ARCH: Arch = Arch::Sparc;

#[cfg(target_arch = "sparc64")]
/// `target_arch` when building this crate: `sparc64`
pub const TARGET_ARCH: Arch = Arch::Sparc64;

#[cfg(target_arch = "wasm32")]
/// `target_arch` when building this crate: `wasm32`
pub const TARGET_ARCH: Arch = Arch::Wasm32;

#[cfg(target_arch = "x86")]
/// `target_arch` when building this crate: `x86`
pub const TARGET_ARCH: Arch = Arch::X86;

#[cfg(target_arch = "x86_64")]
/// `target_arch` when building this crate: `x86_64`
pub const TARGET_ARCH: Arch = Arch::X86_64;

#[cfg(target_os = "android")]
/// `target_os` when building this crate: `android`
pub const TARGET_OS: OS = OS::Android;

#[cfg(target_os = "cuda")]
/// `target_os` when building this crate: `cuda`
pub const TARGET_OS: OS = OS::Cuda;

#[cfg(target_os = "dragonfly")]
/// `target_os` when building this crate: `dragonfly`
pub const TARGET_OS: OS = OS::Dragonfly;

#[cfg(target_os = "emscripten")]
/// `target_os` when building this crate: `emscripten`
pub const TARGET_OS: OS = OS::Emscripten;

#[cfg(target_os = "freebsd")]
/// `target_os` when building this crate: `freebsd`
pub const TARGET_OS: OS = OS::FreeBSD;

#[cfg(target_os = "fuchsia")]
/// `target_os` when building this crate: `fuchsia`
pub const TARGET_OS: OS = OS::Fuchsia;

#[cfg(target_os = "haiku")]
/// `target_os` when building this crate: `haiku`
pub const TARGET_OS: OS = OS::Haiku;

#[cfg(target_os = "hermit")]
/// `target_os` when building this crate: `hermit`
pub const TARGET_OS: OS = OS::Hermit;

#[cfg(target_os = "illumos")]
/// `target_os` when building this crate: `illumos`
pub const TARGET_OS: OS = OS::Illumos;

#[cfg(target_os = "ios")]
/// `target_os` when building this crate: `ios`
pub const TARGET_OS: OS = OS::iOS;

#[cfg(target_os = "linux")]
/// `target_os` when building this crate: `linux`
pub const TARGET_OS: OS = OS::Linux;

#[cfg(target_os = "macos")]
/// `target_os` when building this crate: `macos`
pub const TARGET_OS: OS = OS::MacOS;

#[cfg(target_os = "netbsd")]
/// `target_os` when building this crate: `netbsd`
pub const TARGET_OS: OS = OS::NetBSD;

#[cfg(target_os = "openbsd")]
/// `target_os` when building this crate: `openbsd`
pub const TARGET_OS: OS = OS::OpenBSD;

#[cfg(target_os = "redox")]
/// `target_os` when building this crate: `redox`
pub const TARGET_OS: OS = OS::Redox;

#[cfg(target_os = "solaris")]
/// `target_os` when building this crate: `solaris`
pub const TARGET_OS: OS = OS::Solaris;

#[cfg(target_os = "tvos")]
/// `target_os` when building this crate: `tvos`
pub const TARGET_OS: OS = OS::TvOS;

#[cfg(target_os = "wasi")]
/// `target_os` when building this crate: `wasi`
pub const TARGET_OS: OS = OS::Wasi;

#[cfg(target_os = "windows")]
/// `target_os` when building this crate: `windows`
pub const TARGET_OS: OS = OS::Windows;

#[cfg(target_os = "vxworks")]
/// `target_os` when building this crate: `vxworks`
pub const TARGET_OS: OS = OS::VxWorks;
