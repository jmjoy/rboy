#![crate_name = "rboy"]
#![crate_type = "lib" ]

#![feature(io, core, path, path_ext)]
#![cfg_attr(test, feature(old_io))]

#[macro_use] extern crate log;
extern crate time;
extern crate podio;

pub use keypad::KeypadKey;

pub mod device;

mod cpu;
mod gbmode;
mod gpu;
mod keypad;
mod mbc;
mod mmu;
mod register;
mod serial;
mod sound;
mod timer;
mod util;
