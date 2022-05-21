#![warn(rust_2018_idioms, unreachable_pub, missing_docs)]
#![deny(unsafe_op_in_unsafe_fn)]

//! A library for hosting gomoku games in various rules.

/// Gomoku boards.
pub mod board;

/// Functionality for holding gomoku games.
pub mod game;

/// Gomoku variants and opening rules.
pub mod rule;
