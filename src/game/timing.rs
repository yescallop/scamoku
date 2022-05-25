use std::time::Duration;

/// The timing settings of a game.
#[derive(Debug, Clone, Default)]
pub struct TimingSettings {
    /// The timeout for each move.
    pub move_timeout: Duration,
    /// The timeout for the game.
    pub game_timeout: Duration,
    /// The time added after each move.
    pub time_increment: Duration,
}