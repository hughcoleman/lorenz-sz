//! Simulator of the [Lorenz SZ40 cipher machine](https://en.wikipedia.org/wiki/Lorenz_cipher).
//!
//! ## Example
//!
//! ```rust
//! use lorenz_sz::{Machine, patterns::ZMUG_PATTERN};
//!
//! let machine = Machine::new(&ZMUG_PATTERN);
//! for k in machine.keystream().take(50) {
//!     // You'll need to handle the encryption yourself; this crate only
//!     // implements the pseudo-random number generator.
//!     println!("{}", k);
//! }
//! ```

#![no_std]

use core::iter::{Cycle, Iterator, Peekable};
use core::slice::Iter;

pub type ChiPattern = ([bool; 41], [bool; 31], [bool; 29], [bool; 26], [bool; 23]);
pub type MuPattern = ([bool; 37], [bool; 61]);
pub type PsiPattern = ([bool; 43], [bool; 47], [bool; 51], [bool; 53], [bool; 59]);

/// A Lorenz SZ40 machine.
pub struct Machine<'a> {
    chi: &'a ChiPattern,
    mu: &'a MuPattern,
    psi: &'a PsiPattern,
}

impl<'a> Machine<'a> {
    /// Create a machine from a set of wheel patterns.
    pub fn new((chi, mu, psi): &'a (ChiPattern, MuPattern, PsiPattern)) -> Self {
        Machine { chi, mu, psi }
    }

    /// Obtain an iterator over the keystream produced by the machine.
    pub fn keystream(&self) -> Keystream {
        Keystream {
            // I suppose that the chi streams don't need to be peekable, since
            // they always step. But it makes the typing easier, and presumably
            // the compiler is smart enough to optimise this.
            chi: (
                self.chi.0.iter().cycle().peekable(),
                self.chi.1.iter().cycle().peekable(),
                self.chi.2.iter().cycle().peekable(),
                self.chi.3.iter().cycle().peekable(),
                self.chi.4.iter().cycle().peekable(),
            ),
            mu: (
                self.mu.0.iter().cycle().peekable(),
                self.mu.1.iter().cycle().peekable(),
            ),
            psi: (
                self.psi.0.iter().cycle().peekable(),
                self.psi.1.iter().cycle().peekable(),
                self.psi.2.iter().cycle().peekable(),
                self.psi.3.iter().cycle().peekable(),
                self.psi.4.iter().cycle().peekable(),
            ),
        }
    }
}

type Impulse<'a> = Peekable<Cycle<Iter<'a, bool>>>;

/// A keystream produced by a Lorenz SZ40 machine.
pub struct Keystream<'a> {
    chi: (
        Impulse<'a>,
        Impulse<'a>,
        Impulse<'a>,
        Impulse<'a>,
        Impulse<'a>,
    ),
    mu: (Impulse<'a>, Impulse<'a>),
    psi: (
        Impulse<'a>,
        Impulse<'a>,
        Impulse<'a>,
        Impulse<'a>,
        Impulse<'a>,
    ),
}

impl<'a> Iterator for Keystream<'a> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // Determine the total motor. For now, we're ignoring limitations, so
        // this is just the basic motor.
        let basic_motor: bool = if *self.mu.1.next().unwrap() {
            *self.mu.0.next().unwrap()
        } else {
            **self.mu.0.peek().unwrap()
        };
        let total_motor = basic_motor;

        // Produce the keystream character.
        let chi: u32 = (*self.chi.0.next().unwrap() as u32)
            | ((*self.chi.1.next().unwrap() as u32) << 1)
            | ((*self.chi.2.next().unwrap() as u32) << 2)
            | ((*self.chi.3.next().unwrap() as u32) << 3)
            | ((*self.chi.4.next().unwrap() as u32) << 4);
        if total_motor {
            Some(
                chi ^ ((*self.psi.0.next().unwrap() as u32)
                    | ((*self.psi.1.next().unwrap() as u32) << 1)
                    | ((*self.psi.2.next().unwrap() as u32) << 2)
                    | ((*self.psi.3.next().unwrap() as u32) << 3)
                    | ((*self.psi.4.next().unwrap() as u32) << 4)),
            )
        } else {
            Some(
                chi ^ ((**self.psi.0.peek().unwrap() as u32)
                    | ((**self.psi.1.peek().unwrap() as u32) << 1)
                    | ((**self.psi.2.peek().unwrap() as u32) << 2)
                    | ((**self.psi.3.peek().unwrap() as u32) << 3)
                    | ((**self.psi.4.peek().unwrap() as u32) << 4)),
            )
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_produces_zmug_keystream() {
        // This test-case is based on CyberChef's implementation of Lorenz.
        let machine = Machine::new(&patterns::ZMUG_PATTERN);
        let mut keystream = machine.keystream();
        assert_eq!(keystream.next(), Some(0b01010 ^ 0b10001));
        assert_eq!(keystream.next(), Some(0b11000 ^ 0b10001));
        assert_eq!(keystream.next(), Some(0b00101 ^ 0b01110));
        assert_eq!(keystream.next(), Some(0b11101 ^ 0b10110));
        assert_eq!(keystream.next(), Some(0b00110 ^ 0b11100));
        let mut keystream = keystream.skip(995);
        assert_eq!(keystream.next(), Some(0b11110));
        assert_eq!(keystream.next(), Some(0b01110));
        assert_eq!(keystream.next(), Some(0b11111));
        assert_eq!(keystream.next(), Some(0b10111));
        assert_eq!(keystream.next(), Some(0b11010));
    }
}

pub mod patterns {
    use crate::{ChiPattern, MuPattern, PsiPattern};

    /// The cams used by the infamous 30 August 1941 "HQIBPEXEZMUG" message.
    #[rustfmt::skip]
    pub const ZMUG_PATTERN: (ChiPattern, MuPattern, PsiPattern) = (
        (
            [false, false, true , true , false, false, true , true , true , false, false, false, false, true , true , true , false, false, true , true , true , false, false, false, false, true , false, false, true , true , false, true , true , false, false, false, true , true , false, true , true ],
            [true , false, false, false, true , true , false, false, true , true , false, false, false, true , false, true , true , true , true , false, true , true , true , false, false, false, false, true , true , false, true ],
            [false, false, true , true , true , true , false, true , true , false, false, false, true , true , true , false, false, false, true , true , false, false, false, true , true , false, false, true , false],
            [true , true , false, true , false, false, true , true , true , false, true , false, false, true , true , false, false, false, true , true , false, false, true , false, true , false],
            [false, true , false, true , false, false, false, false, true , true , true , false, true , false, false, false, true , true , true , true , false, false, true ],
        ),
        (
            [false, true , true , true , false, true , true , true , false, true , true , true , false, true , false, true , true , false, true , true , true , false, true , true , true , false, true , true , false, true , false, true , true , false, true , false, true ],
            [true , true , false, true , false, true , false, true , true , true , true , false, true , false, true , false, true , true , true , false, true , true , true , false, true , true , true , false, true , true , false, true , true , false, true , true , false, true , true , false, true , true , true , false, true , false, true , false, true , true , true , false, true , true , true , false, true , false, true , true , false],
        ),
        (
            [true , false, false, false, true , true , true , false, false, true , true , true , false, false, true , true , true , true , false, false, false, true , true , false, false, false, true , true , false, false, true , true , true , false, false, false, true , true , false, false, true , false, true ],
            [false, true , true , false, true , false, false, true , true , true , false, false, true , true , true , false, false, true , true , false, false, false, true , true , true , true , false, false, false, true , true , true , false, false, true , true , false, false, true , true , true , false, false, false, true , false, false],
            [false, true , true , true , false, false, true , true , false, false, false, true , true , false, false, false, true , true , true , false, false, false, true , false, false, false, true , true , true , true , false, false, false, true , false, false, true , true , true , false, false, true , true , true , false, false, true , true , false, false, true ],
            [false, true , false, true , true , true , false, false, true , true , false, false, false, true , true , false, false, true , false, false, true , true , true , false, false, true , true , false, false, false, true , false, false, false, true , true , true , true , false, false, true , true , true , false, false, true , true , false, false, true , true , true , false],
            [true , false, true , true , false, false, false, true , false, false, true , true , true , false, false, true , true , false, false, true , true , true , false, false, true , true , false, false, false, true , true , true , true , false, false, true , false, false, true , true , true , false, false, true , true , true , true , false, false, false, true , false, false, false, true , true , true , false, false],
        ),
    );
}
