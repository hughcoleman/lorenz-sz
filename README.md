# lorenz-sz

Simulator of the [Lorenz SZ40 cipher machine](https://en.wikipedia.org/wiki/Lorenz_cipher).

## Example

```rust
use lorenz_sz::{Machine, patterns::ZMUG_PATTERN};

let machine = Machine::new(&ZMUG_PATTERN);
for k in machine.keystream().take(50) {
    // You'll need to handle the encryption yourself; this crate only
    // implements the pseudo-random number generator.
    println!("{}", k);
}
```
