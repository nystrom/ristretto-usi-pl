# Perc

An IR with explicit heap memory layout, but temporaries for stack and register variables.
The IR is flat and easy to optimize.
This IR is close to assembly, but good for doing optimizations.

We generate Asm with just a simple 1-pass translation.
