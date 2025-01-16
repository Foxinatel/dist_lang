# A Language for distributed computation

Internally referred to Twoes (pronounced "toos") as a playful quip on it's intention as the spiritual successor to the Blues language. This name is likely to change in future.

### Language Details:

- Statically typed
- Call by value
- Takes the shape of a lambda calculus

### Frontend

While the option of reusing the PEST grammar written for the Blues language was available, the parser has instead been written from the ground up using the Logos lexer the Chumsky parser combinator library.

### Backend

This language utilises a custom-written CEK machine as its core runtime.