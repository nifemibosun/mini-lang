## Side Note
The language is still in development, but we are open to contributions.

# Introduction to Mini-lang

**Mini-lang** is a lightweight, interpreted general-purpose language built with Rust. It is designed for simplicity and versatility, making it ideal for learning about interpreters and experimenting with custom language solutions.

Mini-lang aims to provide a clear and concise syntax, allowing users to quickly write and execute programs for various use cases. Whether you're exploring programming language design or contributing to its development, Mini-lang offers a robust platform for innovation.

Join the journey of building and enhancing this exciting language!

# Features

- **Interpreted Execution**: Mini-lang codes are executed directly, making it easy to write and test code quickly.
- **Lightweight Design**: Built with simplicity in mind, Mini-lang is easy to understand and extend.
- **Rust-Powered**: Leveraging the performance and safety of Rust, Mini-lang is efficient and reliable.
- **Customizable**: A great platform for experimenting with new language features or designing domain-specific languages.

# Installation Guide

Follow these steps to set up Mini-lang:

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/nifemibosun/mini-lang.git
   cd mini-lang
   ```

2. **Build the code**:
   ```bash
   cargo build --release
   ```

3. **Use the built binary to run a mini file**:
   ```bash
   ./target/release/mini examples/test.mini
   ```

## Quick Start Guide

Here's a simple example to get started with Mini-lang:

1. **Hello, World!**

   Create a file `hello.mini`:
   ```mini
   func main() {
       print("Hello, World!")
   }
   ```

2. **Run the program:**

   Test the code:
   ```bash
   ./target/release/mini hello.mini
   ```

   Expected Output:
   ```plaintext
   Hello, World!
   ```

3. **Simple Math**

   Create a file `math.mini`:
   ```mini
   function main() {
       // explicit type for a variable
       let a: int32 = 5
       // types can also be inferred
       let b = 10 // b is inferred as int32 
       print(a + b)
   }
   ```

4. **Run the program:**

   Test the code:
   ```bash
   ./target/release/mini math.mini
   ```

   Expected Output:
   ```plaintext
   15
   ```
