# Introduction to Mini-lang

**Mini-lang** is a lightweight, interpreted scripting language built with Rust, inspired by ChatGPT. It is designed for simplicity and versatility, making it ideal for learning about interpreters and experimenting with custom scripting solutions.

Mini-lang aims to provide a clear and concise syntax, allowing users to quickly write and execute scripts for various use cases. Whether you're exploring programming language design or contributing to its development, Mini-lang offers a robust platform for innovation.

Join the journey of building and enhancing this exciting language!

# Features

- **Interpreted Execution**: Mini-lang scripts are executed directly, making it easy to write and test code quickly.
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
   ./target/release/mini test.mini
   ```

## Quick Start Guide

Here's a simple example to get started with Mini-lang:

1. **Hello, World!**

   Create a script `hello.mini`:
   ```mini
   print("Hello, World!")
   ```

2. **Run the script:**

   Test the code:
   ```bash
   ./target/release/mini hello.mini
   ```

   Expected Output:
   ```plaintext
   Hello, World!
   ```

3. **Simple Math**

   Create a script `math.mini`:
   ```mini
   let a = 5
   let b = 10
   print(a + b)
   ```

4. **Run the script:**

   Test the code:
   ```bash
   ./target/release/mini math.mini
   ```

   Expected Output:
   ```plaintext
   15
   ```
