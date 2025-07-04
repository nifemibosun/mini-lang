# mini-lang
A modern, systems programming language focused on clarity and efficiency. Also to clarify the name mini does not mean it aims to be a toy language.

## Installation

### Prerequisites

To build and run Mini, you need to have **Rust** and its package manager, **Cargo**, installed on your system.

If you don't have Rust installed, you can install it via `rustup` by running the following command in your terminal:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## Building from Source

### Clone the repository: 
```bash
git clone https://github.com/nifemibosun/mini-lang.git

cd mini-lang
```

### Build the project:
```bash
cargo build --release
```
This will compile the project and create an optimized executable in the target/release/ directory.


## Usage
Once compiled, you can use it to run mini source from your terminal.

so create a mini source call it hello_world.mini and paste in this code:
```mini
func mini() {
    println("Hello, Mini\n");
}
```

### Running a File
To execute a mini source file:

``` bash
./target/release mini run hello_world.mini
```

### Interactive Prompt (REPL)
To enter the interactive prompt, where you can type mini code line by line:
```bash
./target/release mini
```

```bash
> print "Hello from REPL!";
// (Output will be tokens for now)
> 1 + 2;
// (Output will be tokens for now)
> exit
```

### Help and Version
To see the usage instructions and available flags:
```bash
mini --help
# or
mini -h
```

To display the current version of Mini:
```bash
mini -v
```


## Project Status
Mini is currently in its very early stages.

Contributing
Contributions are very much welcomed!