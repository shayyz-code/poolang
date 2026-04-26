# Getting Started

## Installation

### Using Automated Installers

The fastest way to install PooLang is via our universal installers:

**Linux / macOS**
```bash
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.sh | sh
```

**Windows**
```powershell
powershell -c "irm https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.ps1 | iex"
```

### From Source

If you have Rust and Cargo installed, you can build from source:

```bash
git clone https://github.com/shayyz-code/poolang.git
cd poolang
cargo build --release
```

The binary will be available at `./target/release/poo`.

## Running your first program

Create a file named `hello.poo`:

```poo
use std::pout;

pout("Hello, Guinea Pig!");
```

Run it using the PooLang CLI:

```bash
poo hello.poo
```

## Next Steps

Check out the [Variables & Types](./syntax/variables.md) section to learn more about PooLang syntax.
