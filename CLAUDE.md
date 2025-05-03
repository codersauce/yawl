# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Build project: `cargo build`
- Run project: `cargo run -- <source_file>`
- Run with specific target: `cargo run -- <source_file> --target [arm64|x86_64]`
- Run tests: `cargo test`
- Run specific test: `cargo test <test_name>`
- Check code: `cargo check`
- Format code: `cargo fmt`
- Lint code: `cargo clippy`
- Build stdlib for specific target: `cargo build -p yawl-stdlib --target [aarch64-apple-darwin|x86_64-apple-darwin]`

## Architecture Support
This project supports both ARM64 and x86_64 architectures:
- For ARM64: Ensure the standard library is built for aarch64-apple-darwin
- For x86_64: Ensure the standard library is built for x86_64-apple-darwin
- Use the `--target` flag to specify which architecture to compile for
- Default is "native" which auto-detects the current architecture

## Code Style Guidelines
- Use 4-space indentation
- Imports: Group standard library, external crates, and local modules
- Structs/Enums: Use PascalCase for types and enum variants
- Variables/Functions: Use snake_case for variables, methods, and functions
- Error handling: Use `anyhow::Result` for functions that can fail
- Prefix variable names with type indicator (e.g. `nVar` for numbers)
- Use descriptive test names that explain the functionality being tested
- Write unit tests for new functionality
- Format code with `rustfmt` before committing
- Use `?` operator for error propagation
- Document public APIs with doc comments