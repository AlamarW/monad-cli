# CLAUDE.md - Technical Context for AI Assistance

This file contains technical context and architectural considerations for AI assistants working on monad-cli.

## Project Vision

Create a composable CLI toolkit in Haskell where commands naturally chain together using monadic transformations, eliminating the need for interface adapters like `xargs`.

## Core Problem

Unix CLI tools have inconsistent interfaces:
- Some commands read from stdin (e.g., `grep`, `wc`)
- Some commands take arguments (e.g., `ls`, `cat`)
- Some do both (e.g., `cat`)

This forces users to use `xargs` to bridge the gap. Example:
```bash
# Doesn't work - ls doesn't read from stdin by default
find -type d -name "bob" | ls

# Requires xargs as glue
find -type d -name "bob" | xargs ls
```

## Solution Approach

Use Haskell's type system and monads to create a unified interface where:
1. All commands operate on a common data type
2. Commands compose using monadic bind (`>>=`) or do-notation
3. The type system ensures correct composition

## Open Research Questions

### 1. Data Model (CRITICAL - UNRESOLVED)

**Question**: What should be the common type that all commands operate on?

**Options to consider**:
- `Stream FilePath` - Simple, but limited to file operations
- `Stream Text` - General but loses structure
- `Stream Value` where `Value` is a sum type (files, text, numbers, records, etc.)
- `Stream (Map Text Value)` - Structured records like PowerShell objects
- Something else entirely

**Constraints**:
- Must support file operations (ls, find)
- Must support text operations (grep, sed, awk)
- Must support data transformation (cut, sort, uniq)
- Should be extensible for future command types

**Current Status**: Undecided. This is the foundational design decision.

### 2. Command Interface Design

How should commands be represented? Options:
- Functions: `Command a b` = `Stream a -> Stream b`
- Monad transformers: `CommandT m a`
- Free monads: `Free CommandF a`
- Arrow-based composition

### 3. Error Handling

How to handle errors in the pipeline:
- `ExceptT` transformer for explicit error handling?
- Follow Unix tradition of silent failures?
- Structured error types?

## Initial Implementation Targets

Start with these commands to prove the concept:
1. `ls` - List files/directories
2. `find` - Search for files
3. `grep` - Search text
4. `cat` - Output file contents

These cover the main interface patterns:
- Output generation (ls, find)
- Filtering (grep)
- Transformation (cat)

## Build Setup

- **Build System**: Stack
- **Language**: Haskell
- **Project Stage**: Exploratory research

## Development Philosophy

This is an exploratory project investigating whether monadic composition can solve real CLI interoperability problems. If it works, it will be used as a daily driver, so practical usability matters as much as theoretical elegance.

## Development Methodology

### Test Driven Development (TDD)

**MANDATORY**: All code must be developed using Test Driven Development.

1. Write a failing test first
2. Write the minimum code to make the test pass
3. Refactor based on ETC principles (see below)

No code should be written without a corresponding test written first.

### ETC: Easy To Change

**MANDATORY**: The refactoring phase of TDD must be guided by the ETC principle.

**Core Principle**: Every design decision should be evaluated based on: "How easy will it be for a new person to change this?"

When refactoring, ask:
- Can a newcomer understand what needs to change?
- Can they locate where to make the change?
- Can they make the change without breaking other things?
- Can they verify their change worked?

**Design for changeability**:
- Clear module boundaries
- Explicit dependencies
- Self-documenting code through types and names
- Minimal coupling between components
- Tests that serve as documentation

The goal is not just working code, but code that invites modification by developers who don't yet know the system.

## Key Architectural Considerations

1. **Performance**: Streaming is essential - can't materialize entire datasets
2. **Lazy Evaluation**: Leverage Haskell's laziness for efficiency
3. **Unix Philosophy**: Commands should do one thing well, but compose perfectly
4. **Backward Compatibility**: May need to interop with traditional Unix tools initially

## Notes for Development

- The data model decision should be made before implementing commands
- Consider writing example usage code first to validate the design
- Type signatures will be crucial for ensuring correct composition
- Tests should verify both correctness and composability
