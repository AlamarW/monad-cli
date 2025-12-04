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

## Data Model (RESOLVED)

**Decision**: Hybrid approach combining PowerShell-style records with Haskell type safety.

```haskell
data Value = VText Text | VInt Int | VPath FilePath | VBool Bool
type Record = Map Text Value
type Pipeline = Stream Record
```

**Rationale**:
- **Type Safety**: `Value` sum type enables pattern matching, exhaustiveness checking, and compile-time guarantees
- **Flexibility**: `Map Text Value` allows commands to add fields incrementally without breaking downstream commands
- **Composability**: Commands can work polymorphically on any record containing specific typed fields
- **Extensibility**: New `Value` variants can be added as needed for future command types

**How commands use this model**:
1. Commands read from `Pipeline` (Stream of Records)
2. Commands transform/filter/enrich records
3. Commands output to `Pipeline`
4. Type system ensures field values match expected types

**Example**:
```haskell
-- find outputs records with "path" field
find :: Pipeline

-- ls enriches with size, permissions
ls :: Pipeline -> Pipeline
ls = fmap $ \record ->
  case Map.lookup "path" record of
    Just (VPath p) -> enrichWithFileInfo p record
    _ -> record

-- grep adds matched_line field
grep :: Text -> Pipeline -> Pipeline

-- Composition
find >>= ls >>= grep "error" >>= sortBy "size"
```

## Open Research Questions

### 1. Command Interface Design

How should commands be represented? Options:
- Functions: `Command a b` = `Stream a -> Stream b`
- Monad transformers: `CommandT m a`
- Free monads: `Free CommandF a`
- Arrow-based composition

### 2. Error Handling

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

- The data model has been decided: `Stream (Map Text Value)` hybrid approach
- Start implementing commands using the defined data model
- Consider writing example usage code first to validate the design
- Type signatures will be crucial for ensuring correct composition
- Tests should verify both correctness and composability
