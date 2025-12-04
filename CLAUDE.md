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

data ErrorInfo = ErrorInfo
  { errorCommand :: Text
  , errorMessage :: Text
  , errorInput :: Maybe Record
  } deriving (Show)

type Pipeline = Stream (Either ErrorInfo Record)
```

**Rationale**:
- **Type Safety**: `Value` sum type enables pattern matching, exhaustiveness checking, and compile-time guarantees
- **Flexibility**: `Map Text Value` allows commands to add fields incrementally without breaking downstream commands
- **Composability**: Commands can work polymorphically on any record containing specific typed fields
- **Extensibility**: New `Value` variants can be added as needed for future command types
- **Error Handling**: `Either ErrorInfo Record` provides explicit error handling with short-circuit semantics (first error stops the pipeline)

**How commands use this model**:
1. Commands read from `Pipeline` (Stream of Either ErrorInfo Record)
2. Commands pattern match on `Left` (error) or `Right` (success)
3. Commands transform/filter/enrich successful records
4. Commands propagate errors or produce new errors
5. Type system ensures field values match expected types

**Example**:
```haskell
-- find outputs records with "path" field
find :: FilePath -> Pipeline

-- ls enriches with size, permissions, handles errors
ls :: Pipeline -> Pipeline
ls = fmap $ \eitherRecord ->
  case eitherRecord of
    Left err -> Left err  -- Propagate errors
    Right record ->
      case Map.lookup "path" record of
        Just (VPath p) -> Right $ enrichWithFileInfo p record
        _ -> Left $ ErrorInfo "ls" "Missing path field" (Just record)

-- grep adds matched_line field
grep :: Text -> Pipeline -> Pipeline

-- Composition using & operator (errors short-circuit)
find "." & ls & grep "error" & sortBy "size"
```

## Command Interface Design (RESOLVED)

**Decision**: Commands are plain functions that compose using the `&` operator from `Data.Function`.

```haskell
-- Commands are functions operating on Pipeline
find :: FilePath -> Pipeline
ls :: Pipeline -> Pipeline
grep :: Text -> Pipeline -> Pipeline

-- Composition uses & (reverse application)
find "." & ls & grep "error"
```

**Rationale**:
- **Simplicity**: Commands are just functions, no complex type wrappers needed
- **Familiar UX**: `&` provides left-to-right flow like Unix pipes (`|`)
- **Standard Library**: `&` already exists in `Data.Function` as `flip ($)`
- **No Learning Curve**: Users don't need to understand monads, arrows, or transformers

## CLI Usage Model (RESOLVED)

**Primary Model**: Compiled binary with command-line arguments

```bash
monad-cli 'find "." & ls & grep "error"'
```

Users pass pipeline expressions as strings. The tool parses and executes them.

**Future Possibility**: REPL/Interactive shell

```bash
$ monad-cli
> find "." & ls & grep "error"
[results...]
>
```

An interactive shell could be added with minimal additional cost.

## Error Handling (RESOLVED)

**Decision**: Use `Either ErrorInfo Record` with short-circuit semantics.

```haskell
data ErrorInfo = ErrorInfo
  { errorCommand :: Text
  , errorMessage :: Text
  , errorInput :: Maybe Record
  } deriving (Show)

type Pipeline = Stream (Either ErrorInfo Record)
```

**Rationale**:
- **No Silent Failures**: Errors are explicit and visible
- **Type Safety**: Compiler enforces error handling via `Either`
- **Short-Circuit**: First error stops pipeline execution (clear failure semantics)
- **Informative**: `ErrorInfo` captures what failed, why, and the input context
- **Haskell-Idiomatic**: Leverages standard Either monad for error handling

**Behavior**:
- Commands pattern match on `Left` (error) or `Right` (success)
- Errors propagate through the pipeline automatically
- First error encountered stops execution
- Final output indicates success with results or failure with error details

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

**Resolved Decisions**:
- **Data model**: `Stream (Either ErrorInfo (Map Text Value))` hybrid approach with explicit error handling
- **Command interface**: Plain functions composed with `&` operator
- **Error handling**: `Either` monad with short-circuit semantics (no silent failures)
- **CLI usage model**: Compiled binary accepting pipeline expressions as strings (REPL possible future)
- All foundational decisions are complete - implementation can proceed

**Next Steps**:
- Start implementing commands using the defined data model and interface
- Commands should be plain functions operating on `Pipeline = Stream (Either ErrorInfo Record)`
- Use `&` from `Data.Function` for composition
- All commands must handle errors explicitly (pattern match on Left/Right)
- Consider writing example usage code first to validate the design
- Type signatures will be crucial for ensuring correct composition
- Tests should verify both correctness, composability, and error handling
