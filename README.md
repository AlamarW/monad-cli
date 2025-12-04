# monad-cli

An experimental reimagining of basic command line tools in Haskell, using monadic transformations to enable seamless command composition.

## Motivation

Traditional Unix CLI tools have interface mismatches that create friction when composing commands. For example, to find all directories containing "bob" and list their contents, you can't simply write:

```bash
find -type d -name "bob" | ls
```

Instead, you need `xargs`:

```bash
find -type d -name "bob" | xargs ls
```

The problem: `xargs` exists solely to bridge the gap between commands that output to stdout and commands that take arguments. Some commands consume stdin, others don't. This inconsistency is inelegant.

## Approach

**Hypothesis**: If all basic commands operate in the same context and can be transformed monadically, they'll achieve greater interoperability and expressiveness.

By leveraging Haskell's type system and monadic composition, `monad-cli` aims to provide a unified interface where commands naturally compose without interface adapters.

## Status

Early exploration phase. The project is investigating:
- What common data model enables composition across diverse command types (ls, find, grep, cat, etc.)
- How to design command interfaces that compose naturally
- Whether this approach is practical for daily CLI use

If successful, this will become a daily driver CLI toolkit.

## Technical Details

- **Language**: Haskell
- **Build System**: Stack
- **Target Commands**: Starting with basic file operations (ls, find, grep, cat, etc.)
