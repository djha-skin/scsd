# AI Development Notes

## Project Understanding

SCSD (Source-Control-Storable Database) is both:

1. A text file format for storing tabular data
2. An API for manipulating tables

### File Format
- Nearly a strict subset of CommonMark with Pandoc pipe tables extension
- One document = One database
- Each database has:
  - Name (H1 header)
  - Optional description
  - Zero or more tables
- Each table has:
  - Name (H2 header)
  - Optional description  
  - Header row with column names
  - Type specification row
  - Data rows

### Data Types
- Strings (-)
- Keywords/Symbols (:-) 
- Numbers (-:)
- Booleans (:-:)
- All columns are nullable

### Required Library Features
The Common Lisp library needs to provide:

1. Parsing
   - Read SCSD format files into memory
   - Support all data types
   - Handle escaped characters

2. Serialization
   - Write tables back to SCSD format
   - Maintain formatting and descriptions

3. Table Operations
   - Natural join
   - Natural equijoin  
   - Cartesian product
   - Row filtering
   - Column filtering

### Development Strategy
1. Start with core parsing functionality
2. Add serialization 
3. Implement table manipulation operations
4. Add convenience functions and optimizations

### Project Status
Initial setup complete. Starting parser implementation.

### Notes To Self
- Read ABNF grammar carefully when implementing parser
- Consider using parser combinators or similar approach
- Pay attention to string escaping rules
- Tables should be primary objects that support operations
- Keep data structures immutable where possible
- Use internal packages (`#:scsd/...`) to avoid polluting the main `#:scsd` package. Re-export public API from main `scsd.lisp`.

## Session 2025-05-04 15:38

- Resumed session on 2025-05-04 15:38:25.
- Reviewed `prompt`, `AI.md`, and `TODO.md`.
- Completed initial setup tasks:
    - Created `scsd.asd`, `package.lisp`.
    - Set up test framework (`fiveam`, `scsd-test.asd`, `tests/main.lisp`, `scripts/test.ros`), including troubleshooting ASDF issues.
    - Updated `README.md` with development setup.
- Starting Parser Implementation Phase 1.
- Task: "Create basic parser package and entry points".
    - Created `src/package-parser.lisp` (package `#:scsd/parser`).
    - Created `src/parser.lisp` (stub `scsd/parser:parse-scsd`).
    - Updated `scsd.asd` to include `src` module.
    - Updated `package.lisp` and `scsd.lisp` to define and re-export `scsd:parse-scsd`.
    - Resolved package circular dependency issue.
- Task "Create basic parser package and entry points" completed. Tests pass.
- Task: "Add utilities for reading files line by line".
    - Created `src/utils.lisp`.
    - Defined package `#:scsd/utils`.
    - Added `utils.lisp` to `scsd.asd`.
    - Implemented `scsd/utils:read-lines` function.
    - Updated `#:scsd/parser` package to `:use #:scsd/utils`.
- Task "Add utilities for reading files line by line" completed. Tests pass.
- Task: "Add utilities for trimming whitespace and basic string manipulation".
    - Added `trim-whitespace` and `string-starts-with-p` to `src/utils.lisp`.
    - Exported new functions from `#:scsd/utils`.
    - Fixed compilation error caused by escaped quote in `string-trim`.
- Task "Add utilities for trimming whitespace and basic string manipulation" completed. Tests pass.
- Task: "Add simple test file with just a database name".
    - Created `tests/data/` directory.
    - Created `tests/data/minimal_db.scsd`.
- Task "Add simple test file with just a database name" completed.
- Starting next task: "Add tests for basic file reading".
    - Added `test-data-path` helper to `tests/main.lisp`.
    - Added `read-lines-utility` test to `tests/main.lisp`.
    - Updated `:use` clause in `#:scsd-test` package definition.
- Task "Add tests for basic file reading" completed. Tests pass.
- Starting next task: "Add function to detect if line starts with single hash". Will add to `src/parser.lisp`.
