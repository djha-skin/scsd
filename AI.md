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
- Task: "Add tests for basic file reading".
    - Added `test-data-path` helper to `tests/main.lisp`.
    - Added `read-lines-utility` test to `tests/main.lisp`.
    - Updated `:use` clause in `#:scsd-test` package definition.
- Task "Add tests for basic file reading" completed. Tests pass.
- Starting Parser Implementation Phase 2: Database Name.
- Task: "Add function to detect if line starts with single hash".
    - Added `database-title-line-p` to `src/parser.lisp`.
    - Updated `parse-scsd` stub to use `read-lines` and `database-title-line-p`.
- Task "Add function to detect if line starts with single hash" completed. Tests pass.
- Task: "Add function to extract database name from title line".
    - Added `extract-database-name` to `src/parser.lisp`.
    - Updated `parse-scsd` stub to call extractor.
- Task "Add function to extract database name from title line" completed. Tests pass.
- Task: "Add error handling for missing database name".
    - Created `src/conditions.lisp` defining `scsd-parse-error` and `missing-database-title-error`.
    - Updated `scsd.asd` to load conditions file.
    - Updated `#:scsd/parser` package to use conditions.
    - Modified `parse-scsd` to signal `missing-database-title-error` if no title line is found.
- Task "Add error handling for missing database name" completed. Tests pass.
- Task: "Add error handling for malformed database title line".
    - Defined `malformed-database-title-error` condition in `src/conditions.lisp`.
    - Updated `parse-scsd` to signal `malformed-database-title-error` if extracted name is empty.
    - Fixed compilation issues (FORMAT string in condition, DECLARE placement in parser).
- Task "Add error handling for malformed database title line" completed. Tests pass.
- Task: "Add tests for database name parsing".
    - Added tests `database-title-predicate`, `database-name-extraction`, `parse-scsd-db-name-errors` to `tests/main.lisp`.
    - Imported required internal symbols into `#:scsd-test` package.
    - Fixed package name conflict between `scsd:parse-scsd` and imported `scsd/parser:parse-scsd` by removing `:use #:scsd` from test package.
- Task "Add tests for database name parsing" completed. Tests pass.
- Starting Parser Implementation Phase 3: Database Description.
- Task: "Add function to detect description lines (not starting with # or |)".
    - Added `description-line-p` to `src/parser.lisp`.
    - Updated `parse-scsd` stub to demonstrate usage.
- Task "Add function to detect description lines (not starting with # or |)" completed. Tests pass.
- Starting next task: "Add function to collect description lines until table or EOF". Will update `parse-scsd`.
