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
- **Whitespace Handling:** Remember that leading/trailing whitespace *is significant* within data cells (between pipes in data rows) **and column headers** and must be preserved. Whitespace *should be ignored/trimmed* when identifying markers in **type specification lines**.


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
- Task: "Add function to collect description lines until table or EOF".
    - Refactored `parse-scsd` slightly to track `current-index`.
    - Updated description processing loop in `parse-scsd` to collect lines using `description-line-p` and advance `current-index`.
    - Added `table-title-line-p` predicate (needed later).
    - Fixed compilation error with DECLARE placement.
- Task "Add function to collect description lines until table or EOF" completed. Tests pass.
- Task: "Add function to properly join description lines".
    - Added `join-lines` utility function to `src/utils.lisp` and exported it.
    - Updated `parse-scsd` to use `join-lines` on the collected description lines.
- Task "Add function to properly join description lines" completed. Tests pass.
- Task: "Add tests for description parsing".
    - Created test files: `db_no_desc`, `db_single_line_desc`, `db_multi_line_desc`, `db_desc_leading_whitespace`.
    - Added test `description-line-predicate` to `tests/main.lisp`.
    - Added skipped test `parse-scsd-db-description` (pending parser return value).
- Task "Add tests for description parsing" completed. Tests pass.
- Starting Parser Implementation Phase 4: Table Name.
- Task: "Add function to detect table start (double hash)".
    - Verified `table-title-line-p` exists in `src/parser.lisp`.
- Task "Add function to detect table start (double hash)" completed.
- Task: "Add function to extract table name".
    - Added `extract-table-name` to `src/parser.lisp`.
    - Updated `parse-scsd` stub to loop through lines after description, detect table titles, and call extractor.
- Task "Add function to extract table name" completed. Tests pass.
- Task: "Add validation for table name format".
    - Defined `malformed-table-title-error` condition in `src/conditions.lisp`.
    - Updated `parse-scsd` table loop to signal error if extracted table name is empty.
    - Fixed `declare` placement error during compilation.
- Task "Add validation for table name format" completed. Tests pass.
- Task: "Add tests for table name parsing".
    - Added test files: `db_no_tables`, `db_single_table`, `db_multiple_tables`, `db_malformed_table`, `db_invalid_table_marker`.
    - Added tests `table-title-predicate`, `table-name-extraction`, `parse-scsd-table-name-errors` to `tests/main.lisp`.
    - Imported required internal symbols.
    - Corrected logic/expectations in `parse-scsd-table-name-errors` based on ABNF spec.
- Task "Add tests for table name parsing" completed. Tests pass.
- Starting Parser Implementation Phase 5: Table Description.
- Task: "Add function to collect table description lines". 
    - Updated parser loop to collect description lines after table title.
- Task "Add function to collect table description lines" completed. Tests pass.
- Task: "Add tests for table description parsing".
    - Added test files: `table_no_desc.scsd`, `table_single_line_desc.scsd`, `table_multi_line_desc.scsd`.
    - Added skipped test `parse-scsd-table-description`.
- Task "Add tests for table description parsing" completed. Tests pass.
- Starting Parser Implementation Phase 6: Column Headers.
- Task: "Add function to detect header line (starts with |)".
    - Added predicate `pipe-table-line-p` to `src/parser.lisp`.
    - Updated parser loop to check for header line using the predicate.
- Task "Add function to detect header line (starts with |)" completed. Tests pass.
- Task: "Add function to split header line into column names".
    - Added `split-pipe-table-line` to `src/parser.lisp` (using `str:split`).
    - Added `:str` dependency to `scsd.asd`.
    - Imported `str:split` into `#:scsd/parser` package.
    - Updated `scripts/test.ros` to `ql:quickload :str`.
    - Updated parser loop to call splitter.
- Task "Add function to split header line into column names" completed. Tests pass.
- Task: "Add function to validate column name format".
    - Added `malformed-header-error` condition.
    - Updated parser to check for `null` or `""` in split column names.
- Task "Add function to validate column name format" completed. Tests pass.
- Task: "Add function to store column names".
    - Updated parser loop to assign validated column names to local variable `column-names`.
- Task "Add function to store column names" completed. Tests pass.
- Task: "Add tests for header parsing".
    - Added test files `header_empty_col.scsd`, `header_whitespace_cols.scsd`, `header_missing.scsd`, `header_invalid.scsd`.
    - Added tests `header-line-predicate`, `header-line-split`, `parse-scsd-header-errors`.
    - Fixed bugs/expectations in tests and `split-pipe-table-line`.
- Task "Add tests for header parsing" completed. Tests pass.
- Starting Parser Implementation Phase 7: Column Types.
- Task: "Add function to parse type specification line".
    - Updated parser loop to find line after header, check if it's a pipe table line, split, trim, and check count.
    - Added `missing-typespec-error` and `mismatched-typespec-error` conditions.
    - Fixed errors during testing (DECLARE placement, test expectations).
- Task "Add function to parse type specification line" completed. Tests pass.
- Task: "Add function to validate type markers".
    - Added loop to check `trimmed-types` against valid markers (`member ... :test #'string=`).
    - Signal `malformed-typespec-error` if invalid marker found.
- Task "Add function to validate type markers" completed. Tests pass.
- Task: "Add function to store column types".
    - Updated parser to assign validated `trimmed-types` to local variable `column-types`.
- Task "Add function to store column types" completed. Tests pass.
- Task: "Add tests for type specification parsing".
    - Added test files: `types_valid.scsd`, `types_whitespace.scsd`, `types_invalid_marker.scsd`, `types_mismatch_count.scsd`, `types_missing.scsd`.
    - Added test case `parse-scsd-typespec-errors` covering valid types, whitespace, missing line, mismatched count, and invalid marker errors.
- Task "Add tests for type specification parsing" completed. Tests pass.
- Starting Parser Implementation Phase 8: Basic Data Rows.
- Starting next task: "Add function to detect data rows".
