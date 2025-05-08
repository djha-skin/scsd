# Project Tasks

## In Progress
- [X] Add support for basic escape sequences

## In Progress
- [ ] Implement parsing of ASCII punctuation escapes (\\", \\\\, \\/, etc.)
- [ ] Handle malformed ASCII punctuation escape sequences
- [ ] Implement parsing of \\uXXXX escape sequences
- [ ] Implement parsing of \\UXXXXXXXX escape sequences
- [ ] Handle malformed unicode escape sequences
- [ ] Add unicode support for `make-string`
None currently.

## Done
- Read and understood project requirements
- Created AI.md with project understanding
- Created initial TODO.md for task tracking
- Refined tasks for more incremental development
- [X] Create ASDF system definition
- [X] Set up basic package structure
- [X] Add initial unit test framework
- [X] Create basic README with development setup instructions
- [X] Create basic parser package and entry points
- [X] Add utilities for reading files line by line
- [X] Add utilities for trimming whitespace and basic string manipulation
- [X] Add simple test file with just a database name
- [X] Add tests for basic file reading
- [X] Add function to detect if line starts with single hash
- [X] Add function to extract database name from title line
- [X] Add error handling for missing database name
- [X] Add error handling for malformed database title line
- [X] Add tests for database name parsing
  - [X] Test valid names
  - [X] Test missing hash
  - [X] Test empty name
  - [X] Test malformed lines
- [X] Add function to detect description lines (not starting with # or |)
- [X] Add function to collect description lines until table or EOF
- [X] Add function to properly join description lines
- [X] Add tests for description parsing
  - [X] Test single line description
  - [X] Test multi-line description
  - [X] Test empty description
  - [X] Test description with blank lines
- [X] Add function to detect table start (double hash)
- [X] Add function to extract table name
- [X] Add validation for table name format
- [X] Add tests for table name parsing
  - [X] Test valid table names
  - [X] Test invalid table markers
  - [X] Test empty table names
- [X] Add function to collect table description lines
- [X] Add tests for table description parsing
  - [X] Test single line description
  - [X] Test multi-line description
  - [X] Test empty description
- [X] Add function to detect header line (starts with |)
- [X] Add function to split header line into column names
- [X] Add function to validate column name format
- [X] Add function to store column names
- [X] Add tests for header parsing
  - [X] Test basic column names
  - [X] Test empty column names
  - [X] Test malformed header lines
  - [X] Test whitespace handling
- [X] Add function to parse type specification line
  - [X] Ensure type marker parsing trims whitespace (e.g., `| - |` vs `|-|`)
- [X] Add function to validate type markers
  - [X] Validate string type (-)
  - [X] Validate keyword type (:-)
  - [X] Validate number type (-:)
  - [X] Validate boolean type (:-:)
- [X] Add function to store column types
- [X] Add tests for type specification parsing
  - [X] Test each valid type
  - [X] Test invalid type markers
  - [X] Test mismatched column counts
  - [X] Test type parsing with extra whitespace (e.g., `| - |`, `| :- |`)
- [X] Add function to detect data rows
- [X] Add function to split row into fields
  - [X] Ensure splitting does *not* trim whitespace around cell content
- [X] Add function to validate field count matches headers
- [X] Add basic string field parsing
  - [X] Ensure string field parsing preserves leading/trailing whitespace within cell pipes
- [X] Add tests for basic row parsing
  - [X] Test valid rows
  - [X] Test empty fields
  - [X] Test mismatched field counts
  - [X] Test rows with leading/trailing whitespace in string fields
- [X] Add number parsing
  - [X] Integer parsing
  - [X] Float parsing
  - [X] Scientific notation
- [X] Add boolean parsing
- [X] Add keyword/symbol parsing
- [X] Add null value handling
- [X] Add tests for data type parsing
  - [X] Test each data type
  - [X] Test edge cases
  - [X] Test invalid values

## Backlog

### Initial Setup
*None remaining*

### Parser Implementation - Phase 1: Basic Structure
*None remaining*

### Parser Implementation - Phase 2: Database Name
*None remaining*

### Parser Implementation - Phase 3: Database Description
*None remaining*

### Parser Implementation - Phase 4: Table Name
*None remaining*

### Parser Implementation - Phase 5: Table Description
*None remaining*

### Parser Implementation - Phase 6: Column Headers
*None remaining*

### Parser Implementation - Phase 7: Column Types
*None remaining*

### Parser Implementation - Phase 8: Basic Data Rows
*None remaining*

### Parser Implementation - Phase 9: Data Type Parsing
*None remaining*

### Parser Implementation - Phase 10: String Escaping
- [ ] Add support for unicode escapes
- [ ] Add support for ASCII punctuation escapes
- [ ] Add tests for string escaping
  - [ ] Test each escape sequence
  - [ ] Test invalid escapes

### Parser Implementation - Phase 10: String Escaping
- [ ] Add support for ASCII punctuation escapes
- [ ] Add tests for string escaping
  - [ ] Test each escape sequence
  - [ ] Test invalid escapes
- [ ] Define basic table structure
- [ ] Add column metadata storage
- [ ] Add row storage
- [ ] Add basic table access functions
- [ ] Add table validation
- [ ] Add database structure
- [ ] Add database validation

### Initial Serialization
- [ ] Add basic string representation
- [ ] Add table metadata serialization
- [ ] Add row serialization
- [ ] Add database serialization

### Basic Operations
- [ ] Add row access functions
- [ ] Add column access functions
- [ ] Add basic row filtering
- [ ] Add basic column filtering

### Documentation
- [ ] Document installation process
- [ ] Document basic usage
- [ ] Add function documentation
- [ ] Add example code

## Triage
- Advanced table operations (joins, etc.)
- Performance optimization
- Error recovery strategies
- Integration testing
- CI/CD setup
- Distribution strategy
- Additional convenience functions
- Pretty printing options
- REPL-friendly features
