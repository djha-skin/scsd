# Project Tasks

## In Progress
- [ ] Add function to validate type markers
  - [ ] Validate string type (-)
  - [ ] Validate keyword type (:-)
  - [ ] Validate number type (-:)
  - [ ] Validate boolean type (:-:)

## Blocked
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
- [ ] Add function to store column types
- [ ] Add tests for type specification parsing
  - [ ] Test each valid type
  - [ ] Test invalid type markers
  - [ ] Test mismatched column counts
  - [ ] Test type parsing with extra whitespace (e.g., `| - |`, `| :- |`)

### Parser Implementation - Phase 8: Basic Data Rows
- [ ] Add function to detect data rows
- [ ] Add function to split row into fields
  - [ ] Ensure splitting does *not* trim whitespace around cell content
- [ ] Add function to validate field count matches headers
- [ ] Add basic string field parsing
  - [ ] Ensure string field parsing preserves leading/trailing whitespace within cell pipes
- [ ] Add tests for basic row parsing
  - [ ] Test valid rows
  - [ ] Test empty fields
  - [ ] Test mismatched field counts
  - [ ] Test rows with leading/trailing whitespace in string fields

### Parser Implementation - Phase 9: Data Type Parsing
- [ ] Add number parsing
  - [ ] Integer parsing
  - [ ] Float parsing
  - [ ] Scientific notation
- [ ] Add boolean parsing
- [ ] Add keyword/symbol parsing
- [ ] Add null value handling
- [ ] Add tests for data type parsing
  - [ ] Test each data type
  - [ ] Test edge cases
  - [ ] Test invalid values

### Parser Implementation - Phase 10: String Escaping
- [ ] Add support for basic escape sequences
- [ ] Add support for unicode escapes
- [ ] Add support for ASCII punctuation escapes
- [ ] Add tests for string escaping
  - [ ] Test each escape sequence
  - [ ] Test invalid escapes

### Data Structure Implementation
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
