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
Initial setup complete. Parser implementation progressing.

### Notes To Self
- Read ABNF grammar carefully when implementing parser
- Consider using parser combinators or similar approach
- Pay attention to string escaping rules
- Tables should be primary objects that support operations
- Keep data structures immutable where possible
- Use internal packages (`#:scsd/...`) to avoid polluting the main `#:scsd` package. Re-export public API from main `scsd.lisp`.
- **Whitespace Handling:** Remember that leading/trailing whitespace *is significant* within data cells (between pipes in data rows) **and column headers** and must be preserved. Whitespace *should be ignored/trimmed* when identifying markers in **type specification lines**.


## Session 2025-05-04 15:38

[Previous session contents...]

## Session 2025-05-08 14:42 - String Escape Sequences

Starting work on implementing string escape sequence handling.

The ABNF grammar specifies the following escape sequences that need to be supported:

1. ASCII escape sequences:
   - `\a` - alarm/bell (0x07)
   - `\b` - backspace (0x08)
   - `\n` - line feed (0x0A)
   - `\v` - vertical tab (0x0B)
   - `\t` - tab (0x09)

2. Unicode escape sequences:
   - `\uXXXX` - 16-bit Unicode escapes where XXXX is 4 hex digits

3. ASCII punctuation escapes:
   - Any ASCII punctuation character can be escaped with a backslash
   - This includes |, -, :, \, etc.
   - Used to include literal characters that would otherwise have special meaning

Implementation Plan:

1. Create escape sequence handling utilities:
   - Add `unescape-string` function to handle all escape sequences
   - Add `escape-string` function for future serialization support
   - Added unit tests for each type of escape sequence

2. Add error conditions for malformed escape sequences:
   - Invalid/incomplete Unicode escapes 
   - Invalid ASCII escapes
   - Incomplete/trailing backslashes

3. Integrate escape handling into parser:
   - Update string field parsing to handle escapes
   - Add tests with escape sequences in:
     - Table/column names
     - Data cells 
     - Type specifications (dashes/colons must be escaped to be used literally)

4. Update error reporting:
   - Add line/column info for escape-related errors
   - Provide helpful error messages for malformed escapes

Strategy:
- Start with simple escape handling (backslash + character)
- Add more complex Unicode escape handling after basic functionality is working
- Use Common Lisp's reader facilities where appropriate (e.g., for hex digits)
- Keep code split into small, focused functions for better error handling