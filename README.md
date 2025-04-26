# scsd

Source-control-storable database.

A text-based data format that is "just markdown". It is nearly a strict subset
of [CommonMark (version 0.31.2)](https://spec.commonmark.org/0.31.2/) with 
the [Pandoc](https://pandoc.org/) [pipe
tables
extension](https://www.uv.es/wikibase/doc/cas/pandoc_manual_2.7.3.wiki?95).

## Example document

```markdown

# `dans_database`

Dan's database is a collection of data that is so, so interesting.

## `the_rotterdam`

|uid|field1|field2|field3|
|-:|:-|-|:-:|


```

## Goals

* Be a first-class citizen within a code SCM repository. Therefore, be
  text-based way to record and manipulate data.

* Support most "normal" SQL operations, particularly joins.

* Support accurate recording of JSON data types within record fields.

* Support whole-column types (rather than field-specific types).

* Please the eye.

* Follow the principle of least-surprise.

* Don't make developers learn a new
  format.

* Support easy, single-character-lookahead parsing.

* Support comments and descriptions.

* Support the Lisp symbol type.

## Non-Goals

* No need to support all of SQL.
  * No stored procedures
  * No non-nullable columns

## Description

It's just a subset of markdown as described above. Here is that subset:


### Lines

Everything is line-based. Lines are separated by one carriage return or one
newline character or one CR followed by one LF. This is called a "line
separation".

### Comments

Comments are written as they would have to be in markdown:
`<!-- Like this -->`. 

### Database name

Each document starts with a single hash followed by a space (`# `). What follows
thereafter is the database name until a line separation is found.

An optional set of lines may be present that do not start with either a pipe or
a hash character. These lines will be collected as a multi-line string
comprising the database description.

### Table name

Each table starts with a line starting with two hashes followed by a space (`##
`). What follows thereafter is the table name until a line separation is found.

An optional set of lines may be present that do not start with either a pipe or
a hash character. These lines will be collected as a multi-line string
comprising the table description.

### Table Header

Thereafter, there MUST be a line starting with a pipe character (`|`). This line
is a pipe-separated list of column names in the table. Spaces are not trimmed or
removed; all characters between the pipes are taken literally as the table name.
The line must end with a pipe character followed by a line separation.

### Table Type Line

The next line is the dash line. It specifies the types that will be in different
columns. It should start with the pipe character and have just as many
pipe-separated fields as the table header. Each field MUST be one of the
following:

* The character `-` as a string indicator. This indicates the column is a
  string.

* `:-` as an atom or symbol character. This indicates the column is a keyword,
  symbol, atom, or string, in that order, if the parsing language has a concept of
  one of these.

* `-:` means the field contains a number, as parsed like JSON.

* `:-:` means the field contains a boolean value of `true` or `false`.

As previously stated, each field must be separated by others via the pipe
character. The line must end with a pipe character just before the line
separation.

### Table cell line

Each subsequent line must start with a pipe. It must contain as many
pipe-separated fields as the header and table type lines. It must end with a
pipe before the line separation. Fields in the JSON number columns or JSON
boolean columns must conform to that spec during parsing. Empty fields are
always allowed by the spec, and signify a JSON-like null value. 

### Strings

Strings must not contain non-printables, tabs, or line separation characters.
They support backslash escaping of any ASCII punctuation character (to be
conformant with CommonMark), `\a` for alarm, `\b` for backspace, `\f` for form
feed, `\n` for line feed, `\r` for carriage return, `\v` for vertical tab, `\t`
for tab, and `\uXXXX` for 16-bit unicode character encoding. In particular,
dashes, colons, and pipes MUST be escaped. A single unescaped dash (`-`) in the
table cell lines indicates an empty string. All characters between the pipes of
the cell are considered part of the string; there is no trimming, whitespace or
otherwise. From an SQL perspective, all columns (even the first one) are
considered nullable, at least for the purposes of the serialization format.

### Keywords

Keywords are serialized just as strings are, except that they indicate a
keyword, atom, or symbol. This string "means something".

### Numbers

Just like JSON numbers.

### Booleans

Just like JSON booleans.
