# scsd

Source-control-storable database.

A text-based data format that is "just markdown". It is nearly a strict subset
of [CommonMark (version 0.31.2)](https://spec.commonmark.org/0.31.2/) with 
the [Pandoc](https://pandoc.org/) [pipe
tables
extension](https://www.uv.es/wikibase/doc/cas/pandoc_manual_2.7.3.wiki?95).

## Example document

```markdown
-*- mode: markdown -*-
vim: set filetype=markdown tabstop=2 shiftwidth=2 formatoptions+=cn expandtab

# hospital

ACME hospital records. Distribution is expressly prohibited outside the
organization by applicable law.

## people

This table houses **all** the people in the database.

|person_id|firstname|lastname|birth-year|birth-month|birth-day|
|-:|-|-|-|-:|-:|-:|-:|
|1|Daniel|Haskin|1901|01|01|
|2|Joshua|Melburn|1982|04|06|
|3|Fay|Bingham|2022|07|15|
|4|Jordan|Slinger|1995|03|28|
|5|Bo|Kent|1951|09|03|

## departments

A root list of department entities.

|did|name|slug|
|-:|-|:-|
|1|Podiatry|pod_dept|
|2|Pediatric Medicine|ped_dept|

## doctors

A *many-to-one* relation between doctors and departments.

|doctor_id|person_id|department_id|
|-:|-:|-:|
|1|2|2|
|2|5|2|

## patients

A *many-to-many* relation between doctors and patients.

|patient_id|person_id|doctor_id|
|-:|-:|-:|
|1|1|1|
|2|5|1|
|3|3|2|
|4|3|1|
```

Should look like this:


> -*- mode: markdown -*-
> vim: set filetype=markdown tabstop=2 shiftwidth=2 formatoptions+=cn expandtab
> 
> # hospital
> 
> ACME hospital records. Distribution is expressly prohibited outside the
> organization by applicable law.
> 
> ## people
> 
> This table houses **all** the people in the database.
> 
> |person_id|firstname|lastname|birth-year|birth-month|birth-day|
> |-:|-|-|-|-:|-:|-:|-:|
> |1|Daniel|Haskin|1901|01|01|
> |2|Joshua|Melburn|1982|04|06|
> |3|Fay|Bingham|2022|07|15|
> |4|Jordan|Slinger|1995|03|28|
> |5|Bo|Kent|1951|09|03|
> 
> ## departments
> 
> A root list of department entities.
> 
> |did|name|slug|
> |-:|-|:-|
> |1|Podiatry|pod_dept|
> |2|Pediatric Medicine|ped_dept|
> 
> ## doctors
> 
> A *many-to-one* relation between doctors and departments.
> 
> |doctor_id|person_id|department_id|
> |-:|-:|-:|
> |1|2|2|
> |2|5|2|
> 
> ## patients
> 
> A *many-to-many* relation between doctors and patients.
> 
> |patient_id|person_id|doctor_id|
> |-:|-:|-:|
> |1|1|1|
> |2|5|1|
> |3|3|2|
> |4|3|1|

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

### Preamble

All lines before the first line starting with a single hash followed by a space
(`# `) is called the _preamble_. It is raw text stored as-is by the parser so
that when the database is written back out to the file again after modification
by a program the preamble may be preserved. This preamble allows for editor mode
lines and "comment"-like things that authors may wish to keep with the database.

### Database Name

The first line starting with a single hash followed by a space (`# `) is called
the _database name line_. What follows after those two characters until the end
of the line separation is the database name. This corresponds to an ATX Header 1
line in CommonMark, but is actually storing the database name.

A set of zero or more lines present that do not start with either a pipe or
a hash character are collected as a multi-line string. This
comprises the _database description_. Like the preamble, it is stored as raw
text with the database so that when apps read the file, modify the data, and
write the file back out, the database description is preserved.

A database consists of zero or more tables.

### Tables

A table has a name, a description, and a pipe-separated table. The first row in
the table is has the column names. The second row houses the column types in a
way that displays well in a markdown reader. After that row, there are data
rows. Ther are zero or more data rows. If there are zero rows, there must still
be column name and type rows.

Unlike in CommonMark, all pipe-separated lines MUST start with and end with a
pipe, rather than simply allowing one or the other.

#### Table name

Each table starts with an empty line followed by a line starting with two hashes
followed by a space (`## `). What follows thereafter is the table name until a
line separation is found. This corresponds to an ATX Header 1 line in
CommonMark, but is actually storing the table name.

A set of zero or more lines present that do not start with either a pipe or
a hash character are collected as a multi-line string. This
comprises the _table description_. Like the preamble, it is stored as raw
text with the database so that when apps read the file, modify the data, and
write the file back out, the table description is preserved.

#### First Pipe Table Row: Column Names

Thereafter, there MUST be an empty line followed by a line starting with a pipe
character (`|`). This line
is a pipe-separated list of column names in the table. Spaces are not trimmed or
removed; all characters between the pipes are taken literally as the column
name of that column.

#### Second Pipe Table Row: Column Types

Thereafter, there MUST be a pipe-separated list of column types in the table.
Types are specified in a specific way which allows the table to be rendered by a
markdown viewer. This line corresponds to the dash line in normal pipe tables in
markdown. It should start with the pipe character (`|`) and have just as many
pipe-separated fields as the row of column names has. Each field MUST be one of
the following:

* The character `-` as a string indicator. This indicates the column is a
  string.

* `:-` as an atom or symbol character. This indicates the column is a keyword,
  symbol, atom, or string, in that order, if the parsing language has a concept of
  one of these.

* `-:` means the field contains a number, as parsed like JSON.

* `:-:` means the field contains a boolean value of `true` or `false`.

This is different than normal pipe tables in markdown. Instead of allowing any
number of dashes in a field in the dash line, SCSD dash line fields must only
have one dash (`-`). Also in SCSD, colons are not just for alignment; they also
specify datatype.

As previously stated, each field must be separated by others via the pipe
character (`|`). The line must end with a pipe character just before the line
separation.

#### Table cell line

Each subsequent line must start with a pipe. It must contain as many
pipe-separated fields as the column names row and column types row. It must end
with a pipe (`|`) before the line separation. Fields in the JSON number columns
or JSON boolean columns must conform to that spec during parsing. Empty fields
are always allowed by the spec, and signify a JSON-like null value. 

### Strings

Strings must not contain non-printable characters, tab characters (ASCII `HT`
character), or line separation characters (ASCII `LF` character).
They support backslash escaping of `\a` for the alarm (ASCII `BEL` character),
`\b` for backspace (ASCII `DEL` character), `\f` for form
feed (ASCII `FF` character), `\n` for line feed, `\r` for carriage return, `\v`
for vertical tab, `\t` for tab, and `\uXXXX` for 16-bit unicode character
encoding. They also support escaping dashes, colons, and pipes.
A single unescaped dash (`-`) in the
table cell lines indicates an empty string. All characters between the pipes of
the cell are considered part of the string; there is no trimming, whitespace or
otherwise.

### Keywords

Keywords (fields in columns of the keyword or `:-` type) are serialized just as
strings are, except that they indicate a keyword, atom, or symbol. This string
"means something". In Common Lisp, they are to be parsed out as keywords.

### Numbers

Numbers (fields in columns of the number or `-:` type) must follow the same
specification as and are parsed similarly to JSON numbers.

### Booleans

Booleans (fields in columns of the boolean or `:-:` type) must be the strings
"true" or "false".

## ABNF

```abnf

cr = %0d    ; ASCII CR character

lf = %0a    ; ASCII LF character

line-sep = [cr] lf

dash = "-"
colon = ":"
hash = "#"
space = " "
backslash = %5c ; ASCII backslash (`\`) character

ascii-punct = %21 - %2f ; These are just
            / %3a - %40 ; all the ASCII punctuation characters
            / %5b - %60 ; specified in byte hex format
            / %7b - %7e

nonzero-digit = %31 - %39 ; The unicode characters "1" through "9"

zero = "0"

digit = "0" / nonzero-digit

exp = "E" / "e"

integer = "0"
        / ["-"] nonzero-digit *digit

number = integer ["." *digit] [exp integer]

hex = %30 - %39 ; "0" - "9"
    / %41 - %46 ; "A" - "F"

escaped = "a"
        / "b"
        / "n"
        / "v"
        / "t"
        / "u" 4hex ; uXXXX (unicode escape)
        / "-"
        / ":"
        / "|"

unescaped-char = %32 - %2c   ; Up to dash
               / %2e - %39   ; Past dash, up to colon
               / %3b - %5b   ; Past colon, up to backslash
               / %5d - %7b   ; Past backslash, up to pipe
               / %7d - %ff   ; Past pipe

string-char = backslash escaped
            / unescaped-char

nonempty-string = 1*string-char

empty-string = "-"

string = nonempty-string
       / empty-string

boolean = "true"
        / "false"

non-eol = %00 - %09 ; Up to line feed
        / %0b - %0c ; Past line feed, up to carriage return
        / %0e - %ff ; Past carriage return

description-start = %00 - %09 ; Up to line feed
                  / %0b - %0c ; Past line feed, up to carriage return
                  / %0e - %22 ; Past carriage return, up to hash
                  / %24 - %7b ; Past hash, up to pipe
                  / %7d - %ff ; Past pipe

description-line = *1( description-start *non-eol ) line-sep

description = *description-line

column-name = nonempty-string

table-column-names-row = "|" 1*( column-name "|" ) line-sep

column-type-string = "-"

column-type-keyword = ":-"

column-type-boolean = ":-:"

column-type-number = "-:"

column-type = column-type-string
            / column-type-keyword
            / column-type-boolean
            / column-type-number

table-column-types-row = "|" 1*( column-type "|" ) line-sep

field-data = *1( string
               / keyword
               / number
               / boolean )

table-record-row = "|" 1*(field-data "|") line-sep

table-name = 1*non-eol

table-title = "## " table-name line-sep

table-description = description

table = line-sep table-column-names-row table-coumn-types-row *table-table-record-row

table-section = line-sep table-title table-description table-data

database-name = 1*non-eol

database-title = "# " database-name line-sep

database-description = description

preamble = description

database = preamble line-sep database-title database-description *table-section
```
