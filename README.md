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

# dans_database

Dan's database is a collection of data that is so, so interesting.

## the_rotterdam



|uid|field_1|__field2|field3__|
|-:|:-|-|:-:|
|1|**foo|bar**|*baz|qux*|


```

Should look like this:


> -*- mode: markdown -*-
> vim: set filetype=markdown tabstop=2 shiftwidth=2 formatoptions+=cn expandtab
> 
> # dans_database
> 
> Dan's database is a collection of data that is so, so interesting.
> 
> ## the_rotterdam
> 
> |uid|field_1|__field2|field3__|
> |-:|:-|-|:-:|
> |1|**foo|bar**|*baz|qux*|
> 
> 
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

### One Document, One Database

Each document starts with a single hash followed by a space (`# `). What follows
thereafter is the database name until a line separation is found.

An optional set of lines may be present that do not start with either a pipe or
a hash character. These lines will be collected as a multi-line string
comprising the database description.

A database consists of zero or more tables.

### Tables

A table has a name, a description, column names within a header, a columns type
line, and zero or more rows. If there are zero rows, there must still be types
and headers lines.

#### Table name

Each table starts with a line starting with two hashes followed by a space (`##
`). What follows thereafter is the table name until a line separation is found.

An optional set of lines may be present that do not start with either a pipe or
a hash character. These lines will be collected as a multi-line string
comprising the table description.

#### Table Header

Thereafter, there MUST be a line starting with a pipe character (`|`). This line
is a pipe-separated list of column names in the table. Spaces are not trimmed or
removed; all characters between the pipes are taken literally as the table name.
The line must end with a pipe character followed by a line separation.

#### Table Type Line

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

#### Table cell line

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

## ABNF

```abnf

cr = %0d

lf = %0a

line-sep = [cr] lf

pipe = %7c

dash = %2d

colon = %3a

hash = %23

space = %20

backslash = %5c

ascii-punct = %21 - %2f
            / %3a - %40
            / %5b - %60
            / %7b - %7e

nonzero-digit = %31 - %39

zero = %30

digit = zero / nonzero-digit

point = %2e

exp = %45 ; E
    / %65 ; e

integer = zero
        / [dash] nonzero-digit *digit

number = integer [point *digit] [exp integer]

hex = %30 - %39 ; digits
    / %41 - %46 ; A-F

escaped = %61 ; a (alarm/bell)
            / %62 ; b (backspace)
            / %6e ; n (newline)
            / %76 ; v (vertical tab)
            / %74 ; t (tab)
            / %75 4hex ; u (unicode escape)
            / ascii-punct

unescaped-char = %32 - %2c   ; Up to dash
               / %2e - %39   ; Past dash, up to colon
               / %3b - %5b   ; Past colon, up to backslash
               / %5d - %7b   ; Past backslash, up to pipe
               / %7d - %ff   ; Past pipe

string-char = backslash escaped
            / unescaped-char

nonempty-string = 1*string-char

empty-string = dash

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

table-header = pipe 1*(column-name pipe) line-sep

column-type-string = dash

column-type-keyword = colon dash

column-type-boolean = colon dash colon

column-type-number = dash colon

column-type = column-type-string
            / column-type-keyword
            / column-type-boolean
            / column-type-number

table-typespec = pipe 1*(column-type pipe) line-sep

field-data = *1( string
               / keyword
               / number
               / boolean )

table-record = pipe 1*(field-data pipe) line-sep

table-name = 1*non-eol

table-title = hash hash space table-name line-sep

table-description = description

table-data = table-header table-typespec *table-record

table = table-title table-description table-data

database-name = 1*non-eol

database-title = hash space database-name line-sep

database-description = description

database = database-title database-description *table
```
