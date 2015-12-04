# Dawdle - the DDL suggestor tool

Meant as a tool for analyzing CSVs and suggesting a DDL.
Generates an executable called `dawdle` once installed.

## Usage examples
`$ dawdle --input="example_with_comma.csv" --with-header -s ","`
This means analyze the file `example_with_comma.csv`, assume the first line is the header and use a comma as a separator.
The example output is

    create table example_with_comma (id int not null,
                                     name varchar(65) not null,
                                     created_at datetime not null,
                                     updated_at datetime not null)

To see all command line flags, run `$ dawdle --help`.

## Types
Currently, the syntax is basic(-ish) SQL.
Supported types are:
* tinyint
* smallint
* int
* bigint
* real
* float
* date
* datetime (==timestamp)
* varchar
