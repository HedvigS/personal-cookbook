CLDF for dummies
================
Hedvig Skirgård
2023-09-01

# CLDF for dummies

This document outlines some of the very basics of the Cross-Linguistic
Data Format (CLDF). CLDF is a way of organizing language data, in
particular data sets with many different languages in it. The basic
organisation is a set of csv-sheets (languages.csv, forms.csv etc).
These documents are linked to each other in a specific way which makes
it possible to combine them into an interlinked database. At the same
time, because they are just plain csv-sheets so they can easily be read
in by most data analysis software programs like python, R, julia etc or
just regular spreadsheet programs like LibreOffice or Microsoft Excel.
It is not necessary to use FileMakerPro, Microsoft Access or similar
programs.

It’s plain, flat and simpler than you might think. In this document, you
will learn the very basics on how it works and how to read them in.

The data format was first published in 2018 \[1\] and has since then
expanded to include a large amount of different datasets.

The format is well-documented. This document is a very basic intro, for
more advanced queries go to <https://github.com/cldf/cldf/#readme> and
<https://cldf.clld.org/>

## Before we start

Good things to keep in mind:

- the absolute best way to learn how CLDF works is to poke around in an
  existing dataset. Open the files, check what’s in there, form
  assumptions and then check if the assumptions are always true. Below
  are two recommended starter-datasets
  - Wordlist: NorthEuraLex v4.0
    <https://github.com/lexibank/northeuralex/tree/v4.0/cldf>
  - Structure: Grambank v1.0.3
    <https://github.com/grambank/grambank/tree/v1.0.3/cldf>  
- many CLDF-datasets are continuously released, so make sure to keep
  track of which **version** you are using.
- if you use python, make sure to check out pycldf and cldfbench
- if you use R, keep an eye out for rcldf which is in development
- this document is about how to navigate existing CLDF-datasets as an
  end-user, not how to make one.
- there already exists a lot of documentation on how CLDF works, this
  document is not meant to be exhaustive but just a gentle entry to get
  you going. For more, see
  - <https://github.com/cldf/cldf/#readme>
  - <https://cldf.clld.org/>

## How to know if you’re dealing with a CLDF-dataset

You are dealing with a CLDF-dataset if there is a folder called “cldf”
with files like “languages.csv”, “values.csv” and
“StructureDataset-metadata.json” in it. The last file will be different
depending on the type of dataset.

Here are some examples of data sets that are available in CLDF-format
that you may have encountered:

- WALS (World Atlas of Language Structures)

- PHOIBLE (Phonetics Information Base and Lexicon)

- D-PLACE (Database of Places, Language, Culture and Environment)

- Glottolog

- Lexibank

- Grambank

  ### Types of CLDF-datasets

There are five types of CLDF-datasets. They are also known as “modules”.

- Wordlist (lexicon, has Forms and often Cognates)
- Structure dataset (grammar or other types of information with one
  value for a Parameter and a Feature, has Values)
- Dictionary (particular kind of lexicon, has Entries and Senses)
- Parallel text (collections of paragraphs of the same text in different
  languages, has Forms, Segments and FunctionalEquivalents)
- generic (no specifics)

## Contents

Each CLDF-dataset consists minimally of:

- a set of tables (usually in csv-sheets)
- a json-file

The tables are usually in csv-format and contain the data itself. The
json file has information *about* the dataset, for example the type of
dataset is, what the contents are, what the filenames are etc.

Many CLDF-datasets also contain a bibTeX-file with bibliographic
references for the data. In such cases, each data-point is tied to a
reference by the key in the bibTeX entry. Usually the key is in a column
called “Source” in the ValueTable or FormTable. The bibTeX file is
usually called “sources.bib”. If it’s called something else, it’ll say
in the meta-data json file.

## Tables inside the datasets

There are some tables that occur in most CLDF-datasets, and some that
occur only in certain types. For example, there is no table with word
forms for Structure data sets - that’s for wordlists and Dictionaries.

The tables have specific names in the CLDF-world and have pre-defined
specifics. The names are different from their filenames. You can see
which name is tied to which csv-file in the json. “LanguageTable” is
usually found in the file languages.csv, “CodeTable” in codes.csv,
“ValueTable” in values.csv, “CognateTable” in cognates.csv etc.

Each table is usually tied to a pre-defined CLDF standard. For example,
FormTables need to have the columns “ID”, “Form” and “Language_ID”.

- LanguageTable -\> languages.csv (contains minimally ID)
- FormTable -\> forms.csv (contains minimally ID, Form, Language_ID,
  Parameter_ID)
- ParameterTable -\> parameters.csv (contains minimally ID, Name) etc.

The json-meta data file says which table is in which file, **you can’t
always bank on LanguageTable being in languages.csv**.

Tables can have more columns than the minimal requirement.

For more specifics, see this file for CLDF v1.0
<http://cldf.clld.org/v1.0/terms.rdf>.

#### Tables in most CLDF-dataset

Here are CLDF-tables that occur in most CLDF-datasets.

- LanguageTable - list of all of the languages in the dataset. May also
  include things classified by Glottolog as dialects or proto-languages.
  Includes meta-information like longitude, language family etc.
- ParameterTable - contains a definition of the variables. For lexicon,
  these are the concepts, for grammar these are the features.

Wordlist also contain

- FormTable - the forms for each concept for each language
- CognateTable (not obligatory) - the cognate classification per form
  per concept per language

Structure data-sets also contain

- ValueTable - the value for each parameter and language. Usually also
  Comment and Source.
- CodeTable - The list of possible values for each parameter. For
  example, GB020 in Grambank is a binary feature and can take 0, 1 and ?
  whereas EA016 in the Ethnographic Atlas (D-PLACE) can take 1, 2 or 9.
  The options are exclusive of each other for each data-point.

Good to know: for the CLDF-dataset of D-PLACE, the LanguageTable
contains a row per *society*. There is a column for the Glottocode of
the language associated with that society.

# Example: Wordlist

Below is a tiny Wordlist CLDF-dataset. This dataset contains 3 words in
2 languages. The first two tables, LanguageTable and ParameterTable
contains information about the languages and parameters - in this case
concepts. The FormTable contains the actual forms. For one of the
concepts, one of the languages has two words and both are listed.

The meta-data json is not included here. You can see an example of a
Wordlist-metadata json file here:
<https://github.com/lexibank/abvd/blob/master/cldf/cldf-metadata.json>.

**LanguageTable**

One row = one language (or sometimes dialect or proto-language, i.e
above language in a tree). The ID column uniquely identifies each
language in the dataset. In other tables, the column that links to the
ID column here is called “Language_ID”.

| ID  | Name     | Glottocode |
|-----|----------|------------|
| 15  | Bintulu  | bint1246   |
| 18  | CHamorro | cham1312   |

Good to know: Sometimes the IDs in LanguageTable are Glottocodes or ISO
639-3 codes, but they don’t have to be. They just have to be unique
within that dataset. In Grambank, the ID’s are Glottocodes, but WALS has
its own specific unique code-system different from both Glottocodes and
ISO 639-3. If you want Glottocodes, go look for a column called
Glottocode in the LanguageTable - don’t use the ID column.

Good to know 2: Glottocodes contain 4 letters or numbers and then 4
numbers. The first 4 characters are not always letters. For example,
`ww2p1234` and `3adt1234` are existing glottocodes.

**ParameterTable**

One row = one parameter. The ID column uniquely identifies each
parameter in the dataset. In other tables, the column that links to the
ID column here is called “Parameter_ID”.

| ID         | Name    | Concepticon_ID |
|------------|---------|----------------|
| 144_toburn | to burn | 2102           |
| 2_left     | left    | 244            |

**FormTable**

One row = one form. The ID column uniquely identifies each form in the
dataset. In other tables, the column that links to the ID column here is
called “Form_ID”. Here we also see Parameter_ID, which links to the
column ID in the ParameterTable and Language_ID which links to the
column ID in the LanguageTable.

| ID              | Parameter_ID | Language_ID | Form   | Source        |
|-----------------|--------------|-------------|--------|---------------|
| 15-144_toburn-1 | 144_toburn   | 15          | pegew  | Blust-15-2005 |
| 15-144_toburn-2 | 144_toburn   | 15          | tinew  | Blust-15-2005 |
| 18-2_left       | 2_left       | 18          | akague | 38174         |

**Source**

Optional file, but often present in the form of a bibTeX-file. One entry
= one source. The bibTeX file is usually called “sources.bib”, but not
necessary (check metadata.json as usual). The bibTeX Key (the first
string after `@BIBTEXENTRYTYPE{`) maps onto the Source column in the
FormTable above.

    @misc{Blust-15-2005,
        author = {Blust},
        date = {2005},
        howpublished = {personal communication}
    }

    @book{38174,
        author = {Topping, Donald M. and Ogo, Pedro M. and Dungca, Bernadita C.},
        address = {Honolulu},
        publisher = {The University Press of Hawaii},
        title = {Chamorro-English dictionary},
        year = {1975}
    }

## example: Wordlist - linking together

Each of the tables has a column called “ID”. This column allows us to
link the tables together. The column “Language_ID” in the FormTable maps
onto the column “ID” in the LanguageTable, and so on.

- Langugage_ID -\> ID column in LanguageTable
- Parameter_ID -\> ID column in ParameterTable
- Form_ID -\> ID column in FormTable.

There is no column “Form_ID” inside the FormTable, it’s just called ID
there. Same with Parameter_ID and the ParameterTable and so on.

**WARNING** Some LanguageTables contain a column called “Language_ID”
which is **not** the same as the ID column. For dialects, this column
contains the Glottocode of the language that they are a dialect of. For
example, Eastern Low Navarrese is a dialect of Basque. The glottocode of
this dialect is east1470. The glottocode of the language Basque is
basq1248. If a LanguageTable has the column Language_ID, it would
contain basq1248 for the dialect. This helps when you might want to
match by the language-level rather than dialect-level.The LanguageTable
in Glottolog contains a column of this kind called “Language_ID”. In
Grambank, there is a similar column, but it is called
“Language_level_ID”.

With the above information, we can now combine the tables if we want.
For example, we can rename the ID column in each of the tables to
“Language_ID”, “Parameter_ID” and “Form_ID” and then join. In the
example below, not all columns are shown due to space. Nota Bene that
both ParameterTable and LanguageTable contains the column “Name”, so
they would have to be dropped or otherwise handled (for example renamed
to “Parameter_name” and “Language_name”).

| Form_ID         | Parameter_ID | Language_ID | Form   | Source        | Glottocode | Concepticon_ID |
|-----------------|--------------|-------------|--------|---------------|------------|----------------|
| 15-144_toburn-1 | 144_toburn   | 15          | pegew  | Blust-15-2005 | bint1246   | 2102           |
| 15-144_toburn-2 | 144_toburn   | 15          | tinew  | Blust-15-2005 | bint1246   | 2102           |
| 18-2_left       | 2_left       | 18          | akague | 38174         | cham1312   | 244            |

# Example: Structure

TBA

# CLLD and CLDF

CLDF is a type of data-format, the set of tables etc. CLLD is a larger
project and stands for Cross-Linguistic Linked Data. CLDF is a part of
CLLD. CLLD also does web applications, for example
<https://clics.clld.org/>. CLDF data interfaces smoothly with CLLD web
applications.

## Advanced

This document is only a very basic intro. If you want to learn more, go
to: <https://github.com/cldf/cldf/#readme>.

## References

\[1\] Forkel, R., List, J. M., Greenhill, S. J., Rzymski, C., Bank, S.,
Cysouw, M. Hammarström, H., Haspelmath, M., Kaiping, G.A. and Gray, R.
D. (2018). Cross-Linguistic Data Formats, advancing data sharing and
re-use in comparative linguistics. Scientific data, 5(1), 1-10.
