# hs-inqbox

This project is a sandbox to build the command-line tool written in Haskell.

## Goal

Building a command line tool which supports to write structured japanese text.

## Features to be implemented

### Creating project

`ilya init`

The command above creates configuration resources in current directory
and makes the directory project home.

* creating `.ilya` directory in current position.
* appending appropriate `.gitignore` entries.

### Compiling sources

`ilya compile`

The command above reads source files and parses syntax
and furthermore generate compiled output.

* reading source files from `src/main/**/*.tex`.
* parsing each sources and convert them to syntax tree.
* outputting formatted document to `target/`

### Building PFD using TeX (must have been already installed)

`ilya build`

* executing building lifecycle.

### Cleaning up output resrouces

`ilya clean`

## Project Directory Structure

```
your-project-home/

    build.ilya

    src/

    target/

    .ilya/
        config
        indexes/
        logs/

~/
    .ilya/

        issues/
            list
            history

        indexes/

```

## Source Syntax

### Headers

`# Chapter 1`

`## Section 1`

`### Sub-Section 1`

### Paragraphs Delimiter

One or more blank lines.

### Lists

Unordered Lists

```
* List item 1
* List item 2
* List item 3
```

### Spreaking Parts

`Actor's Name: contents of his speech.`

Colon means a separator of actor's name and their speaking contents.

A Speaking part is a block element.

So it will continue until a blank line appears after the speaking part.

### Comments

`% Comment line`

This is same as TeX.

### Blockquote

`> Something quoted`

### Annotation

`@`
