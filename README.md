helm-bibtex
===========

A helm source for searching entries in BibTeX bibliographies.

Development is in the early stages and the source is perhaps not yet ready for daily use.  For parsing of BibTeX files the source uses functions from the Ebib package.

Here is a screenshot showing a search for entries containing the word *reading*:

![A search for entries containing the word correlation](screenshot.png)

Planned features:

- Icon showing whether there is a PDF for an entry.
- Icon showing whether there are notes for an entry.
- Make sort column configurable.
- Make column with and column order configurable.
- Sort according to a column.

## Installation

Have Emacs 24 or higher.  Put the file `helm-bibtex.el` in a
directory included in your load-path.  Add the following line to your
start-up file (typically init.el):

    (autoload 'helm-bibtex "helm-bibtex" "" t)

Tell helm-bibtex where your BibTeX file is by setting the customization variable `helm-bibtex-bibliography`.

    (setq helm-bibtex-bibliography "path/to/your/bibtex-file.bib")

Other customization variables are:

- `helm-bibtex-library-path`: Location where PDFs of the entries are stored
- `helm-bibtex-pdf-open-function`: Function used for opening PDFs (the default function opens them in Emacs)
- `helm-bibtex-notes-path`: Location where notes are stored (one file per entry)
- `helm-bibtex-notes-extension`: File extension of the files containing the notes

Helm-bibtex assumes that PDFs and notes have the BibTeX key of their associated entries as their base names plus ".pdf" for PDFs and whatever extension is configured for notes (".org" by default).

## Usage

The command `helm-bibtex` starts a new search.  There are several actions for BibTeX entries:

- Open the PDF file associated with an entry if available (default).
- Insert a LaTeX cite macro at point.
- Edit notes associated with an entry.
- Show the entry in the BibTeX file.
