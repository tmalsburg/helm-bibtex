helm-bibtex
===========

A helm source for searching entries in BibTeX bibliographies.

Development is in the early stages.  So expect to encounter bugs.  For parsing of BibTeX files, functions from the Ebib package are used plus some code that was adapted from Ebib.

Here is a screenshot showing a search for entries containing the word *reading*:

![A search for entries containing the word correlation](screenshot.png)

Planned features:

- Icon showing whether there is a PDF for an entry.
- Icon showing whether there are notes for an entry.
- Sort according to a column.
- Make sort column configurable.
- Make column with and column order configurable.

## Requirements

In order to run helm-bibtex, a number of packages have to be installed on your system.  The easiest way to get these packages is perhaps through [MELPA](http://melpa.milkbox.net/#/).

- [Helm](http://melpa.milkbox.net/#/helm): incremental search framework (helm-bibtex is a plug-in for Helm)
- [Ebib](http://melpa.milkbox.net/#/ebib): a BibTeX database manager for Emacs
- [dash](http://melpa.milkbox.net/#/dash): a powerful list-processing library
- [s](http://melpa.milkbox.net/#/s): the long lost Emacs string manipulation library.
- [f](http://melpa.milkbox.net/#/f): a modern API for working with files and directories

## Installation

Have Emacs 24 or higher.  Put the file `helm-bibtex.el` in a directory included in your load-path.  Add the following line to your start-up file (typically init.el):

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
- Insert a BibTeX key at point.
- Edit notes associated with an entry.
- Show the entry in the BibTeX file.

To execute an action, select an entry and press `TAB` to see the list of available actions.
