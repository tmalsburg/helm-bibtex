helm-bibtex
===========

A helm source for searching entries in a BibTeX bibliography.

## Key features:

- Quick access to your bibliography from within Emacs
- Provides instant search results as you type
- Insert BibTeX key of selected entry in your current document
- Open the PDF associated with an entry
- Take notes about selected entry
- Edit selected entry
- Quick access to online bibliographic databases

Here is a screenshot showing a search for entries containing the word *reading*:

![A search for entries containing the word correlation](screenshot.png)

At the bottom of the list are dummy entries that can be used to search online databases.  The following databases are preconfigured:

- Google Scholar
- arXiv
- Pubmed
- Bodleian Library
- Library of Congress
- Deutsche Nationalbibliothek
- British National Library
- Bibliothèque National de France
- Gallica Bibliothèque Numérique

This list can be extended using the configuration variable `helm-bibtex-fallback-options`.

## Requirements

In order to run helm-bibtex, a number of packages have to be installed on your system.  The easiest way to get these packages is through [MELPA](http://melpa.milkbox.net/#/).

- [Helm](http://melpa.milkbox.net/#/helm): incremental search framework (helm-bibtex is a plug-in for Helm)
- [Ebib](http://melpa.milkbox.net/#/ebib): a BibTeX database manager for Emacs
- [dash](http://melpa.milkbox.net/#/dash): a powerful list-processing library
- [s](http://melpa.milkbox.net/#/s): the long lost Emacs string manipulation library.
- [f](http://melpa.milkbox.net/#/f): a modern API for working with files and directories

## Installation

Helm-bibtex can be installed via [MELPA](http://melpa.milkbox.net/#/helm-bibtex).  Alternatively, put the file `helm-bibtex.el` in a directory included in your load-path.  Add the following line to your start-up file (typically init.el):

    (autoload 'helm-bibtex "helm-bibtex" "" t)

Tell helm-bibtex where your BibTeX file is by setting the customization variable `helm-bibtex-bibliography`.

    (setq helm-bibtex-bibliography "path/to/your/bibtex-file.bib")

Other customization variables are:

- `helm-bibtex-library-path`: Location where PDFs of the entries are stored
- `helm-bibtex-pdf-open-function`: Function used for opening PDFs (the default function opens them in Emacs)
- `helm-bibtex-notes-path`: Location where notes are stored (one file per entry)
- `helm-bibtex-notes-extension`: File extension of the files containing the notes
- `helm-bibtex-fallback-options`: Online databases for which dummy entries are provided
- `helm-bibtex-browser-function`: The browser that is used to access online databases

Helm-bibtex assumes that PDFs and notes have the BibTeX key of their associated entries as their base names plus ".pdf" for PDFs and whatever extension is configured for notes (".org" by default).

## Usage

The command `helm-bibtex` starts a new search.  It is recommended to bind it to a convenient key for quick access.  For filtering the following fields are used: author, title, year, and entry-type.  Regular expressions can be used as search terms.  Example searches:

Everything ever published by Janet Fodor:

    janet fodor

All phdtheses:

    phdthesis

Lyn Frazier's PhD thesis:

    phdthesis frazier

Publications about eyetracking (regular expression used to match various spellings: *eyetracking*, *eye tracking*, *eye-tracking*):

    eye.?tracking

Conference presentations in 2013:

    2013 inproceedings

Publications from 2010 and 2011:

    \(2010\|2011\)

Articles co-authored by David Caplan and Gloria Waters:

    article waters caplan

There are several actions for matching BibTeX entries:

- Open the PDF file associated with an entry if available (default).
- Insert BibTeX key at point.
- Edit notes associated with an entry.
- Show the entry in the BibTeX file.

To execute an action, select an entry and press `TAB` to see the list of available actions.

