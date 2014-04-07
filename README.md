helm-bibtex
===========

A helm source for searching entries in BibTeX bibliographies.

Development is in the early stages and the source is perhaps not yet ready for daily use.  For parsing of BibTeX files the source uses functions from the Ebib package.

Here is a screenshot showing a search for entries containing the word *reading*.

![A search for entries containing the word correlation](screenshot.png)

Planned features:

- Action for opening the PDF file associated with an entry if available.
- Action for editing notes associated with an entry.
- Action for inserting a LaTeX cite macro at point.
- Action for showing the entry in the BibTeX file.
- Icon showing whether there is a PDF for an entry.
- Icon showing whether there are notes for an entry.
- Make sort column configurable.
- Make column with and column order configurable.
- Sort according to a column.

## Installation and use

Have Emacs 24 or higher.  Put the file `helm-bibtex.el` in a
directory included in your load-path.  Add the following line to your
start-up file (typically init.el):

    (autoload 'helm-bibtex "helm-bibtex" "" t)

Tell helm-bibtex where your BibTeX file is by setting the customization variable `helm-bibtex-bibliography`.

    (setq helm-bibtex-bibliography "path/to/your/bibtex-file.bib")

The command `helm-bibtex` starts a new search.
