;;; ivy-bibtex.el --- A BibTeX bibliography manager based on Helm

;; Copyright 2014 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Requires: ((ivy "0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A BibTeX bibliography manager based on Helm.
;;
;; News:
;; - 02/25/2016: Support for pre- and postnotes for pandoc-citeproc
;;   citations.
;; - 11/23/2015: Added support for keeping all notes in one
;;   org-file.  See customization variable `helm-bibtex-notes-path'.
;; - 11/10/2015: Added support for PDFs specified in a BibTeX
;;   field.  See customization variable `helm-bibtex-pdf-field'.
;; - 11/09/2015: Improved insertion of LaTeX cite commands.
;;
;; See NEWS.org for old news.
;;
;; Key features:
;; - Quick access to your bibliography from within Emacs
;; - Tightly integrated workflows
;; - Provides instant search results as you type
;; - Powerful search expressions
;; - Open the PDFs, URLs, or DOIs associated with an entry
;; - Insert LaTeX cite commands, Ebib links, or Pandoc citations,
;;   BibTeX entries, or plain text references at point, attach PDFs to
;;   emails
;; - Attach notes to publications
;; - Quick access to online bibliographic databases such as Pubmed,
;;   arXiv, Google Scholar, Library of Congress, etc.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

;;; Install:

;; Put this file in a directory included in your load path or install
;; ivy-bibtex from MELPA (preferred).  Then add the following in your
;; Emacs startup file:
;;
;;     (require 'ivy-bibtex)
;;
;; Alternatively, you can use autoload:
;;
;;     (autoload 'ivy-bibtex "ivy-bibtex" "" t)
;;
;; Requirements are parsebib, helm, s, dash, and f.  The easiest way
;; to install these packages is through MELPA.  Make sure helm is
;; properly configured (see
;; https://github.com/emacs-helm/helm#install-from-emacs-packaging-system).
;;
;; Let ivy-bibtex know where it can find your bibliography by setting
;; the variable `bibtex-completion-bibliography'.  See the manual for more details:
;;
;;   https://github.com/tmalsburg/helm-bibtex#minimal-configuration

;;; Usage:

;;; Code:

(require 'ivy)

(defvar bibtex-completion-frontend 'ivy)
(require 'bibtex-completion)

(defun ivy-bibtex-candidates-formatter (candidates)
  (let ((width (frame-width)))
    (bibtex-completion-candidates-formatter candidates width)))

;;;###autoload
(defun ivy-bibtex (&optional arg)
  "Search BibTeX entries.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (when arg
    (setq bibtex-completion-bibliography-hash ""))
  (bibtex-completion-init)
  (ivy-read "BibTeX Items: "
            (bibtex-completion-candidates 'ivy-bibtex-candidates-formatter)
            :caller 'ivy-bibtex
            :action 'bibtex-completion-insert-key))

(ivy-set-actions
 'ivy-bibtex
 '(("p" bibtex-completion-open-pdf "Open PDF file (if present)")
   ("u" bibtex-completion-open-url-or-doi "Open URL or DOI in browser")
   ("c" bibtex-completion-insert-citation "Insert citation")
   ("r" bibtex-completion-insert-reference "Insert reference")
   ("k" bibtex-completion-insert-key "Insert BibTeX key")
   ("b" bibtex-completion-insert-bibtex "Insert BibTeX entry")
   ("a" bibtex-completion-add-PDF-attachment "Attach PDF to email")
   ("e" bibtex-completion-edit-notes "Edit notes")
   ("s" bibtex-completion-show-entry "Show entry"))) 

(provide 'ivy-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ivy-bibtex.el ends here
