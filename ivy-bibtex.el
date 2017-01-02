;;; ivy-bibtex.el --- A bibliography manager based on Ivy

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Requires: ((swiper "0.7.0") (parsebib "1.0") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5") (biblio "0.2"))

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

;; A BibTeX bibliography manager based on Ivy and the
;; bibtex-completion backend.  If you are familiar with helm-bibtex,
;; this is the ivy version.
;;
;; News:
;; - 11/24/2016: Added support for bare relative paths to PDF
;;   files.  Concatenates the path in the `file' field to all paths
;;   in `bibtex-completion-library-path'.
;; - 11/24/2016: Added citation function for APA-style citations in org
;;   files.  See `bibtex-completion-format-citation-org-apa-link-to-PDF'.
;; - 11/18/2016: Added support for bibliographies in org-bibtex
;;   format.  See docstring of `bibtex-completion-bibliography'.
;; - 11/10/2016: Layout of search results can now be customized.
;; - 09/29/2016: Performance improvements in ivy-bibtex.  Note: If
;;   you changed your default action in ivy-bibtex, you have to rename
;;   the action, e.g. from `bibtex-completion-insert-key` to
;;   `ivy-bibtex-insert-key`.  For details see
;;   https://github.com/tmalsburg/helm-bibtex#change-the-default-action
;; - 09/20/2016: Added fallback options to ivy frontend.
;; - 04/18/2016: Improved support for Mendely/Jabref/Zotero way of
;;   referencing PDFs.
;; - 04/12/2016: Published ivy version of helm-bibtex.
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
;;
;; Install:
;;
;;   Put this file in a directory included in your load path or
;;   install ivy-bibtex from MELPA (preferred).  Then add the
;;   following in your Emacs startup file:
;;
;;     (require 'ivy-bibtex)
;;
;;   Alternatively, you can use autoload:
;;
;;     (autoload 'ivy-bibtex "ivy-bibtex" "" t)
;;
;;   Requirements are parsebib, swiper, s, dash, and f.  The easiest way
;;   to install these packages is through MELPA.
;;
;;   Let ivy-bibtex know where it can find your bibliography by
;;   setting the variable `bibtex-completion-bibliography'.  See the
;;   manual for more details:
;;
;;     https://github.com/tmalsburg/helm-bibtex/blob/master/README.ivy-bibtex.org
;;
;; Usage:
;;
;;    Do M-x ivy-bibtex and start typing a search query when prompted.

;;; Code:

(require 'ivy)
(require 'bibtex-completion)

(defcustom ivy-bibtex-default-action 'ivy-bibtex-open-any
  "The default action for the `ivy-bibtex` command."
  :group 'bibtex-completion
  :type 'function)
  
(defun ivy-bibtex-display-transformer (candidate)
  (let* ((width (1- (frame-width)))
         (idx (get-text-property 0 'idx candidate))
         (entry (cdr (nth idx (ivy-state-collection ivy-last)))))
    (bibtex-completion-format-entry entry width)))

(defmacro ivy-bibtex-ivify-action (action name)
  "Wraps the function ACTION in another function named NAME which
extracts the key from the candidate selected in ivy and passes it
to ACTION."
  `(defun ,name (candidate)
     (let ((key (cdr (assoc "=key=" (cdr candidate)))))
       (,action (list key)))))

(ivy-bibtex-ivify-action bibtex-completion-open-any ivy-bibtex-open-any)
(ivy-bibtex-ivify-action bibtex-completion-open-pdf ivy-bibtex-open-pdf)
(ivy-bibtex-ivify-action bibtex-completion-open-url-or-doi ivy-bibtex-open-url-or-doi)
(ivy-bibtex-ivify-action bibtex-completion-insert-citation ivy-bibtex-insert-citation)
(ivy-bibtex-ivify-action bibtex-completion-insert-reference ivy-bibtex-insert-reference)
(ivy-bibtex-ivify-action bibtex-completion-insert-key ivy-bibtex-insert-key)
(ivy-bibtex-ivify-action bibtex-completion-insert-bibtex ivy-bibtex-insert-bibtex)
(ivy-bibtex-ivify-action bibtex-completion-add-PDF-attachment ivy-bibtex-add-PDF-attachment)
(ivy-bibtex-ivify-action bibtex-completion-edit-notes ivy-bibtex-edit-notes)
(ivy-bibtex-ivify-action bibtex-completion-show-entry ivy-bibtex-show-entry)
(ivy-bibtex-ivify-action bibtex-completion-add-pdf-to-library ivy-bibtex-add-pdf-to-library)

(defun ivy-bibtex-fallback (search-expression)
  "Select a fallback option for SEARCH-EXPRESSION. This is meant
to be used as an action in `ivy-read`, with `ivy-text` as search
expression."
  (ivy-read "Fallback options: "
            (bibtex-completion-fallback-candidates)
            :caller 'ivy-bibtex-fallback
            :action (lambda (candidate) (bibtex-completion-fallback-action (cdr candidate) search-expression))))

;;;###autoload
(defun ivy-bibtex (&optional arg)
  "Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (ivy-read "BibTeX Items: "
            (bibtex-completion-candidates)
            :caller 'ivy-bibtex
            :action ivy-bibtex-default-action))

;;;###autoload
(defun ivy-bibtex-with-local-bibliography (&optional arg)
  "Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (let ((bibtex-completion-bibliography (bibtex-completion-find-local-bibliography)))
    (ivy-bibtex arg)))

(ivy-set-display-transformer
 'ivy-bibtex
 'ivy-bibtex-display-transformer)

(ivy-set-actions
 'ivy-bibtex
 '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
   ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser")
   ("c" ivy-bibtex-insert-citation "Insert citation")
   ("r" ivy-bibtex-insert-reference "Insert reference")
   ("k" ivy-bibtex-insert-key "Insert BibTeX key")
   ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
   ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email")
   ("e" ivy-bibtex-edit-notes "Edit notes")
   ("s" ivy-bibtex-show-entry "Show entry")
   ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library")
   ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options"))) 

(provide 'ivy-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ivy-bibtex.el ends here
