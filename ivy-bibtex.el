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
;; - 09/06/2018: Added virtual APA field `author-or-editor` for use in
;;   notes templates.
;; - 02/06/2018: Reload bibliography proactively when bib files are
;;   changed.
;; - 21/10/2017: Added support for multiple PDFs and other file
;;   types.  See `bibtex-completion-pdf-extension' and
;;   `bibtex-completion-find-additional-pdfs' for details.
;; - 10/10/2017: Added support for ~@string~ constants.
;; - 02/10/2017: Date field is used when year is undefined.
;; - 29/09/2017: BibTeX entry, citation macro, or org-bibtex entry at
;;   point, will be pre-selected in helm-bibtex and ivy-bibtex giving
;;   quick access to PDFs and other functions.
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
  "The default action for the `ivy-bibtex' command."
  :group 'bibtex-completion
  :type 'function)

(defcustom ivy-bibtex-local-bibliography-face 'ivy-virtual
  "The face used by the `ivy-bibtex' command to display entries from the local bibliography."
  :group 'bibtex-completion
  :type 'face)

(defvar ivy-bibtex-local-bibliography-length nil
  "The number of candidates in the local bibliography.  This is set internally by `ivy-bibtex'.")

(defun ivy-bibtex-display-transformer (candidate)
  "Format CANDIDATE for display in ivy."
  (let* ((width (1- (frame-width)))
         (idx (get-text-property 0 'idx candidate))
         (entry (cdr (nth idx (ivy-state-collection ivy-last))))
         (str (bibtex-completion-format-entry entry width)))
    (if (< idx ivy-bibtex-local-bibliography-length)
        (propertize str 'face ivy-bibtex-local-bibliography-face)
      str)))

(defmacro ivy-bibtex-ivify-action (action name)
  "Wraps the function ACTION in another function named NAME which
extracts the key from the candidate selected in ivy and passes it
to ACTION."
  `(defun ,name (candidate)
     (let ((key (cdr (assoc "=key=" (cdr candidate))))
           (idx (get-text-property 0 'idx (ivy-state-current ivy-last))))
       (,action (list (propertize key 'scope (if (< idx ivy-bibtex-local-bibliography-length)
                                                 'local
                                               'global)))))))

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
to be used as an action in `ivy-read', with `ivy-text' as search
expression."
  (ivy-read "Fallback options: "
            (bibtex-completion-fallback-candidates)
            :caller 'ivy-bibtex-fallback
            :action (lambda (candidate) (bibtex-completion-fallback-action (cdr candidate) search-expression))))

;;;###autoload
(defun ivy-bibtex (&optional arg scope input)
  "Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread.

If SCOPE is 'global or 'local, only the global or local
bibliography is searched, respectively.  This is set iternally by
`ivy-bibtex-with-global-bibliography' and
`ivy-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications)."
  (interactive "P")
  (bibtex-completion-init scope)
  (when arg
    (bibtex-completion-clear-cache))
  (let* ((candidates (bibtex-completion-candidates))
         (local-length (length (cdr candidates)))
         (candidates (append (cdr candidates) (car candidates)))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates))))
    (setq ivy-bibtex-local-bibliography-length local-length)
    (ivy-read "BibTeX entries: "
              candidates
              :initial-input input
              :preselect preselect
              :caller 'ivy-bibtex
              :action ivy-bibtex-default-action)))

;;;###autoload
(defun ivy-bibtex-with-global-bibliography (&optional arg)
  "Search BibTeX entries in globcal bibliography using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (ivy-bibtex arg 'global))

;;;###autoload
(defun ivy-bibtex-with-local-bibliography (&optional arg)
  "Search BibTeX entries in local bibliography using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (ivy-bibtex arg 'local))

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
