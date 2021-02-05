;;; embark-bibtex.el --- A bibliography manager based on Embark

;; Author: Maxime Tréca <maxime.treca@gmail.com>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/tmalsburg/embark-bibtex
;; Version: 1.0.0
;; Package-Requires: ((bibtex-completion "1.0.0") (cl-lib "0.5"))

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

;; A BibTeX bibliography manager based on built-in completion, embark
;; completion actions and the bibtex-completion backend.
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
;;   install embark-bibtex from MELPA (preferred).  Then add the
;;   following in your Emacs startup file:
;;
;;     (require 'embark-bibtex)
;;
;;   Alternatively, you can use autoload:
;;
;;     (autoload 'embark-bibtex "embark-bibtex" "" t)
;;
;;   Requirements are parsebib, embark, s, dash, and f.  The easiest way
;;   to install these packages is through MELPA.
;;
;;   Let embark-bibtex know where it can find your bibliography by
;;   setting the variable `bibtex-completion-bibliography'.  See the
;;   manual for more details:
;;
;;     https://github.com/tmalsburg/helm-bibtex/blob/master/README.embark-bibtex.org
;;
;; Usage:
;;
;;    Do M-x embark-bibtex and start typing a search query when prompted.

;;; Code:

(require 'bibtex-completion)

(defvar embark-bibtex-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'embark-bibtex-open-pdf)
    (define-key map (kbd "u") 'embark-bibtex-open-url-or-doi)
    (define-key map (kbd "c") 'embark-bibtex-insert-citation)
    (define-key map (kbd "r") 'embark-bibtex-insert-reference)
    (define-key map (kbd "k") 'embark-bibtex-insert-key)
    (define-key map (kbd "b") 'embark-bibtex-insert-bibtex)
    (define-key map (kbd "a") 'embark-bibtex-add-PDF-attachment)
    (define-key map (kbd "e") 'embark-bibtex-edit-notes)
    (define-key map (kbd "s") 'embark-bibtex-show-entry)
    (define-key map (kbd "l") 'embark-bibtex-add-pdf-to-library)
    map))

(defcustom embark-bibtex-default-action 'embark-bibtex-open-any
  "The default action for the `embark-bibtex` command."
  :group 'bibtex-completion
  :type 'function)

(defmacro embark-bibtex-embarkify-action (action name)
  "Wraps the function ACTION in another function named NAME which
extracts the key from the candidate selected in embark and
passes it to ACTION."
  `(defun ,name (cand)
     (interactive
      (list (embark-bibtex--read "BibTeX entries: ")))
     (,action (list cand))))

(embark-bibtex-embarkify-action bibtex-completion-open-any embark-bibtex-open-any)
(embark-bibtex-embarkify-action bibtex-completion-open-pdf embark-bibtex-open-pdf)
(embark-bibtex-embarkify-action bibtex-completion-open-url-or-doi embark-bibtex-open-url-or-doi)
(embark-bibtex-embarkify-action bibtex-completion-insert-citation embark-bibtex-insert-citation)
(embark-bibtex-embarkify-action bibtex-completion-insert-reference embark-bibtex-insert-reference)
(embark-bibtex-embarkify-action bibtex-completion-insert-key embark-bibtex-insert-key)
(embark-bibtex-embarkify-action bibtex-completion-insert-bibtex embark-bibtex-insert-bibtex)
(embark-bibtex-embarkify-action bibtex-completion-add-PDF-attachment embark-bibtex-add-PDF-attachment)
(embark-bibtex-embarkify-action bibtex-completion-edit-notes embark-bibtex-edit-notes)
(embark-bibtex-embarkify-action bibtex-completion-show-entry embark-bibtex-show-entry)
(embark-bibtex-embarkify-action bibtex-completion-add-pdf-to-library embark-bibtex-add-pdf-to-library)

(defun embark-bibtex--get-candidates ()
  "Return all keys from bibtex-completion-candidates."
  (mapcar
   (lambda (cand)
     (cons (bibtex-completion-format-entry cand (1- (frame-width)))
           (cdr (assoc "=key=" cand))))
   (bibtex-completion-candidates)))

(defun embark-bibtex--read ()
  "Read bibtex-completion entries for completion."
  (completing-read
   "BibTeX entries: "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         '(metadata (category . bibtex))
       (complete-with-action action (embark-bibtex--get-candidates) string predicate)))))

;;;###autoload
(defun embark-bibtex (bib-entry)
  "Search BibTeX entries using `completing-read' and embark
actions."
  (interactive
   (progn
     (bibtex-completion-init)
     (list (cdr (assoc (embark-bibtex--read) (embark-bibtex--get-candidates))))))
  (apply embark-bibtex-default-action (list bib-entry)))

(provide 'embark-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; embark-bibtex.el ends here
