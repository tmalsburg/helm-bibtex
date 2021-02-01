;;; embark-bibtex.el --- A bibliography manager based on Embark

;; Author: Maxime Tréca <maxime.treca@gmail.com>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; URL: https://github.com/tmalsburg/embark-bibtex
;; Version: 1.0.0
;; Package-Requires: ((bibtex-completion "1.0.0") (embark "0.10")
;; (cl-lib "0.5") (marginalia "0.2"))

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

(require 'embark)
(require 'marginalia)
(require 'bibtex-completion)

(defcustom embark-bibtex-default-action 'embark-bibtex-open-any
  "The default action for the `embark-bibtex` command."
  :group 'bibtex-completion
  :type 'function)

(defmacro embark-bibtex-embarkify-action (action name)
  "Wraps the function ACTION in another function named NAME which
extracts the key from the candidate selected in embark and
passes it to ACTION."
  `(defun ,name (cand)
     (interactive "s ")
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
  (mapcar (lambda (cand)
            (cdr (assoc "=key=" cand)))
          (bibtex-completion-candidates)))

;;;###autoload
(defun embark-bibtex (bib-entry)
  "Search BibTeX entries using `completing-read' and embark
actions."
  (interactive
   (list
    (completing-read
     "BibTeX entries: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           '(metadata (category . bibtex))
         (complete-with-action action (embark-bibtex--get-candidates) string predicate))))))
  (apply embark-bibtex-default-action (list bib-entry)))

(defun marginalia-annotate-bibtex (bib-key)
  "Return completion annotations for a bibtex key."
  (let ((bib-entry (bibtex-completion-get-entry bib-key)))
    (concat
     (marginalia--fields
      ((cdr (assoc "title" bib-entry)) :width 70 :truncate 70)
      ((cdr (assoc "author" bib-entry)) :width 30 :truncate 30)
      ((cdr (assoc "year" bib-entry))  :width 5 :truncate 5)))))

(add-to-list 'marginalia-annotators-light '(bibtex . marginalia-annotate-bibtex))
(add-to-list 'marginalia-annotators-heavy '(bibtex . marginalia-annotate-bibtex))

(embark-define-keymap embark-bibtex-map
  "Keymap for actions for bibtex."
  ("p" embark-bibtex-open-pdf)
  ("u" embark-bibtex-open-url-or-doi)
  ("c" embark-bibtex-insert-citation)
  ("r" embark-bibtex-insert-reference)
  ("k" embark-bibtex-insert-key)
  ("b" embark-bibtex-insert-bibtex)
  ("a" embark-bibtex-add-PDF-attachment)
  ("e" embark-bibtex-edit-notes)
  ("s" embark-bibtex-show-entry)
  ("l" embark-bibtex-add-pdf-to-library))

(add-to-list 'embark-keymap-alist '(bib . embark-bibtex-map))

(provide 'embark-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; embark-bibtex.el ends here
