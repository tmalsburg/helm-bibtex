;;; bibtex-actions.el --- Bibliography manager action

;;; Code:

(require 'bibtex-completion)

(defvar bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'bibtex-actions-open-pdf)
    (define-key map (kbd "u") 'bibtex-actions-open-url-or-doi)
    (define-key map (kbd "c") 'bibtex-actions-insert-citation)
    (define-key map (kbd "r") 'bibtex-actions-insert-reference)
    (define-key map (kbd "k") 'bibtex-actions-insert-key)
    (define-key map (kbd "b") 'bibtex-actions-insert-bibtex)
    (define-key map (kbd "a") 'bibtex-actions-add-PDF-attachment)
    (define-key map (kbd "e") 'bibtex-actions-edit-notes)
    (define-key map (kbd "s") 'bibtex-actions-show-entry)
    (define-key map (kbd "l") 'bibtex-actions-add-pdf-to-library)
    map))

(defun bibtex-actions--get-candidates ()
  "Return all keys from bibtex-completion-candidates."
  (mapcar
   (lambda (cand)
     (cons (bibtex-completion-format-entry cand (1- (frame-width)))
           (cdr (assoc "=key=" cand))))
   (bibtex-completion-candidates)))

(defun bibtex-actions--read ()
  "Read bibtex-completion entries for completion."
  (bibtex-completion-init)
  (completing-read
   "BibTeX entries: "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         '(metadata (category . bibtex))
       (complete-with-action action (bibtex-actions--get-candidates) string predicate)))))

(defun bibtex-completion--read ()
  "Select BibTeX entries in completion system."
  (cdr (assoc (bibtex-actions--read) (bibtex-actions--get-candidates))))

(defmacro bibtex-actions-define-action (action doc)
  "Wraps the function ACTION in another function named NAME which
extracts the key from the candidate selected in embark and
passes it to ACTION."
  (let* ((old-name (symbol-name action))
         (mid-name (substring old-name 17 (length old-name)))
         (new-name (intern (concat "bibtex-actions" mid-name))))
    `(defun ,new-name (cand)
       ,doc
       (interactive (list (bibtex-completion--read)))
       (,action (list cand)))))

(bibtex-actions-define-action
 bibtex-completion-open-any
 "Open the PDFs associated with the BibTeX entry.
If multiple PDFs are found, ask for the one to open using
‘completion-read’. If no PDF is found, try to open a URL or DOI
in the browser instead.")

(bibtex-actions-define-action
 bibtex-completion-open-pdf
 "Open the PDFs associated with the BibTeX entry.
If multiple PDFs are found, ask for the one to open using
‘completion-read’.")

(bibtex-actions-define-action
 bibtex-completion-open-url-or-doi
 "Open the URL or DOI associated with a BibTeX entry in a
 browser.")

(bibtex-actions-define-action
 bibtex-completion-insert-citation
 "Insert citation for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-insert-reference
 "Insert reference for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-insert-key
 "Insert key for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-insert-bibtex
 "Insert entry for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-add-PDF-attachment
 "Attach the PDF of a BibTeX entry where available.")

(bibtex-actions-define-action
 bibtex-completion-edit-notes
 "Open the notes associated with a BibTeX entry using
 ‘bibtex-completion-edit-notes-function’.")

(bibtex-actions-define-action
 bibtex-completion-show-entry
 "Show the selected entry in the relevant BibTeX file.")

(bibtex-actions-define-action
 bibtex-completion-add-pdf-to-library
 "Add a PDF to the library for the selected BibTeX entry.
The PDF can be added either from an open buffer, a file, or a
URL.")

(provide 'bibtex-actions)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bibtex-actions.el ends here
