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

(defcustom bibtex-actions-default-action 'bibtex-actions-open-any
  "The default action for the `bibtex-actions` command."
  :group 'bibtex-completion
  :type 'function)

(defmacro bibtex-actions-define-action (action doc)
  "Wraps the function ACTION in another function named NAME which
extracts the key from the candidate selected in embark and
passes it to ACTION."
  (let* ((old-name (symbol-name action))
         (mid-name (substring old-name 17 (length old-name)))
         (new-name (intern (concat "bibtex-actions" mid-name))))
    `(defun ,new-name (cand)
       ,doc
       (interactive
        (list (cdr (assoc (bibtex-actions--read) (bibtex-actions--get-candidates)))))
       (,action (list cand)))))

(bibtex-actions-define-action bibtex-completion-open-any "")
(bibtex-actions-define-action bibtex-completion-open-pdf "")
(bibtex-actions-define-action bibtex-completion-open-url-or-doi "")
(bibtex-actions-define-action bibtex-completion-insert-citation "")
(bibtex-actions-define-action bibtex-completion-insert-reference "")
(bibtex-actions-define-action bibtex-completion-insert-key "")
(bibtex-actions-define-action bibtex-completion-insert-bibtex "")
(bibtex-actions-define-action bibtex-completion-add-PDF-attachment "")
(bibtex-actions-define-action bibtex-completion-edit-notes "")
(bibtex-actions-define-action bibtex-completion-show-entry "")
(bibtex-actions-define-action bibtex-completion-add-pdf-to-library "")

(defun bibtex-actions--get-candidates ()
  "Return all keys from bibtex-completion-candidates."
  (mapcar
   (lambda (cand)
     (cons (bibtex-completion-format-entry cand (1- (frame-width)))
           (cdr (assoc "=key=" cand))))
   (bibtex-completion-candidates)))

(defun bibtex-actions--read ()
  "Read bibtex-completion entries for completion."
  (completing-read
   "BibTeX entries: "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         '(metadata (category . bibtex))
       (complete-with-action action (bibtex-actions--get-candidates) string predicate)))))

;;;###autoload
(defun bibtex-actions (bib-entry)
  "Search BibTeX entries using `completing-read' and actions."
  (interactive
   (progn
     (bibtex-completion-init)
     (list (cdr (assoc (bibtex-actions--read) (bibtex-actions--get-candidates))))))
  (apply bibtex-actions-default-action (list bib-entry)))

(provide 'bibtex-actions)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bibtex-actions.el ends here
