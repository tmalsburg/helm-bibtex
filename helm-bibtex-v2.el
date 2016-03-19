
(require 'helm)
(require 'helm-net)
(require 'helm-plugin)
(require 'helm-easymenu)
(require 'bibtex-completion)

(defcustom helm-bibtex-full-frame t
  "Non-nil means open `helm-bibtex' using the entire window. When
nil, the window will split below."
  :group 'helm-bibtex
  :type 'boolean)

(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["BibTeX" helm-bibtex t])

(defun helm-bibtex-candidates-formatter (candidates source)
  "Formats BibTeX entries for display in results list."
  (cl-loop
   with width = (with-helm-window (bibtex-completion-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (bibtex-completion-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
     for fields = '("author" "title" "year" "=has-pdf=" "=has-note=" "=type=")
   else
     for fields = '("editor" "title" "year" "=has-pdf=" "=has-note=" "=type=")
   for fields = (-map (lambda (it)
                        (bibtex-completion-clean-string
                          (bibtex-completion-get-value it entry " ")))
                      fields)
   for fields = (-update-at 0 'bibtex-completion-shorten-authors fields)
   collect
   (cons (s-format "$0 $1 $2 $3$4 $5" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 36 (- width 53) 4 1 1 7)))
         entry-key)))

(defun helm-bibtex-open-pdf (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (bibtex-completion-open-pdf keys)))

(defun helm-bibtex-open-url-or-doi (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (bibtex-completion-open-url-or-doi keys)))

(defun helm-bibtex-insert-citation (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (bibtex-completion-insert-citation keys))))

(defun helm-bibtex-insert-reference (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (bibtex-completion-insert-reference keys))))

(defun helm-bibtex-insert-key (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (bibtex-completion-insert-key keys))))

(defun helm-bibtex-insert-bibtex (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (bibtex-completion-insert-bibtex keys))))

(defun helm-bibtex-add-PDF-attachment (_)
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (bibtex-completion-add-PDF-attachment keys))))

(defvar helm-source-bibtex
  '((name                                      . "BibTeX entries")
    (init                                      . bibtex-completion-init)
    (candidates                                . bibtex-completion-candidates)
    (filtered-candidate-transformer            . helm-bibtex-candidates-formatter)
    (action . (("Open PDF file (if present)"   . helm-bibtex-open-pdf)
               ("Open URL or DOI in browser"   . helm-bibtex-open-url-or-doi)
               ("Insert citation"              . helm-bibtex-insert-citation)
               ("Insert reference"             . helm-bibtex-insert-reference)
               ("Insert BibTeX key"            . helm-bibtex-insert-key)
               ("Insert BibTeX entry"          . helm-bibtex-insert-bibtex)
               ("Attach PDF to email"          . helm-bibtex-add-PDF-attachment)
               ("Edit notes"                   . bibtex-completion-edit-notes)
               ("Show entry"                   . bibtex-completion-show-entry))))
  "Source for searching in BibTeX files.")

(defvar helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . bibtex-completion-fallback-candidates)
    (no-matchplugin)
    (nohighlight)
    (action          . bibtex-completion-fallback-action))
  "Source for online look-up.")

;;;###autoload
(defun helm-bibtex (&optional arg)
  "Search BibTeX entries.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (when arg
    (setq bibtex-completion-bibliography-hash ""))
  (helm :sources '(helm-source-bibtex helm-source-fallback-options)
        :full-frame helm-bibtex-full-frame
        :buffer "*helm bibtex*"
        :candidate-number-limit 500))

(provide 'helm-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
