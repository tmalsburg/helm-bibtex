
(require 'helm)
(require 'helm-net)
(require 'helm-plugin)
(require 'helm-easymenu)
(require 'bibtex-completion)

;; The defvars and defvaraliases below allow people to continue using
;; their old helm-bibtex configurations:

(defvar helm-bibtex-bibliography)
(defvar helm-bibtex-library-path)
(defvar helm-bibtex-pdf-open-function)
(defvar helm-bibtex-pdf-symbol)
(defvar helm-bibtex-format-citation-functions)
(defvar helm-bibtex-notes-path)
(defvar helm-bibtex-notes-template-multiple-files)
(defvar helm-bibtex-notes-template-one-file)
(defvar helm-bibtex-notes-key-pattern)
(defvar helm-bibtex-notes-extension)
(defvar helm-bibtex-notes-symbol)
(defvar helm-bibtex-fallback-options)
(defvar helm-bibtex-browser-function)
(defvar helm-bibtex-additional-search-fields)
(defvar helm-bibtex-no-export-fields)
(defvar helm-bibtex-cite-commands)
(defvar helm-bibtex-cite-default-command)
(defvar helm-bibtex-cite-prompt-for-optional-arguments)
(defvar helm-bibtex-cite-default-as-initial-input)
(defvar helm-bibtex-pdf-field)

(defvaralias 'bibtex-completion-bibliography 'helm-bibtex-bibliography)
(defvaralias 'bibtex-completion-library-path 'helm-bibtex-library-path)
(defvaralias 'bibtex-completion-pdf-open-function 'helm-bibtex-pdf-open-function)
(defvaralias 'bibtex-completion-pdf-symbol 'helm-bibtex-pdf-symbol)
(defvaralias 'bibtex-completion-format-citation-functions 'helm-bibtex-format-citation-functions)
(defvaralias 'bibtex-completion-notes-path 'helm-bibtex-notes-path)
(defvaralias 'bibtex-completion-notes-template-multiple-files 'helm-bibtex-notes-template-multiple-files)
(defvaralias 'bibtex-completion-notes-template-one-file 'helm-bibtex-notes-template-one-file)
(defvaralias 'bibtex-completion-notes-key-pattern 'helm-bibtex-notes-key-pattern)
(defvaralias 'bibtex-completion-notes-extension 'helm-bibtex-notes-extension)
(defvaralias 'bibtex-completion-notes-symbol 'helm-bibtex-notes-symbol)
(defvaralias 'bibtex-completion-fallback-options 'helm-bibtex-fallback-options)
(defvaralias 'bibtex-completion-browser-function 'helm-bibtex-browser-function)
(defvaralias 'bibtex-completion-additional-search-fields 'helm-bibtex-additional-search-fields)
(defvaralias 'bibtex-completion-no-export-fields 'helm-bibtex-no-export-fields)
(defvaralias 'bibtex-completion-cite-commands 'helm-bibtex-cite-commands)
(defvaralias 'bibtex-completion-cite-default-command 'helm-bibtex-cite-default-command)
(defvaralias 'bibtex-completion-cite-prompt-for-optional-arguments 'helm-bibtex-cite-prompt-for-optional-arguments)
(defvaralias 'bibtex-completion-cite-default-as-initial-input 'helm-bibtex-cite-default-as-initial-input)
(defvaralias 'bibtex-completion-pdf-field 'helm-bibtex-pdf-field)

;; Helm-specific configurations:

(defcustom helm-bibtex-full-frame t
  "Non-nil means open `helm-bibtex' using the entire window. When
nil, the window will split below."
  :group 'helm-bibtex
  :type 'boolean)

(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["BibTeX" helm-bibtex t])

;; Candidate formatter:

;; The function `window-width' does not necessarily report the correct
;; number of characters that fit on a line.  This is a
;; work-around.  See also this bug report:
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19395
(defun helm-bibtex-window-width ()
  (if (and (not (featurep 'xemacs))
           (display-graphic-p)
           overflow-newline-into-fringe
           (/= (frame-parameter nil 'left-fringe) 0)
           (/= (frame-parameter nil 'right-fringe) 0))
      (window-body-width)
    (1- (window-body-width))))

(defun helm-bibtex-candidates-formatter (candidates _)
  (let ((width (with-helm-window (helm-bibtex-window-width))))
    (bibtex-completion-candidates-formatter candidates width)))

;; Warp bibtex-completion actions with some helm-specific code:

(defmacro helm-bibtex-helmify-action (action name)
  "Wraps the function ACTION in another function named NAME which
passes the candidates marked in helm to ACTION.  Also uses
with-helm-current-buffer such that when ACTION inserts text and
it comes out in the right buffer."
  `(defun ,name (_)
     (let ((keys (helm-marked-candidates :with-wildcard t)))
       (with-helm-current-buffer
         (,action keys)))))

(helm-bibtex-helmify-action bibtex-completion-open-pdf helm-bibtex-open-pdf)
(helm-bibtex-helmify-action bibtex-completion-open-url-or-doi helm-bibtex-open-url-or-doi)
(helm-bibtex-helmify-action bibtex-completion-insert-citation helm-bibtex-insert-citation)
(helm-bibtex-helmify-action bibtex-completion-insert-reference helm-bibtex-insert-reference)
(helm-bibtex-helmify-action bibtex-completion-insert-key helm-bibtex-insert-key)
(helm-bibtex-helmify-action bibtex-completion-insert-bibtex helm-bibtex-insert-bibtex)
(helm-bibtex-helmify-action bibtex-completion-add-PDF-attachment helm-bibtex-add-PDF-attachment)

;; Helm sources:

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

;; Helm-bibtex command:

;;;###autoload
(defun helm-bibtex (&optional arg)
  "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
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
