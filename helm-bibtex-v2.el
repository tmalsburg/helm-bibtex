
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

;; The function `window-width' does not necessarily report the correct
;; number of characters that fit on a line.  This is a
;; work-around.  See also this bug report:
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19395
(defun heln-bibtex-window-width ()
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
