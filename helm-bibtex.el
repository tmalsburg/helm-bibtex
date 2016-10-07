;;; helm-bibtex.el --- A bibliography manager based on Helm

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 2.0.0
;; Package-Requires: ((helm "1.5.5") (parsebib "1.0") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5") (biblio "0.2"))

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

;; A bibliography manager for Emacs, based on Helm and the
;; bibtex-completion backend.
;;
;; News:
;; - 04/18/2016: Improved support for Mendely/Jabref/Zotero way of
;;   referencing PDFs.
;; - 04/06/2016: Generic functions are factored out into a backend for
;;   use with other completion frameworks like ivy.
;; - 04/02/2016: Added support for biblio.el which is useful for
;;   importing BibTeX from CrossRef and other sources.  See new
;;   fallback options and the section "Importing BibTeX from CrossRef"
;;   on the GitHub page.
;;
;; See NEWS.org for old news.
;;
;; Key features:
;; - Quick access to your bibliography from within Emacs
;; - Powerful search capabilities
;; - Provides instant search results as you type
;; - Tightly integrated with LaTeX authoring, emails, Org mode, etc.
;; - Open the PDFs, URLs, or DOIs associated with an entry
;; - Insert LaTeX cite commands, Ebib links, or Pandoc citations,
;;   BibTeX entries, or plain text references at point, attach PDFs to
;;   emails
;; - Support for note taking
;; - Quick access to online bibliographic databases such as Pubmed,
;;   arXiv, Google Scholar, Library of Congress, etc.
;; - Imports BibTeX entries from CrossRef and other sources.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

;;; Install:

;; Put this file in a directory included in your load path or install
;; helm-bibtex from MELPA (preferred).  Then add the following in your
;; Emacs startup file:
;;
;;     (require 'helm-bibtex)
;;
;; Alternatively, you can use autoload:
;;
;;     (autoload 'helm-bibtex "helm-bibtex" "" t)
;;
;; Requirements are parsebib, helm, s, dash, and f.  The easiest way
;; to install these packages is through MELPA.  Make sure helm is
;; properly configured (see
;; https://github.com/emacs-helm/helm#install-from-emacs-packaging-system).
;;
;; Let helm-bibtex know where it can find your bibliography by setting
;; the variable `bibtex-completion-bibliography'.  See the manual for
;; more details:
;;
;;   https://github.com/tmalsburg/helm-bibtex#minimal-configuration

;;; Usage:

;; You can search entries using the command `helm-bibtex'.  Select an
;; entry and press TAB to access all available actions.  At the end of
;; the list of matches you find some dummy entries that can be used
;; for searching in online databases.  Apart from that, familiarize
;; yourself with Helm.  It's more powerful that you might think.

;;; Code:

(require 'helm)
(require 'helm-net)
(require 'helm-easymenu)
(require 'bibtex-completion)

;; The following allows people to continue using their old helm-bibtex
;; configurations:

(cl-loop
 for var in '("bibliography" "library-path" "pdf-open-function"
              "pdf-symbol" "format-citation-functions" "notes-path"
              "notes-template-multiple-files"
              "notes-template-one-file" "notes-key-pattern"
              "notes-extension" "notes-symbol" "fallback-options"
              "browser-function" "additional-search-fields"
              "no-export-fields" "cite-commands"
              "cite-default-command"
              "cite-prompt-for-optional-arguments"
              "cite-default-as-initial-input" "pdf-field")
 for oldvar = (intern (concat "helm-bibtex-" var))
 for newvar = (intern (concat "bibtex-completion-" var))
 do
 (defvaralias newvar oldvar)
 (make-obsolete-variable oldvar newvar "2016-03-20"))

;; Helm-specific configurations:

(defcustom helm-bibtex-full-frame t
  "Non-nil means open `helm-bibtex' using the entire window. When
nil, the window will split below."
  :group 'bibtex-completion
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
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (bibtex-completion-get-value "=key=" entry)
   collect (cons (bibtex-completion-format-entry entry width) entry-key)))

;; Warp bibtex-completion actions with some helm-specific code:

(defmacro helm-bibtex-helmify-action (action name)
  "Wraps the function ACTION in another function named NAME which
passes the candidates marked in helm to ACTION.  Also uses
with-helm-current-buffer such that when ACTION inserts text
it comes out in the right buffer."
  `(defun ,name (_)
     (let ((keys (helm-marked-candidates :with-wildcard t)))
       (with-helm-current-buffer
         (,action keys)))))

(helm-bibtex-helmify-action bibtex-completion-open-pdf helm-bibtex-open-pdf)
(helm-bibtex-helmify-action bibtex-completion-open-url-or-doi helm-bibtex-open-url-or-doi)
(helm-bibtex-helmify-action bibtex-completion-open-any helm-bibtex-open-any)
(helm-bibtex-helmify-action bibtex-completion-insert-citation helm-bibtex-insert-citation)
(helm-bibtex-helmify-action bibtex-completion-insert-reference helm-bibtex-insert-reference)
(helm-bibtex-helmify-action bibtex-completion-insert-key helm-bibtex-insert-key)
(helm-bibtex-helmify-action bibtex-completion-insert-bibtex helm-bibtex-insert-bibtex)
(helm-bibtex-helmify-action bibtex-completion-add-PDF-attachment helm-bibtex-add-PDF-attachment)
(helm-bibtex-helmify-action bibtex-completion-edit-notes helm-bibtex-edit-notes)
(helm-bibtex-helmify-action bibtex-completion-show-entry helm-bibtex-show-entry)

;; Helm sources:

(defvar helm-source-bibtex
  (helm-build-sync-source "BibTeX entries"
    :init 'bibtex-completion-init
    :candidates 'bibtex-completion-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
             "Open PDF, URL or DOI"       'helm-bibtex-open-any             
             "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
             "Insert citation"            'helm-bibtex-insert-citation
             "Insert reference"           'helm-bibtex-insert-reference
             "Insert BibTeX key"          'helm-bibtex-insert-key
             "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
             "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
             "Edit notes"                 'helm-bibtex-edit-notes
             "Show entry"                 'helm-bibtex-show-entry))
  "Source for searching in BibTeX files.")

(defvar helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . bibtex-completion-fallback-candidates)
    (no-matchplugin)
    (nohighlight)
    (action          . (lambda (candidate) (bibtex-completion-fallback-action candidate helm-pattern))))
  "Source for online look-up.")

;; Helm-bibtex command:

;;;###autoload
(defun helm-bibtex (&optional arg)
  "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (when arg
    (setf (cadr (assoc bibtex-completion-bibliography-type bibtex-completion-cache)) ""))
  (helm :sources (list helm-source-bibtex helm-source-fallback-options)
        :full-frame helm-bibtex-full-frame
        :buffer "*helm bibtex*"
        :candidate-number-limit 500))

;;;###autoload
(defun helm-bibtex-with-local-bibliography (&optional arg)
  "Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography reread."
  (interactive "P")
  (let* ((bibtex-completion-bibliography-type 'local)
         (bibtex-completion-bibliography (bibtex-completion-find-local-bibliography)))
    (helm-bibtex arg)))

(provide 'helm-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bibtex.el ends here
