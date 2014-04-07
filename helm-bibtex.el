;;; helm-bibtex.el --- Helm source for searching in a BibTeX bibliography

;; Copyright 2014 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Requires: ((helm "1.5.5"))

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

;; Bugs:
;;
;; FIXME Matching is done on the transformed entries.  These do not
;; include the full information.  Match in the raw data.

;; Below is a list of planned features:
;;
;; TODO Action open PDF if available.
;; TODO Action edit notes for entry.
;; TODO Action insert LaTeX cite macro.
;; TODO Action show entry in BíbTeX file.
;; TODO Icon showing whether there is a PDF for an entry.
;; TODO Icon showing whether there are notes for an entry.
;; TODO Sort according to a column.
;; TODO Make sort column configurable.
;; TODO Make column with and column order configurable.

;;; Install:

;; Put this file on your Emacs-Lisp load path and add the following in
;; your Emacs startup file:
;;
;;     (require 'helm-bibtex)
;;
;; Alternatively, you can use autoload:
;;
;;     (autoload 'helm-bibtex "helm-bibtex" "" t)
;;
;; In order to specify a bibliography, set the variable
;; `helm-bibtex-bibliography' to point to a BibTeX file.

;;; Usage:

;; In the current version, you can only search entries using the
;; command `helm-bibtex'.  Future versions will add actions that will
;; allow you to edit an entry, to open the associated PDF file, to add
;; notes to an entry, etc.


;;; Code:

(require 'helm)
(require 'ebib)
(require 'cl-lib)
(require 'dash)
(require 's)

(defgroup helm-bibtex nil
  "Helm plugin for searching entries in a BibTeX bibliography."
  :group 'helm)

(defcustom helm-bibtex-bibliography "/home/malsburg/Documents/Uni/Bibliographie/bibliography.bib"
  "The BibTeX file that is used for searching."
  :group 'helm-bibtex
  :type 'file)


(defun helm-bibtex-init ()
    "Reads a BibTeX file and returns a list of conses, one for
each entry.  The first element of these conses is a string
containing authors, title, year, entry-type, and -key of the
entry.  The second element is an alists containing the full
entry."
  ; Open bibliography in buffer:
  (with-temp-buffer
    (insert-file-contents helm-bibtex-bibliography)
    ; Iterate over entries:
    (goto-char (point-min))
    (let (entries (list))
      (while (re-search-forward "^@" nil t) ; find the next entry
        (let ((beg (point)))
          (if (ebib-looking-at-goto-end
                (concat "\\(" ebib-bibtex-identifier "\\)[[:space:]]*[\(\{]") 1)
            (let ((entry-type (downcase
                                (buffer-substring-no-properties beg (point)))))
              (ebib-looking-at-goto-end "[[:space:]]*[\(\{]")
              (if (assoc (intern-soft entry-type) ebib-entry-types)
                  (setq entries (cons (helm-bibtex-read-entry entry-type)
                                      entries))
                (t (ebib-match-paren-forward (point-max)))))
            (error "Error: illegal entry type at line %d."
                   (line-number-at-pos)))))
      (--map (cons (helm-bibtex-clean-string
                     (s-join " " (-map 'cdr it))) it)
            entries))))

(defun helm-bibtex-read-entry (entry-type)
  "Read the entry starting at point and return an association
list containing the fields of the entry."
  (setq entry-type (intern-soft entry-type))
  (let ((limit (save-excursion
                 (backward-char)
                 (ebib-match-paren-forward (point-max))
                 (point)))
        (beg (progn
               (skip-chars-forward " \n\t\f") ; note the space!
               (point)))
        (entry-key nil)
        (record nil))
    (if (ebib-looking-at-goto-end (concat "\\("
                                          ebib-key-regexp
                                          "\\)[ \t\n\f]*,")
                                  1)  ; this delimits the entry key
        (progn                        ; if we found an entry key
          (setq entry-key (buffer-substring-no-properties beg (point)))
          (skip-chars-forward "^,"))) ; move to the comma after the entry key
    (setq record (cl-loop for field = (ebib-find-bibtex-field limit)
             while field 
             if (member (car field) '(author title year))
              collect field))
    (setq record (cons (cons 'entry-type (symbol-name entry-type)) record))
    (cons (cons 'entry-key entry-key) record)))


(defun helm-bibtex-candidates-formatter (candidates)
  "Formats BibTeX entries for display in results list."
  (cl-loop
    for cand in candidates
    for cand = (cdr cand)
    for cand = (--map (helm-bibtex-clean-string
                       (helm-bibtex-get-default it cand "-"))
                      '(author title year entry-type))
    for cand = (cons (helm-bibtex-shorten-authors (car cand)) (cdr cand))
    for width = (save-excursion (with-helm-window (window-width)))
    collect
    (cons (s-format "$0 $1 $2 $3" 'elt
            (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                       cand (list 36 (- width 50) 4 7)))
          (-last-item cand))))


(defun helm-bibtex-clean-string (s)
  "Removes quoting and superfluous white space from BibTeX field
values."
  (replace-regexp-in-string "[\n\t ]+" " "
    (replace-regexp-in-string "[\"{}]+" "" s)))

(defun helm-bibtex-shorten-authors (authors)
  "Returns a comma-separated list of the surnames in authors."
  (s-join ", "
    (cl-loop for a in (s-split " and " authors)
             for p = (s-split "," a t)
             if (eq 1 (length p))
               collect (-last-item (s-split " +" (car p) t))
             else
               collect (car p))))

(defun helm-bibtex-get-default (key alist default)
  "Returns the cdr of the element that has a car matching
key.  If no such element exists, default is returned instead."
  (let ((e (assoc key alist)))
    (if e (cdr e) default)))


(defvar helm-source-bibtex
  '((name . "Search BibTeX entries")
    (candidates . helm-bibtex-init)
    (candidate-transformer . helm-bibtex-candidates-formatter)))

;;;###autoload
(defun helm-bibtex ()
  "Search BibTeX entries."
  (interactive)
  (helm :sources '(helm-source-bibtex)
        :full-frame t
        :candidate-number-limit 500))

;(helm-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bibtex.el ends here
