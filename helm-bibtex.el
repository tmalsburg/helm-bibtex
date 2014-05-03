;;; helm-bibtex.el --- Helm source for searching in a BibTeX bibliography

;; Copyright 2014 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Requires: ((helm "1.5.5") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5"))

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

;; A helm source for searching entries in a BibTeX bibliography.
;;
;; Key features:
;;
;; - Quick access to your bibliography from within Emacs
;; - Provides instant search results as you type
;; - Support for multiple BibTeX files
;; - Insert BibTeX key of selected entry in your current document
;; - Open the PDF associated with an entry
;; - Add notes to an entry
;; - Edit selected entry
;; - Quick access to online bibliographic databases such as Pubmed,
;;   arXiv, Google Scholar, Library of Congress, etc.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

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
;; In order to specify a (list of) bibliography, set the variable
;; `helm-bibtex-bibliography' to point to a list of BibTeX file.

;;; Usage:

;; You can search entries using the command `helm-bibtex'.  Select an
;; entry and press TAB to access all available actions.  At the end of
;; the list of matches you find some dummy entries that can be used
;; for searching in online databases.  Apart from that, familiarize
;; yourself with Helm.  It's more powerful that you might think.

;;; Code:

(require 'helm)
(require 'helm-net)
(require 'helm-plugin)
(require 'ebib)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)

(defgroup helm-bibtex nil
  "Helm plugin for searching entries in a BibTeX bibliography."
  :group 'helm)

(defcustom helm-bibtex-bibliography nil
  "The list of BibTeX files that is used for searching. The first
one will be used when creating new entries."
  :group 'helm-bibtex
  :type '(choice file (repeat file)))

(defcustom helm-bibtex-library-path nil
  "The directory in which PDFs are stored.  Helm-bibtex
assumes that the names of these PDFs are composed of the
BibTeX-key plus a \".pdf\" suffix."
  :group 'helm-bibtex
  :type 'directory)

(defcustom helm-bibtex-pdf-open-function 'find-file
  "The function used for opening PDF files.  This can be an arbitrary
function that takes one argument: the path to the PDF file."
  :group 'helm-bibtex
  :type 'function)

(defcustom helm-bibtex-notes-path nil
  "The directory in which notes are stored.  Helm-bibtex assumes
that the names of these notes are composed of the BibTeX-key plus
a suffix that is specified in `helm-bibtex-notes-extension'."
  :group 'helm-bibtex
  :type 'directory)

(defcustom helm-bibtex-notes-extension ".org"
  "The extension of the files containing notes."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-fallback-options
  '(("Search in Google Scholar" . "http://scholar.google.co.uk/scholar?q=%s")
    ("Search in Pubmed" . "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s")
    ("Search in arXiv" . helm-bibtex-arxiv)
    ("Search in Bodleian Library" . "http://solo.bodleian.ox.ac.uk/primo_library/libweb/action/search.do?vl(freeText0)=%s&fn=search&tab=all")
    ("Search in Library of Congress" . "http://www.loc.gov/search/?q=%s&all=true&st=list")
    ("Search in Deutsche Nationalbibliothek" . "https://portal.dnb.de/opac.htm?query=%s")
    ("Search in British National Library" . "http://explore.bl.uk/primo_library/libweb/action/search.do?&vl(freeText0)=%s&fn=search")
    ("Search in Bibliothèque nationale de France" . "http://catalogue.bnf.fr/servlet/RechercheEquation?host=catalogue?historique1=Recherche+par+mots+de+la+notice&niveau1=1&url1=/jsp/recherchemots_simple.jsp?host=catalogue&maxNiveau=1&categorieRecherche=RechercheMotsSimple&NomPageJSP=/jsp/recherchemots_simple.jsp?host=catalogue&RechercheMotsSimpleAsauvegarder=0&ecranRechercheMot=/jsp/recherchemots_simple.jsp&resultatsParPage=20&x=40&y=22&nbElementsHDJ=6&nbElementsRDJ=7&nbElementsRCL=12&FondsNumerise=M&CollectionHautdejardin=TVXZROM&HDJ_DAV=R&HDJ_D2=V&HDJ_D1=T&HDJ_D3=X&HDJ_D4=Z&HDJ_SRB=O&CollectionRezdejardin=UWY1SPQM&RDJ_DAV=S&RDJ_D2=W&RDJ_D1=U&RDJ_D3=Y&RDJ_D4=1&RDJ_SRB=P&RDJ_RLR=Q&RICHELIEU_AUTRE=ABCDEEGIKLJ&RCL_D1=A&RCL_D2=K&RCL_D3=D&RCL_D4=E&RCL_D5=E&RCL_D6=C&RCL_D7=B&RCL_D8=J&RCL_D9=G&RCL_D10=I&RCL_D11=L&ARSENAL=H&LivrePeriodique=IP&partitions=C&images_fixes=F&son=S&images_animees=N&Disquette_cederoms=E&multimedia=M&cartes_plans=D&manuscrits=BT&monnaies_medailles_objets=JO&salle_spectacle=V&Monographie_TN=M&Periodique_TN=S&Recueil_TN=R&CollectionEditorial_TN=C&Ensemble_TN=E&Spectacle_TN=A&NoticeB=%s")
    ("Search in Gallica Bibliothèque Numérique" . "http://gallica.bnf.fr/Search?q=%s")
    ("Create new entry" . helm-bibtex-create-new-entry))
  "Alist of online sources that can be used to search for
publications.  The key of each entry is the name of the online
source.  The value is the URL used for retrieving results.  This
URL must contain a %s in the position where the search term
should be inserted.  Alternatively, the value can be a function
that will be called when the entry is selected."
  :group 'helm-bibtex
  :type '(alist :key-type string
                :value-type (choice (string :tag "URL")
                            (function :tag "Function"))))

(defcustom helm-bibtex-browser-function nil
  "The browser that is used to access online resources.  If
nil (default), the value of `browse-url-browser-function' is
used.  If that value is nil, Helm uses the first available
browser in `helm-browse-url-default-browser-alist'"
  :group 'helm-bibtex
  :type '(choice
          (const         :tag "Default" :value nil)
          (function-item :tag "Emacs interface to w3m" :value w3m-browse-url)
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "W3 in another Emacs via `gnudoit'"
                         :value  browse-url-w3-gnudoit)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Galeon" :value  browse-url-galeon)
          (function-item :tag "Epiphany" :value  browse-url-epiphany)
          (function-item :tag "Netscape" :value  browse-url-netscape)
          (function-item :tag "eww" :value  eww-browse-url)
          (function-item :tag "Mosaic" :value  browse-url-mosaic)
          (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
          (function-item :tag "Text browser in an xterm window"
                         :value browse-url-text-xterm)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "KDE" :value browse-url-kde)
          (function-item :tag "Elinks" :value browse-url-elinks)
          (function-item :tag "Specified by `Browse Url Generic Program'"
                         :value browse-url-generic)
          (function-item :tag "Default Windows browser"
                         :value browse-url-default-windows-browser)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function-item :tag "GNOME invoking Mozilla"
                         :value browse-url-gnome-moz)
          (function-item :tag "Default browser"
                         :value browse-url-default-browser)
          (function      :tag "Your own function")
          (alist         :tag "Regexp/function association list"
                         :key-type regexp :value-type function))
)


(defun helm-bibtex-init ()
  "Reads the BibTeX files and returns a list of conses, one for
each entry.  The first element of these conses is a string
containing authors, title, year, type, and key of the
entry.  This is string is used for matching.  The second element
is an alists containing the full entry."
  ;; Open bibliography in buffer:
  (with-temp-buffer
    (mapc 'insert-file-contents 
          (if (listp helm-bibtex-bibliography)
              helm-bibtex-bibliography
            (list helm-bibtex-bibliography)))
    ;; Iterate over entries:
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
                  (ebib-match-paren-forward (point-max))))
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


(defun helm-bibtex-candidates-formatter (candidates source)
  "Formats BibTeX entries for display in results list."
  (cl-loop
    for cand in candidates
    for cand = (cdr cand)
    for entry-key = (helm-bibtex-get-default 'entry-key cand nil) 
    for cand = (--map (helm-bibtex-clean-string
                       (helm-bibtex-get-default it cand "-"))
                      '(author title year entry-type))
    for cand = (cons (helm-bibtex-shorten-authors (car cand)) (cdr cand))
    for width = (save-excursion (with-helm-window (window-width)))
    collect
    (cons (s-format "$0 $1 $2 $3" 'elt
            (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                       cand (list 36 (- width 50) 4 7)))
          entry-key)))


(defun helm-bibtex-clean-string (s)
  "Removes quoting and superfluous white space from BibTeX field
values."
  (replace-regexp-in-string "[\n\t ]+" " "
    (replace-regexp-in-string "[\\\"{}]+" "" s)))

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


(defun helm-bibtex-open-pdf (entry)
  "Open the PDF associated with the entry using the function
specified in `helm-bibtex-pdf-open-function',"
  (let ((path (f-join helm-bibtex-library-path (s-concat entry ".pdf"))))
    (if (f-exists? path)
        (funcall helm-bibtex-pdf-open-function path)
      (message "No PDF for this entry: %s" entry))))

(defun helm-bibtex-insert-key (entry)
  "Insert the BibTeX key at point."
  (insert entry))

(defun helm-bibtex-edit-notes (entry)
  "Open the notes associated with the entry using `find-file'."
  (let ((path (f-join helm-bibtex-notes-path (s-concat entry helm-bibtex-notes-extension))))
    (find-file path)))

(defun helm-bibtex-buffer-visiting (file)
  (or (get-file-buffer file)
      (find-buffer-visiting file)))

(defun helm-bibtex-show-entry (entry)
  "Show the entry in the BibTeX file."
  (catch 'break
    (dolist (bibtex-file helm-bibtex-bibliography)
      (let ((buf (helm-bibtex-buffer-visiting bibtex-file)))
        (find-file bibtex-file)
        (goto-char (point-min))
        (if (search-forward entry nil t)
            (throw 'break t)
          (unless buf
            (kill-buffer)))))))

(defun helm-bibtex-fallback-action (cand)
  (let ((browse-url-browser-function
          (or helm-bibtex-browser-function
              browse-url-browser-function))
        (cand1 (cdr (assoc cand helm-bibtex-fallback-options))))
    (cond 
      ((stringp cand1)
        (helm-browse-url (format cand1 (url-hexify-string helm-pattern))))
      ((functionp cand1)
        (funcall cand1))
      (t (error "Don't know how to interpret this: %s" cand1)))))

(defun helm-bibtex-arxiv ()
  "Search for the current `helm-pattern' in arXiv."
  (let* ((browse-url-browser-function
          (or helm-bibtex-browser-function
              browse-url-browser-function))
         (terms (s-split "\s+" helm-pattern))
         (terms (-map 'url-hexify-string terms))
         (terms (if (> (length terms) 1) (cons "AND" terms) terms)))
    (helm-browse-url (format "http://arxiv.org/find/all/1/all:+%s/0/1/0/all/0/1"
                             (s-join "+" terms)))))

(defun helm-bibtex-create-new-entry ()
  "Open the BibTeX and place point at the end."
  (find-file (first helm-bibtex-bibliography))
  (goto-char (point-max)))


(defvar helm-source-bibtex
  '((name                                    . "Search BibTeX entries")
    (candidates                              . helm-bibtex-init)
    (filtered-candidate-transformer          . helm-bibtex-candidates-formatter)
    (action . (("Open PDF file (if present)" . helm-bibtex-open-pdf)
               ("Insert BibTeX key at point" . helm-bibtex-insert-key)
               ("Edit notes"                 . helm-bibtex-edit-notes)
               ("Show entry in BibTex file"  . helm-bibtex-show-entry)))))

(defvar helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . (lambda () (-map 'car helm-bibtex-fallback-options)))
    (no-matchplugin)
    (nohighlight)
    (action          . helm-bibtex-fallback-action))
  "Source for online look-up.")

;;;###autoload
(defun helm-bibtex ()
  "Search BibTeX entries."
  (interactive)
  (helm :sources '(helm-source-bibtex helm-source-fallback-options)
        :full-frame t
        :candidate-number-limit 500))

(provide 'helm-bibtex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bibtex.el ends here
