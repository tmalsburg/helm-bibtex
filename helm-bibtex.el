;;; helm-bibtex.el --- Helm source for managing BibTeX bibliographies

;; Copyright 2014 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Requires: ((helm "1.5.5") (parsebib "1.0") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5"))

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

;; A helm source for managing BibTeX bibliographies.
;;
;; News:
;; - 01/11/2015:
;;   - Added resolution of cross-references.  Missing fields are
;;     imported from the cross-referenced entry (useful for conference
;;     abstracts).
;;   - Editors are shown when there are no authors (useful for
;;     conference proceedings).
;;   - Fields that should be ignored when exporting BibTeX can be
;;     configured via `helm-bibtex-no-export-fields' (useful to keep
;;     private notes private).
;; - 01/29/2015: Better formatting of plain text references
;;   following the APA style guide.
;; - 01/16/2015: The parsed bibliography is now cached which can
;;   dramatically improve responsiveness with larger bibliographies.
;;
;; Key features:
;; - Quick access to your bibliography from within Emacs
;; - Provides instant search results as you type
;; - Quick access to online bibliographic databases such as Pubmed,
;;   arXiv, Google Scholar, Library of Congress, etc.
;; - Support for multiple BibTeX files
;; - Open the PDF associated with an entry
;; - Open the URL or DOI of an entry in the browser
;; - Insert LaTeX cite command, ebib link, or pandoc citation
;;   depending on document type
;; - Insert BibTeX entry or plain text reference at point (useful when
;;   sharing BibTeX with colleagues via email)
;; - Attach PDF of entry to an email.
;; - Add notes to an entry
;; - Edit selected entry
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
;; Requirements are parsebib, helm, s, dash, f, and cl-lib.  The easiest
;; way to install these packages is perhaps through MELPA.
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
(require 'parsebib)
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
  "The function used for opening PDF files.  This can be an
arbitrary function that takes one argument: the path to the PDF
file.  The default is `find-file' which opens the PDF in
Emacs (either with docview or, if installed, the much superior
pdf-tools.  When set to `helm-open-file-with-default-tool', the
systems default viewer for PDFs is used."
  :group 'helm-bibtex
  :type 'function)

(defcustom helm-bibtex-pdf-symbol "⌘"
  "Symbol used to indicate that a PDF file is available for a
publication.  This should be a single character."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-format-citation-functions
  '((org-mode      . helm-bibtex-format-citation-ebib)
    (latex-mode    . helm-bibtex-format-citation-cite)
    (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
    (default       . helm-bibtex-format-citation-default))
  "The functions used for formatting citations.  The publication
can be cited, for example, as \cite{key} or ebib:key depending on
the major mode of the current buffer.  Note that the functions
should accept a list of keys as input.  With multiple marked
entries one can insert multiple keys at once,
e.g. \cite{key1,key2}. See the functions
`helm-bibtex-format-citation-ebib' and
`helm-bibtex-format-citation-cite' as examples."
  :group 'helm-bibtex
  :type '(alist :key-type symbol :value-type function))

(defcustom helm-bibtex-notes-path nil
  "The place where notes are stored.  This is either a file, in
which case all notes are stored in that file, or a directory, in
which case each publication gets its own notes file in that
directory.  In the latter case, helm-bibtex assumes that the
names of the note files are composed of the BibTeX-key plus a
suffix that is specified in `helm-bibtex-notes-extension'."
  :group 'helm-bibtex
  :type '(choice file directory))

(defcustom helm-bibtex-notes-template
  "\n* ${author} (${year}): ${title}\n  :PROPERTIES:\n  :BIBTEX-KEY: ${=key=}\n  :END:\n"
  "Template used to create a new note.  '${field-name}' can be
used to insert the value of a BibTeX field into the template."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-notes-extension ".org"
  "The extension of the files containing notes.  This is only
used when `helm-bibtex-notes-path' is a directory (not a file)."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-notes-symbol "✎"
  "Symbol used to indicate that a publication has notes.  This
should be a single character."
  :group 'helm-bibtex
  :type 'string)

(defcustom helm-bibtex-fallback-options
  '(("Google Scholar" . "http://scholar.google.co.uk/scholar?q=%s")
    ("Pubmed" . "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s")
    ("arXiv" . helm-bibtex-arxiv)
    ("Bodleian Library" . "http://solo.bodleian.ox.ac.uk/primo_library/libweb/action/search.do?vl(freeText0)=%s&fn=search&tab=all")
    ("Library of Congress" . "http://www.loc.gov/search/?q=%s&all=true&st=list")
    ("Deutsche Nationalbibliothek" . "https://portal.dnb.de/opac.htm?query=%s")
    ("British National Library" . "http://explore.bl.uk/primo_library/libweb/action/search.do?&vl(freeText0)=%s&fn=search")
    ("Bibliothèque nationale de France" . "http://catalogue.bnf.fr/servlet/RechercheEquation?host=catalogue?historique1=Recherche+par+mots+de+la+notice&niveau1=1&url1=/jsp/recherchemots_simple.jsp?host=catalogue&maxNiveau=1&categorieRecherche=RechercheMotsSimple&NomPageJSP=/jsp/recherchemots_simple.jsp?host=catalogue&RechercheMotsSimpleAsauvegarder=0&ecranRechercheMot=/jsp/recherchemots_simple.jsp&resultatsParPage=20&x=40&y=22&nbElementsHDJ=6&nbElementsRDJ=7&nbElementsRCL=12&FondsNumerise=M&CollectionHautdejardin=TVXZROM&HDJ_DAV=R&HDJ_D2=V&HDJ_D1=T&HDJ_D3=X&HDJ_D4=Z&HDJ_SRB=O&CollectionRezdejardin=UWY1SPQM&RDJ_DAV=S&RDJ_D2=W&RDJ_D1=U&RDJ_D3=Y&RDJ_D4=1&RDJ_SRB=P&RDJ_RLR=Q&RICHELIEU_AUTRE=ABCDEEGIKLJ&RCL_D1=A&RCL_D2=K&RCL_D3=D&RCL_D4=E&RCL_D5=E&RCL_D6=C&RCL_D7=B&RCL_D8=J&RCL_D9=G&RCL_D10=I&RCL_D11=L&ARSENAL=H&LivrePeriodique=IP&partitions=C&images_fixes=F&son=S&images_animees=N&Disquette_cederoms=E&multimedia=M&cartes_plans=D&manuscrits=BT&monnaies_medailles_objets=JO&salle_spectacle=V&Monographie_TN=M&Periodique_TN=S&Recueil_TN=R&CollectionEditorial_TN=C&Ensemble_TN=E&Spectacle_TN=A&NoticeB=%s")
    ("Gallica Bibliothèque Numérique" . "http://gallica.bnf.fr/Search?q=%s"))
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
                         :key-type regexp :value-type function)))

(defcustom helm-bibtex-additional-search-fields nil
  "The fields that are used for searching in addition to author,
editor, title, year, BibTeX key, and entry type."
  :group 'helm-bibtex
  :type 'list)

(defcustom helm-bibtex-no-export-fields nil
  "A list of fields that should be ignored when exporting BibTeX
entries."
  :group 'helm-bibtex
  :type 'list)

(easy-menu-add-item nil '("Tools" "Helm" "Tools") ["BibTeX" helm-bibtex t])

(defvar helm-bibtex-bibliography-hash nil
  "The hash of the content of the configured bibliography
files.  If this hash has not changed since the bibliography was
last parsed, a cached version of the parsed bibliography will be
used.")

(defvar helm-bibtex-cached-candidates nil
  "The a list of candidates obtained when the configured
bibliography files were last parsed.")


(defun helm-bibtex-init ()
  "Checks that the files and directories specified by the user
actually exist."
  (mapc (lambda (file)
          (unless (f-exists? file)
                  (user-error "BibTeX file %s could not be found." file)))
        (if (listp helm-bibtex-bibliography)
            helm-bibtex-bibliography
          (list helm-bibtex-bibliography))))

(defun helm-bibtex-candidates ()
  "Reads the BibTeX files and returns a list of conses, one for
each entry.  The first element of these conses is a string
containing authors, editors, title, year, type, and key of the
entry.  This is string is used for matching.  The second element
is the entry (only the fields listed above) as an alist."
  ;; Open configured bibliographies in temporary buffer:
  (with-temp-buffer 
    (mapc #'insert-file-contents
          (if (listp helm-bibtex-bibliography)
              helm-bibtex-bibliography
            (list helm-bibtex-bibliography)))
    ;; Check hash of bibliography and reparse if necessary:
    (let ((bibliography-hash (secure-hash 'sha256 (current-buffer))))
      (unless (and helm-bibtex-cached-candidates
                   (string= helm-bibtex-bibliography-hash bibliography-hash))
        (message "Loading bibliography ...")
        (let* ((entries (helm-bibtex-parse-bibliography))
               (entries (helm-bibtex-resolve-crossrefs entries))
               (entries (helm-bibtex-prepare-entries entries))
               (entries (nreverse entries)))
          (setq helm-bibtex-cached-candidates
                (--map (cons (helm-bibtex-clean-string
                              (s-join " " (-map #'cdr it))) it)
                       entries)))
        (setq helm-bibtex-bibliography-hash bibliography-hash))
      helm-bibtex-cached-candidates)))

(defun helm-bibtex-resolve-crossrefs (entries)
  "Expand all entries with fields from cross-references entries."
   (cl-loop
    with entry-hash = 
      (cl-loop
       with ht = (make-hash-table :test #'equal :size (length entries))
       for entry in entries
       for key = (helm-bibtex-get-value "=key=" entry)
       ;; Other types than proceedings and books can be
       ;; cross-referenced, but I suppose that isn't really used:
       if (member (downcase (helm-bibtex-get-value "=type=" entry))
                  '("proceedings" "book"))
       do (puthash (downcase key) entry ht)
       finally return ht)
    for entry in entries
    for crossref = (helm-bibtex-get-value "crossref" entry)
    if crossref
      collect (append entry (gethash (downcase crossref) entry-hash))
    else
      collect entry))

(defun helm-bibtex-parse-bibliography ()
  "Parse the BibTeX entries listed in the current buffer and
return a list of entry keys in the order in which the entries
appeared in the BibTeX files."
  (goto-char (point-min))
  (cl-loop
   for entry-type = (parsebib-find-next-item)
   while entry-type
   unless (member-ignore-case entry-type '("preamble" "string" "comment"))
   collect (-map (lambda (it)
                   (cons (downcase (car it)) (cdr it)))
                 (parsebib-read-entry entry-type))))

(defun helm-bibtex-get-entry (entry-key)
  "Given a BibTeX key this function scans all bibliographies
listed in `helm-bibtex-bibliography' and returns an alist of the
record with that key.  Fields from crossreferenced entries are
appended to the requested entry."
  (let* ((entry (helm-bibtex-get-entry1 entry-key))
         (crossref (helm-bibtex-get-value "crossref" entry))
         (crossref (when crossref (helm-bibtex-get-entry1 crossref))))
    (append entry crossref)))

(defun helm-bibtex-get-entry1 (entry-key)
  (with-temp-buffer
    (mapc #'insert-file-contents 
          (if (listp helm-bibtex-bibliography)
              helm-bibtex-bibliography
            (list helm-bibtex-bibliography)))
    (goto-char (point-min))
    (re-search-forward (concat "^@\\(" parsebib--bibtex-identifier
                               "\\)[[:space:]]*[\(\{][[:space:]]*"
                               (regexp-quote entry-key) "[[:space:]]*,"))
    (let ((entry-type (match-string 1)))
      (helm-bibtex-prepare-entry (parsebib-read-entry entry-type)))))

(defun helm-bibtex-prepare-entries (entries)
  "Do some preprocessing of the entries."
  (cl-loop
   with fields = (append '("title" "year" "crossref")
                         (--map (if (symbolp it) (symbol-name it) it)
                                helm-bibtex-additional-search-fields))
   for entry in entries
   collect (helm-bibtex-prepare-entry entry
            (cons (if (assoc "author" entry) "author" "editor") fields))))

(defun helm-bibtex-prepare-entry (entry &optional fields)
  "Prepare ENTRY for display.
ENTRY is an alist representing an entry as returned by
parsebib-read-entry. All the fields not in FIELDS are removed
from ENTRY, with the exception of the \"=type=\" and \"=key=\"
fields. If FIELDS is empty, all fields are kept. Also add a
=has-pdf= and/or =has-notes= field, if they exist for ENTRY."
  (when entry ; entry may be nil, in which case just return nil
    (when fields
      (setq fields (append fields (list "=type=" "=key="))))
    (let* ((entry (if fields
                       (--filter (member-ignore-case (car it) fields) entry)
                     entry))
           (entry-key (cdr (assoc "=key=" entry))))
      ;; Normalize case of entry type:
      (setcdr (assoc "=type=" entry) (downcase (cdr (assoc "=type=" entry))))
      ;; Check for PDF and notes:
      (if (and helm-bibtex-library-path
               (f-exists? (f-join helm-bibtex-library-path (s-concat entry-key ".pdf"))))
          (setq entry (cons (cons "=has-pdf=" helm-bibtex-pdf-symbol) entry)))
      (if (and helm-bibtex-notes-path
               (f-exists? (f-join helm-bibtex-notes-path
                                  (s-concat entry-key helm-bibtex-notes-extension))))
          (setq entry (cons (cons "=has-note=" helm-bibtex-notes-symbol) entry)))
      ;; Remove duplicated fields:
      (let ((-compare-fn (lambda (x y) (string= (car x) (car y)))))
        (-uniq entry)))))


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

(defun helm-bibtex-candidates-formatter (candidates source)
  "Formats BibTeX entries for display in results list."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (helm-bibtex-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
     for fields = '("author" "title" "year" "=has-pdf=" "=has-note=" "=type=")
   else
     for fields = '("editor" "title" "year" "=has-pdf=" "=has-note=" "=type=")
   for fields = (--map (helm-bibtex-clean-string
                        (helm-bibtex-get-value it entry " "))
                       fields)
   for fields = (-update-at 0 'helm-bibtex-shorten-authors fields)
   collect
   (cons (s-format "$0 $1 $2 $3$4 $5" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 36 (- width 53) 4 1 1 7)))
         entry-key)))


(defun helm-bibtex-clean-string (s)
  "Removes quoting and superfluous white space from BibTeX field
values."
  (if s (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\\\"{}]+" "" s))
    nil))

(defun helm-bibtex-shorten-authors (authors)
  "Returns a comma-separated list of the surnames in authors."
  (if authors
      (cl-loop for a in (s-split " and " authors)
               for p = (s-split "," a t)
               for sep = "" then ", "
               concat sep
               if (eq 1 (length p))
               concat (-last-item (s-split " +" (car p) t))
               else
               concat (car p))
    nil))

(defun helm-bibtex-open-pdf (_)
  "Open the PDF associated with the entry using the function
specified in `helm-bibtex-pdf-open-function',"
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (dolist (key keys)
      (let ((path (f-join helm-bibtex-library-path (s-concat key ".pdf"))))
        (if (f-exists? path)
            (funcall helm-bibtex-pdf-open-function path)
          (let ((path (f-join helm-bibtex-library-path (s-concat key ".txt"))))
            (if (f-exists? path)
                (funcall helm-bibtex-pdf-open-function path)
              (message "No PDF or TXT for this entry: %s" key))))))))

(defun helm-bibtex-open-url-or-doi (_)
  "Open the associated URL or DOI in a browser."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (dolist (key keys)
      (let* ((entry (helm-bibtex-get-entry key))
             (url (helm-bibtex-get-value "url" entry))
             (doi (helm-bibtex-get-value "doi" entry))
             (browse-url-browser-function
              (or helm-bibtex-browser-function
                  browse-url-browser-function)))
        (if url (helm-browse-url url)
          (if doi (helm-browse-url
                   (s-concat "http://dx.doi.org/" doi)))
          (message "No URL or DOI found for this entry: %s"
                   key))))))

(defun helm-bibtex-format-citation-default (keys)
  "Default formatter for keys, separates multiple keys with commas."
  (s-join ", " keys))

(defun helm-bibtex-format-citation-cite (keys)
  "Formatter for LaTeX citation macro."
  (format "\\cite{%s}" (s-join ", " keys)))

(defun helm-bibtex-format-citation-pandoc-citeproc (keys)
  "Formatter for pandoc-citeproc citations."
  (format "[%s]" (s-join "; " (--map (concat "@" it) keys))))

(defun helm-bibtex-format-citation-ebib (keys)
  "Formatter for ebib references."
  (s-join ", "
          (--map (format "ebib:%s" it) keys)))

(defun helm-bibtex-format-citation-org-link-to-PDF (keys)
  "Formatter for org-links to PDF."
  (s-join ", "
   (--map (format "[[%s][%s]]" (f-join helm-bibtex-library-path (s-concat it ".pdf")) it) keys)))

(defun helm-bibtex-insert-citation (_)
  "Insert citation at point.  The format depends on
`helm-bibtex-format-citation-functions'."
  (let ((keys (helm-marked-candidates :with-wildcard t))
        (format-function
         (cdr (or (assoc major-mode helm-bibtex-format-citation-functions)
                  (assoc 'default   helm-bibtex-format-citation-functions)))))
    (insert
     (funcall format-function keys))))

(defun helm-bibtex-insert-reference (_)
  "Insert a reference for each selected entry."
  (let* ((keys (helm-marked-candidates :with-wildcard t))
         (refs (--map
                (s-word-wrap fill-column
                             (concat "\n- " (helm-bibtex-apa-format-reference it)))
                keys)))
   (insert "\n" (s-join "\n" refs) "\n")))

(defun helm-bibtex-apa-format-reference (key)
  "Returns a plain text reference in APA format for the
publication specified by KEY."
  (let*
   ((entry (helm-bibtex-get-entry key))
    (ref (pcase (downcase (helm-bibtex-get-value "=type=" entry))
           ("article"
            (s-format
             "${author} (${year}). ${title}. ${journal}, ${volume}(${number}), ${pages}."
             'helm-bibtex-apa-get-value entry))
           ("inproceedings"
            (s-format
             "${author} (${year}). ${title}. In ${editor}, ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("book"
            (s-format
             "${author} (${year}). ${title}. ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("phdthesis"
            (s-format
             "${author} (${year}). ${title} (Doctoral dissertation). ${school}, ${address}."
             'helm-bibtex-apa-get-value entry))
           ("inbook"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("incollection"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("proceedings"
            (s-format
             "${editor} (Eds.). (${year}). ${booktitle}. ${address}: ${publisher}."
             'helm-bibtex-apa-get-value entry))
           ("unpublished"
            (s-format
             "${author} (${year}). ${title}. Unpublished manuscript."
             'helm-bibtex-apa-get-value entry))
           (_
            (s-format
             "${author} (${year}). ${title}."
             'helm-bibtex-apa-get-value entry)))))
    (replace-regexp-in-string "\\([.?!]\\)\\." "\\1" ref))) ; Avoid sequences of punctuation marks.

(defun helm-bibtex-apa-get-value (field entry &optional default)
  "Return FIELD or ENTRY formatted following the APA
guidelines.  Return DEFAULT if FIELD is not present in ENTRY."
  (let ((value (helm-bibtex-get-value field entry))
        (entry-type (helm-bibtex-get-value "=type=" entry)))
    (if value
       (pcase field
         ;; https://owl.english.purdue.edu/owl/resource/560/06/
         ("author" (helm-bibtex-apa-format-authors value))
         ("editor"
          (if (string= entry-type "proceedings")
              (helm-bibtex-apa-format-editors value)
            (helm-bibtex-apa-format-editors value)))
         ;; When referring to books, chapters, articles, or Web pages,
         ;; capitalize only the first letter of the first word of a
         ;; title and subtitle, the first word after a colon or a dash
         ;; in the title, and proper nouns. Do not capitalize the first
         ;; letter of the second word in a hyphenated compound word.
         ("title" (replace-regexp-in-string ; remove braces
                   "[{}]"
                   ""
                    (replace-regexp-in-string ; upcase initial letter
                    "^[[:alpha:]]"
                    'upcase
                    (replace-regexp-in-string ; preserve stuff in braces from being downcased
                     "\\(^[^{]*{\\)\\|\\(}[^{]*{\\)\\|\\(}.*$\\)\\|\\(^[^{}]*$\\)"
                     'downcase
                     value))))
         ("booktitle" value)
         ;; Maintain the punctuation and capitalization that is used by
         ;; the journal in its title.
         ("pages" (s-join "–" (s-split "[^0-9]+" value t)))
         (_ value))
      "")))

(defun helm-bibtex-apa-format-authors (value)
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "{" a)
             collect
             (replace-regexp-in-string "[{}]" "" a)
             into authors
           else if (s-index-of "," a)
             collect
             (let ((p (s-split " *, *" a t)))
               (concat
                (car p) ", "
                (s-join " " (--map (concat (s-left 1 it) ".")
                                   (s-split " " (cadr p))))))
             into authors
           else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (-last-item p) ", "
                (s-join " " (--map (concat (s-left 1 it) ".")
                                   (-butlast p)))))
             into authors
           finally return
             (let ((l (length authors)))
               (cond
                 ((= l 1) (car authors))
                 ((< l 8) (concat (s-join ", " (-butlast authors))
                                  ", & " (-last-item authors)))
                 (t (concat (s-join ", " authors) ", ..."))))))

(defun helm-bibtex-apa-format-editors (value)
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "," a)
             collect
             (let ((p (s-split " *, *" a t)))
               (concat
                (s-join " " (--map (concat (s-left 1 it) ".")
                                   (s-split " " (cadr p))))
                " " (car p)))
             into authors
           else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (s-join " " (--map (concat (s-left 1 it) ".")
                                   (-butlast p)))
                " " (-last-item p)))
             into authors
           finally return
             (let ((l (length authors)))
               (cond
                 ((= l 1) (car authors))
                 ((< l 8) (concat (s-join ", " (-butlast authors))
                                  ", & " (-last-item authors)))
                 (t (concat (s-join ", " authors) ", ..."))))))

(defun helm-bibtex-get-value (field entry &optional default)
  "Return the requested value or `default' if the value is not
defined.  Surrounding curly braces are stripped."
  (let ((value (cdr (assoc-string field entry 'case-fold))))
    (if value
        (s-collapse-whitespace
         (replace-regexp-in-string
          "\\(^[[:space:]]*[\"{][[:space:]]*\\)\\|\\([[:space:]]*[\"}][[:space:]]*$\\)"
          ""
          value))
      default)))

(defun helm-bibtex-insert-key (_)
  "Insert BibTeX key at point."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (insert
      (funcall 'helm-bibtex-format-citation-default keys))))

(defun helm-bibtex-insert-bibtex (_)
  "Insert BibTeX key at point."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (insert (s-join "\n" (--map (helm-bibtex-make-bibtex it) keys)))))

(defun helm-bibtex-make-bibtex (key)
  (let* ((entry (helm-bibtex-get-entry key))
         (entry-type (helm-bibtex-get-value "=type=" entry)))
    (format "@%s{%s,\n%s}\n"
            entry-type key
            (cl-loop
             for field in entry
             for name = (car field)
             for value = (cdr field)
             unless (member name
                            (append (--map (if (symbolp it) (symbol-name it) it)
                                           helm-bibtex-no-export-fields)
                             '("=type=" "=key=" "=has-pdf=" "=has-note=" "crossref")))
             concat
             (format "  %s = %s,\n" name value)))))

(defun helm-bibtex-add-PDF-attachment (_)
  "Attach the PDFs of the selected entries."
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (dolist (key keys)
      (let ((path (f-join helm-bibtex-library-path (s-concat key ".pdf"))))
        (if (f-exists? path)
            (mml-attach-file path)
          (message "No PDF for this entry: %s" key))))))

(defun helm-bibtex-edit-notes (key)
  "Open the notes associated with the entry using `find-file'."
  (if (f-directory? helm-bibtex-notes-path)
      ;; One notes file per publication: just open the file.
      (let ((path (f-join helm-bibtex-notes-path
                          (s-concat key helm-bibtex-notes-extension))))
        (find-file path))
    ;; One file for all notes: find the notes or create new section
    ;; from the template:
    (find-file helm-bibtex-notes-path)
    (goto-char (point-min))
    (if (or (search-forward (concat ":BIBTEX-KEY: " key) nil t) (search-forward (concat "* " key) nil t))
        (when (eq major-mode 'org-mode)
          (org-show-context)
          (recenter-top-bottom 1))
      (goto-char (point-max))
      (insert (s-format helm-bibtex-notes-template
                        'helm-bibtex-apa-get-value
                        (helm-bibtex-get-entry key))))))

(defun helm-bibtex-buffer-visiting (file)
  (or (get-file-buffer file)
      (find-buffer-visiting file)))

(defun helm-bibtex-show-entry (key)
  "Show the entry in the BibTeX file."
  (catch 'break
    (dolist (bibtex-file (if (listp helm-bibtex-bibliography)
                             helm-bibtex-bibliography
                           (list helm-bibtex-bibliography)))
      (let ((buf (helm-bibtex-buffer-visiting bibtex-file)))
        (find-file bibtex-file)
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^@\\(" parsebib--bibtex-identifier
                     "\\)[[:space:]]*[\(\{][[:space:]]*"
                     (regexp-quote key) "[[:space:]]*,") nil t)
            (throw 'break t)
          (unless buf
            (kill-buffer)))))))

(defun helm-bibtex-fallback-action (url-or-function)
  (let ((browse-url-browser-function
          (or helm-bibtex-browser-function
              browse-url-browser-function)))
    (cond 
      ((stringp url-or-function)
        (helm-browse-url (format url-or-function (url-hexify-string helm-pattern))))
      ((functionp url-or-function)
        (funcall url-or-function))
      (t (error "Don't know how to interpret this: %s" url-or-function)))))

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

(defun helm-bibtex-fallback-candidates ()
  "Compile list of fallback options.  These consist of the online
resources defined in `helm-bibtex-fallback-options' plus one
entry for each BibTeX file that will open that file for editing."
  (let ((bib-files (if (listp helm-bibtex-bibliography)
                       helm-bibtex-bibliography
                     (list helm-bibtex-bibliography))))
    (-concat 
      (--map (cons (s-concat "Create new entry in " (f-filename it))
                   `(lambda () (find-file ,it) (goto-char (point-max))))
             bib-files)
      helm-bibtex-fallback-options)))


(defvar helm-source-bibtex
  '((name                                      . "BibTeX entries")
    (init                                      . helm-bibtex-init)
    (candidates                                . helm-bibtex-candidates)
    (filtered-candidate-transformer            . helm-bibtex-candidates-formatter)
    (action . (("Open PDF file (if present)"   . helm-bibtex-open-pdf)
               ("Open URL or DOI in browser"   . helm-bibtex-open-url-or-doi)
               ("Insert citation"              . helm-bibtex-insert-citation)
               ("Insert reference"             . helm-bibtex-insert-reference)
               ("Insert BibTeX key"            . helm-bibtex-insert-key)
               ("Insert BibTeX entry"          . helm-bibtex-insert-bibtex)
               ("Attach PDF to email"          . helm-bibtex-add-PDF-attachment)
               ("Edit notes"                   . helm-bibtex-edit-notes)
               ("Show entry"                   . helm-bibtex-show-entry))))
  "Source for searching in BibTeX files.")

(defvar helm-source-fallback-options
  '((name            . "Fallback options")
    (match             (lambda (_candidate) t))
    (candidates      . helm-bibtex-fallback-candidates)
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
