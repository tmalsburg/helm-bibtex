;;; bibtex-completion.el --- A BibTeX backend for completion frameworks

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;;         Justin Burkett <justin@burkett.cc>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 1.0.0
;; Package-Requires: ((parsebib "1.0") (s "1.9.0") (dash "2.6.0") (f "0.16.2") (cl-lib "0.5"))

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

;; A BibTeX backend for completion frameworks

;; There are currently two fronends: helm-bibtex and ivy-bibtex.
;;
;; See the github page for details:
;;
;;    https://github.com/tmalsburg/helm-bibtex

;;; Code:

(require 'browse-url)
(require 'parsebib)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'biblio)

(defgroup bibtex-completion nil
  "Helm plugin for searching entries in a BibTeX bibliography."
  :group 'completion)

(defcustom bibtex-completion-bibliography nil
  "The BibTeX file or list of BibTeX files."
  :group 'bibtex-completion
  :type '(choice file (repeat file)))

(defcustom bibtex-completion-library-path nil
  "A directory or list of directories in which PDFs are stored.
Bibtex-completion assumes that the names of these PDFs are
composed of the BibTeX-key plus a \".pdf\" suffix."
  :group 'bibtex-completion
  :type '(choice directory (repeat directory)))

(defcustom bibtex-completion-pdf-open-function 'find-file
  "The function used for opening PDF files.  This can be an
arbitrary function that takes one argument: the path to the PDF
file.  The default is `find-file' which opens the PDF in
Emacs (either with docview or, if installed, the much superior
pdf-tools.  When set to `helm-open-file-with-default-tool', the
systems default viewer for PDFs is used."
  :group 'bibtex-completion
  :type 'function)

(defcustom bibtex-completion-pdf-symbol "⌘"
  "Symbol used to indicate that a PDF file is available for a
publication.  This should be a single character."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-ebib)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default))
  "The functions used for formatting citations.  The publication
can be cited, for example, as \cite{key} or ebib:key depending on
the major mode of the current buffer.  Note that the functions
should accept a list of keys as input.  With multiple marked
entries one can insert multiple keys at once,
e.g. \cite{key1,key2}. See the functions
`bibtex-completion-format-citation-ebib' and
`bibtex-completion-format-citation-cite' as examples."
  :group 'bibtex-completion
  :type '(alist :key-type symbol :value-type function))

(defcustom bibtex-completion-notes-path nil
  "The place where notes are stored.  This is either a file, in
which case all notes are stored in that file, or a directory, in
which case each publication gets its own notes file in that
directory.  In the latter case, bibtex-completion assumes that the
names of the note files are composed of the BibTeX-key plus a
suffix that is specified in `bibtex-completion-notes-extension'."
  :group 'bibtex-completion
  :type '(choice file directory))

(defcustom bibtex-completion-notes-template-multiple-files
  "#+TITLE: Notes on: ${author} (${year}): ${title}\n\n"
  "Template used to create a new note when each note is stored in
a separate file.  '${field-name}' can be used to insert the value
of a BibTeX field into the template."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-template-one-file
  "\n* ${author} (${year}): ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :END:\n\n"
  "Template used to create a new note when all notes are stored
in one file.  '${field-name}' can be used to insert the value of
a BibTeX field into the template."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-key-pattern
  ":Custom_ID: +%s\\( \\|$\\)"
  "The pattern used to find entries in the notes file.  Only
relevant when all notes are stored in one file.  The key can be
inserted into the pattern using the `format` function."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-extension ".org"
  "The extension of the files containing notes.  This is only
used when `bibtex-completion-notes-path' is a directory (not a file)."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-notes-symbol "✎"
  "Symbol used to indicate that a publication has notes.  This
should be a single character."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-fallback-options
  '(("CrossRef                                  (biblio.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-crossref-backend search-expression)))
    ("arXiv                                     (biblio.el)"
     . (lambda (search-expression) (biblio-lookup #'biblio-arxiv-backend search-expression)))
    ("DBLP (computer science bibliography)      (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-dblp-backend search-expression)))
    ("HAL (French open archive)                 (biblio.el)"
     . (lambda (search-expression) (biblio--lookup-1 #'biblio-hal-backend search-expression)))
    ("Google Scholar                            (web)"
     . "https://scholar.google.co.uk/scholar?q=%s")
    ("Pubmed                                    (web)"
     . "https://www.ncbi.nlm.nih.gov/pubmed/?term=%s")
    ("Bodleian Library                          (web)"
     . "http://solo.bodleian.ox.ac.uk/primo_library/libweb/action/search.do?vl(freeText0)=%s&fn=search&tab=all")
    ("Library of Congress                       (web)"
     . "https://www.loc.gov/search/?q=%s&all=true&st=list")
    ("Deutsche Nationalbibliothek               (web)"
     . "https://portal.dnb.de/opac.htm?query=%s")
    ("British National Library                  (web)"
     . "http://explore.bl.uk/primo_library/libweb/action/search.do?&vl(freeText0)=%s&fn=search")
    ("Bibliothèque nationale de France          (web)"
     . "http://catalogue.bnf.fr/servlet/RechercheEquation?host=catalogue?historique1=Recherche+par+mots+de+la+notice&niveau1=1&url1=/jsp/recherchemots_simple.jsp?host=catalogue&maxNiveau=1&categorieRecherche=RechercheMotsSimple&NomPageJSP=/jsp/recherchemots_simple.jsp?host=catalogue&RechercheMotsSimpleAsauvegarder=0&ecranRechercheMot=/jsp/recherchemots_simple.jsp&resultatsParPage=20&x=40&y=22&nbElementsHDJ=6&nbElementsRDJ=7&nbElementsRCL=12&FondsNumerise=M&CollectionHautdejardin=TVXZROM&HDJ_DAV=R&HDJ_D2=V&HDJ_D1=T&HDJ_D3=X&HDJ_D4=Z&HDJ_SRB=O&CollectionRezdejardin=UWY1SPQM&RDJ_DAV=S&RDJ_D2=W&RDJ_D1=U&RDJ_D3=Y&RDJ_D4=1&RDJ_SRB=P&RDJ_RLR=Q&RICHELIEU_AUTRE=ABCDEEGIKLJ&RCL_D1=A&RCL_D2=K&RCL_D3=D&RCL_D4=E&RCL_D5=E&RCL_D6=C&RCL_D7=B&RCL_D8=J&RCL_D9=G&RCL_D10=I&RCL_D11=L&ARSENAL=H&LivrePeriodique=IP&partitions=C&images_fixes=F&son=S&images_animees=N&Disquette_cederoms=E&multimedia=M&cartes_plans=D&manuscrits=BT&monnaies_medailles_objets=JO&salle_spectacle=V&Monographie_TN=M&Periodique_TN=S&Recueil_TN=R&CollectionEditorial_TN=C&Ensemble_TN=E&Spectacle_TN=A&NoticeB=%s")
    ("Gallica Bibliothèque Numérique            (web)"
     . "http://gallica.bnf.fr/Search?q=%s"))
  "Alist of online sources that can be used to search for
publications.  The key of each entry is the name of the online
source.  The value is the URL used for retrieving results.  This
URL must contain a %s in the position where the search term
should be inserted.  Alternatively, the value can be a function
that will be called when the entry is selected."
  :group 'bibtex-completion
  :type '(alist :key-type string
                :value-type (choice (string :tag "URL")
                            (function :tag "Function"))))

(defcustom bibtex-completion-browser-function nil
  "The browser that is used to access online resources.  If
nil (default), the value of `browse-url-browser-function' is
used.  If that value is nil, Helm uses the first available
browser in `helm-browse-url-default-browser-alist'"
  :group 'bibtex-completion
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

(defcustom bibtex-completion-additional-search-fields nil
  "The fields that are used for searching in addition to author,
editor, title, year, BibTeX key, and entry type."
  :group 'bibtex-completion
  :type 'list)

(defcustom bibtex-completion-no-export-fields nil
  "A list of fields that should be ignored when exporting BibTeX
entries."
  :group 'bibtex-completion
  :type 'list)

(defcustom bibtex-completion-cite-commands '("cite" "Cite" "parencite"
"Parencite" "footcite" "footcitetext" "textcite" "Textcite"
"smartcite" "Smartcite" "cite*" "parencite*" "supercite" "autocite"
"Autocite" "autocite*" "Autocite*" "citeauthor" "Citeauthor"
"citeauthor*" "Citeauthor*" "citetitle" "citetitle*" "citeyear"
"citeyear*" "citedate" "citedate*" "citeurl" "nocite" "fullcite"
"footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite"
"fnotecite")
  "The list of LaTeX cite commands.  When creating LaTeX
citations, these can be accessed as future entries in the
minibuffer history, i.e. by pressing the arrow down key.  The
default entries are taken from biblatex.  There is currently no
special support for multicite commands and volcite et al.  These
commands can be used but bibtex-completion does not prompt for their
extra arguments."
  :group 'bibtex-completion
  :type '(choice string (repeat string)))

(defcustom bibtex-completion-cite-default-command "cite"
  "The LaTeX cite command that is used if the user doesn't enter
anything when prompted for such a command."
  :group 'bibtex-completion
  :type 'string)

(defcustom bibtex-completion-cite-prompt-for-optional-arguments t
  "If t, bibtex-completion prompts for pre- and postnotes for
LaTeX cite commands.  Choose nil for no prompts."
  :group 'bibtex-completion
  :type 'boolean)

(defcustom bibtex-completion-cite-default-as-initial-input nil
  "This variable controls how the default command defined in
`bibtex-completion-cite-default-command' is used.  If t, it is inserted
into the minibuffer before reading input from the user.  If nil,
it is used as the default if the user doesn't enter anything."
  :group 'bibtex-completion
  :type 'boolean)

(defcustom bibtex-completion-pdf-field nil
  "The name of the BibTeX field in which the path to PDF files is
stored or nil if no such field should be used.  If an entry has
no value for this field, or if the specified file does not exist,
or if this variable is nil, bibtex-completion will look up the PDF in
the directories listed in `bibtex-completion-library-path'."
  :group 'bibtex-completion
  :type 'string)

(defvar bibtex-completion-cache nil
  "A cache storing the hash of the bibliography content and the corresponding list of entries, for each bibliography file, obtained when the bibliography was last parsed. When the current bibliography hash is identical to the cached hash, the cached list of candidates is reused, otherwise the bibliography file is reparsed.")


(defun bibtex-completion-init ()
  "Checks that the files and directories specified by the user
actually exist."
  (mapc (lambda (file)
          (unless (f-file? file)
                  (user-error "BibTeX file %s could not be found." file)))
        (-flatten (list bibtex-completion-bibliography))))

(defun bibtex-completion-clear-cache (&optional files)
  "Clears FILES from cache. If FILES is omitted, all files in `bibtex-completion-biblography' are cleared."
  (setq bibtex-completion-cache
        (cl-remove-if
         (lambda (x)
           (member (car x)
                   (or files
                       (-flatten (list bibtex-completion-bibliography)))))
         bibtex-completion-cache)))
    
(defun bibtex-completion-candidates ()
  "Reads the BibTeX files and returns a list of conses, one for
each entry.  The first element of these conses is a string
containing authors, editors, title, year, type, and key of the
entry.  This is string is used for matching.  The second element
is the entry (only the fields listed above) as an alist."
  (let ((files (nreverse (-flatten (list bibtex-completion-bibliography))))
        reparsed-files)
    ;; Open each bibliography file in a temporary buffer,
    ;; check hash of bibliography and reparse if necessary:
    (cl-loop
     for file in files
     do
     (with-temp-buffer
       (insert-file-contents file)
       (let ((bibliography-hash (secure-hash 'sha256 (current-buffer))))
         (unless (string= (cadr (assoc file bibtex-completion-cache))
                          bibliography-hash)
           (bibtex-completion-clear-cache (list file))
           (message "Parsing bibliography file %s ..." file)
           (push (-cons* file
                         bibliography-hash
                         (bibtex-completion-parse-bibliography))
                 bibtex-completion-cache)
           ;; Mark file as reparsed.
           ;; This will be useful to resolve cross-references:
           (push file reparsed-files)))))
    ;; If some files were reparsed, resolve cross-references:
    (when reparsed-files
      (message "Resolving cross-references ...")
      (bibtex-completion-resolve-crossrefs files reparsed-files))
    ;; Finally return the list of candidates:
    (nreverse
     (cl-loop
      for file in files
      append (cddr (assoc file bibtex-completion-cache))))))

(defun bibtex-completion-resolve-crossrefs (files reparsed-files)
  "Expand all entries with fields from cross-referenced entries in FILES, assuming that only those files in REPARSED-FILES were reparsed whereas the other files in FILES were up-to-date."
  (cl-loop
   with entry-hash = (bibtex-completion-make-entry-hash files reparsed-files)
   for file in files
   for entries = (cddr (assoc file bibtex-completion-cache))
   if (member file reparsed-files)
   ;; The file was reparsed.
   ;; Resolve crossrefs then make candidates for all entries:
   do (setf
       (cddr (assoc file bibtex-completion-cache))
       (cl-loop
        for entry in entries
        ;; Entries are alists of \(FIELD . VALUE\) pairs.
        for crossref = (bibtex-completion-get-value "crossref" entry)
        collect (bibtex-completion-make-candidate
                 (if crossref
                     (bibtex-completion-remove-duplicated-fields
                      ;; Insert an empty field so we can discard the crossref info if needed:
                      (append entry
                              (acons "" ""
                                     (gethash (downcase crossref) entry-hash))))
                   entry))))
   else
   ;; The file was not reparsed.
   ;; Resolve crossrefs then make candidates for the entries with a crossref field:
   do (setf
       (cddr (assoc file bibtex-completion-cache)) 
       (cl-loop
        for entry in entries
        ;; Entries are \(STRING . ALIST\) conses.
        for entry-alist = (cdr entry)
        for crossref = (bibtex-completion-get-value "crossref" entry-alist)
        collect (if crossref
                    (bibtex-completion-make-candidate
                     (bibtex-completion-remove-duplicated-fields
                      ;; Discard crossref info and resolve crossref again:
                      (append (--take-while (> (length (car it)) 0) entry-alist)
                              (acons "" ""
                                     (gethash (downcase crossref) entry-hash)))))
                  entry)))))

(defun bibtex-completion-make-entry-hash (files reparsed-files)
  "Return a hash table of all bibliography entries in FILES, assuming that only those files in REPARSED-FILES were reparsed whereas the other files in FILES were up-to-date."
  (cl-loop
   with entries =
     (cl-loop
      for file in files
      for entries = (cddr (assoc file bibtex-completion-cache))
      if (member file reparsed-files)
      ;; Entries are alists of \(FIELD . VALUE\) pairs.
      append entries
      ;; Entries are \(STRING . ALIST\) conses.
      else
      append (mapcar 'cdr entries))
   with ht = (make-hash-table :test #'equal :size (length entries))
   for entry in entries
   for key = (bibtex-completion-get-value "=key=" entry)
   ;; Other types than proceedings and books can be
   ;; cross-referenced, but I suppose that isn't really used:
   if (member (downcase (bibtex-completion-get-value "=type=" entry))
              '("proceedings" "book"))
   do (puthash (downcase key) entry ht)
   finally return ht))

(defun bibtex-completion-make-candidate (entry)
  "Return a candidate for ENTRY."
  (cons (bibtex-completion-clean-string
         (s-join " " (-map #'cdr entry)))
        entry))

(defun bibtex-completion-parse-bibliography ()
  "Parse the BibTeX entries listed in the current buffer and
return a list of entries in the order in which they appeared in the BibTeX file. Also do some preprocessing of the entries."
  (goto-char (point-min))
  (cl-loop
   with fields = (append '("title" "year" "crossref")
                         (-map (lambda (it) (if (symbolp it) (symbol-name it) it))
                               bibtex-completion-additional-search-fields))
   for entry-type = (parsebib-find-next-item)
   while entry-type
   unless (member-ignore-case entry-type '("preamble" "string" "comment"))
   collect (let* ((entry (parsebib-read-entry entry-type))
                  (fields (cons (if (assoc-string "author" entry 'case-fold)
                                    "author"
                                  "editor")
                                fields)))
             (-map (lambda (it)
                     (cons (downcase (car it)) (cdr it)))
                   (bibtex-completion-prepare-entry entry fields)))))

(defun bibtex-completion-get-entry (entry-key)
  "Given a BibTeX key this function scans all bibliographies
listed in `bibtex-completion-bibliography' and returns an alist of the
record with that key.  Fields from crossreferenced entries are
appended to the requested entry."
  (let* ((entry (bibtex-completion-get-entry1 entry-key))
         (crossref (bibtex-completion-get-value "crossref" entry))
         (crossref (when crossref (bibtex-completion-get-entry1 crossref))))
    (bibtex-completion-remove-duplicated-fields (append entry crossref))))

(defun bibtex-completion-get-entry1 (entry-key &optional do-not-find-pdf)
  (save-excursion
    (with-temp-buffer
      (mapc #'insert-file-contents
            (-flatten (list bibtex-completion-bibliography)))
      (goto-char (point-min))
      (re-search-forward (concat "^@\\(" parsebib--bibtex-identifier
                                "\\)[[:space:]]*[\(\{][[:space:]]*"
                                (regexp-quote entry-key) "[[:space:]]*,"))
      (let ((entry-type (match-string 1)))
        (reverse (bibtex-completion-prepare-entry (parsebib-read-entry entry-type) nil do-not-find-pdf))))))

(defun bibtex-completion-find-pdf-in-field (key-or-entry)
  "Returns the path of the PDF specified in the field
`bibtex-completion-pdf-field' if that file exists.  Returns nil if no
file is specified, or if the specified file does not exist, or if
`bibtex-completion-pdf-field' is nil."
  (when bibtex-completion-pdf-field
    (let* ((entry (if (stringp key-or-entry)
                      (bibtex-completion-get-entry1 key-or-entry t)
                    key-or-entry))
           (value (bibtex-completion-get-value bibtex-completion-pdf-field entry)))
      (cond
       ((not value) nil)         ; Field not defined.
       ((f-file? value) (list value))   ; A bare full path was found.
       (t                        ; Zotero/Mendeley/JabRef format:
        (let ((value (replace-regexp-in-string "\\([^\\]\\);" "\\1\^^" value)))
          (cl-loop  ; Looping over the files:
           for record in (s-split "\^^" value)
           ; Replace unescaped colons by field separator:
           for record = (replace-regexp-in-string "\\([^\\]\\|^\\):" "\\1\^_" record)
           ; Unescape stuff:
           for record = (replace-regexp-in-string "\\\\\\(.\\)" "\\1" record)
           ; Now we can safely split:
           for record = (s-split "\^_" record)
           for file-name = (nth 0 record)
           for path = (or (nth 1 record) "")
           for paths = (if (s-match "^[A-Z]:" path)
                           (list path)                 ; Absolute Windows path
                                                       ; Something else:
                         (append
                          (list
                           path
                           (f-join (f-root) path) ; Mendeley #105
                           (f-join (f-root) path file-name)) ; Mendeley #105
                          (--map (f-join it path)
                                 (-flatten bibtex-completion-library-path)) ; Jabref #100
                          (--map (f-join it path file-name)
                                 (-flatten bibtex-completion-library-path)))) ; Jabref #100
           for result = (-first 'f-exists? paths)
           if result collect result)))))))

(defun bibtex-completion-find-pdf-in-library (key-or-entry)
  "Searches the directories in `bibtex-completion-library-path' for a
PDF whose names is composed of the BibTeX key plus \".pdf\".  The
path of the first matching PDF is returned."
  (let* ((key (if (stringp key-or-entry)
                  key-or-entry
                (bibtex-completion-get-value "=key=" key-or-entry)))
         (path (-first 'f-file?
                       (--map (f-join it (s-concat key ".pdf"))
                              (-flatten (list bibtex-completion-library-path))))))
    (when path (list path))))

(defun bibtex-completion-find-pdf (key-or-entry)
  "Returns the path of the PDF associated with the specified
entry.  This is either the path specified in the field
`bibtex-completion-pdf-field' or, if that does not exist, the first PDF
in any of the directories in `bibtex-completion-library-path' whose
name is \"<bibtex-key>.pdf\".  Returns nil if no PDF is found."
  (or (bibtex-completion-find-pdf-in-field key-or-entry)
      (bibtex-completion-find-pdf-in-library key-or-entry)))

(defun bibtex-completion-prepare-entry (entry &optional fields do-not-find-pdf)
  "Prepare ENTRY for display.
ENTRY is an alist representing an entry as returned by
parsebib-read-entry. All the fields not in FIELDS are removed
from ENTRY, with the exception of the \"=type=\" and \"=key=\"
fields. If FIELDS is empty, all fields are kept. Also add a
=has-pdf= and/or =has-note= field, if they exist for ENTRY.  If
DO-NOT-FIND-PDF is non-nil, this function does not attempt to
find a PDF file."
  (when entry ; entry may be nil, in which case just return nil
    (let* ((fields (when fields (append fields (list "=type=" "=key=" "=has-pdf=" "=has-note="))))
           ; Check for PDF:
           (entry (if (and (not do-not-find-pdf) (bibtex-completion-find-pdf entry))
                      (cons (cons "=has-pdf=" bibtex-completion-pdf-symbol) entry)
                    entry))
           (entry-key (cdr (assoc "=key=" entry)))
           ; Check for notes:
           (entry (if (or
                       ;; One note file per entry:
                       (and bibtex-completion-notes-path
                            (f-directory? bibtex-completion-notes-path)
                            (f-file? (f-join bibtex-completion-notes-path
                                             (s-concat entry-key
                                                       bibtex-completion-notes-extension))))
                       ;; All notes in one file:
                       (and bibtex-completion-notes-path
                            (f-file? bibtex-completion-notes-path)
                            (with-current-buffer (find-file-noselect bibtex-completion-notes-path)
                              (widen)
                              (goto-char (point-min))
                              (re-search-forward (format bibtex-completion-notes-key-pattern entry-key) nil t))))
                      (cons (cons "=has-note=" bibtex-completion-notes-symbol) entry)
                    entry))
           ; Remove unwanted fields:
           (entry (if fields
                       (--filter (member-ignore-case (car it) fields) entry)
                     entry)))
      ;; Normalize case of entry type:
      (setcdr (assoc "=type=" entry) (downcase (cdr (assoc "=type=" entry))))
      ;; Remove duplicated fields:
      (bibtex-completion-remove-duplicated-fields entry))))

(defun bibtex-completion-remove-duplicated-fields (entry)
  "Remove duplicated fields from ENTRY."
  (cl-remove-duplicates entry
                        :test (lambda (x y) (string= (s-downcase x) (s-downcase y)))
                        :key 'car :from-end t))



(defun bibtex-completion-format-entry (entry width)
  "Formats a BibTeX entry for display in results list."
  (let* ((fields (list (if (assoc-string "author" entry 'case-fold) "author" "editor")
                       "title" "year" "=has-pdf=" "=has-note=" "=type="))
         (fields (-map (lambda (it)
                         (bibtex-completion-clean-string
                          (bibtex-completion-get-value it entry " ")))
                       fields))
         (fields (-update-at 0 'bibtex-completion-shorten-authors fields)))
    (s-format "$0 $1 $2 $3$4 $5" 'elt
              (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                         fields (list 36 (- width 53) 4 1 1 7)))))


(defun bibtex-completion-clean-string (s)
  "Removes quoting and superfluous white space from BibTeX field
values."
  (if s (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\"{}]+" "" s))
    nil))

(defun bibtex-completion-shorten-authors (authors)
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


(defun bibtex-completion-open-pdf (keys)
  "Open the PDFs associated with the marked entries using the
function specified in `bibtex-completion-pdf-open-function'.  All paths
in `bibtex-completion-library-path' are searched.  If there are several
matching PDFs for an entry, the first is opened."
  (--if-let
      (-flatten
       (-map 'bibtex-completion-find-pdf keys))
      (-each it bibtex-completion-pdf-open-function)
    (message "No PDF(s) found.")))

(defun bibtex-completion-open-url-or-doi (keys)
  "Open the associated URL or DOI in a browser."
  (dolist (key keys)
    (let* ((entry (bibtex-completion-get-entry key))
           (url (bibtex-completion-get-value "url" entry))
           (doi (bibtex-completion-get-value "doi" entry))
           (browse-url-browser-function
            (or bibtex-completion-browser-function
                browse-url-browser-function)))
      (if url
          (browse-url url)
        (if doi (browse-url
                 (s-concat "http://dx.doi.org/" doi))
          (message "No URL or DOI found for this entry: %s"
                   key))))))

(defun bibtex-completion-open-any (keys)
  "Open the PDFs associated with the marked entries using the
function specified in `bibtex-completion-pdf-open-function'.  If no PDF is
found, try to open a URL or DOI in the browser instead."
  (dolist (key keys)
    (let ((pdf (bibtex-completion-find-pdf key)))
      (if pdf
          (funcall bibtex-completion-pdf-open-function (car pdf))
        (bibtex-completion-open-url-or-doi (list key))))))

(defun bibtex-completion-format-citation-default (keys)
  "Default formatter for keys, separates multiple keys with commas."
  (s-join ", " keys))

(defvar bibtex-completion-cite-command-history nil
  "History list for LaTeX citation commands.")

(defun bibtex-completion-format-citation-cite (keys)
  "Formatter for LaTeX citation commands. Prompts for the command and
for arguments if the commands can take any. If point is inside or just after a citation command, only adds KEYS to it."
  (let (macro)
    (cond
     ((and (require 'reftex-parse nil t)
           (setq macro (reftex-what-macro 1))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are inside a cite macro. Insert key at point, with appropriate delimiters.
      (delete-horizontal-space)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              (if (member (following-char) '(?\} ?,))
		     ""
                ", ")))
     ((and (equal (preceding-char) ?\})
           (require 'reftex-parse nil t)
           (save-excursion
             (forward-char -1)
             (setq macro (reftex-what-macro 1)))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are right after a cite macro. Append key and leave point at the end.
      (delete-char -1)
      (delete-horizontal-space t)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              "}"))
     (t
      ;; We are not inside or right after a cite macro. Insert a full citation.
      (let* ((initial (when bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default (unless bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default-info (if default (format " (default \"%s\")" default) ""))
             (cite-command (completing-read
                            (format "Cite command%s: " default-info)
                            bibtex-completion-cite-commands nil nil initial
                            'bibtex-completion-cite-command-history default nil)))
        (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
            (format "\\%s{%s}" cite-command (s-join ", " keys))
          (let ((prenote (if bibtex-completion-cite-prompt-for-optional-arguments
                             (read-from-minibuffer "Prenote: ")
                           ""))
                (postnote (if bibtex-completion-cite-prompt-for-optional-arguments
                              (read-from-minibuffer "Postnote: ")
                            "")))
            (if (and (string= "" prenote) (string= "" postnote))
                (format "\\%s{%s}" cite-command (s-join ", " keys))
              (format "\\%s[%s][%s]{%s}" cite-command prenote postnote (s-join ", " keys))))))))))

(defun bibtex-completion-format-citation-pandoc-citeproc (keys)
  "Formatter for pandoc-citeproc citations."
  (let* ((prenote  (if bibtex-completion-cite-prompt-for-optional-arguments (read-from-minibuffer "Prenote: ") ""))
         (postnote (if bibtex-completion-cite-prompt-for-optional-arguments (read-from-minibuffer "Postnote: ") ""))
         (prenote  (if (string= "" prenote)  "" (concat prenote  " ")))
         (postnote (if (string= "" postnote) "" (concat ", " postnote))))
    (format "[%s%s%s]" prenote (s-join "; " (--map (concat "@" it) keys)) postnote)))

(defun bibtex-completion-format-citation-ebib (keys)
  "Formatter for ebib references."
  (s-join ", "
          (--map (format "ebib:%s" it) keys)))

(defun bibtex-completion-format-citation-org-link-to-PDF (keys)
  "Formatter for org-links to PDF.  Uses first matching PDF if
several are available.  Entries for which no PDF is available are
omitted."
  (s-join ", " (cl-loop
                for key in keys
                for pdfs = (bibtex-completion-find-pdf key)
                append (--map (format "[[%s][%s]]" it key) pdfs))))

(defun bibtex-completion-insert-citation (keys)
  "Insert citation at point.  The format depends on
`bibtex-completion-format-citation-functions'."
  (let ((format-function
         (cdr (or (assoc major-mode bibtex-completion-format-citation-functions)
                  (assoc 'default   bibtex-completion-format-citation-functions)))))
    (insert
     (funcall format-function keys))))

(defun bibtex-completion-insert-reference (keys)
  "Insert a reference for each selected entry."
  (let* ((refs (--map
                (s-word-wrap fill-column
                             (concat "\n- " (bibtex-completion-apa-format-reference it)))
                keys)))
    (insert "\n" (s-join "\n" refs) "\n")))

(defun bibtex-completion-apa-format-reference (key)
  "Returns a plain text reference in APA format for the
publication specified by KEY."
  (let*
   ((entry (bibtex-completion-get-entry key))
    (ref (pcase (downcase (bibtex-completion-get-value "=type=" entry))
           ("article"
            (s-format
             "${author} (${year}). ${title}. ${journal}, ${volume}(${number}), ${pages}.${doi}"
             'bibtex-completion-apa-get-value entry))
           ("inproceedings"
            (s-format
             "${author} (${year}). ${title}. In ${editor}, ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("book"
            (s-format
             "${author} (${year}). ${title}. ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("phdthesis"
            (s-format
             "${author} (${year}). ${title} (Doctoral dissertation). ${school}, ${address}."
             'bibtex-completion-apa-get-value entry))
           ("inbook"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("incollection"
            (s-format
             "${author} (${year}). ${title}. In ${editor} (Eds.), ${booktitle} (pp. ${pages}). ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("proceedings"
            (s-format
             "${editor} (Eds.). (${year}). ${booktitle}. ${address}: ${publisher}."
             'bibtex-completion-apa-get-value entry))
           ("unpublished"
            (s-format
             "${author} (${year}). ${title}. Unpublished manuscript."
             'bibtex-completion-apa-get-value entry))
           (_
            (s-format
             "${author} (${year}). ${title}."
             'bibtex-completion-apa-get-value entry)))))
   (replace-regexp-in-string "\\([.?!]\\)\\." "\\1" ref))) ; Avoid sequences of punctuation marks.

(defun bibtex-completion-apa-get-value (field entry &optional default)
  "Return FIELD or ENTRY formatted following the APA
guidelines.  Return DEFAULT if FIELD is not present in ENTRY."
  (let ((value (bibtex-completion-get-value field entry))
        (entry-type (bibtex-completion-get-value "=type=" entry)))
    (if value
       (pcase field
         ;; https://owl.english.purdue.edu/owl/resource/560/06/
         ("author" (bibtex-completion-apa-format-authors value))
         ("editor"
          (if (string= entry-type "proceedings")
              (bibtex-completion-apa-format-editors value)
            (bibtex-completion-apa-format-editors value)))
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
                     (lambda (x) (downcase (s-replace "\\" "\\\\" x)))
                     value))))
         ("booktitle" value)
         ;; Maintain the punctuation and capitalization that is used by
         ;; the journal in its title.
         ("pages" (s-join "–" (s-split "[^0-9]+" value t)))
         ("doi" (s-concat " http://dx.doi.org/" value))
         (_ value))
      "")))

(defun bibtex-completion-apa-format-authors (value)
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
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (s-split " " (cadr p))))))
             into authors
           else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (-last-item p) ", "
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (-butlast p)))))
             into authors
           finally return
             (let ((l (length authors)))
               (cond
                 ((= l 1) (car authors))
                 ((< l 8) (concat (s-join ", " (-butlast authors))
                                  ", & " (-last-item authors)))
                 (t (concat (s-join ", " authors) ", ..."))))))

(defun bibtex-completion-apa-format-editors (value)
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "," a)
             collect
             (let ((p (s-split " *, *" a t)))
               (concat
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
                                  (s-split " " (cadr p))))
                " " (car p)))
             into authors
           else
             collect
             (let ((p (s-split " " a t)))
               (concat
                (s-join " " (-map (lambda (it) (concat (s-left 1 it) "."))
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

(defun bibtex-completion-get-value (field entry &optional default)
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

(defun bibtex-completion-insert-key (keys)
  "Insert BibTeX key at point."
  (insert
   (funcall 'bibtex-completion-format-citation-default keys)))

(defun bibtex-completion-insert-bibtex (keys)
  "Insert BibTeX key at point."
  (insert (s-join "\n" (--map (bibtex-completion-make-bibtex it) keys))))

(defun bibtex-completion-make-bibtex (key)
  (let* ((entry (bibtex-completion-get-entry key))
         (entry-type (bibtex-completion-get-value "=type=" entry)))
    (format "@%s{%s,\n%s}\n"
            entry-type key
            (cl-loop
             for field in entry
             for name = (car field)
             for value = (cdr field)
             unless (member name
                            (append (-map (lambda (it) (if (symbolp it) (symbol-name it) it))
                                          bibtex-completion-no-export-fields)
                             '("=type=" "=key=" "=has-pdf=" "=has-note=" "crossref")))
             concat
             (format "  %s = %s,\n" name value)))))

(defun bibtex-completion-add-PDF-attachment (keys)
  "Attach the PDFs of the selected entries where available."
  (--if-let
      (-flatten
       (-map 'bibtex-completion-find-pdf keys))
      (-each it 'mml-attach-file)
    (message "No PDF(s) found.")))

(define-minor-mode bibtex-completion-notes-mode
  "Minor mode for managing notes."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'bibtex-completion-exit-notes-buffer)
            (define-key map (kbd "C-c C-w") 'org-refile)
            map)
  (setq-local
   header-line-format
   (substitute-command-keys
    " Finish \\[bibtex-completion-exit-notes-buffer], refile \\[org-refile]")))

;; Define global minor mode. This is needed to the toggle minor mode.
(define-globalized-minor-mode bibtex-completion-notes-global-mode bibtex-completion-notes-mode bibtex-completion-notes-mode)

(defun bibtex-completion-exit-notes-buffer ()
  "Exit notes buffer and delete its window.
This will also disable `bibtex-completion-notes-mode' and remove the header
line."
  (interactive)
  (widen)
  (bibtex-completion-notes-global-mode -1)
  (setq-local
   header-line-format nil)
  (save-buffer)
  (let ((window (get-buffer-window (file-name-nondirectory bibtex-completion-notes-path))))
    (if (and window (not (one-window-p window)))
        (delete-window window)
      (switch-to-buffer (other-buffer)))))

(defun bibtex-completion-edit-notes (keys)
  "Open the notes associated with the selected entries using `find-file'."
  (dolist (key keys)
    (if (and bibtex-completion-notes-path
             (f-directory? bibtex-completion-notes-path))
                                        ; One notes file per publication:
        (let* ((path (f-join bibtex-completion-notes-path
                             (s-concat key bibtex-completion-notes-extension))))
          (find-file path)
          (unless (f-exists? path)
            (insert (s-format bibtex-completion-notes-template-multiple-files
                              'bibtex-completion-apa-get-value
                              (bibtex-completion-get-entry key)))))
                                        ; One file for all notes:
      (unless (and buffer-file-name
                   (f-same? bibtex-completion-notes-path buffer-file-name))
        (find-file-other-window bibtex-completion-notes-path))
      (widen)
      (show-all)
      (goto-char (point-min))
      (if (re-search-forward (format bibtex-completion-notes-key-pattern key) nil t)
                                        ; Existing entry found:
          (when (eq major-mode 'org-mode)
            (org-narrow-to-subtree)
            (re-search-backward "^\*+ " nil t)
            (org-cycle-hide-drawers nil)
            (bibtex-completion-notes-mode 1))
                                        ; Create a new entry:
        (let ((entry (bibtex-completion-get-entry key)))
          (goto-char (point-max))
          (insert (s-format bibtex-completion-notes-template-one-file
                            'bibtex-completion-apa-get-value
                            entry)))
        (when (eq major-mode 'org-mode)
          (org-narrow-to-subtree)
          (re-search-backward "^\*+ " nil t)
          (org-cycle-hide-drawers nil)
          (goto-char (point-max))
          (bibtex-completion-notes-mode 1))))))

(defun bibtex-completion-buffer-visiting (file)
  (or (get-file-buffer file)
      (find-buffer-visiting file)))

(defun bibtex-completion-show-entry (keys)
  "Show the first selected entry in the BibTeX file."
  (catch 'break
    (dolist (bibtex-file (-flatten (list bibtex-completion-bibliography)))
      (let ((key (car keys))
            (buf (bibtex-completion-buffer-visiting bibtex-file)))
        (find-file bibtex-file)
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^@\\(" parsebib--bibtex-identifier
                     "\\)[[:space:]]*[\(\{][[:space:]]*"
                     (regexp-quote key) "[[:space:]]*,") nil t)
            (throw 'break t)
          (unless buf
            (kill-buffer)))))))

(defun bibtex-completion-add-pdf-to-library (keys)
  "Add a PDF to the library for the first selected entry. The PDF can be added either from an open buffer or a file."
  (let* ((key (car keys))
         (source (char-to-string
                  (read-char-choice "Add pdf from [b]uffer or [f]ile? " '(?b ?f))))
         (buffer (when (string= source "b")
                   (read-buffer-to-switch "Add pdf buffer: ")))
         (file (when (string= source "f")
                 (expand-file-name (read-file-name "Add pdf file: " nil nil t))))
         (path (-flatten (list bibtex-completion-library-path)))
         (path (if (cdr path)
                   (completing-read "Add pdf to: " path nil t)
                 (car path)))
         (pdf (expand-file-name (concat key ".pdf") path)))
    (cond
     (buffer
      (with-current-buffer buffer
        (write-file pdf)))
     (file
      (copy-file file pdf)))))

(defun bibtex-completion-fallback-action (url-or-function search-expression)
  (let ((browse-url-browser-function
          (or bibtex-completion-browser-function
              browse-url-browser-function)))
    (cond
      ((stringp url-or-function)
        (browse-url (format url-or-function (url-hexify-string search-expression))))
      ((functionp url-or-function)
        (funcall url-or-function search-expression))
      (t (error "Don't know how to interpret this: %s" url-or-function)))))

(defun bibtex-completion-fallback-candidates ()
  "Compile list of fallback options.  These consist of the online
resources defined in `bibtex-completion-fallback-options' plus one
entry for each BibTeX file that will open that file for editing."
  (let ((bib-files (-flatten (list bibtex-completion-bibliography))))
    (-concat
      (--map (cons (s-concat "Create new entry in " (f-filename it))
                   `(lambda (_search-expression) (find-file ,it) (goto-char (point-max)) (newline)))
             bib-files)
      bibtex-completion-fallback-options)))

(defun bibtex-completion-find-local-bibliography ()
  "Return a list of BibTeX files associated with the current file. If the current file is a BibTeX file, return this file. Otherwise, try to use `reftex' to find the associated BibTeX files. If this fails, return `bibtex-completion-bibliography'."
  (or (and (buffer-file-name)
           (string= (or (f-ext (buffer-file-name)) "") "bib")
           (list (buffer-file-name)))
      (and (buffer-file-name)
           (require 'reftex-parse nil t)
           (reftex-locate-bibliography-files
            (if (fboundp 'TeX-master-directory)
                (TeX-master-directory)
              (file-name-directory (buffer-file-name)))))
      bibtex-completion-bibliography))

(provide 'bibtex-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bibtex-completion.el ends here
