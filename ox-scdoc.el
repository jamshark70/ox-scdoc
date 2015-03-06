

(org-export-define-derived-backend 'scdoc 'ascii
  :translate-alist
  '(
    ; (element . function)
    (bold . org-scdoc-bold)
    (code . org-scdoc-code)
    (example-block . org-scdoc-example-block)
    (export-block . org-scdoc-export-block)
    (export-snippet . org-scdoc-export-snippet)
    (fixed-width . org-scdoc-fixed-width)
    ; (footnote-reference . org-ascii-footnote-reference)  ??
    (headline . org-scdoc-headline)
    (inline-src-block . org-scdoc-inline-src-block)
    (italic . org-scdoc-italic)
    (item . org-scdoc-item)
    ;(latex-environment . org-ascii-latex-environment)
    ;(latex-fragment . org-ascii-latex-fragment)
    (link . org-scdoc-link)
    (plain-list . org-scdoc-plain-list)
    ;(plain-text . org-ascii-plain-text)
    (section . org-scdoc-section)
    (special-block . org-scdoc-special-block)
    (src-block . org-scdoc-src-block)
    (table . org-scdoc-table)
    (table-cell . org-scdoc-table-cell)
    (table-row . org-scdoc-table-row)
    (template . org-scdoc-template)  ;; IMPORTANT!! Main doc!
    (verbatim . org-scdoc-verbatim))
  :export-block "SCDOC"
  :options-alist
  '((:categories "CATEGORIES" nil nil t)
    (:related "RELATED" nil nil t)
    (:summary "SUMMARY" nil nil t)
    (:redirect "REDIRECT" nil nil t))

)


(defgroup org-export-scdoc nil
  "Options for exporting Org mode files to SCDoc."
  :tag "Org Export SCDoc"
  :group 'org-export)

(defcustom org-scdoc-text-markup-alist '((bold . "strong::%s::")
					 (code . "code::%s::")
					 (italic . "emphasis::%s::")
					 (strike-through . "%s")
					 (underline . "%s")
					 (verbatim . "code::%s::"))
  "Alist of SCDoc expressions to convert text markup."
  :group 'org-export-scdoc
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))

(defun org-scdoc--text-markup (text markup info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel.  See
`org-scdoc-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-scdoc-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ;; Else use format string.
     (t (format fmt text)))))


; Template
(defun org-scdoc-template (contents info)
  "Return complete document string after SCDoc conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (plist-get info :title))
	(categories (plist-get info :categories))
	(related (plist-get info :related))
	(summary (plist-get info :summary))
	(redirect (plist-get info :redirect))
	(output (format "TITLE:: %s\n" title)))
    (if categories (setq output (concat output (format "CATEGORIES:: %s\n" categories))))
    (if related (setq output (concat output (format "RELATED:: %s\n" related))))
    (if summary (setq output (concat output (format "SUMMARY:: %s\n" summary))))
    (if redirect (setq output (concat output (format "REDIRECT:: %s\n" redirect))))
    (concat output "\n\n" contents)))
    

; Translators


(defun org-scdoc-bold (bold contents info)
  "Transcode BOLD from Org to Scdoc.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-scdoc--text-markup contents 'bold info))

(defun org-scdoc-code (code contents info)
  "Transcode a CODE object from Org to Scdoc.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-scdoc--text-markup (org-element-property :value code) 'code info))

(defun org-scdoc-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Scdoc.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (org-string-nw-p (org-element-property :value example-block))
    (org-scdoc--wrap-label
     example-block
     (format "teletype::\n%s\n::"
	     (org-export-format-code-default example-block info)))))

(defun org-scdoc-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to SCDoc.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (string= (org-element-property :type export-block) "SCDOC")
    (org-element-property :value export-block)))

(defun org-scdoc-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Scdoc.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'scdoc)
    (org-element-property :value export-snippet)))

(defun org-scdoc-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Scdoc.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "teletype::\n%s\n::"
	  (org-remove-indentation
	   (org-element-property :value fixed-width))))


;;;; HEADLINE
;;;;; Support funcs
(defun org-scdoc-headline-is-method-section-p (headline info)
  "Is this headline either CLASSMETHODS or INSTANCEMETHODS?"
  (let* ((title (org-export-data (org-element-property :title headline) info))
	 (allcaps (upcase title)))
    (member allcaps '("CLASSMETHODS" "INSTANCEMETHODS"))))

(defun org-scdoc-headline-is-method-p (headline info)
  "Is this headline a METHOD tag?"
  (let ((tags (org-export-get-tags headline info)))
    (or (member "METHOD" (mapcar 'upcase tags))
	(let ((parent (org-export-get-parent headline)))
	  (and parent (org-scdoc-headline-is-method-section-p parent info))))))

(defun org-scdoc-headline-is-argument-p (headline info)
  "Is HEADLINE (an element) an argument name or a special headline?"
  (let* ((title (org-export-data (org-element-property :title headline) info))
	 (allcaps (upcase title))
	 (tags (org-export-get-tags headline info)))
    (or (member "ARG" (mapcar 'upcase tags))
	(and (not (member allcaps '("RETURNS" "DISCUSSION")))
	     (let ((parent (org-export-get-parent headline)))
	       (and parent (org-scdoc-headline-is-method-p parent info)))))))

;;;;; Headline func
(defun org-scdoc-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ASCII.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((title (org-export-data (org-element-property :title headline) info))
	   (allcaps (upcase title))
	   (levelnum (org-export-get-relative-level element info))
	   ;(tags (org-export-get-tags element info))
	   (parent (org-export-get-parent headline))
	   (output 
	    (cond
	     ((and (= levelnum 1)
		   (member allcaps '("DESCRIPTION" "CLASSMETHODS" "INSTANCEMETHODS" "EXAMPLES")))
	      (concat allcaps "::\n"))
	     ((= levelnum 1) (format "SECTION:: %s\n" title))
	     ((= levelnum 2)
	      (if (and parent (org-scdoc-headline-is-method-section-p parent info))
		  (format "METHOD:: %s\n" title)
		(format "SUBSECTION:: %s\n" title)))
	     ((= levelnum 3)
	      (cond
	       ((org-scdoc-headline-is-argument-p headline info)
		(format "ARGUMENT:: %s\n" title))
	       ; if it isn't an arg and parent is a method,
	       ; it must be RETURNS or DISCUSSION
	       ((and parent (org-scdoc-headline-is-method-p parent info))
		(format "%s::\n" (upcase title)))
	       ((and parent (org-scdoc-headline-is-method-section-p parent info))
		(format "SUBSECTION:: %s\n" title))
	       (t (format "STRONG::%s::\n\n" title))))
	     (t
	      (cond
	       ((org-scdoc-headline-is-argument-p headline info)
		(format "ARGUMENT:: %s\n" title))
	       ((and parent (org-scdoc-headline-is-method-p parent info))
		(format "%s::\n" (upcase title)))
	       (t (format "STRONG::%s::\n\n" title)))))))
      (concat output contents))))


(defun org-scdoc-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Scdoc.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((code (org-element-property :value inline-src-block)))
    (format "code::%s::" code)))

(defun org-scdoc-italic (italic contents info)
  "Transcode ITALIC from Org to Scdoc.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-scdoc--text-markup contents 'italic info))

(defun org-scdoc-item (item contents info)
  "Transcode an ITEM element from Org to Scdoc.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((tag (org-element-property :tag item)))
  (if tag
      (format "## %s || %s\n" tag contents)
      (format "## %s\n" contents))))

; TODO: org-scdoc-link
(defun org-scdoc-link (thing contents info))

(defun org-scdoc-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Scdoc.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (attr (org-export-read-attribute :attr_latex plain-list))
	 (scdoc-type (let ((env (plist-get attr :environment)))
		       (cond (env (format "%s" env))
			     ((eq type 'ordered) "numberedlist")
			     ((eq type 'descriptive) "definitionlist")
			     (t "list")))))
    (format "%s::\n%s::"
	    scdoc-type
	    contents)))

(defun org-scdoc-section (section contents info)
  "Transcode a SECTION element from Org to Scdoc.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

(defun org-scdoc-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Scdoc.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((type (org-element-property :type special-block))
	 (tag (cond
	       ((string= type "boxnote") "note")
	       (t type))))
    (format "%s::\n%s\n::" tag contents)))

(defun org-scdoc-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Scdoc.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "code::\n%s::" (org-export-format-code-default src-block info)))

(defun org-scdoc-table (table contents info)
  "Transcode a TABLE element from Org to Scdoc.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (format "table::\n%s\n::" contents))

(defun org-scdoc-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Scdoc.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat
   contents
   (when (org-export-get-next-element table-cell info) " || ")))

(defun org-scdoc-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Scdoc.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  (format "## %s\n" contents))

(defun org-scdoc-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Scdoc.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "code::%s::" (org-element-property :value verbatim)))


;;; End-user functions

;;;###autoload
(defun org-scdoc-export-as-scdoc
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Scdoc buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org SCDOC Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'scdoc "*Org SCDOC Export*"
    async subtreep visible-only body-only ext-plist (lambda ())))

;;;###autoload
(defun org-scdoc-convert-region-to-scdoc ()
  "Assume the current region has org-mode syntax, and convert it to Scdoc.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an Scdoc buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'scdoc))

;;;###autoload
(defun org-scdoc-export-to-scdoc
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Scdoc file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'scdoc outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-scdoc-publish-to-scdoc (plist filename pub-dir)
  "Publish an Org file to Scdoc.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'scdoc filename ".tex" plist pub-dir))


(provide 'ox-scdoc)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-scdoc.el ends here
