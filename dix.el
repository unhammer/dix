;;; dix.el --- minor mode for editing Apertium XML dictionary files

;; Copyright (C) 2009-2014 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.3
;; Url: http://wiki.apertium.org/wiki/Emacs
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;; (add-to-list 'load-path "/path/to/dix.el-folder")
;; (autoload 'dix-mode "dix"
;;   "dix-mode is a minor mode for editing Apertium XML dictionary files."  t)
;; (add-hook 'nxml-mode-hook
;; 	  (lambda () (and buffer-file-name
;; 			  (string-match "^modes\\.xml$\\|\\.\\(dix\\|metadix\\|t[0-9s]x\\|lrx\\)$"
;;                                      buffer-file-name)
;; 			  (dix-mode 1))))

;; Useful functions:
;; `C-c L' creates an LR-restricted copy of the <e>-element at point,
;; `C-c R' an RL-restricted one. `C-TAB' cycles through the
;; restriction possibilities (LR, RL, none), while `M-n' and `M-p'
;; move to the next and previous "important bits" of <e>-elements
;; (just try it!). `C-c S' sorts a pardef, while `C-c G' moves point
;; to the pardef of the entry at point, leaving mark where you left
;; from. Inside a pardef, `C-c A' shows all usages of that pardef
;; within the dictionaries represented by the string `dix-dixfiles',
;; while `C-c D' gives you a list of all pardefs which use these
;; suffixes (where a suffix is the contents of an <l>-element).

;; `M-x dix-suffix-sort' is a general function, useful outside of dix
;; XML files too, that just reverses each line, sorts them, and
;; reverses them back. `C-c % %' is a convenience function for
;; regexp-replacing text within certain XML elements, eg. all <e>
;; elements; `C-c % r' and `C-c % l' are specifically for <r> and <l>
;; elements, respectively.

;; I like having the following set too:
;; (setq nxml-sexp-element-flag t 		; treat <e>...</e> as a sexp
;;       nxml-completion-hook '(rng-complete t) ; C-RET completes based on DTD
;;       rng-nxml-auto-validate-flag nil)       ; 8MB of XML takes a while
;; You can always turn on validation again with C-c C-v. Validation
;; is necessary for the C-RET completion, which is really handy in
;; transfer files.

;; I haven't bothered with defining a real indentation function, but
;; if you like having all <i> elements aligned at eg. column 25, the
;; align rules defined here let you do M-x align on a region to
;; achieve that, and also aligns <p> and <r>. Set your favorite
;; column numbers with M-x customize-group RET dix.

;; Plan / long term todo:
;; - Switch default keybindings so they follow Emacs Key Bindings
;;   conventions
;; - Yank into <i/l/r> or pardef n="" should replace spaces with either
;;   a <b/> or a _
;; - Functions shouldn't modify the kill-ring.
;; - Functions should be agnostic to formatting (ie. only use nxml
;;   movement functions, never forward-line).
;; - Real indentation function for one-entry-one-line format.
;; - `dix-LR-restriction-copy' should work on a region of <e>'s.
;; - `dix-expand-lemma-at-point' (either using `dix-goto-pardef' or
;;   `lt-expand')
;; - Some sort of interactive view of the translation process. When
;;   looking at a word in monodix, you should easily get confirmation on
;;   whether (and what) it is in the bidix or other monodix (possibly
;;   just using `apertium-transfer' and `lt-proc' on the expanded
;;   paradigm).
;; - Function for creating a prelimenary list of bidix entries from
;;   monodix entries, and preferably from two such lists which
;;   we "paste" side-by-side.
;; - `dix-LR-restriction-copy' (and the other copy functions) could
;;   add a="author"
;; - `dix-dixfiles' could auto-add files from Makefile?
;; - `dix-sort-e-by-r' doesn't work if there's an <re> element after
;;   the <r>; and doesn't sort correctly by <l>-element, possibly to
;;   do with spaces
;; - `dix-reverse' should be able to reverse on a regexp match, so
;;   that we can do `dix-suffix-sort' by eg. <l>-elements.

;;; Code:

(defconst dix-version "0.1.4")

(require 'nxml-mode)
(require 'cl-lib)
(require 'easymenu)

;;;============================================================================
;;;
;;; Define the formal stuff for a minor mode named dix.
;;;

(defvar dix-mode-map (make-sparse-keymap)
  "Keymap for dix minor mode.")

(defgroup dix nil
  "Minor mode for editing Apertium XML dictionaries."
  :tag "Apertium dix"
  :group 'nxml)

;;;###autoload
(define-minor-mode dix-mode
  "Toggle dix-mode.
With arg, turn on dix-mode if and only if arg is positive.

dix-mode is a minor mode for editing Apertium XML dictionary files.

                             KEY BINDINGS
                             ------------
\\{dix-mode-map}

Entering dix-mode calls the hook dix-mode-hook.
------------------------------------------------------------------------------"
  :init-value nil
  :lighter    " dix"
  :keymap     dix-mode-map
  :require    nxml-mode

  (dix-maybe-yas-activate))


;;;============================================================================
;;;
;;; Menu
;;;

(easy-menu-define dix-mode-easy-menu dix-mode-map "dix-mode menu"
  '("dix"
    ["View pardef" dix-view-pardef
     :help "View the pardef in another window"]
    ["Go to pardef" dix-goto-pardef]
    ("Guess pardef of the word on this line..."
     :help "Write a single word on a line, place point somewhere inside the word, and this will guess the pardef using the above entries."
     ["with no PoS restriction" dix-guess-pardef
      :help "Write a single word on a line, place point somewhere inside the word, and this will guess the pardef using the above entries."]
     ["looking only at __n-pardefs" dix-guess-pardef__n
      :help "Write a single word on a line, place point somewhere inside the word, and this will guess the pardef using the above __n-entries."]
     ["looking only at __vblex_adj-pardefs" dix-guess-pardef__vblex_adj
      :help "Write a single word on a line, place point somewhere inside the word, and this will guess the pardef using the above __vblex_adj-entries."])
    "---"
    ["Sort pardef" dix-sort-pardef
     :help "Must be called from within a pardef"]
    ["Grep for this pardef in dix-dixfiles" dix-grep-all
     :help "Must be called from within a pardef. Uses the variable dix-dixfiles"]
    ["Show Duplicate pardefs" dix-find-duplicate-pardefs
     :help "Must be called from within a pardef. Calculate must have been called at least once"]
    ["Calculate and Show Duplicate pardefs" (dix-find-duplicate-pardefs 'recompile)
     :keys "C-u C-c D"
     :help "Must be called from within a pardef. Slower, but must be called at least once before showing duplicate pardefs"]
    "---"
    ["Narrow Buffer to Given sdef" dix-narrow-to-sdef
     :help "Show only that part of the buffer which contains a given sdef, eg. work only on nouns for a while. Widen with `C-x n w' as per usual."]
    "---"
    ["Change Restriction of <e> (LR, RL, none)" dix-restriction-cycle]
    ["Swap sense translation of this <e> with above <e>" dix-sense-swap
     :help "Use with slr/srl entries to swap the translations of two <e>'s -- <r> if slr, <l> if srl."]
    ["Go to Next Useful Position in the Buffer" dix-next]
    ["Go to Previous Useful Position in the Buffer" dix-previous]
    ("Replace Regexp Within..."
     ["Certain Elements" dix-replace-regexp-within-elt
      :help "Prompts for an element name"]
     ["<l> Elements" dix-replace-regexp-within-l]
     ["<r> Elements" dix-replace-regexp-within-r])
    ("Copy <e> and..."
     ["Keep Contents" dix-copy
      :help "Make a copy of the current <e> element"]
     ["Apply an LR Restriction" dix-LR-restriction-copy
      :help "Make a copy of the current <e> element"]
     ["Apply an RL Restriction" dix-RL-restriction-copy
      :help "Make a copy of the current <e> element"]
     ["Increase slr sense index" dix-slr-copy
      :help "Make a copy of the current <e> element"]
     ["Increase srl sense index" dix-srl-copy
      :help "Make a copy of the current <e> element"]
     ["Clear Contents" (dix-copy 'remove-lex)
      :keys "C-u C-c C"
      :help "Make a copy of the current <e> element"]
     ["Prepend kill-buffer into lm and <i>" dix-copy-yank
      :help "Make a copy of the current <e> element"])
    ["Turn one-word-per-line into XML using above <e> as template" dix-xmlise-using-above-elt
     :help "Write one word (or colon-separated word-pair) per line, then use the above <e> as a template to turn them into XML"]
    ["I-search Within lm's (rather buggy)" dix-word-search-forward]
    "---"
    ["Go to transfer rule number" dix-goto-rule-number]
    "---"
    ["Customize dix-mode" (customize-group 'dix)]
    ["Help for dix-mode" (describe-function 'dix-mode)
     :keys "C-h m"]
    ["Show dix-mode Version" (message "dix-mode version %s" dix-version)]))

;;;============================================================================
;;;
;;; Helpers
;;;

(defmacro dix-with-sexp (&rest body)
  "Execute `BODY' with `nxml-sexp-element-flag' set to true."
  (declare (indent 1) (debug t))
  `(let ((old-sexp-element-flag nxml-sexp-element-flag))
     (setq nxml-sexp-element-flag t)
     (let ((ret ,@body))
       (setq nxml-sexp-element-flag old-sexp-element-flag)
       ret)))
(defmacro dix-with-no-case-fold (&rest body)
  "Execute `BODY' with `case-fold-search' set to nil."
  (declare (indent 1) (debug t))
  `(let ((old-case-fold-search case-fold-search))
     (setq case-fold-search nil)
     ,@body
     (setq case-fold-search old-case-fold-search)))

(defvar dix-parse-bound 10000
  "Max amount of chars (not lines) to parse through in dix xml operations.
Useful since dix tend to get huge. Relative bound. Decrease the
number if operations ending in \"No parent element\" take too
long.")

(put 'dix-bound-error 'error-conditions '(error dix-parse-error dix-bound-error))
(put 'dix-bound-error 'error-message "Hit `dix-parse-bound' when parsing")
(put 'dix-barrier-error 'error-conditions '(error dix-parse-error dix-barrier-error))
(put 'dix-barrier-error 'error-message "Hit barrier when parsing")

(defun dix-backward-up-element (&optional arg bound)
  "Modified from `nxml-backward-up-element' to include optional argument BOUND.
Optional argument ARG says how many elements to move."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-up-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point-min) (point)))
	  (let ((token-end (nxml-token-before)))
	    (goto-char (cond ((or (memq xmltok-type '(start-tag
						      partial-start-tag))
				  (and (memq xmltok-type
					     '(empty-element
					       partial-empty-element))
				       (< (point) token-end)))
			      xmltok-start)
			     ((nxml-scan-element-backward
			       (if (and (eq xmltok-type 'end-tag)
					(= (point) token-end))
				   token-end
				 xmltok-start)
			       t
			       bound)
			      xmltok-start)
			     (t (signal 'dix-bound-error "No parent element")))))
	  (setq arg (1- arg)))
      (nxml-scan-error
       (goto-char (cadr err))
       (apply 'error (cddr err))))))

(defun dix-up-to (eltname &optional barrier)
  "Move point to start of element `ELTNAME' (a string, eg. \"e\")
which we're looking at. Optional `BARRIER' is the outer element,
so we don't go all the way through the file looking for our
element (ultimately constrained by the variable
`dix-parse-bound').  Ideally `dix-backward-up-element' should
stop on finding another `ELTNAME' element."
  (nxml-token-after)
  (when (eq xmltok-type 'space)
    (goto-char (1+ (nxml-token-after)))
    (nxml-token-after))
  (goto-char xmltok-start)
  (let ((tok (xmltok-start-tag-qname))
	(bound (max (point-min)
		    (- (point) dix-parse-bound))))
    (while (not (or (equal tok eltname)
		    (equal tok barrier)
		    (equal tok (concat "<" eltname))))
      (dix-backward-up-element 1 bound)
      (nxml-token-after)
      (setq tok (xmltok-start-tag-qname)))
    (if (equal tok barrier)
	(signal 'dix-barrier-error (format "Didn't find %s" eltname)))))

(defun dix-enclosing-is-mono-section ()
  "Heuristically answer if the element we're inside is (monolingual) <section>.
A `dix-enclosing-elt' from outside an <e> in a <section> will
often hit `dix-parse-bound', in which case we just search back
for some hints."
  (let ((elt (dix-enclosing-elt 'noerror)))
    (or (and elt (equal elt "section"))
	(save-excursion
	  (and (re-search-backward " lm=\"\\|<pardef\\|</section>" nil 'noerror)
	       (equal " lm=\"" (match-string 0)))))))

(defun dix-enclosing-elt-helper (bound)
  (dix-backward-up-element 1 bound)
  (nxml-token-after)
  (xmltok-start-tag-qname))

(defun dix-enclosing-elt (&optional noerror)
  "Return name of element we're in.
Optional argument NOERROR will make parse bound errors return
nil."
  (let ((bound (max (point-min)
		    (- (point) dix-parse-bound))))
    (save-excursion
      (if noerror
	  (condition-case nil
	      (dix-enclosing-elt-helper bound)
	    (dix-bound-error nil))
	(dix-enclosing-elt-helper bound)))))


(defun dix-pardef-at-point (&optional clean)
  "Give the name of the pardef we're in.
Optional argument CLEAN removes trailing __n and such."
  (save-excursion
    (dix-up-to "pardef" "pardefs")
    (re-search-forward "n=\"" nil t)
    (let ((pardef (symbol-name (symbol-at-point))))
      (if clean (replace-regexp-in-string
		 "\\([^/_]*\\)/?\\([^/_]*\\)__.*"
		 "\\1\\2"
		 pardef)
	pardef))))

(defun dix-lemma-at-point ()
  (if (string-match "-[^.]+\\.[^.]+$" (buffer-file-name))
      (dix-l/r-word-at-point) ;; bidix
    (save-excursion   ;; monodix
      (dix-up-to "e" "section")
      (re-search-forward "lm=\"\\([^\"]*\\)" nil t)
      (match-string-no-properties 1))))

(defun dix-i-at-point ()
  ;; TODO less roundabout
  (let ((rs (dix-split-root-suffix)))
    (concat (car rs) (cdr rs))))

(defun dix-par-at-point ()
  (save-excursion
    (dix-up-to "e" "section")
    (re-search-forward "<par[^/>]*n=\"\\([^\"]*\\)" nil t)
    (match-string-no-properties 1)))


(defun dix-pardef-suggest-at-point ()
  "Return a list of pardef names for suggestions.

First we look in the context around point (up to
`dix-parse-bound' in both directions), then append the full list
from <pardefs>. Tries to be fast, so no actual XML parsing,
meaning commented out pardefs may be suggested as well."
  (save-restriction
    (widen)
    (let* ((par-rex "<par [^>]*n=['\"]\\([^'\"> ]+\\)")
	   (pardef-rex "<pardef [^>]*n=['\"]\\([^'\"> ]+\\)")
	   (pardefs-end (or (save-excursion
			      (re-search-backward "</pardefs>" nil 'noerror))
			    (point-min)))
	   (bound-above (max pardefs-end
			     (- (point) dix-parse-bound)))
	   (bound-below (min (+ (point) dix-parse-bound)
			     (point-max)))
	   pdnames)
      (save-excursion
	(while (re-search-backward par-rex bound-above 'noerror)
	  (add-to-list 'pdnames (match-string-no-properties 1))))
      (save-excursion
	(while (re-search-forward par-rex bound-below 'noerror)
	  (add-to-list 'pdnames (match-string-no-properties 1))))
      (save-excursion
	(goto-char pardefs-end)
	(while (re-search-backward pardef-rex nil 'noerror)
	  (add-to-list 'pdnames (match-string-no-properties 1))))
      (nreverse pdnames))))

(defun dix-pardef-suggest-for (lemma)
  "Return a list of pardef names to suggest for `LEMMA'.

Names used near point are prioritised, and names marked for
lemma-suffixes that don't match the suffix of the lemma (e.g.
pardef \"foo/er__verb\" when the lemma is \"fooable\") are
filtered out."
  (cl-remove-if-not (lambda (par)
			  (if (string-match "/\\([^_]+\\)_" par)
			      (string-match (concat (match-string 1 par) "$") lemma)
			    'no-slash-so-match-all))
       (dix-pardef-suggest-at-point)))

(defun dix-pardef-type-of-e ()
  (let ((par (dix-par-at-point)))
    (when (string-match "[^_]*__\\([^\"]*\\)" par)
      (match-string-no-properties 1 par))))

(defun dix-split-root-suffix ()
  "Return a pair of the contents of <i> and the letters following
the slash of the pardef. Does not give the correct root of it's
not all contained within an <i> (eg. lemma pardefs will give
wrong roots)."
  (save-excursion
    (dix-up-to "e" "section")
    (let ((e-end (nxml-scan-element-forward (point))))
      (nxml-down-element 2)
      (cons (symbol-name (dix-with-sexp (sexp-at-point)))
	    (progn
	      (nxml-up-element)
	      (when (re-search-forward "n=\"[^/]*/\\([^_\"]*\\)[^\"]*\"" e-end 'noerror)
		(match-string-no-properties 1)))))))

(defun dix-get-attrib (attributes name)
  "Find attribute with attribute name `NAME' (a string) in the
list `ATTRIBUTES' of the same format as
`xmltok-attributes'. Return nil if no such attribute is found."
  (if attributes
      (if (equal name (buffer-substring-no-properties
		       (xmltok-attribute-name-start (car attributes))
		       (xmltok-attribute-name-end (car attributes))))
	  (car attributes)
	(dix-get-attrib (cdr attributes) name))))

(defun dix-attrib-start (attributes name)
  "Return start position of attribute by `NAME' only if it exists.
`ATTRIBUTES' is of the format of `xmltok-attributes'."
  (let ((attrib (dix-get-attrib attributes name)))
    (when attrib (xmltok-attribute-value-start attrib))))

(defvar dix-interesting
  '(;; dix:
    ("clip" "pos" "side" "part")
    ("e" "lm" "slr" "srl" "r" "c")
    ("par" "n")
    ("section" "id" "type")
    ("pardef" "n")
    ("s" "n")
    ;; transfer:
    ("sdef" "n")
    ("b" "pos")
    ("with-param" "pos")
    ("call-macro" "n")
    ("def-macro" "n" "npar")
    ("cat-item" "lemma" "tags" "name")
    ("attr-item" "lemma" "tags")
    ("list-item" "v")
    ("list" "n")
    ("def-attr" "n")
    ("def-cat" "n")
    ("def-list" "n")
    ("def-var" "n")
    ("pattern-item" "n")
    ("chunk" "name" "case" "namefrom")
    ("var" "n")
    ("lit" "v")
    ("lit-tag" "v")
    ;; modes:
    ("pipeline")
    ("mode" "name" "install")
    ("program" "name")
    ("file" "name")
    ;; lrx:
    ("match" "lemma" "tags")
    ("select" "lemma" "tags")
    ;; tsx:
    ("def-label" "name" "closed")
    ("def-mult" "name")
    ("tags-item" "tags" "lemma")
    ("label-item" "label")
    ("tagger" "name"))
  "Association list of elements and which attributes are considered interesting.
Used by `dix-next'.")

(defvar dix-skip-empty
  '("dictionary" "alphabet" "sdefs" "pardefs" "lu" "p" "e" "tags" "chunk" "tag" "pattern" "rule" "action" "out" "b" "def-macro" "choose" "when" "test" "equal" "not" "otherwise" "let" "forbid" "label-sequence" "tagset")
  "Skip past these elements when using `dix-next'.
They'll not be skipped if they have interesting attributes as defined by
`dix-interesting', however.")
;;; TODO: skip <[lr]><g><b/> and go to nearest CDATA in e.g. <l><g><b/>for</g></l>

(defun dix-nearest (pivot backward &rest args)
  "(dix-nearest 3 nil 1 2 3 4 5 6 7) => 4
   (dix-nearest 3 nil 1 2 3) => nil
   (dix-nearest 3 t 1 2 3) => 2
   (dix-nearest 3 t 1 2 4) => 2"
  (let ((cmp (if backward '< '>))
	(nearest (if backward 'max 'min)))
    (let ((OK (remove nil
		      (mapcar (lambda (x)
				(when (and x (funcall cmp x pivot)) x))
			      args))))
      (when OK (apply nearest OK)))))

(defun dix-nearest-interesting (attributes pivot backward interest)
  "Find the position of the nearest member of list `interest'
which is also a member of `attributes' (in the format of
`xmltok-attributes') but not crossing `pivot'."
  (apply 'dix-nearest pivot backward
	 (mapcar (lambda (attname)
		   (dix-attrib-start attributes attname))
		 interest)))

(defun dix-next-one (&optional backward)
  "Move forward one interesting element.
Helper for `dix-next' (move back if BACKWARD non-nil).
TODO: handle pardef entries too; make non-recursive."

  (defun move (spot)
    (if (if backward (< spot (point)) (> spot (point)))
	(goto-char spot)
      (progn (forward-char (if backward -1 1))
	     (dix-next-one backward))))

  (let* ((token-end (nxml-token-before))
	 (token-next (if backward
			 xmltok-start
		       (1+ token-end)))
	 (qname (xmltok-start-tag-qname))
	 (interest (cdr (assoc qname dix-interesting)))
	 (near-int (dix-nearest-interesting xmltok-attributes
					    (point)
					    backward
					    interest)))
    (cond ((eq (point) (if backward (point-min) (point-max)))
	   t)

	  ((memq xmltok-type '(prolog comment))
	   (goto-char token-next)
	   (dix-next-one backward))

	  (near-int			; interesting attribute
	   (move near-int))		; to go to

	  ((or interest	; interesting element but no next interesting attribute
	       (member qname dix-skip-empty)) ; skip if empty
	   (move token-next)
	   (dix-next-one backward))

	  ((memq xmltok-type '(space data end-tag))
	   (and (goto-char token-next)
		(not (and backward ; need to goto these elts from data
			  (nxml-token-before) ; before looping on:
			  (member (xmltok-start-tag-qname) '("r" "l" "i"))))
		(dix-next-one backward)))

	  ;; TODO: should instead while-loop until the next member of
	  ;; dix-interesting, or maybe the default should be to go to
	  ;; the next _attribute_, whatever it is?
 	  (t (move token-end)))))


(defun dix-compile-suffix-map (partype)
  "Build a hash map where keys are sorted lists of suffixes in
pardefs, eg. '(\"en\" \"ing\" \"s\"), and the value is a list of
the pardef names containing these suffixes.

Argument `partype' is eg. adj, vblex, vblex_adj, ..., and is the
string following \"__\", thus assumes you keep to the Apertium
standard. Also assumes there is no \"_\" before \"__\" in pardef
names."
  (let ((suffmap (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      ;; find all pardefs of `partype' in the file:
      (while (re-search-forward
	      (concat "pardef[^n>]*n=\"\\([^\"]*__" partype "\\)\"") nil 'noerror)
	(let ((pardef (match-string-no-properties 1))
	      (sufflist (dix-compile-sorted-suffix-list)))
	  (puthash sufflist
		   (cons pardef (gethash sufflist suffmap)) suffmap))))
    suffmap))

(defvar dix-suffix-maps nil
  "Internal association list used to store compiled suffix maps;
keys are symbols formed from the string `partype' (see
`dix-compile-suffix-map' and interactive function
`dix-find-duplicate-pardefs').")
(make-variable-buffer-local 'dix-suffix-maps)

(defun dix-get-pardefs (sufflist suffmap)
  "Get the list of pardefs in `suffmap' which have the list of
suffixes `sufflist'. See `dix-compile-suffix-map' for more
information."
  (gethash (sort sufflist 'string-lessp) suffmap))

(defun dix-compile-sorted-suffix-list ()
  "Used for generating lookup keys for `dix-compile-suffix-map'
and `dix-get-pardefs'."
  (save-excursion
    (let (sufflist)
      (condition-case nil
	  (progn (dix-up-to "pardef" "pardefs"))
	(dix-parse-error (dix-goto-pardef)))
      ;; find all suffixes within this pardef:
      (let ((end (save-excursion (dix-with-sexp (forward-sexp))
				 (point))))
	(while (re-search-forward "<l>\\([^<]*\\)</l>" end 'noerror)
	  (when (match-string 1)
	    (setq sufflist (cons (match-string-no-properties 1) sufflist)))))
      (sort sufflist 'string-lessp))))

(defun assoc-delete-all (key alist)
  (if alist
      (if (equal (caar alist) key)
	  (assoc-delete-all key (cdr alist))
	(cons (car alist)
	      (assoc-delete-all key (cdr alist))))))



;;;============================================================================
;;;
;;; Yasnippet helpers
;;;


;;; Note, due to
;;; https://github.com/AndreaCrotti/yasnippet-snippets/issues/41 you
;;; should do (remhash 'nxml-mode yas--tables) after (yas-reload-all)

(defun dix-maybe-yas-activate ()
  (when (fboundp 'yas-activate-extra-mode)
    (yas-activate-extra-mode 'dix-mode)))

;;; In case yasnippet is lazy-loaded after dix-mode, ensure dix-mode
;;; snippets are used:
(eval-after-load 'yasnippet
  '(mapc (lambda (buf)
	   (when dix-mode (dix-maybe-yas-activate)))
	 (buffer-list)))



(defvar-local dix-yas-key-rex ""
  "Used by `dix-yas-update-key-rex' for caching the regex-opt of
  possible snippet keys.")
(defvar-local dix-yas-key-rex-tables nil
  "Used by `dix-yas-update-key-rex' for checking if we need to
  update `dix-yas-key-rex'.")

(defun dix-yas-update-key-rex ()
  "Update `dix-yas-key-rex', used by `dix-yas-skip-backwards-to-key'."
  (let ((tables (yas--get-snippet-tables)))
    (unless (equal tables dix-yas-key-rex-tables)
      ;; Only update key-rex if tables changed (but is this equal test slow?):
      (setq dix-yas-key-rex-tables tables)
      (setq dix-yas-key-rex
	    (let (keys) (mapc
			 (lambda (table)
			   (let* ((keyhash (yas--table-hash table)))
			     (when keyhash
			       (maphash (lambda (k v) (push k keys)) keyhash))))
			 dix-yas-key-rex-tables)
		 (concat (regexp-opt keys) "$"))))))

(defun dix-yas-skip-backwards-to-key (start-point)
  "Skip backwards to the first possible yasnippet key.

This is meant to be used in `yas-key-syntaxes', since the
defaults don't let you expand e.g. \"<s>\" without having
whitespace before it. To use this function, put the following in
your init file:

    (eval-after-load 'yasnippet
      '(add-to-list 'yas-key-syntaxes 'dix-yas-skip-backwards-to-key))

Only has an effect in `dix-mode' so the above shouldn't change
how yasnippet expansion works in other modes."
  (when dix-mode
    (dix-yas-update-key-rex)
    (let* ((linebeg (save-excursion (goto-char start-point)
                                    (line-beginning-position)))
           (haystack (buffer-substring-no-properties linebeg start-point)))
      (when (string-match dix-yas-key-rex haystack)
	(goto-char (+ linebeg (match-beginning 0)))))))



(defun dix-yas-prev-lemma ()
  (dix-yas-prev-thing "lemma" 'dix-lemma-at-point))

(defun dix-yas-prev-par ()
  (dix-yas-prev-thing "lemma__POS" 'dix-par-at-point))

(defun dix-yas-prev-thing (default thing-at-point-fn)
  (condition-case nil
      (save-excursion
	(dix-up-to "e" "section")
	(condition-case nil
	    (progn
	      (dix-with-sexp (backward-sexp))
	      (let ((thing (funcall thing-at-point-fn)))
		(if (string-match "^$\\|^\\$" thing)
		    default
		  thing)))
	  (error default)))
    (dix-parse-error default)))

(defun dix-yas-message-pardef (pdname)
  "Just show the full pardef of `PDNAME' at point in *Messages* buffer."
  (save-excursion
    (save-restriction
      (widen)
      (dix-goto-pardef pdname)
      (let* ((beg (point))
	     (end (1+ (nxml-scan-element-forward beg))))
	(message (buffer-substring-no-properties beg end)))))
  pdname)

(defun dix-yas-pdname-to-pos (string)
  (if (numberp (string-match "__\\([^_]+\\)$" string))
      (format "<s n=\"%s\"/>" (match-string 1 string))
    ""))

(defun dix-yas-pdname-to-suffix (string)
  (if (numberp (string-match "/\\([^_]+\\)" string))
      (match-string-no-properties 1 string)
    ""))

(defun dix-yas-fix-suffix-w/pdname ()
  (let* ((suffix (dix-yas-pdname-to-suffix yas-text))
	 (suffix/i-rex (concat (regexp-quote suffix) "</i>")))
    (when (re-search-backward suffix/i-rex (line-beginning-position) 'noerror)
      (replace-match "</i>" 'fixedcase 'literal))))

(defun dix-yas-choose-pdname ()
  (when yas-moving-away-p
    (dix-yas-fix-suffix-w/pdname))
  (dix-yas-message-pardef
   (yas-choose-value
    (dix-pardef-suggest-for (dix-lemma-at-point)))))

(defun dix-yas-lm-to-i ()
  (replace-regexp-in-string " " "<b/>" yas-text))

;;;============================================================================
;;;
;;; Interactive functions
;;;

(defun dix-find-duplicate-pardefs (&optional recompile)
  "Find all pardefs with this list of suffixes (contents of <l>
elements); if there are several of them they might be
duplicates. Optional prefix argument `recompile' forces a
re-check of all pardefs.

Uses internal function `dix-compile-suffix-map' which assumes
that pardefs are named according to the regular Apertium scheme,
eg. \"lik/e__vblex\" (ie. all pardefs of the same group have
\"__\" before the group name, and there are no \"_\" before
\"__\").

Returns the list of pardef names."
  (interactive "P")
  (let* ((partype
	  (save-excursion
	    (condition-case nil
		(progn (dix-up-to "pardef" "pardefs"))
	      (dix-parse-error (dix-goto-pardef)))
	    (re-search-forward
	     (concat "pardef[^n>]*n=\"[^\"]*__\\([^\"]*\\)" ) nil 'noerror)
	    (match-string-no-properties 1)))
	 (foundmap (cdr (assoc-string partype dix-suffix-maps))))
    (let* ((suffmap
	    (if (or recompile (not foundmap))
		(dix-compile-suffix-map partype)
	      foundmap))
	   (pardefs (dix-get-pardefs (dix-compile-sorted-suffix-list)
				     suffmap)))
      (when (or recompile (not foundmap))
	(setq dix-suffix-maps (assoc-delete-all partype dix-suffix-maps))
	(add-to-list 'dix-suffix-maps (cons partype suffmap) 'append))
      (message (prin1-to-string pardefs))
      pardefs)))

(defvar dix-vr-langs nil "List of language codes (strings) allowed in the vr attribute of this dictionary.")
(defvar dix-vl-langs nil "List of language codes (strings) allowed in the vl attribute of this dictionary.")
(put 'dix-vr-langs 'safe-local-variable 'listp)
(put 'dix-vl-langs 'safe-local-variable 'listp)

(defun dix-v-cycle ()
  "Cycle through possible values of the `vr' or `vl' attributes of the <e>
element at point.

Doesn't yet deal with elements that specify both vr and vl.

For this to be useful, put something like this at the end of your file:

<!--
Local Variables:
dix-vr-langs: (\"nno\" \"nob\")
End:
-->
"
  (interactive)
  (save-excursion
    (dix-up-to "e" "pardef")
    (let* ((def-dir (if dix-vr-langs "r" "l"))
           (langs (list (cons "r" dix-vr-langs)
                        (cons "l" dix-vr-langs)))
           (old		     ; find what, if any, restriction we have:
	    (save-excursion
	      (if (re-search-forward " v\\([rl]\\)=\"\\([^\"]+\\)\"" (nxml-token-after) 'noerror 1)
		  (cons (match-string 1) (match-string 2)))))
           (dir (if old (car old) def-dir))
           (old-lang (when old (cdr old)))
           (dir-langs (cdr (assoc dir langs)))
           (next (car-safe (if old-lang
                               (cdr (member old-lang dir-langs))
                             dir-langs)))
           (new (if next
                    (format " v%s=\"%s\"" dir next)
                  "")))
      ;; restrict:
      (forward-word)
      (if old (delete-region (match-beginning 0)
			     (match-end 0)))
      (insert new)
      (unless (looking-at ">") (just-one-space))
      ;; formatting, remove whitespace:
      (goto-char (nxml-token-after))
      (unless (looking-at "<")
	(goto-char (nxml-token-after)))
      (delete-horizontal-space)
      (cond  ((looking-at "<i") (indent-to dix-i-align-column))
	     ((save-excursion (search-forward "</pardef>" nil 'noerror 1))
	      (indent-to dix-pp-align-column))
	     ((looking-at "<p") (indent-to dix-pb-align-column))))))

(defun dix-restriction-cycle (&optional dir)
  "Cycle through possible values of the `r' attribute of the <e>
element at point. Optional argument DIR is a string, either
\"\", \"LR\" or \"RL\"."
  (interactive)
  (save-excursion
    (dix-up-to "e" "pardef")
    (let* ((old		     ; find what, if any, restriction we have:
	    (save-excursion
	      (if (re-search-forward " r=\"\\(..\\)\"" (nxml-token-after) 'noerror 1)
		  (match-string 1))))
	   (dir (if dir dir
		  (if old		; find our new restriction:
		      (if (equal old "LR")
			  "RL"	; "LR" => "RL"
			"")	; "RL" =>  ""
		    "LR")))	;  ""  => "LR"
	   (new (if (equal dir "") ""
		  (concat " r=\"" dir "\""))))
      ;; restrict:
      (forward-word)
      (if old (delete-region (match-beginning 0)
			     (match-end 0)))
      (insert new)
      (unless (looking-at ">") (just-one-space))
      ;; formatting, remove whitespace:
      (goto-char (nxml-token-after))
      (unless (looking-at "<")
	(goto-char (nxml-token-after)))
      (delete-horizontal-space)
      (cond  ((looking-at "<i") (indent-to dix-i-align-column))
	     ((save-excursion (search-forward "</pardef>" nil 'noerror 1))
	      (indent-to dix-pp-align-column))
	     ((looking-at "<p") (indent-to dix-pb-align-column))))))

(defun dix-LR-restriction-copy (&optional RL)
  "Make a copy of the Apertium element we're looking at, and add
an LR restriction to the copy. A prefix argument makes it an RL
restriction."
  (interactive "P")
  (save-excursion
    (dix-copy)
    (let ((dir (if RL "RL" "LR")))
      (dix-restriction-cycle dir)))
  ;; move point to end of relevant word:
  (dix-up-to "e" "pardef")
  (nxml-down-element 2) (when RL (nxml-forward-element))
  (nxml-down-element 1) (goto-char (nxml-token-after)))

(defun dix-RL-restriction-copy ()
  "Make a copy of the Apertium element we're looking at, and
add an RL restriction to the copy."
  (interactive)
  (dix-LR-restriction-copy 'RL))

(defun dix-copy (&optional remove-lex)
  "Make a copy of the Apertium element we're looking at. Optional
prefix argument `remove-lex' removes the contents of the lm
attribute and <i> or <p> elements."
  (interactive "P")
  ;; todo: find the first one of these: list-item, e, def-var, sdef, attr-item, cat-item, clip, pattern-item,
  (dix-up-to "e" "pardef")
  (let ((beg (or (re-search-backward "^[\t ]*" (line-beginning-position) 'noerror) (point)))
	(origend (1+ (save-excursion
		       (goto-char (nxml-scan-element-forward (point)))
		       (or (re-search-forward "[\t ]*$" (line-end-position) 'noerror) (point))))))
    (goto-char origend)
    (insert (buffer-substring-no-properties beg origend))
    (let ((copyend (point)))
      (when remove-lex
	(save-excursion
	  (goto-char origend)
	  (save-restriction
	    (narrow-to-region origend copyend)
	    (while (re-search-forward "lm=\\\"[^\\\"]*\\\"" nil 'noerror 1)
	      (replace-match "lm=\"\""))
	    (goto-char (point-min))
	    (while (re-search-forward "<i>.*</i>" nil 'noerror 1)
	      (replace-match "<i></i>"))
	    (goto-char (point-min))
	    (while (re-search-forward "<p>.*</p>" nil 'noerror 1)
	      (replace-match "<p><l></l><r></r></p>")))))
      ;; Put point at next useful spot, but don't move past this element:
      (goto-char origend)
      (dix-next)
      (if (> (point) copyend) (goto-char origend)))))



(defun dix-copy-yank ()
  "Make a copy of the Apertium element we're looking at, and yank
into the beginning of the lm and <i>."
  (interactive)
  (dix-copy)
  (dix-next 1)
  (yank)
  (dix-next 1)
  (yank))

(defun dix-increase-sense (&optional dir)
  "Increase the number in the (deprecated) slr attribute.
If given, optional argument `DIR' increases srl instead."
  (interactive)
  (let ((dir (or dir "slr")))
    (save-excursion
      (dix-up-to "e" "section")
      (let* ((old	     ; find what, if any, restriction we have:
	      (save-excursion
		(if (re-search-forward (concat " " dir "=\"\\([0-9]+\\)\"\\| c=\" *\\(0\\)\"") (nxml-token-after) 'noerror)
		    (or (match-string 1)
			(match-string 2)))))
	     (new (number-to-string (if old (1+ (string-to-number old)) 1))))
	;; restrict:
	(forward-word)
	(if old (delete-region (match-beginning 0)
			       (match-end 0)))
	(insert (concat " " dir "=\"" new "\""))
	(unless (looking-at ">") (just-one-space))
	;; formatting, remove whitespace:
	(goto-char (nxml-token-after))
	(unless (looking-at "<") (goto-char (nxml-token-after)))
	(delete-horizontal-space)
	(cond ((looking-at "<i") (indent-to dix-i-align-column))
	      ((looking-at "<p") (indent-to dix-pb-align-column)))))))

(defun dix-l-at-point-reg ()
  "Return <l> of <e> at point as pair of buffer positions."
  (save-excursion
    (dix-up-to "e" "pardef")
    (nxml-down-element 2)
    (cons (dix-token-start) (nxml-scan-element-forward (point)))))

(defun dix-r-at-point-reg ()
  "Return <r> of <e> at point as pair of buffer positions."
  (save-excursion
    (dix-up-to "e" "pardef")
    (nxml-down-element 2)
    (nxml-forward-element 1)
    (cons (dix-token-start) (nxml-scan-element-forward (point)))))

(defun dix-first-cdata-of-elt (pos)
  "Return first available CDATA of elt after POS as string."
  (save-excursion
    (goto-char pos)
    (nxml-down-element 1)
    (let ((beg (point)))
      (dix-with-sexp (forward-sexp))
      (buffer-substring beg (point)))))

(defun dix-l-word-at-point ()
  "Return first available CDATA of <l> of <e> at point as string."
  (dix-first-cdata-of-elt (car (dix-l-at-point-reg))))

(defun dix-r-word-at-point ()
  "Return first available CDATA of <r> of <e> at point as string."
  (dix-first-cdata-of-elt (car (dix-r-at-point-reg))))

(defun dix-l/r-at-point-reg ()
  "Return nearest region of of <l>/<r> as pair of buffer positions."
  (let ((l (dix-l-at-point-reg))
        (r (dix-r-at-point-reg)))
    (cond
     ((< (point) (cdr l)) l)
     ((> (point) (car r)) r)
     ((< (- (point)
            (cdr l))
         (- (car r)
            (point)))
      l)
     (t r))))

(defun dix-l/r-word-at-point ()
  "Return first available CDATA of nearest <l> or <r> as string."
  (dix-first-cdata-of-elt (car (dix-l/r-at-point-reg))))

(defun dix-sense-swap ()
  "Swap this translation with the above.
If this <e> has an slr, swap the <r>'s, if this <e> has an srl, swap the <l>'s.

When using, make sure point is at an entry marked slr/srl, and
the above <e> is part of the same sense group."
  (interactive)
  (dix-up-to "e" "pardef")
  (let ((dir
	 (save-excursion
	   (when (re-search-forward (concat " \\(slr\\|srl\\)=\"[0-9][0-9]*\"") (nxml-token-after) 'noerror)
	     (match-string-no-properties 1)))))
    (if dir
	(let* ((reg1 (save-excursion
		       (dix-with-sexp (nxml-backward-element 1))
		       (if (string= "slr" dir)
			   (dix-r-at-point-reg)
			 (dix-l-at-point-reg))))
	       (elt1 (buffer-substring (car reg1) (cdr reg1))))
	  (let* ((reg2 (if (string= "slr" dir)
			   (dix-r-at-point-reg)
			 (dix-l-at-point-reg)))
		 (elt2 (buffer-substring (car reg2) (cdr reg2))))
	    (goto-char (car reg2))
	    (delete-region (car reg2) (cdr reg2))
	    (insert elt1)
	    (goto-char (car reg1))
	    (delete-region (car reg1) (cdr reg1))
	    (insert elt2))
	  ;; So that we can move an element up several places with
	  ;; consecutive tab presses:
	  (goto-char (car reg1)))
      (message "No slr/srl found in this <e>-element"))))

(defun dix-slr-copy (&optional srl)
  "Make a copy of the Apertium element we're looking at, and
increase the slr sense attribute of the copy (optionally adding
it if it's not present). Optional prefix argument `srl' makes it
an srl sense."
  (interactive "P")
  (let ((dir (if srl "srl" "slr")))
    (save-excursion
      (dix-copy)
      (dix-increase-sense dir)))
  ;; move point to end of relevant word:
  (dix-up-to "e" "section")
  (nxml-forward-element)
  (nxml-down-element 2) (unless srl (nxml-forward-element))
  (nxml-down-element 1) (goto-char (nxml-token-after)))

(defun dix-srl-copy ()
  "Make a copy of the Apertium element we're looking at, and
increase the srl sense attribute of the copy (optionally adding
it if it's not present)."
  (interactive)
  (dix-slr-copy 'srl))

(defvar dix-char-alist
  ;; TODO: Emacs<23 uses utf8, latin5 etc. while Emacs>=23 uses unicode
  ;; for internal representation; use (string< emacs-version "23")
  '((?a 225)
    (?A 193)
    (?s 353)
    (?S 352)
    (?t 359)
    (?T 358)
    (?n 331)
    (?N 330)
    (?d 273)
    (?D 272)
    (?c 269)
    (?C 268)
    (?a 2273)
    (?A 2241)
    (?z 382)
    (?Z 381)
    (?s 331937)
    (?S 331936)
    (?t 331943)
    (?T 331942)
    (?n 331883)
    (?N 331882)
    (?d 331825)
    (?D 331824)
    (?c 331821)
    (?C 331820))
  "Sámi alphabet:
ášertŧuiopåŋđæølkjhgfdsazčcvbnmÁŠERTŦUIOPÅŊĐÆØLKJHGFDSAZČCVBNM")

(put 'dix-char-table 'char-table-extra-slots 0) ; needed if 0?

(defvar dix-asciify-table
  ;; TODO: seems like this has to be eval'ed after loading something else...
  (let ((ct (make-char-table 'dix-char-table)))
    (dolist (lst dix-char-alist ct)
      (mapc (lambda (x) (aset ct x (car lst))) (cdr lst))))
  "Converts Sámi characters into ascii equivalent.")

(defun dix-asciify (str)
  "Used before sorting to turn á into a, etc."
  (let ((cpos 0))
    (while (< cpos (length str))
      (let ((tr (aref dix-asciify-table (elt str cpos))))
	(when tr (aset str cpos tr)))
      (incf cpos)))
  str)

(defun dix-token-start ()
  "Give the position of the start of the following element.
Useful after e.g. `nxml-down-element' if there's whitespace."
  (if (looking-at "<")
      (point)
    (nxml-token-after)))

(defun dix-sort-e-by-l (reverse beg end &optional by-r)
  "Sort region alphabetically by contents of <l> element (or by
<r> element if optional argument `by-r' is true); argument means
descending order. Assumes <e> elements never occupy more than one
line.

Called from a program, there are three arguments:
`reverse' (non-nil means reverse order), `beg' and `end' (region
to sort).  The variable `sort-fold-case' determines whether
alphabetic case affects the sort order.

Note: will not work if you have several <e>'s per line!"
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; make `end-of-line' and etc. to ignore fields:
	  ((inhibit-field-text-motion t))
	(sort-subr
	 reverse
	 (lambda ()	                ; nextrec
	   (goto-char (nxml-token-after))
	   (re-search-backward "\\s *" (line-beginning-position) 'noerror))
	 (lambda ()			; endrec
	   (dix-up-to "e")
	   (nxml-forward-element)
	   (re-search-forward "\\s *" (line-end-position) 'noerror)
	   (let ((next-tok (nxml-token-after))) ; skip comments before eol:
	     (while (and (eq xmltok-type 'comment)
			 (<= next-tok (line-end-position)))
	       (goto-char next-tok)
	       (re-search-forward "\\s *" (line-end-position) 'noerror)
	       (setq next-tok (nxml-token-after)))))
	 (lambda ()			; startkey
	   (nxml-down-element 1)
	   (let ((slr (dix-get-slr xmltok-attributes)))
	     (nxml-down-element 1)
	     (let* ((lstart (point))
		    (lend (progn (nxml-forward-element) (point)))
		    (rstart (dix-token-start))
		    (rend (progn (nxml-forward-element) (point)))
		    (l (dix-asciify (buffer-substring-no-properties lstart lend)))
		    (r (dix-asciify (buffer-substring-no-properties rstart rend))))
	       (if by-r
		   (concat r l)
		 (concat l slr r))))))))))

(defun dix-get-slr (attributes)
  "`attributes' is of the format of `xmltok-attributes', returns
the string value of the slr attribute if it's set, otherwise
\"0\". Should probably be padded since we use it for sorting, but
so far there are never slr's over 10 anyway..."
  (let ((att (dix-get-attrib attributes "slr")))
    (if att
	(buffer-substring-no-properties (xmltok-attribute-value-start att)
					(xmltok-attribute-value-end att))
      "0")))

(defun dix-sort-e-by-r (reverse beg end)
  (interactive "P\nr")
  (dix-sort-e-by-l reverse beg end 'by-r))

(defun dix-sort-pardef (reverse)
  "Sort a pardef using `dix-sort-e-by-r'."
  (interactive "P")
  (save-excursion
    (let (beg end)
      (dix-up-to "pardef" "pardefs")
      ;; get beginning of first elt within pardef:
      (setq beg (save-excursion (goto-char (nxml-token-after))
				(nxml-token-after)))
      ;; nxml-token-before is beginning of <pardef>; set xmltok-start
      ;; to beginning of </pardef>:
      (if (nxml-scan-element-forward (nxml-token-before))
	  (dix-sort-e-by-r reverse beg xmltok-start)))))

(defun dix-reverse-lines (beg end)
  "Reverse each line in the region. Used by `dix-suffix-sort'. If
called non-interactively, reverse each full line from `beg' to
`end' (inclusive, never reverses part of a line)."
  (interactive "r")
  (save-excursion
    (if (and (>= beg (line-beginning-position))
	     (<= end (line-end-position)))
	(dix-reverse-region (line-beginning-position)
			    (line-end-position))
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (dix-reverse-region (line-beginning-position)
			      (line-end-position))
	  (forward-line))))))

(defun dix-reverse-region (beg end)
  "Reverse the text between positions `beg' and `end' in the
buffer. Used by `dix-reverse-lines'."
  (interactive "r")
  (let ((line (buffer-substring beg end)))
    (delete-region beg end)
    (insert (apply 'string (reverse (string-to-list line))))))

(defun dix-suffix-sort (beg end)
  "Sort the region by the reverse of each line, useful for
finding compound words which could have the same paradigm."
  (interactive "r")
  (dix-reverse-lines beg end)
  (sort-lines nil beg end)
  (dix-reverse-lines beg end))

(defun dix-replace-regexp-within-elt (regexp to-string eltname &optional delimited start end)
  "Does exactly what `query-replace-regexp' does, except it
restricts `regexp' and `to-string' to text within `eltname'
elements. Note: this function does not ensure that `to-string' is
free from instances of the end `eltname', so it's easy to break
if you so wish."
  (interactive
   (let ((common (query-replace-read-args
		  (if (and transient-mark-mode mark-active)
		      "Query replace regexp in region within elements" "Query replace regexp within elements")
		  t))
	 (eltname (read-from-minibuffer "Element name: ")))
     (list (nth 0 common) (nth 1 common) eltname (nth 2 common)
	   (if (and transient-mark-mode mark-active) (region-beginning))
	   (if (and transient-mark-mode mark-active) (region-end)))))
  (perform-replace (concat "\\(<" eltname ">.*\\)" regexp "\\(.*</" eltname ">\\)")
		   (concat "\\1" to-string "\\2")
		   t t delimited nil nil start end))

(defun dix-replace-regexp-within-l (regexp to-string &optional delimited start end)
  "Call `dix-replace-regexp-within-elt' on <l> elements."
  (interactive
   (let ((common (query-replace-read-args
		  (if (and transient-mark-mode mark-active)
		      "Query replace regexp in region within <l>'s" "Query replace regexp within <l>'s")
		  t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (and transient-mark-mode mark-active) (region-beginning))
	   (if (and transient-mark-mode mark-active) (region-end)))))
  (dix-replace-regexp-within-elt regexp to-string "l" delimited start end))

(defun dix-replace-regexp-within-r (regexp to-string &optional delimited start end)
  "Call `dix-replace-regexp-within-elt' on <r> elements."
  (interactive
   (let ((common (query-replace-read-args
		  (if (and transient-mark-mode mark-active)
		      "Query replace regexp in region within <r>'s" "Query replace regexp within <r>'s")
		  t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (and transient-mark-mode mark-active) (region-beginning))
	   (if (and transient-mark-mode mark-active) (region-end)))))
  (dix-replace-regexp-within-elt regexp to-string "r" delimited start end))


(defvar dix-search-substring nil
  "Set by `dix-word-search-forward'.")

(defun dix-word-search-forward (&optional whole-word)
  "Incremental word-search for dix files. In monodix, searches
only within lm attributes, in bidix, searches only between > and
< symbols. If optional prefix argument `whole-word' is given, you
have to type the whole word in to get a (correct) hit, otherwise
you can search for partial words.

TODO:
- Check if this can be rewritten as a isearch-filter-predicate
- Figure out why two backspaces are needed (seems to 'temporarily
  fail' when searching)
- Unless we're doing a substring search, we should probably not
  do an incremental search (that is, not search until user
  presses enter), but word-search-forward isn't good
  enough (doesn't highlight, no way to C-s to the next hit...)"
  (interactive "P")
  (setq dix-search-substring (not whole-word))
  (isearch-mode
   'forward 'regexp
   (lambda ()
     (let* ((bidix (string-match "\\...*-..*\\.dix" (buffer-file-name)))
	    (l (if bidix ">" "lm=\""))
	    (r (if bidix "<" "\""))
	    (affix (when dix-search-substring
                     "[^<\"]*")))
       (setq isearch-string
	     (concat l affix
                     (replace-regexp-in-string " "
                                               "\\\\( \\\\|<b/>\\\\)"
                                               isearch-message)
                     affix r))
       (goto-char isearch-opoint)
       (setq isearch-forward t)	; o/w the first C-s goes backward, for some reason
       (isearch-search)
       (isearch-push-state)
       (isearch-update)))))


(defun dix-find-rhs-mismatch ()
  "Find possible mismatches in <r> elements (ie. a pardef where
two <e>'s have different suffixes in their <r>'s).

Only stops at the first mismatch within one pardef."
  (interactive)
  (defun next-pardef ()
    (and (search-forward "pardef" nil t) (next-rhs)))
  (defun next-rhs ()
    (re-search-forward "<r>\\([^<]*\\)<\\|\\(</pardef>\\)" nil t))
  (let ((keep-looking (next-pardef))	; find first hit
	(last-rhs (match-string 1)))
    ;; Check next ones for mismatches:
    (while keep-looking
      (if (equal (match-string 2) "</pardef>")
	  (setq keep-looking (next-pardef) ; skip to next <pardef>
		last-rhs (match-string 1))
	(if (equal (match-string 1) last-rhs)
	    (next-rhs)			; skip to next <e>
	  (setq keep-looking nil))))
    ;; Echo results:
    (if (match-string 1)
	(and (goto-char (match-end 1))
	     (message
	      (concat "Possible mismatch in <r>: " last-rhs " vs " (match-string 1))))
      (message "No mismatches discovered."))))

(defun dix-next (&optional step)
  "Moves forward `step' steps (default 1) in <e> elements between
the important places (lm attribute, <i>/<r>/<l> data, n attribute
of <par>/<s>; and then onto the next <e> element). See also
`dix-previous'."
  (interactive "p")
  (let* ((step (if step step 1))
	 (backward (< step 0)))
    (when (> (abs step) 0)
	(dix-next-one backward)
	(dix-next (if backward (1+ step) (1- step))))))

(defun dix-previous (&optional step)
  "Moves backward `step' steps (default 1) in <e> elements. See
also `dix-next'."
  (interactive "p")
  (dix-next (- (if step step 1))))

(defun dix-nearest-pdname (origin)
  "Return the pardef-name nearest `origin' within an <e> element."
  (save-excursion
    (dix-up-to "e")
    (let* ((e-end (nxml-scan-element-forward (nxml-token-before)))
	   (pdname (and (re-search-forward "par\\s *n=\"\\([^\"]*\\)\"" e-end)
			(match-string-no-properties 1))))
      (while (and (re-search-forward "<par\\s *n=\"\\([^\"]*\\)\"" e-end 'noerror)
		  (< (match-beginning 0) origin))
	(setq pdname (match-string-no-properties 1)))
      pdname)))

(defun dix-goto-pardef (&optional pdname)
  "Call from an entry to go to its pardef. Mark is pushed so you
can go back with C-u \\[set-mark-command]."
  (interactive)
  (let* ((pdname (or pdname (dix-nearest-pdname (point))))
	 (pos (save-excursion
		(goto-char (point-min))
		(when (re-search-forward
		       (concat "<pardef *n=\"\\(" (regexp-quote pdname) "\\)\"")
                       nil t)
		  (match-beginning 0)))))
    (if pos
	(progn
	  (push-mark)
	  (goto-char pos)
	  (unless (equal (match-string 1) pdname)
	    (message "WARNING: pardef-names don't quite match: \"%s\" and \"%s\"" pdname (match-string 1))))
      (message "Couldn't find pardef %s" pdname))))

(defun dix-view-pardef ()
  "Show pardef in other window. The pardef is just inserted into
a new buffer where you can e.g. edit at will and then paste back.
The nice thing is that for each call of this function, the pardef
is added to the *dix-view-pardef* buffer, so you get a temp
buffer where you can eg. collapse pardefs."
  ;; TODO: would it be better to `clone-indirect-buffer-other-window'
  ;; with a buffer restriction, so we could edit the pardef without
  ;; having to copy-paste it back?
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (dix-goto-pardef)
      (let* ((beg (point))
	     (end (1+ (nxml-scan-element-forward beg)))
	     (pardef (buffer-substring beg end)))
	(save-selected-window
	  (pop-to-buffer "*dix-view-pardef*")
	  (insert pardef)
	  (nxml-mode)
	  (dix-mode 1)
	  (goto-char (point-max))	; from `end-of-buffer'
	  (overlay-recenter (point))
	  (recenter -3))))))


(defun dix-goto-rule-number (num)
  "Go to <rule> number `num' in the file. When called
interactively, asks for a rule number (or uses the prefix
argument)."
  (interactive "NRule number: ")
  (let ((found nil)
	(cur num))
    (save-excursion
      (goto-char (point-min))
      (while (and (> cur 0)
		  (setq found (dix-rule-forward)))
	(setq cur (- cur 1))))
    (if found
	(goto-char found)
      (message "Couldn't find rule number %d" num))))

(defun dix-rule-forward ()
  "Move point to the next transfer <rule>. Return point if we can
find a non-commented <rule> before the end of the buffer,
otherwise nil"
  (setq found-a-rule (re-search-forward "<rule[ >]" nil t)
	keep-looking (and (nxml-token-before) (eq xmltok-type 'comment)))
  (while (and found-a-rule
	      keep-looking)
    (setq found-a-rule (re-search-forward "<rule[ >]" nil t)
	  keep-looking (and (nxml-token-before) (eq xmltok-type 'comment))))
  (and found-a-rule
       (not keep-looking)
       (point)))


(defvar dix-modes
  '((nn-nb ("lt-proc"    "/l/n/nn-nb.automorf.bin")
	   ("cg-proc" "/l/n/nn-nb.rlx.bin")
	   ("apertium-tagger -g" "/l/n/nn-nb.prob")
	   ("apertium-pretransfer")
	   ("apertium-transfer" "/l/n/apertium-nn-nb.nn-nb.t1x" "/l/n/nn-nb.t1x.bin" "/l/n/nn-nb.autobil.bin")
	   ("lt-proc -g" "/l/n/nn-nb.autogen.bin"))
    (nb-nn ("lt-proc"    "/l/n/nb-nn.automorf.bin")
	   ("cg-proc" "/l/n/nb-nn.rlx.bin")
	   ("apertium-tagger -g" "/l/n/nb-nn.prob")
	   ("apertium-pretransfer")
	   ("apertium-transfer" "/l/n/apertium-nn-nb.nb-nn.t1x" "/l/n/nb-nn.t1x.bin" "/l/n/nb-nn.autobil.bin")
	   ("lt-proc -g" "/l/n/nb-nn.autogen.bin"))))
(make-variable-buffer-local 'dix-modes)

(defun dix-analyse (&optional no-disambiguate)
  "Very bare-bones at the moment.

Todo: read modes.xml instead of those using those dix-path*
variables, and allow both directions (although should have some
option to override the modes.xml reading).

Todo: word-at-point function which ignores xml stuff."
  (interactive "P")
  (save-selected-window
    (let ((modes dix-modes)
	  (word (word-at-point))
	  last-output)
      (pop-to-buffer "*dix-analysis*")
      (dolist (mode modes)
	(insert "==> " (symbol-name (car mode)) " <==\n")
	(setq last-output word)
	(dolist (cmd (cdr mode))
	  (let ((cmdstr (mapconcat 'concat cmd " ")))
	    (insert " " cmdstr ":\n")
	    (setq last-output
		  (substring		; trim off final newline
		   (shell-command-to-string
		    (concat "echo '" last-output "' | " cmdstr))
		   0 -1))
 	    (insert last-output "\n\n")
	    (when (and no-disambiguate (or (string= "lt-proc" (car cmd))
					   (string= "lt-proc -w" (car cmd))))
	      (insert (setq last-output (dix-analysis-split last-output)) "\n"))))))
    (nxml-mode)
    (toggle-truncate-lines 0)
    (goto-char (point-max))		; from `end-of-buffer'
    (overlay-recenter (point))
    (recenter -3)))

(defun dix-analysis-split (ambig)
  (let* ((first (string-match "/" ambig))
	 (surface (substring ambig (string-match "\\^" ambig) first))
	 (analyses (substring ambig first (string-match "\\$" ambig))))
    (mapconcat (lambda (analysis) (concat surface "/" analysis "$"))
	       (split-string analyses "/" 'omitnulls) " ")))

(defun dix-rstrip (whole end)
  "Remove substring `end' from string `whole' and return the result."
  (if (string= end
	       (substring whole
			  (- (length whole) (length end))))
      (substring whole 0 (- (length whole) (length end)))
    (error (concat "The string \"" end "\" does not end \"" whole "\""))))

(defun dix-consume-i (substr)
  "Try to remove `substr' from this <i>-element."
  (when (> (length substr) 0)
    (let ((end (+ (point) (length substr))))
      (if (string= (xmltok-start-tag-qname) "i")
	  (if (string= substr (buffer-substring-no-properties (point) end))
	      (delete-region (point) end)
	    (error "dix-consume-i did not find substring"))
	(error "bailing out, dix-consume-i can't handle elements other than <i>")))))

(defun dix-guess-pardef (&optional partype)
  "How to use: write a long word, e.g. a compound noun like
\"øygruppe\", in the dix file below all the nouns, put point
between \"øy\" and \"gruppe\", and if \"gruppe\" is defined
somewhere above, this function will turn the word into:

<e lm=\"øygruppe\">    <i>øygrupp</i><par n=\"lø/e__n\"/></e>

Optional string argument `partype' lets you restrict the search
to entries with pardef names ending in that string (e.g. \"__n\").

You can also add mwe's like

setje# fast

and get them turned into

<e lm=\"setje fast\">      <i>set</i><par n=\"set/je__vblex\"/><p><l><b/>fast</l><r><g><b/>fast</g></r></p></e>

assuming there's a line like

<e lm=\"setje\">      <i>set</i><par n=\"set/je__vblex\"/></e>

somewhere above.

TODO: ideally, instead of having to place the cursor at the
compound border, it should try to get the longest possible match,
the same as trying this function from first char, then from
second, etc., but preferably using a more efficient method..."
  (interactive)
  (let* ((queue-start (save-excursion (search-forward "#" (line-end-position) 'noerror)))
	 (queue (when queue-start
		  (replace-regexp-in-string
		   "^#" ""
		   (buffer-substring-no-properties queue-start (line-end-position)))))
	 ;; rhs of point is the part we search for, the lhs and queue are particular to this word
	 (rhs-end (if queue-start
		      (1- queue-start)
		    (line-end-position)))
	 (rhs (buffer-substring-no-properties (point) rhs-end))
	 (pos (save-excursion
		(re-search-backward
		 (concat "<e.* lm=\"[^\"]*" rhs "\".*" partype "\"") nil 'noerror 1))))
    (if pos
	(let ((e (buffer-substring-no-properties pos (nxml-scan-element-forward pos)))
	      (word (buffer-substring-no-properties (line-beginning-position)
						    rhs-end)))
	  (delete-region (line-beginning-position) (line-end-position))
	  (insert e)
	  (when queue
	    (save-excursion
	      (nxml-backward-down-element)
	      (insert (concat "<p><l>"
			      (replace-regexp-in-string " " "<b/>" queue)
			      "</l><r><g>"
			      (replace-regexp-in-string " " "<b/>" queue)
			      "</g></r></p>"))))
	  (nxml-backward-single-balanced-item)
	  (dix-next)
	  (let* ((lmbound (progn (nxml-token-after)
				 (nxml-attribute-value-boundary (point))))
		 (oldlm (buffer-substring-no-properties (car lmbound) (cdr lmbound)))
		 ;; (oldroot.suffix (dix-split-root-suffix))
		 ;; (oldroot (car oldroot.suffix))
		 ;; (suffix (cdr oldroot.suffix))
		 ;; (nroot (if suffix
		 ;; 	   (if (string= (substring word (- (length suffix))) suffix)
		 ;; 	       (substring word 0 (- (length suffix)))
		 ;; 	     (error "Pardef suffix didn't match!"))
		 ;; 	 word))
		 (oldlhs (dix-rstrip oldlm rhs))
		 (lhs (dix-rstrip word rhs)))
	    (delete-region (car lmbound) (cdr lmbound))
	    (insert (concat word queue))
	    (dix-next)
	    (dix-consume-i oldlhs)
	    (unless (string= (xmltok-start-tag-qname) "i")
	      (nxml-backward-up-element)
	      (insert "<i></i>")
	      (backward-char 4))
	    (insert (replace-regexp-in-string " " "<b/>" lhs))
	    (beginning-of-line) (insert (concat "<!-- " oldlm " -->")) (end-of-line)))
      (message "No fitting word found :-/"))))

(defun dix-guess-pardef__n ()
  "Like `dix-guess-pardef', but restricted to pardefs with names
ending in __n"
  (interactive)
  (dix-guess-pardef "__n"))
(defun dix-guess-pardef__vblex_adj ()
  "Like `dix-guess-pardef', but restricted to pardefs with names
ending in __vblex_adj"
  (interactive)
  (dix-guess-pardef "__vblex_adj"))

(defun dix-add-par ()
  "Just add a <par>-element, guessing a name from nearest above par."
  (interactive)
  (let ((par-guess
         (save-excursion
           (or
            (when (re-search-backward "<par n=\"\\([^\"]*\\)" nil 'noerror 1)
              (match-string-no-properties 1))
            (when (re-search-backward "<pardef n=\"\\([^\"]*\\)" nil 'noerror 1)
              (match-string-no-properties 1))))))
    (dix-up-to "e" "section")
    (nxml-forward-element)
    (nxml-backward-down-element)
    (let ((p (+ (point) 8)))
      (insert (format "<par n=\"%s\"/>" par-guess))
      (goto-char p))))

(defun dix-add-s ()
  "Just add an <s>-element, guessing a name from nearest previous s.
The guesser will first try to find a tag from the same
context (ie. a tag that was seen after the tag we're adding a tag
after)."
  (interactive)
  (let ((reg (dix-l/r-at-point-reg)))
    ;; (elt (buffer-substring-no-properties (+ 1 (car reg))
    ;;                                      (+ 2 (car reg))))
    ;; Move so we're inside the l or r:
    (when (or (< (point) (car reg))
              (> (point) (cdr reg)))
      (goto-char (car reg)))
    ;; If we were inside an s, move before it:
    (nxml-token-after)
    (when (equal (xmltok-start-tag-qname) "s")
      (nxml-up-element))
    ;; Go to start of next s, or end of l/r:
    (if (re-search-forward "<s +[^>]+/>" (cdr reg) 'noerror 1)
        (goto-char (match-beginning 0))
      (goto-char (- (cdr reg) 4)))
    (let ((s-guess
           (save-excursion
             (or
              (when (re-search-backward (concat
                                         ;; First try finding a tag following the tag we're at:
                                         (regexp-quote (buffer-substring (- (point) 13)
                                                                         (point)))
                                         "<s n=\"\\([^\"]*\\)")
                                        nil 'noerror 1)
                (match-string-no-properties 1))
              (when (re-search-backward "<s n=\"\\([^\"]*\\)" nil 'noerror 1)
                (match-string-no-properties 1))
              (when (re-search-backward "<sdef n=\"\\([^\"]*\\)" nil 'noerror 1)
                (match-string-no-properties 1))
              "sg")))
          (p (+ (point) 6)))
      (insert (format "<s n=\"%s\"/>" s-guess))
      (goto-char p))))


(defun dix-point-after-> ()
  "True if point is exactly after the > symbol."
  (equal (buffer-substring-no-properties (1- (point)) (point))
         ">"))

(defun dix-space ()
  "This should return a space, unless we're inside the data area
of <g>, <r>, <l> or <i>, in which case we want a <b/>. If we're
in the attribute of a <par> or <pardef>, we insert an underscore.

A bit hacky I guess, but I don't want to require nxhtml just to
get nxml-where-path, and reimplementing an XML Path seems rather
too much work for this."

  (defun in-elt (names)	; nxml-token-before must be called before this
    (let ((eltname (save-excursion
		     (goto-char xmltok-start)
		     (when (equal xmltok-type 'data)
		       (nxml-token-before)
		       (goto-char xmltok-start))
		     (xmltok-start-tag-qname))))
      (and eltname (member eltname names))))

  (nxml-token-before)
  (cond ((and (or (eq xmltok-type 'data)
		  (and (memq xmltok-type '(start-tag empty-element))
		       (dix-point-after->)))
	      (in-elt '("g" "b" "r" "l" "i")))
	 "<b/>")
	((and (catch 'in-attr
		(dolist (attr xmltok-attributes)
		  (if (and (xmltok-attribute-value-start attr)
			   (>= (point) (xmltok-attribute-value-start attr))
			   (xmltok-attribute-value-end   attr)
			   (<= (point) (xmltok-attribute-value-end   attr))
			   (equal (xmltok-attribute-local-name attr) "n"))
		      (throw 'in-attr t))))
	      (in-elt '("par" "pardef")))
	 "_")
	(t
	 " ")))

(defun dix-insert-space ()
  "Inserts a `dix-space' at point."
  (interactive)
  (insert (dix-space)))

(defcustom dix-hungry-backspace nil
  "Delete whole XML elements (<b/>, comments) with a single press
of the backspace key. Set to nil if you don't want this behaviour."
  :type 'boolean
  :group 'dix)

(defun dix-backspace ()
  "Delete a character backward, unless we're looking at the end
of <b/> or a comment, in which case we delete the whole element.

Note: if we're looking at the relevant elements, prefix arguments
are ignored, while if we're not, a prefix argument will be passed
to the regular `delete-backward-char'."
  (interactive)
  (if (and dix-hungry-backspace
	   (nxml-token-before)
	   (or
	    (and (eq xmltok-type 'empty-element)
		 (equal (xmltok-start-tag-qname) "b"))
	    (and (eq xmltok-type 'comment)
		 (dix-point-after->))))
      (delete-region (point) xmltok-start)
    (call-interactively 'delete-backward-char)))

(defun dix-< (literal)			; not in use yet
  "Inserts < in space or unclosed tags, otherwise moves to the
beginning of the element."
  (interactive "*P")
  (if literal
      (self-insert-command (prefix-numeric-value literal))
    (nxml-token-after)
    (cond ((memq xmltok-type '(space data not-well-formed partial-start-tag))
	   (insert-char ?< 1))
	  (t (progn (nxml-up-element)
		    (dix-with-sexp (backward-sexp)))))))
(defun dix-> (literal)
  "Inserts > in space or unclosed tags, otherwise moves to the
end of the element."
  (interactive "*P")
  (if literal
      (self-insert-command (prefix-numeric-value literal))
    (nxml-token-before)
    (cond ((memq xmltok-type '(space not-well-formed partial-start-tag))
	   (insert-char ?> 1))
	  (t (nxml-up-element)))))


(defun dix-xmlise-using-above-elt ()
  "Simple yasnippet-like function to turn a plain list
into <e> entries. Write a bunch of words, one word per line,
below a previous <e> entry, then call this function to apply that
entry as a template on the word list. Example (with point
somewhere in the word list):


<e lm=\"baa\">      <i>ba</i><par n=\"ba/a__n\"/></e>
bada
bam bada
nana:mana

=>

<e lm=\"baa\">      <i>ba</i><par n=\"ba/a__n\"/></e>
<e lm=\"bada\">      <i>bad</i><par n=\"ba/a__n\"/></e>
<e lm=\"bam bada\">      <i>bam<b/>bad</i><par n=\"ba/a__n\"/></e>
<e lm=\"mana\">      <p><l>nan</l><r>man</r></p><par n=\"ba/a__n\"/></e>



Bidix example:

<e><p><l>ja<b/>nu<b/>ain<s n=\"Adv\"/></l><r>og<b/>så<b/>videre<s n=\"adv\"/></r></p></e>
kánske:kanskje
lahka:slags

=>

<e><p><l>ja<b/>nu<b/>ain<s n=\"Adv\"/></l><r>og<b/>så<b/>videre<s n=\"adv\"/></r></p></e>
<e><p><l>kánske<s n=\"Adv\"/></l><r>kanskje<s n=\"adv\"/></r></p></e>
<e><p><l>lahka<s n=\"Adv\"/></l><r>slags<s n=\"adv\"/></r></p></e>
"
  ;; TODO: remove the ugly
  ;; TODO: support for turning :<: and :>: into restrictions
  (interactive)
  (nxml-token-before)
  (when (eq xmltok-type 'data)
    (let* ((template-basis-end (save-excursion (goto-char xmltok-start)
					       ;; TODO: skip (include) comments
					       (re-search-backward "[ \t]*$" (line-beginning-position) 'no-errors)
					       (forward-line)
					       (point)))
	   (template-basis-start (save-excursion (nxml-backward-single-balanced-item)
						 (re-search-backward "^[ \t]*" (line-beginning-position) 'no-errors)
						 (point)))
	   (template-basis (buffer-substring-no-properties template-basis-start template-basis-end))
	   (template
	    ;; Create a format string like <e lm="%"><i>%s</i><par n="foo"/></e>
	    ;; TODO: might be more understandable with regex string-match?
	    (with-temp-buffer
	      (insert template-basis)
	      (goto-char (point-min))
	      (cond ((save-excursion (search-forward "<i>" (line-end-position) 'noerror))
		     (delete-region
		      (goto-char (match-end 0))
		      (save-excursion (search-forward "</i>" (line-end-position))
				      (match-beginning 0)))

		     (insert "%s"))
		    ((save-excursion (search-forward "<l>" (line-end-position) 'noerror))
		     (delete-region
		      (goto-char (match-end 0))
		      (save-excursion (re-search-forward "<s \\|</l>" (line-end-position))
				      (match-beginning 0)))

		     (insert "%s")
		     (search-forward "<r>" (line-end-position))
		     (delete-region
		      (match-end 0)
		      (save-excursion (re-search-forward "<s \\|</r>" (line-end-position))
				      (match-beginning 0)))
		     (insert "%s")))
	      (goto-char (point-min))
	      (when (save-excursion (search-forward " lm=\"" (line-end-position) 'noerror))
		(delete-region
		 (goto-char (match-end 0))
		 (save-excursion (search-forward "\"" (line-end-position))
				 (match-beginning 0)))
		(insert "%s"))
	      (buffer-substring-no-properties (point-min) (point-max))))
	   (lmsuffix
	    ;; regexp to remove from <i>'s if <i> is shorter than lm in the template:
	    (concat (and (string-match "<i>\\(.*\\)</i>"
				       template-basis)
			 (string-match (concat " lm=\"" (match-string 1 template-basis) "\\([^\"]+\\)\"")
				       template-basis)
			 (match-string 1 template-basis))
		    "$"))
	   (inlist-start (save-excursion (nxml-token-before)
                                         (goto-char xmltok-start)
                                         (re-search-forward "[^ \t\n]")
                                         (match-beginning 0)))
	   (inlist-end (save-excursion (goto-char (nxml-token-after))
                                       (re-search-backward "[^ \t\n]")
                                       (match-end 0)))
	   (inlist (split-string
		    (dix-trim-string (buffer-substring-no-properties
				      inlist-start
				      inlist-end))
		    "\n"
		    'omit-nulls))
	   (outlist (mapcar
		     (lambda (line)
		       ;; if there's no `:', use the whole line for both <l> and <r>,
		       ;; if there's one `:', use that to split into <l> and <r>
		       (let* ((lr (split-string line ":"))
			      (lm (if (cdr lr) (cadr lr) (car lr)))
			      (l (replace-regexp-in-string lmsuffix ""
							   (replace-regexp-in-string " " "<b/>" (car lr))))
			      (r (replace-regexp-in-string lmsuffix ""
							   (replace-regexp-in-string " " "<b/>" (if (cdr lr)
												    (cadr lr)
												  (car lr))))))
			 (when (cl-caddr lr) (error "More than one : in line: %s" line))
			 (format (if (equal l r)
				     template
				   ;; both <l> and <r> in input, perhaps change <i/> to <l/>...<r/>:
				   (replace-regexp-in-string "<i>%s</i>"
							     "<p><l>%s</l><r>%s</r></p>"
							     template))
				 (if (string-match " lm=\"%s\"" template) lm l)
				 (if (string-match " lm=\"%s\"" template) l r)
				 r)))
		     inlist)))
      ;; Delete the old inlist, and insert the new outlist:
      (delete-region inlist-start inlist-end)
      (insert (apply #'concat outlist)))))

(defun dix-trim-string (s)
  "Trim leading and trailing spaces, tabs and newlines off `s'."
  (cond ((not (stringp s)) nil)
	((string-match "^[ \t\n]*\\(\\(?:.\\|\n\\)*[^ \t\n]+\\)[ \t\n]*" s)
	 (match-string 1 s))
	(t s)))

(defvar dix-expansion-left nil
  "Cons of the tmpfile used to store the expansion, and the
timestamp of the file last time we expanded it, as given
by (sixth (file-attributes dix-monodix-left)).")
(defvar dix-monodix-left "/l/n/apertium-nn-nb.nn.dix")

(defun dix-expansion-sentinel (proc change-str)
  (when (string= "finished\n" change-str)
    (message "Expansion updated")
    (kill-buffer "*lt-expand*")))

(defun dix-update-expansion (expansion monodix update-expansion)
  "Due to emacs variable scoping, we have to include a function
`update-expansion' that updates the `expansion' variable with a
new tmpfile and `monodix' timestamp."
  (if (or (not expansion)
	  (and expansion
	       (not (equal (sixth (file-attributes monodix))
			   (cdr expansion)))))
      (let ((tmpfile (dix-trim-string
		      (shell-command-to-string "mktemp -t expansion.XXXXXXXXXX"))))
	(message "Expansion out of date with left monodix, hang on...")
	(if (and (file-attributes tmpfile) (file-attributes monodix))
	    (progn
	      (shell-command
	       (concat "lt-expand " monodix " " tmpfile " &") "*lt-expand*")
	      (funcall update-expansion tmpfile (sixth (file-attributes monodix)))
	      (set-process-sentinel (get-buffer-process "*lt-expand*") #'dix-expansion-sentinel))
	  (error "mktemp command failed: %s" tmpfile)))
    (message "Expansion up-to-date")))

(defun dix-update-expansions ()
  (interactive)
  (dix-update-expansion dix-expansion-left dix-monodix-left
			(lambda (file time)
			  (setq dix-expansion-left (cons file time)))))

(defun dix-expand (lemma pos)
  (shell-command (concat "grep ':\\([>]:\\)*" lemma "<" pos ">' " (car dix-expansion-left))))
(defun dix-expand-possibilities (tags)
  "There should be no leading < in `tags', but you can have
several with >< between them."
  (shell-command (concat "grep ':\\([>]:\\)*[^<]*<" tags "' " (car dix-expansion-left)
			 " | sed 's/.*:[^\\<]*\\</</' | sort | uniq")))

(defun dix-narrow-to-sdef-narrow (sdef sec-start sec-end)
  (goto-char sec-start)
  (search-forward (concat "<s n=\"" sdef "\""))
  (search-backward "<e")
  (beginning-of-line)
  (narrow-to-region (point)
		    (save-excursion
		      (goto-char sec-end)
		      (search-backward (concat "<s n=\"" sdef "\""))
		      (search-forward "</e")
		      (end-of-line)
		      (point))))

(defun dix-narrow-to-sdef (&optional no-widen)
  "Narrow buffer to a region between the first and the last
occurence of a given sdef in a given section; lets you
tab-complete on sdefs and sections.

Optional prefix argument `no-widen' lets you narrow even more in
on a previously narrowed buffer (the default behaviour for
`narrow-to-region'), otherwise the buffer is widened first."
  (interactive "P")
  (dix-with-no-case-fold
      (let (sdefs)
        (save-excursion ;; find all sdefs
          (save-restriction (widen)
                            (goto-char (point-min))
                            (while (re-search-forward
                                    "<sdef[^>]*n=\"\\([^\"]*\\)\"" nil 'noerror)
                              (add-to-list 'sdefs (match-string-no-properties 1)))))
        (let ((sdef (completing-read "sdef/POS-tag: " sdefs nil 'require-match))
              id start end sections)
          (save-excursion ;; find all sections
            (save-restriction (widen)
                              (goto-char (point-min))
                              (while (setq start (re-search-forward
                                                  "<section[^>]*id=\"\\([^\"]*\\)\"" nil 'noerror))
                                (setq id (match-string-no-properties 1))
                                (setq end (re-search-forward "</section>"))
                                (if (search-backward (concat "<s n=\"" sdef "\"") start 'noerror)
                                    (add-to-list 'sections (list id start end))))))
          ;; narrow to region between first and last occurrence of sdef in chosen section
          (let* ((ids (mapcar 'car sections))
                 (id (if (cdr sections)
                         (completing-read "Section:" ids nil 'require-match
                                          (if (cdr ids) nil (car ids)))
                       (caar sections)))
                 (section (assoc id sections)))
            (unless no-widen (widen))
            (dix-narrow-to-sdef-narrow sdef (second section) (cl-caddr section)))))))

;;; The following is rather nn-nb-specific stuff. Todo: generalise or remove.
(defun dix-move-to-top ()
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active)
	(exchange-point-and-mark)
      (progn
	(dix-up-to "e")
	(push-mark (nxml-scan-element-forward (point)))))
    (when (re-search-backward "\\S " nil t)
      (forward-char))
    (let* ((beg (point))
	   (end (mark))
	   (region (buffer-substring beg end)))
      (delete-region beg end)
      (goto-char (point-min))
      (end-of-line)
      (insert region)))
  (re-search-forward "\\S "))

(defcustom dix-dixfiles '("*.dix" "dev/*dix") "String list of dictionary files to grep with `dix-grep-all'."
  :type '(list string)
  :group 'dix)

(defvar dix-greppable
  '(;; dix:
    ("e" "lm" "l" "r" "i")
    ("par" "n")
    ("pardef" "n"))
  "Association list of elements and which attributes are considered interesting for grepping.
Used by `dix-grep-all'.")

(defvar dix-grep-fns
  '(;; dix:
    ("e" . dix-lemma-at-point)
    ("l" . dix-l-word-at-point)
    ("r" . dix-r-word-at-point)
    ("i" . dix-i-at-point)
    ("par" . dix-par-at-point)
    ("pardef" . dix-pardef-at-point))
  "Association list of elements and which functions to find the greppable symbol at point.
Used by `dix-grep-all'.")


(defun dix-nearest-greppable ()
  (let* ((token-end (nxml-token-before))
         (greppable (save-excursion
                      (let ((dix-interesting dix-greppable))
                        (dix-next)
                        (dix-previous)
                        (let ((g (assoc (xmltok-start-tag-qname) dix-grep-fns)))
                          (when g
                            (cons (car g) (funcall (cdr g)))))))))
    (if greppable
        greppable
      (message "Nothing greppable found here (see variables dix-greppable and dix-grep-fns).")
      nil)))

(defun dix-grep-all (&optional include-this)
  "Show all usages of this pardef in related dictionaries.
Related dictionaries are represented by the (customizable) string
`dix-dixfiles'. Unless optional argument INCLUDE-THIS is given,
the current file is excluded from the results."
  (interactive "P")
  (let* ((greppable (dix-nearest-greppable))
         ;; TODO: if par/pardef, want to search only par/pardef, and so on
         (found-in (car greppable))
         (needle (if (and transient-mark-mode mark-active)
                     (buffer-substring (region-beginning)
                                       (region-end))
                   (cdr greppable)))
         (needle-attrib (replace-regexp-in-string "<b/>" " " needle))
         (needle-cdata (replace-regexp-in-string " " "<b/>" needle))
         ;; TODO: exclude current file?
         (files (mapconcat #'identity (dix-existing-dixfiles include-this) " ")))
    (grep (format "grep -nH -e %s -e %s %s"
                  (shell-quote-argument (format "lm=\"%s\"" needle-attrib))
                  (shell-quote-argument (format ">%s<" needle-cdata))
                  files))))

(defun dix-existing-dixfiles (include-this &optional dir)
  "Get the set of existing files that match `dix-dixfiles' patterns.
Excludes the file of the current buffer unless INCLUDE-THIS is
non-nil. The set is uniq'd and turned relative according to
DIR (defaults to `default-directory')."
  (let ((default-directory (or dir default-directory)))
    (let* ((this (buffer-file-name (current-buffer)))
           (existing (apply #'append (mapcar #'file-expand-wildcards dix-dixfiles)))
           (absolute (mapcar #'file-truename existing))
           (uniq (cl-remove-duplicates absolute :test #'equal))
           (w/o-this (if include-this uniq
                       (remove this uniq))))
      (mapcar #'file-relative-name w/o-this))))

;;; Alignment ----------------------------------------------------------------
(defcustom dix-rp-align-column 28 "Column to align pardef <r> elements to with `align'."
  :type 'integer
  :group 'dix)
(defcustom dix-rb-align-column 44 "Column to align bidix <r> elements to with `align'."
  :type 'integer
  :group 'dix)
(defcustom dix-i-align-column 25 "Column to align <i> elements to with `align'."
  :type 'integer
  :group 'dix)
(defcustom dix-ep-align-column 2 "Column to align pardef <e> elements to with `align'.
Not yet implemented, only used by `dix-LR-restriction-copy'."
  :type 'integer
  :group 'dix)
(defcustom dix-pp-align-column 12 "Column to align pardef <p> elements to with `align'."
  :type 'integer
  :group 'dix)
(defcustom dix-pb-align-column 10 "Column to align bidix <p> elements to with `align'."
  :type 'integer
  :group 'dix)

(defun dix-add-align-rule (name regexp column)
  (add-to-list 'align-rules-list
	       `(,name
		 (regexp . ,regexp)
		 (tab-stop . nil)
		 (spacing . 0)
		 (group . 1)
		 (modes . '(nxml-mode))
		 (column . ,column))))
(add-hook
 'align-load-hook
 (lambda ()
  (dix-add-align-rule
    'dix-rp-align "\\s-+\\(\\s-*\\)<r>" 'dix-rp-align-column)
   (dix-add-align-rule                  ;
    'dix-rb-align "\\(\\s-*\\)<r>" 'dix-rb-align-column)
   (dix-add-align-rule
    'dix-i-align "\\(\\s-*\\)<i" 'dix-i-align-column)
   (dix-add-align-rule
    'dix-pb-align "^\\S-*\\(\\s-*\\)<p>" 'dix-pb-align-column)
   (dix-add-align-rule
    'dix-pp-align "^\\s-+\\S-*\\(\\s-*\\)<p>" 'dix-pp-align-column)))

;;; Evil integration ---------------------------------------------------------
(eval-after-load 'evil
  '(progn
     (evil-declare-motion #'dix-next)
     (evil-declare-motion #'dix-previous)
     (evil-declare-motion #'dix-move-to-top)
     (evil-declare-motion #'dix-backward-up-element)
     (evil-declare-motion #'dix-goto-pardef)))

;;; Keybindings --------------------------------------------------------------
(define-key dix-mode-map (kbd "C-c L") 'dix-LR-restriction-copy)
(define-key dix-mode-map (kbd "C-c R") 'dix-RL-restriction-copy)
(define-prefix-command 'dix-sense-prefix)
(define-key dix-mode-map (kbd "C-c s") 'dix-add-s)
(define-key dix-mode-map (kbd "C-c C") 'dix-copy)
(define-key dix-mode-map (kbd "C-c C-y") 'dix-copy-yank)
(define-key dix-mode-map (kbd "<C-tab>") 'dix-restriction-cycle)
(define-key dix-mode-map (kbd "<C-S-tab>") 'dix-v-cycle)
(define-key dix-mode-map (kbd "<S-iso-lefttab>") 'dix-v-cycle)
(define-key dix-mode-map (kbd "M-n") 'dix-next)
(define-key dix-mode-map (kbd "M-p") 'dix-previous)
(define-key dix-mode-map (kbd "C-c S") 'dix-sort-pardef)
(define-key dix-mode-map (kbd "C-c G") 'dix-goto-pardef)
(define-key dix-mode-map (kbd "C-c n") 'dix-goto-rule-number)
(define-key dix-mode-map (kbd "C-c g") 'dix-guess-pardef)
(define-key dix-mode-map (kbd "C-c x") 'dix-xmlise-using-above-elt)
(define-key dix-mode-map (kbd "C-c V") 'dix-view-pardef)
(define-key dix-mode-map (kbd "C-c W") 'dix-word-search-forward)
(define-key dix-mode-map (kbd "C-c A") 'dix-grep-all)
(define-key dix-mode-map (kbd "C-c D") 'dix-find-duplicate-pardefs)
(define-key dix-mode-map (kbd "C-c p") 'dix-add-par)
(define-key dix-mode-map (kbd "C-c C-c") 'dix-analyse)
(define-prefix-command 'dix-replace-prefix)
(define-key dix-mode-map (kbd "C-c %") 'dix-replace-prefix)
(define-key dix-mode-map (kbd "<SPC>") 'dix-insert-space)
(define-key dix-mode-map (kbd "<backspace>") 'dix-backspace)
(define-key dix-mode-map (kbd "C-c % RET") 'dix-replace-regexp-within-elt)
(define-key dix-mode-map (kbd "C-c % %") 'dix-replace-regexp-within-elt)
(define-key dix-mode-map (kbd "C-c % l") 'dix-replace-regexp-within-l)
(define-key dix-mode-map (kbd "C-c % r") 'dix-replace-regexp-within-r)
(define-key dix-mode-map (kbd "C-<") 'dix-<)
(define-key dix-mode-map (kbd "C->") 'dix->)
(define-key dix-mode-map (kbd "C-x n s") 'dix-narrow-to-sdef)

;;; Run hooks -----------------------------------------------------------------
(run-hooks 'dix-load-hook)

(provide 'dix)

;;;============================================================================

;;; dix.el ends here
