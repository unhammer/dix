;;; dix-yas.el --- optional yas-integration with dix.el

;; Copyright (C) 2015-2016 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
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

;; Extensions to dix.el for using it with Yasnippet.

;; Usage:

;; (require 'dix-yas)
;; (add-hook 'dix-mode-hook #'dix-maybe-yas-activate)

;;; Code:

(require 'dix)
(require 'yasnippet)

;;;============================================================================
;;;
;;; Yasnippet helpers
;;;


;;; Note, due to
;;; https://github.com/AndreaCrotti/yasnippet-snippets/issues/41 you
;;; should do (remhash 'nxml-mode yas--tables) after (yas-reload-all)

(defun dix-maybe-yas-activate ()
  "Turn on `yas-minor-mode' in `dix-mode' if possible."
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

This is meant to be used in `yas-key-syntaxes' (which see for
information on START-POINT), since the defaults don't let you
expand e.g. \"<s>\" without having whitespace before it. To use
this function, put the following in your init file:

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

(provide 'dix-yas)

;;;============================================================================

;;; dix-yas.el ends here
