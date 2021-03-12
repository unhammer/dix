;;; asf.el --- Major mode for syntax highlighting Apertium Stream Format -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
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

;; This package provides a major mode for syntax highlighting Apertium
;; Stream Format.

;; Usage:
;;
;; (use-package 'asf)
;;
;; and it'll automatically syntax highlight files named .asf

;;; Code:



(defgroup asf nil
  "Major mode for syntax highlighting Apertium Stream Format."
  :tag "ASF"
  :group 'languages)

(defvar asf-font-lock-keywords `(("<\\([^<>]*\\)>"
                                  1 font-lock-function-name-face)
                                 ("\\([<>]\\)"
                                  1 font-lock-keyword-face)
                                 ("\\([~]\\)"
                                  1 font-lock-variable-name-face)
                                 ("\\([$^+/]\\)"
                                  1 font-lock-negation-char-face))
  "Default expressions to highlight in ASF modes.")

;;;###autoload
(define-derived-mode asf-mode prog-mode "ASF"
  "Major mode for syntax highlighting Apertium Stream Format."
  :group 'asf
  (setq-local font-lock-defaults '(asf-font-lock-keywords)))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asf\\'" . asf-mode))

(provide 'asf)
;;; asf.el ends here
