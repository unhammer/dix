;; Example init file for using Emacs with Apertium and Constraint Grammar

(package-initialize)

;; cg, hfst, dix live on the melpa package archive, so we need to add that:
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Bootstrap: ensure we have 'use-package
(unless (package-installed-p 'use-package)
  (unless (assoc 'use-package package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(setq inhibit-startup-screen t)

;; Allow fairly big files before warning
(setq large-file-warning-threshold 27000000) ; e.g. nob.dix is this big


(use-package cg
  :ensure t)


;; Optional dependency of dix-mode for faster completion:
(use-package strie
  :ensure t)

(use-package dix
  :ensure t
  :commands dix-mode
  :functions (dix-C-c-letter-keybindings)
  :mode (("\\.dix\\'" . nxml-mode)	  ; use dix-mode for these file types
         ("\\.t[0-9s]x\\'" . nxml-mode)
         ("\\.lrx\\'" . nxml-mode)
         ("\\.metalrx\\'" . nxml-mode)
         ("\\.metadix\\'" . nxml-mode)
         ("\\.multidix\\'" . nxml-mode))
  :hook ((dix-mode . dix-C-c-letter-keybindings)) ; enable optional keybindings
  :config
  (setq dix-hungry-backspace t)
  (setq nxml-slash-auto-complete-flag t)
  (add-to-list 'nxml-completion-hook 'rng-complete)
  (add-to-list 'nxml-completion-hook 't))

(use-package hfst-mode
  :ensure t
  :commands hfst-mode
  :mode (("\\.twol\\'" . hfst-mode)	; use hfst-mode for these file types
         ("\\.twolc\\'" . hfst-mode)
         ("\\.xfst\\'" . hfst-mode)
         ("\\.lexc\\'" . hfst-mode)
         ("\\-lex.txt\\'" . hfst-mode)
         ("\\-lexc.txt\\'" . hfst-mode)
         ("\\-morph.txt\\'" . hfst-mode)
         ("\\.pmscript\\'" . hfst-mode)))


;; Some handy global keybindings:
(global-set-key (kbd "C--") #'text-scale-adjust)
(global-set-key (kbd "C-+") #'text-scale-adjust)
(global-set-key (kbd "C-x g") #'goto-line)


;; Optional: nicer-looking M-x menu:
(fido-vertical-mode 1)

;; Optional: nicer tab-completion
(setf completion-styles '(basic flex)
      completion-auto-select t ;; Show completion on first call
      completion-auto-help 'visible ;; Display *Completions* upon first request
      completions-format 'one-column ;; Use only one column
      completions-sort 'historical ;; Order based on minibuffer history
      completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
      completion-ignore-case t)

(setq tab-always-indent 'complete)

(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode))

;; Save place and recent files:
(save-place-mode 1)
(use-package recentf	       ; M-x recentf-open to show recent files
  :config (recentf-mode))

(global-display-line-numbers-mode 1)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; TODO: perhaps https://github.com/joaotavora/mac-key-mode/blob/master/mac-key-mode.el
