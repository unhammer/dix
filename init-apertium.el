;; Example init file for using Emacs with Apertium and Constraint Grammar

(package-initialize)

;; cg.el and use-package live on the melpa package archive
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Bootstrap: ensure we have 'use-package
(unless (package-installed-p 'use-package)
  (unless (assoc 'use-package package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(setq inhibit-startup-screen t)

(use-package cg
  :ensure t)

(setq tab-always-indent 'complete)

;; Nicer-looking completion menu:
(use-package corfu
  :ensure t
  :config (corfu-global-mode))

;; Optional – quite a lot of dependencies, but nice to see git-changed lines:
(use-package git-gutter-fringe+
  :ensure t
  :config (global-git-gutter+-mode))

;; Optional dependency of dix-mode for faster completion:
(use-package strie
  :ensure t)

(use-package dix
  :ensure t
  :commands dix-mode
  :functions (dix-C-c-letter-keybindings)
  :mode (("\\.dix\\'" . nxml-mode)
         ("\\.t[0-9s]x\\'" . nxml-mode)
         ("\\.lrx\\'" . nxml-mode)
         ("\\.metalrx\\'" . nxml-mode)
         ("\\.metadix\\'" . nxml-mode)
         ("\\.multidix\\'" . nxml-mode))
  :hook ((nxml-mode . dix-on-nxml-mode)
         (dix-mode . dix-C-c-letter-keybindings))
  :config
  (setq dix-hungry-backspace t)
  (setq nxml-slash-auto-complete-flag t)
  (add-to-list 'nxml-completion-hook 'rng-complete)
  (add-to-list 'nxml-completion-hook 't))

(use-package hfst-mode
  :ensure t
  :commands hfst-mode
  :mode (("\\.twol\\'" . hfst-mode)
         ("\\.twolc\\'" . hfst-mode)
         ("\\.xfst\\'" . hfst-mode)
         ("\\.lexc\\'" . hfst-mode)
         ("\\-lex.txt\\'" . hfst-mode)
         ("\\-lexc.txt\\'" . hfst-mode)
         ("\\-morph.txt\\'" . hfst-mode)
         ("\\.pmscript\\'" . hfst-mode)))

(global-set-key (kbd "C--") #'text-scale-adjust)
(global-set-key (kbd "C-+") #'text-scale-adjust)

(add-to-list 'safe-local-variable-values
	     '(cg-pre-pipe . "apertium -d . nob-seg | cg-conv a"))

;; Nicer-looking M-x menu:
(fido-vertical-mode 1)

(save-place-mode 1)

(global-linum-mode 1)

(unless (server-running-p)
  (server-start))

;; TODO: perhaps https://github.com/joaotavora/mac-key-mode/blob/master/mac-key-mode.el
