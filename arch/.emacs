;;; .gnus -- Emacs configuration

;; Author: Deyaaeldeen Almahallawi

;;; Commentary:
;;
;;  This setup needs the following software to be installed in your
;;  system:
;;  o texlive: LaTeX
;;  o languagetool: for proofreading
;;
;;  To install, put this file in your home directory.
;;--------------------------------------------------------------------------
;;
;;  TODO:
;;
;;  None.
;;--------------------------------------------------------------------------

;;; Code:

(require 'cl)

(setq user-full-name "Deyaaeldeen Almahallawi"
      user-mail-address "dalmahal@indiana.edu"
      package-enable-at-startup nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

;; Bootstrap  use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-minimum-reported-time 0
      use-package-verbose t)

(use-package ac-math
  :ensure t
  :defer t)

(use-package ac-slime
  :ensure t
  :defer t)

(use-package auctex
  :ensure auctex
  :defer t)

(use-package auctex-latexmk
  :ensure t
  :defer t)

(use-package auto-complete
  :ensure t
  :config
  (setq ac-math-unicode-in-math-p t
	global-auto-complete-mode t)
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-set-trigger-key "C-o"))

(use-package auto-complete-auctex
  :ensure t
  :defer t)

(use-package autodisass-llvm-bitcode
  :ensure t
  :defer t)

(use-package bbdb
  :ensure t
  :defer t
  :config
  (setq
   bbdb-use-pop-up nil
   bbdb-offer-save 1                        ;; 1 means save-without-asking
   bbdb-always-add-address t                ;; add new addresses to existing...
   bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
   bbdb-completion-type nil                 ;; complete on anything
   bbdb-complete-name-allow-cycling t       ;; cycle through matches
   bbbd-message-caching-enabled t           ;; be fast
   bbdb-use-alternate-names t               ;; use AKA
   ;; auto-create addresses from mail
   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
   bbdb-ignore-some-messages-alist
   '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))))

(use-package bibtex
  :ensure t
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :config
  (setq bibtex-align-at-equal-sign t)
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package dired
  :defer
  :config
  (setq dired-listing-switches "-lGh --group-directories-first"))

(use-package dired+
  :ensure t
  :defer t
  :config
  (setq diredp-wrap-around-flag t))

(use-package ebib
  :ensure t
  :defer t)

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds
	'("http://endlessparentheses.com/atom.xml" ;; Emacs Blog
	  "http://www.masteringemacs.org/feed/"    ;; Emacs Blog
	  "http://emacs-fu.blogspot.com/feeds/posts/default"
	  "http://emacsredux.com/atom.xml"         ;; Emacs Blog
	  "http://www.lunaryorn.com/feed.atom"     ;; Emacs Blog
	  "http://www.reddit.com/r/haskell/.rss"
	  "http://www.reddit.com/r/compilers/.rss"
	  "http://www.reddit.com/r/emacs/.rss"
	  "http://www.reddit.com/r/llvm/.rss"
	  "http://www.reddit.com/r/cpp/.rss"
	  )))

(use-package emamux
  :ensure t
  :defer t)

(use-package epa
  :defer t
  :config
  (setq mml2015-use 'epg
	mml2015-verbose t
	epg-user-id indiana-pk
	mml2015-encrypt-to-self t
	mml2015-always-trust nil
	mml2015-cache-passphrase t
	mml2015-passphrase-cache-expiry '36000
	mml2015-sign-with-sender t
	gnus-message-replyencrypt t
	gnus-message-replysign t
	gnus-message-replysignencrypted t
	gnus-treat-x-pgp-sig t
	;;       mm-sign-option 'guided
	;;       mm-encrypt-option 'guided
	mm-verify-option 'always
	mm-decrypt-option 'always
	epg-debug t ;;  then read the *epg-debug*" buffer
	gnus-buttonized-mime-types
	'("multipart/alternative" "multipart/encrypted" "multipart/signed"
	  ".*/signed")))

(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (hook-into-modes 'fci-mode '(prog-mode-hook)))

(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode 1))

(use-package flx-isearch
  :ensure t
  :bind (("C-M-s" . flx-isearch-forward)
	 ("C-M-r" . flx-isearch-backward)))

(use-package flycheck
  :ensure t
  :init
  (hook-into-modes 'flycheck-mode '(prog-mode-hook))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
		flyspell-issue-message-flag nil
		flycheck-display-errors-delay .2))

(use-package flyspell
  :ensure t
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

(use-package gist
  :ensure t
  :bind ("C-c g" . gist-region-or-buffer-private)
  :config
  (setq gist-view-gist t))

(use-package gnus
  :bind ("C-c s" . gnus))

(use-package google-c-style
  :ensure t
  :defer t)

(use-package google-translate
  :ensure t
  :defer t)

(use-package gscholar-bibtex
  :ensure t
  :bind ("C-c g" . gscholar-bibtex)
  :config
  (setq gscholar-bibtex-default-source "Google Scholar"
	gscholar-bibtex-database-file "/mnt/disk/Work/papers/refs.bib"))

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook '(lambda () (setq tab-width 2))))
  :config
  (progn
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-tags-on-save t))
    (define-key haskell-mode-map (kbd "C-c C-l")
     'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z")
      'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t")
      'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i")
      'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c")
      'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c")
      'haskell-process-cabal)
    (define-key haskell-mode-map (kbd "SPC")
      'haskell-mode-contextual-space)
    (define-key haskell-mode-map (kbd "C-c C-s") 'haskell-hoogle)
    (setq haskell-hoogle-command "hoogle")
    (eval-after-load 'haskell-cabal
      '(progn
         (define-key haskell-cabal-mode-map (kbd "C-c C-z")
           'haskell-interactive-switch)
         (define-key haskell-cabal-mode-map (kbd "C-c C-k")
           'haskell-interactive-mode-clear)
         (define-key haskell-cabal-mode-map (kbd "C-c C-c")
           'haskell-process-cabal-build)
         (define-key haskell-cabal-mode-map (kbd "C-c c")
           'haskell-process-cabal)))
    (custom-set-variables '(haskell-process-type 'cabal-repl))))

(use-package helm-bibtex
  :ensure t
  :defer t
  :bind ("C-c b" . helm-bibtex)
  :config
  (setq helm-bibtex-bibliography "/mnt/disk/Work/papers/refs.bib"))

(use-package ido
  :init
  (ido-mode t)
  :config
  (setq ido-use-virtual-buffers t
	ido-auto-merge-work-directories-length -1
	ido-create-new-buffer 'always
	ido-use-filename-at-point 'guess
	ido-use-url-at-point t
	ido-save-directory-list-file (concat user-emacs-directory "ido.last")
	ido-use-faces nil))

(use-package ido-ubiquitous
  :ensure t
  :init
  (ido-ubiquitous-mode 1))

(use-package langtool
  :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar "/usr/share/java/languagetool/languagetool-commandline.jar"
	langtool-java-classpath
          "/usr/share/languagetool:/usr/share/java/languagetool/*"
	  langtool-mother-tongue "en"))

(use-package latex-math-preview
  :ensure t
  :defer t)

(use-package latex-pretty-symbols
  :ensure t
  :defer t)

(use-package latex-preview-pane
  :ensure t
  :defer t)

(use-package llvm-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :bind ("C-x C-g" . magit-status)
  :config
  (defun add-PR-fetch ()
    "If refs/pull is not defined on a GH repo, define it."
    (let ((fetch-address
	   "+refs/pull/*/head:refs/pull/origin/*"))
      (unless (member
	       fetch-address
	       (magit-get-all "remote" "origin" "fetch"))
	(when (string-match
	       "github" (magit-get "remote" "origin" "url"))
	  (magit-git-string
	   "config" "--add" "remote.origin.fetch"
	   fetch-address)))))
  (add-hook 'magit-mode-hook #'add-PR-fetch))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'spell-check-and-wrap-at-80)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))))

(use-package paradox
  :ensure t
  :defer t)

(use-package python-mode
  :ensure t
  :defer t)

(use-package racket-mode
  :ensure t
  :defer t)

(use-package recentf
  :ensure t
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 300))

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package scala-mode2
  :ensure t
  :defer t)

(use-package screenshot
  :ensure t
  :defer t)

(use-package smex
  :ensure t
  :demand t
  :bind ("M-x" . smex)
  :init
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package sml-mode
  :ensure t
  :defer t)

(use-package w3m
  :ensure t
  :defer t)

(use-package writegood-mode
  :ensure t
  :defer t)

;; (setq agda2-include-dirs (list "." (expand-file-name "~/agda-stdlib-0.8.1/src")))
;; (load-file (let ((coding-system-for-read 'utf-8))
;; 	     (shell-command-to-string "agda-mode locate")))
;; (require 'agda-input)

(require 'auto-complete-auctex)
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (require 'ac-math)
	     (add-to-list 'ac-modes 'latex-mode)
	     (setq ac-sources (append '(ac-source-math-unicode
					ac-source-math-latex
					ac-source-latex-commands)
				      ac-sources)
		   fill-column 72
		   compile-command "latexmk -pdf"
		   TeX-auto-save t
		   TeX-parse-self t
		   TeX-save-query nil
		   TeX-PDF-mode t)
	     (setq-default TeX-master nil)
	     (turn-on-auto-fill)
	     (latex-preview-pane-enable)
	     (define-key LaTeX-mode-map (kbd "C-c C-k") 'compile)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)

(show-paren-mode 1)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(delete-selection-mode t)
(transient-mark-mode t)
(global-hl-line-mode 1)
(display-time)
(display-battery-mode)
(visual-line-mode t)

(setq column-number-mode t
      scroll-step 1
      scroll-conservatively 10000
      inhibit-splash-screen t
      initial-scratch-message nil
      ;initial-major-mode 'gnus
      echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t
      x-select-enable-clipboard t
      vc-follow-symlinks t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq version-control t ;; Use version numbers for backups.
      kept-new-versions 10 ;; Number of newest versions to keep.
      kept-old-versions 0 ;; Number of oldest versions to keep.
      delete-old-versions t) ;; Don't ask to delete excess backup versions.

(global-set-key "\C-xl" 'goto-line)
(global-set-key "\M-r" '(lambda () (interactive) (load-file "~/.emacs")))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-c i") '(lambda () (interactive) (indent-region (point-min) (point-max))))

;; disable some annoying keychords
(global-unset-key "\^z")
(global-unset-key "\C-x\C-z")

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode 0)
  (blink-cursor-mode 0))

(defun change-colors (c1 c2)
  (interactive "sback: \nsfore: ")
  (modify-frame-parameters (selected-frame)
                 (list (cons 'background-color
                         c1)
                   (cons 'foreground-color
                         c2)))
  (or window-system
      (face-set-after-frame-default (selected-frame))))

(custom-set-faces
  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :height 120 :family "liberation mono"))))
  '(background "blue")
  '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
  '(font-lock-comment-face ((t (:foreground "darkred"))))
  '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
  '(font-lock-doc-string-face ((t (:foreground "lightblue"))))
  '(font-lock-function-name-face ((t (:foreground "blue"))))
  '(font-lock-keyword-face ((t (:bold t :foreground "steelblue"))))
;  '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
  '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
  '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
  '(font-lock-string-face ((t (:foreground "Aquamarine4")))))
