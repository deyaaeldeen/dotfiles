;;; .emacs -- Emacs configuration

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
(require 'seq) ;; required for emacs25
(setq user-full-name "Deyaaeldeen Almahallawi"
      user-mail-address "dalmahal@indiana.edu"
      package-enable-at-startup nil)

(defconst ref-bib "~/Dropbox/bib/library.bib")
(defconst ref-lib "~/Dropbox/bib/lib/")

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

(use-package ace-window
  :ensure t
  :bind ("C-x x" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package auctex
  :ensure auctex
  :defer t)

(use-package auctex-latexmk
  :ensure t
  :defer t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
  :defer t)

(use-package bibtex
  :ensure t
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :config
  (setq bibtex-align-at-equal-sign t)
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))

(use-package buffer-move
  :ensure t
  :init
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

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

(use-package eimp
  :ensure t
  :defer t
  :config
  (add-hook 'image-mode-hook 'eimp-mode))

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
	  "http://www.reddit.com/r/worldnews/.rss"
	  "http://www.reddit.com/r/bloomington/.rss"
	  "http://hnapp.com/rss?q=type%3Ajob%20-senior")))

(use-package emamux
  :ensure t
  :defer t)

(use-package epa
  :defer t
  :config
  (setq mml2015-use 'epg
	mml2015-verbose t
	epg-user-id "Deyaaeldeen Almahallawi <dalmahal@indiana.edu>"
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
  (flycheck-add-mode 'html-tidy 'html-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc racket)
		flyspell-issue-message-flag nil
		flycheck-display-errors-delay .2
		flycheck-check-syntax-automatically '(save mode-enabled))
  (global-flycheck-mode t))

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
  :bind (("C-c s" . gnus) ("C-c c" . browse-url)))

(use-package google-c-style
  :ensure t
  :defer t)

(use-package google-translate
  :ensure t
  :defer t
  :bind ("C-c t" . google-translate-at-point))

(use-package gscholar-bibtex
  :ensure t
  :bind ("C-c g" . gscholar-bibtex)
  :config
  (setq gscholar-bibtex-default-source "Google Scholar"
	gscholar-bibtex-database-file ref-bib))

(use-package guide-key
  :ensure t
  :init
  (guide-key-mode 1)
  :config
  (setq guide-key/guide-key-sequence t))

(use-package hi2
  :ensure t
  :defer t)

(use-package intero
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    ;; (add-hook 'haskell-mode-hook 'intero-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)
    ;; (add-hook 'haskell-mode-hook 'turn-on-hi2)
    (add-hook 'haskell-mode-hook '(lambda () (setq tab-width 2)))
    (custom-set-variables
     '(haskell-process-suggest-hoogle-imports t)
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-tags-on-save t)
     '(haskell-process-type 'stack-ghci))
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
           'haskell-process-cabal)))))

(use-package helm-bibtex
  :ensure t
  :defer t
  :bind ("C-c b" . helm-bibtex)
  :config
  (setq helm-bibtex-bibliography ref-bib
	helm-bibtex-library-path ref-lib)
  ;; (setq helm-bibtex-pdf-open-function 'org-open-file)
  (setq helm-bibtex-notes-path "~/Dropbox/bib/helm-bibtex-notes"))

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

(use-package latex-preview-pane
  :ensure t
  :defer t)

(use-package latex-pretty-symbols
  :ensure t
  :defer t)

(use-package llvm-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :bind ("C-x C-g" . magit-status))

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

(use-package nyan-mode
  :ensure t
  :init (nyan-mode))

(use-package org
  :config
  (require 'org-ref)
  (setq org-support-shift-select t
	org-completion-use-ido t
	org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t))))

(use-package org-ref
  :ensure t
  :bind (("<f10>" . org-ref-open-bibtex-notes)
	 ("<f11>" . org-ref-open-bibtex-pdf)
	 ("<f12>" . org-ref-open-in-browser))
  :config
  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/bib/notes.org"
	org-ref-default-bibliography (cons ref-bib '())
	org-ref-pdf-directory ref-lib)
  ;; optional but very useful libraries in org-ref
  (require 'doi-utils)
  (require 'org-ref-pdf)
  (require 'org-ref-bibtex)
  (require 'org-ref-arxiv))

(use-package paradox
  :ensure t
  :defer t)

(use-package pdf-tools
  :ensure t
  :init
  (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
  :mode (("\\.pdf$" . pdf-view-mode))
  :config
  (setq pdf-view-display-size 'fit-width
  	pdf-view-continuous t
  	pdf-cache-image-limit 64
  	pdf-cache-prefetch-delay 0.5
  	pdf-view-image-relief 0
  	pdf-view-bounding-box-margin 0.05
  	pdf-view-use-scaling t
  	pdf-outline-buffer-indent 2
  	pdf-outline-display-labels t ; the outline should display labels instead of page numbers.
  	pdf-outline-enable-imenu t
  	pdf-outline-imenu-use-flat-menus nil))

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/bin/fish")
  :bind ("C-c z" . multi-term)
  :config
  (defun term-send-Mright () (interactive) (term-send-raw-string "\e[1;3C"))
  (defun term-send-Mleft  () (interactive) (term-send-raw-string "\e[1;3D"))
  (defun term-send-f11 () (interactive) (term-send-raw-string "\e[23~"))
  (defun term-send-f12 () (interactive) (term-send-raw-string "\e[24~"))
  (setq term-buffer-maximum-size 10000
	term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
	term-bind-key-alist '(("<f12>" . term-send-f12)
			      ("<f11>" . term-send-f11)
			      ("<M-right>" . term-send-Mright)
			      ("<M-left>" . term-send-Mleft)
			      ("M-]" . multi-term-next)
			      ("M-[" . multi-term-prev)
			      ("C-c C-c" . term-interrupt-subjob)
			      ("C-c C-e" . term-send-esc)
			      ("C-p" . previous-line)
			      ("C-n" . next-line)
			      ("C-s" . isearch-forward)
			      ("C-r" . isearch-backward)
			      ("C-c C-j" . term-line-mode)
			      ("C-c C-k" . term-char-mode)
			      ("C-m" . term-send-return)
			      ("C-y" . term-paste)
			      ("M-f" . term-send-forward-word)
			      ("M-b" . term-send-backward-word)
			      ("M-o" . term-send-backspace)
			      ("M-p" . term-send-up)
			      ("M-n" . term-send-down)
			      ("M-M" . term-send-forward-kill-word)
			      ("M-N" . term-send-backward-kill-word)
			      ("<C-backspace>" . term-send-backward-kill-word)
			      ("M-r" . term-send-reverse-search-history)
			      ("M-," . term-send-raw)
			      ("M-." . comint-dynamic-complete)))
  (setq truncate-lines 1))

(use-package python-mode
  :ensure t
  :defer t)

(use-package racket-mode
  :ensure t
  :defer t
  :config
  (global-eldoc-mode 0))

(use-package recentf
  :ensure t
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (recentf-mode 1))

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package scala-mode
  :ensure t
  :defer t)

(use-package screenshot
  :ensure t
  :defer t)

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t
	sml/name-width 10)
  (sml/setup))

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

(use-package workgroups2
  :ensure t
  :init
  (wg-reload-session) ;; tricking it to load
  :config
  (wg-find-session-file "~/.emacs_workgroups")
  (setq wg-prefix-key (kbd "C-c x")
	wg-mode-line-decor-left-brace "["
	wg-mode-line-decor-right-brace "]"  ; how to surround it
	wg-mode-line-decor-divider ":"
	wg-mode-line-display-on t
	wg-flag-modified t)
  (workgroups-mode 1)
  :bind ("C-x C-l" . wg-reload-session))

(use-package writegood-mode
  :ensure t
  :defer t)

;; (setq agda2-include-dirs (list "." (expand-file-name "~/agda-stdlib-0.11/src")))
;; (load-file (let ((coding-system-for-read 'utf-8))
;; 	     (shell-command-to-string "agda-mode locate")))
;; (require 'agda-input)

(use-package tex-site
  :config
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
	       (setq TeX-source-correlate-method 'synctex)
	       (setq reftex-default-bibliography (cons ref-bib '()))
	       (turn-on-auto-fill)
	       (define-key LaTeX-mode-map (kbd "C-c C-k") '(lambda () (interactive)
							     (save-window-excursion
							       (recompile))))
	       (require 'auctex-latexmk nil 'noerror)
	       (auctex-latexmk-setup)
	       (setq-default TeX-master nil)
	       (setq TeX-command-default "LatexMk"
		     auctex-latexmk-inherit-TeX-PDF-mode t)
	       (setq-default TeX-master nil)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))

(use-package xclip
  :ensure t)

;; (use-package unicode-fonts
;;   :ensure t
;;   :init (unicode-fonts-setup))

(use-package zoom-window
  :ensure t
  :bind ("C-x d" . zoom-window-zoom))

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
      scroll-conservatively 101
      inhibit-splash-screen t
      initial-scratch-message nil
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

(fset 'yes-or-no-p 'y-or-n-p)

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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :height 120 :family "liberation mono"))))
 '(background "blue")
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "darkred"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-doc-string-face ((t (:foreground "lightblue"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "steelblue"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "Aquamarine4")))))
(set-face-attribute 'default nil
                    :family "Inconsolata" :height 145 :weight 'normal)

(defun connect-silo ()
  (interactive)
  (dired "/dalmahal@silo.soic.indiana.edu:/u/dalmahal"))

(defun connect-chris ()
  (interactive)
  (dired "/deyaa@129.79.241.140:/home/deyaa"))

(defun connect-cluster ()
  (interactive)
  (dired "/ec2-user@52.43.69.198:/tmp/ec2-user/leapyear-integration-tests/ZZAgbY"))


(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-c C-r") 'sudo-edit)

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(global-set-key (kbd "C-x t") 'window-split-toggle)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(defun set-mac-font (name  size)
    (interactive
     (list (completing-read "font-name: " (mapcar (lambda (n) (list n n)) (mapcar (lambda (p) (car p)) (font-family-list))) nil t) 
           (read-number "size: " 12)))
    (set-face-attribute 'default nil 
                        :family name
                        :slant  'normal
                        :weight 'normal
                        :width  'normal
                        :height (* 10 size)))

(set-mac-font "bitstream vera sans mono" 18)

(setq x-fixed-font-alist
       '("Font Menu"
 	("Misc"
	 ("Bitstream Vera Sans Mono 10" "-apple-bitstream vera sans mono-medium-r-normal--10-180-72-72-m-180-iso10646-1")
	 ("Bitstream Vera Sans Mono 12" "-apple-bitstream vera sans mono-medium-r-normal--12-180-72-72-m-180-iso10646-1")
	 ("Bitstream Vera Sans Mono 14" "-apple-bitstream vera sans mono-medium-r-normal--14-180-72-72-m-180-iso10646-1")
	 ("Bitstream Vera Sans Mono 16" "-apple-bitstream vera sans mono-medium-r-normal--16-180-72-72-m-180-iso10646-1")
	 ("Bitstream Vera Sans Mono 18" "-apple-bitstream vera sans mono-medium-r-normal--18-180-72-72-m-180-iso10646-1")
 	 )))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (csv-mode zoom-window xclip writegood-mode workgroups2 w3m use-package unicode-fonts sml-mode smex smart-mode-line screenshot scala-mode racket-mode python-mode pdf-tools paradox org-ref nyan-mode multi-term markdown-mode magit llvm-mode latex-preview-pane latex-pretty-symbols latex-math-preview langtool intero ido-ubiquitous hi2 guide-key gscholar-bibtex google-translate google-c-style gist flx-isearch flx-ido fill-column-indicator exec-path-from-shell emamux elfeed eimp ebib dired+ cmake-mode buffer-move bbdb autodisass-llvm-bitcode auto-complete-auctex auctex-latexmk ace-window ac-slime ac-math))))
