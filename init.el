;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-
;;
;; Author: Anler Hp <inbox@anler.me>
;; URL: https://gihub.com/anler/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration of Yasu based on Anler Hp's configuration.

;;
(setq debug-on-error nil)
(transient-mark-mode 0) ;; disable transient-mark-mode 


(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/ywata/.local/bin")))


(setq redisplay-dont-pause nil)

(setq backup-inhibited t)
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-j" 'scroll-down)
(define-key global-map "\C-o" 'scroll-down)

(defun do-nothing ()
  "Do nothing interactive command to avoid ring bell for key binding."
  (interactive))


(if t
    (progn
      (define-prefix-command 'ctl-Q-keymap)
      (global-set-key (kbd "C-q") 'ctl-Q-keymap)
      (define-key ctl-Q-keymap (kbd "\C-d") #'view-window-mode)
      ;;(define-key ctl-Q-keymap (kbd "\C-e") #'eval-last-sexp)
      (define-key ctl-Q-keymap (kbd "\C-e") #'next-error)
      (define-key ctl-Q-keymap (kbd "\C-n") #'view-window-next-buffer)
      (define-key ctl-Q-keymap (kbd "\C-p") #'view-window-prev-buffer)
      (define-key ctl-Q-keymap (kbd "\C-s") #'helm-swoop)
      (define-key ctl-Q-keymap (kbd "s") #'avy-isearch)
      (define-key ctl-Q-keymap (kbd "\C-s") #'whitespace-mode)
      (setq outline-minor-mode-prefix "\C-c\C-q")
      
      (define-key global-map (kbd "C-<down-mouse-1>") #'do-nothing)
      (define-key global-map (kbd "C-<mouse-1>") #'do-nothing)
      
      )
  (progn
    ))


(when (fboundp 'window-system)
  (progn
    (dolist (key '("\C-z"))
      (global-unset-key key))
    (menu-bar-mode 1)
    (tool-bar-mode -1)))


(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;;(linum-mode -1)

;(setq debug-on-error t)
;;; Code:
(require 'package)


;(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;                    (not (gnutls-available-p))))
;       (proto (if no-ssl "http" "https")))
;  (add-to-list 'package-archives
;               (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;(add-to-list 'package-archives
;	     '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives
;;("melpa-stable" . "http://stable.melpa.org/packages/")

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(require 'use-package)
(use-package ox-gfm
  :ensure t
  :defer t)
(use-package ox-qmd
  :defer t
  :ensure t)

(use-package org-sticky-header
  :defer t
  :ensure t)
;(use-package org-recent-headings
;  :ensure)
(use-package helm-org-rifle
  :defer t
  :ensure t)

(use-package view-window
  :load-path "site-lisp/view-window"
  :demand)

;(use-package eaw
;  :load-path "site-lisp/eaw"
;  :init (eaw-fullwidth)
;  :ensure)

(use-package whitespace
  :load-path "site-lisp"
  :demand)

;;(use-package benchmark-init
;;  :ensure
;;  :config (benchmark-init/activate))

(use-package smartparens
  :ensure t
  :demand
  :config
  (add-hook 'haskell-mode-hook #'smartparens-mode)
  (add-hook 'elm-mode-hook     #'smartparens-mode)
  (add-hook 'emacs-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'idris-mode        #'smartparens-mode)
  )

(use-package rainbow-delimiters
  :ensure t
  :demand
  :config
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'elm-mode-hook     #'smartparens-mode)
  (add-hook 'idris-mode        #'smartparens-mode)
  )

(use-package open-junk-file
  :defer t
  :ensure t)

(use-package lean-mode
  :defer t
  :ensure t)

(use-package paredit-everywhere
  :defer t
  :ensure t
  :config
;;  (add-hook 'haskell-mode-hook #'paredit-everywhere-mode)
  )

;(use-package paren
;  :defer 3
;  :init
;  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;  (add-hook 'haskell-mode-hook 'show-paren-mode)
;  (add-hook 'sgml-mode-hook 'show-paren-mode))

;(use-package elec-pair
;  :defer 3
;  :init
;  (add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
;  (add-hook 'css-mode-hook 'electric-pair-mode)
;  (add-hook 'haskell-mode-hook 'electric-pair-mode))


(use-package projectile
  :defer t
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'projectile-mode)
;  (add-hook 'jdee-mode-hook    #'projectile-mode)
  )

;;(use-package malabar-mode
;;  :ensure t)

(use-package multiple-cursors
  :defer t
  :ensure t
  :config
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))


(use-package term+
  :defer t)
;; (use-package hindent
;;   :defer 3
;;   :init
;;   (add-to-list 'load-path "~/.local/bin")
;;   (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package twittering-mode
  :defer t
  :ensure t)


(use-package which-key
  :defer t
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)
  (define-key global-map (kbd "M-r")     'helm-resume)
  (define-key global-map (kbd "C-M-h")   'helm-apropos)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (setq helm-ff-newfile-prompt-p nil))

(use-package helm-ls-git
  :ensure t
  :defer t
  :bind
  (("C-q C-l" . helm-ls-git-ls))
  :config
  (global-set-key (kbd "C-x C-d") #'helm-browse-project)
  )

(use-package helm-projectile
  :ensure t
  :defer t
  :config
  (helm-projectile-on))

;;(use-package isearch+
;;  :ensure)

;;(use-package mu4e
;;  :load-path "site-lisp/mu/mu4e")

;(use-package offlineimap
;  :ensure
;  :defer 3)
;(use-package )

;; (use-package jdee
;;   :ensure
;;   :defer 3
;;   :config
;;   (setq jdee-server-dir "~/work/jdee-server/target/"
;; 	jdee-maven-program "/usr/local/bin/mvn"))


;(use-package indium
;  :ensure)

(use-package meghanada
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook
	    (lambda ()
	      ;; meghanada-mode on
	      (meghanada-mode t)
	      (add-hook 'before-save-hook 'delete-trailing-whitespace))))

(use-package agda2-mode
  :load-path "site-lisp/agda-mode"
;  :ensure t
  :config
  (bind-keys :map agda2-mode-map ("C-c C-p" . agda2-abbrevs-code-block))
;  (setq agda2-font-name "DejaVu Sans Mono")
  (load "my-agda2-abbrevs"))



(set-face-attribute 'default nil
		    :family "DejaVu Sans Mono"
		    :height 200     ;20pt
		    :weight 'normal
		    :width  'normal)

; fix \:
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x2982)
			(decode-char 'ucs #x2982))
		  "STIX")

;(load-file (let ((coding-system-for-read 'utf-8))
;                (shell-command-to-string "agda-mode locate")))


(use-package coq-commenter
  :ensure t
  :defer t)
  
(use-package company-coq
  :ensure t
  :defer t
  :config
  (company-coq-features/prettify-symbols 'off)
  (company-coq-features/smart-subscripts 'off))


(use-package proof-site
  :load-path "elpa/proof-general-20181115.1610/generic/"
;  :config
;  (("<left>" . proof-undo-last-successful-command)
;   ("<right>" . proof-assert-next-command-interactive)
;   ("S-<right>" . proot-goto-point)
  
  :init
  (setq coq-prog-name "/Users/ywata/.opam//4.05.0/bin/coqtop")
  (setq coq-indent-proofstart 0)
  (setq coq-indent-modulestart 0)
  (add-hook 'coq-mode-hook #'company-coq-mode)
  :config
  (if (boundp 'pretify-symbols-mode)
      (pretify-symbols-mode nil)))



(load "pg-ssr.el")
(use-package company-coq
  :defer t)

(use-package idris-mode
  :defer t
  :ensure t)
(use-package helm-idris
  :defer t
  :ensure t)
(use-package elm-mode
  :defer t
  :ensure t
  :mode "\\.elm")
(use-package elm-yasnippets
  :defer t
  :ensure t)

(use-package intero
  :ensure t
  :defer 3)

(use-package haskell-mode
  :ensure t
  :mode "\\.hs"
  :init
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
;  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'show-paren-mode)
;  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
;  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'intero-mode)
;  (add-hook 'haskell-mode-hook
;	    (defun haskell-project-mode ()
;	      (interactive)
;	      (when (projectile-project-p)
;		(intero-mode)
;		(flycheck-mode))))

;  :config
;  (defun haskell-mode-before-save-handler ()
;    "Function that will be called before buffer's saving."
;    (when (projectile-project-p)
;      ;(haskell-sort-imports)
;      ;(haskell-mode-stylish-buffer)
;      ))
  )

(use-package flycheck-haskell
  :ensure t
  :config
  (with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))


(use-package haskell-snippets
  :ensure t)
(use-package subword
  :ensure t
  :defer t
  :diminish subword-mode
  :init
  (add-hook 'haskell-mode-hook 'subword-mode))


;(use-package magit
;  :ensure
;  :defer 3
;  :if (display-graphic-p)
;  :init
;  (add-hook 'magit-mode-hook 'hl-line-mode)
;  :config
;  (setenv "GIT_PAGER" ""))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort))

;;(use-package git-messenger
;  :ensure t
;;  :if (display-graphic-p)
;;  :bind ("C-x g p" . git-messenger:popup-message))

;;(use-package what-the-commit
;;  :ensure t
;;  :bind ("C-x g c" . what-the-commit-insert))

;;(use-package github-browse-file
;;  :ensure t
;;  :bind ("C-x g b" . github-browse-file)
;;  :init (setq github-browse-file-show-line-at-point t))


;(use-package helm-migemo
;; :ensure t)
(use-package helm-swoop
  :ensure t
  :config
  )

;(use-package avy
;  :ensure)

;(use-package ivy
;  :ensure)

;(use-package ace-isearch
;  :ensure
;  :config
;  (global-ace-isearch-mode 1)
;  (setq ace-isearch-function #'avy-goto-char)
;  )

;;(use-package programmer-dvorak
;;  :ensure
;;  :config
;;  (set-input-method 'programmer-dvorak)
;;  )
(use-package yw-dvroak
  :load-path "site-lisp"
  :defer 10)

(use-package volatile-highlights
  :ensure t
  :defer t
  :config
  (volatile-highlights-mode))

(use-package highlight-symbol
  :ensure t
  :defer t
  :bind (("C-q C-w" . highlight-symbol-at-point))
  :config
  (add-hook 'haskell-mode-hook #'highlight-symbol-mode))

;;(use-package live-code-talks
;;  :ensure)

;; (use-package key-chord
;;   :ensure
;;   :defer 10
;;   :config
;; ;;  (setq key-chord-two-keys-delay 0.5)
;; ;;  (key-chord-define-global "gl" #'goto-line)
;; ;;  (key-chord-define-global "fj" #'goto-line)
;; )
(use-package z3-mode
  :ensure t
  :defer t
  :defer 10)

;;(use-package plantuml-mode
;;  :ensure
;;  :defer 10)

;;(use-package ssh
;;  :ensure
;;  :defer 10)
;;(use-package ssh-tunnels
;;  :ensure
;;  :defer 10)

;;(use-package ox-textile
;;  :ensure
;;  :defer 30)

;;(use-package typit
;;  :ensure
;;  :defer 30)


;; (use-package ace-jump-mode
;; 	     :ensure t
;; 	     :config
;; 	     (require 'cl)
;; 	     (add-hook 'haskell-mode-hook #'ace-jump-mode)
;; 	     (add-hook 'emacs-mode-hook #'ace-jump-mode)

;; 	     (defun add-keys-to-ace-jump-mode (prefix c &optional mode)
;; 	       (define-key global-map
;; 		 (read-kbd-macro (concat prefix (string c)))
;; 		 `(lambda ()
;; 		    (interactive)
;; 		    (funcall (if (eq ',mode 'word)
;; 				 #'ace-jump-word-mode
;; 			       #'ace-jump-char-mode) ,c))))

;; 	     (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-" c))
;; 	     (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-" c))
;; 	     (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-M-" c 'word))
;; 	     (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-M-" c 'word)))

(setq x->bool (lambda (elt) (not (not elt)))
      darwin-p  (eq system-type 'darwin)

      ns-p      (eq window-system 'ns)
      mac-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
		  (let ((file "/proc/modules"))
		    (and
		     (file-readable-p file)
		     (x->bool
		      (with-temp-buffer
			(insert-file-contents file)
			(goto-char (point-min))
			(re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

(when (and darwin-p (or ns-p mac-p))
  (progn
;    (set-fontset-font "fontset-default" nil
;                      (font-spec :name "DejaVu Sans"))    
;    (add-to-list 'default-frame-alist
; 		 '(font . "-*-*-*-*-*-*-16-*-*-*-m-*-iso10646-1"))
;    (add-to-list 'default-frame-alist
; 		 '(font . "-*-*-*-*-*-*-20-*-*-*-*-*-*-*"))
    (add-to-list 'default-frame-alist '(width . 140))
    (add-to-list 'default-frame-alist '(height . 50))
    (add-to-list 'default-frame-alist '(top . 0))
    (add-to-list 'default-frame-alist '(left . 0))

    (setq ns-alternate-modifier 'meta
	  ns-command-modifier 'meta
	  ns-right-command-modifier 'super
	  ns-right-alternate-modifier 'hyper
    )))
(when mac-p
  (progn
    (mac-auto-ascii-mode 1)
    (x-focus-frame nil)
))

(if (boundp 'mouse-wheel-tilt-scroll)
    (setq mouse-wheel-tilt-scroll t))

(put 'downcase-region 'disabled nil)

;;;;;;;; drop code from here.
