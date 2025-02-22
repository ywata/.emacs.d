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
;(gnutls-algorithm-priority “NORMAL:-VERS-TLS1.3) .
(setq debug-on-error nil)
(transient-mark-mode 0) ;; disable transient-mark-mode
(blink-cursor-mode 0)

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("$HOME/.local/bin")))


; flicker(setq redisplay-dont-pause nil)
(modify-all-frames-parameters '((inhibit-double-buffering . nil)))
;(setq redisplay-dont-pause nil)

(setq backup-inhibited t)
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-j" 'scroll-down)
(define-key global-map "\C-o" 'scroll-down)
(define-key global-map "\C-l" #'recenter)

(defun do-nothing ()
  "Do nothing interactive command to avoid ring bell for key binding."
  (interactive))

(prefer-coding-system 'utf-8)
(if t
    (progn
      (define-prefix-command 'ctl-Q-keymap)
      (global-set-key (kbd "C-q") 'ctl-Q-keymap)
      (define-key ctl-Q-keymap (kbd "\C-d") #'view-window-mode)
      (define-key ctl-Q-keymap (kbd "\C-e") #'eval-last-sexp)
      (define-key ctl-Q-keymap (kbd "\C-e") #'next-error)

      (define-key ctl-Q-keymap (kbd "\C-n") #'bs-cycle-next)
      (define-key ctl-Q-keymap (kbd "\C-p") #'bs-cycle-previous)
      (define-key ctl-Q-keymap (kbd "\C-s") #'whitespace-mode)
      (define-key ctl-Q-keymap (kbd "\C-w") #'weight-insert-default-file)

      (define-key ctl-Q-keymap (kbd "\C-q") #'quoted-insert)
      (setq outline-minor-mode-prefix "\C-c\C-q")
      (define-key global-map (kbd "C-<down-mouse-1>") #'do-nothing)
      (define-key global-map (kbd "C-<mouse-1>") #'do-nothing))
  (progn
    ))

window-system
(when (and (fboundp 'window-system) window-system)
  (progn
    (dolist (key '("\C-z"))
      (global-unset-key key))
    (menu-bar-mode 1)
    (tool-bar-mode -1)))

(show-paren-mode)
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;;(linum-mode -1)

;(setq debug-on-error t)
;;; Code:
(require 'package)

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;                    (not (gnutls-available-p))))
;       (proto (if no-ssl "http" "https")))
;  (add-to-list 'package-archives
;               (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(add-to-list 'package-archives
	     '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives
;;("melpa-stable" . "http://stable.melpa.org/packages/")

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(require 'use-package)
(use-package ox-gfm  :defer t)
(use-package ox-qmd :defer t)

(use-package gnuplot
  :config
  (add-to-list 'exec-path "~/.nix-profile/bin/"))

(use-package org-sticky-header
  :defer t)

;(use-package org-recent-headings
;  :ensure)
;(use-package helm-org-rifle
;  :ensure)


(use-package view-window
  :load-path "site-lisp/view-window"
  :demand)



(use-package flycheck
  :demand
  :config (global-flycheck-mode t))

;(use-package magit-section
					;  :ensure t)
;; (use-package lsp-mode
;;   :demand
;;   :config
;;   ;;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;;   (progn
;;     (setq gc-cons-threshold 30000000)
;;     (setq read-process-output-max 10000000)
;;     (setq lsp-idle-delay 0.2)
;;     (setq lsp-enable-symbol-highlighting nil)
;;     (setq lsp-ui-doc-enable nil)
;;     (setq lsp-ui-doc-show-with-cursor nil)
;;     (setq lsp-ui-doc-show-with-mouse nil)
;;     (setq lsp-lens-enable nil)
;;     (setq lsp-headerline-breadcrumb-enable nil)
;;     (setq lsp-ui-sideline-enable nil)
;;     (setq lsp-ui-sideline-show-code-actions nil)
;;     (setq lsp-ui-sideline-enable nil)
;;     (setq lsp-ui-sideline-show-hover nil)
;;     (setq lsp-modeline-code-actions-enable nil)
;;     (setq lsp-diagnostics-provider :none)
;;     (setq lsp-ui-sideline-enable nil)
;;     (setq lsp-ui-sideline-show-diagnostics nil)
;;     (setq lsp-eldoc-enable-hover nil)
;;     (setq lsp-modeline-diagnostics-enable nil)
;;     (setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
;;     (setq lsp-signature-render-documentation nil)
;;     (setq lsp-completion-provider :none)
;;     (setq lsp-completion-show-detail nil)
;;     (setq lsp-completion-show-kind nil))
;;   )
;(use-package magit-section
;  :demand)
;(use-package lean4-mode
;  :demand
;  :load-path "site-lisp/lean4-mode")

(use-package rustic
  :demand
  :config
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook #'company-mode)  
  (add-hook 'rustic-mode-hook #'cargo-minor-mode)
;;  (add-hook 'rustic-before-compilation-hook #'save-buffer)
  (custom-set-variables
   '(rustic-format-trigger 'on-save)))


;(use-package cargo-mode
;  :config
;  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; (use-package rust-mode
;;   :demand
;;   :config
;;   (add-hook 'rust-mode-hook
;;             (lambda () (setq indent-tabs-mode nil)))
;;   (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
;;   (define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
;;   (add-hook 'rust-mode-hook #'lsp)  
;;  )




;(use-package lean4-mode
;  :straight (lean4-mode :type git :host github :repo "leanprover/lean4-mode")
;  ;; to defer loading the package until required
;  :ensure)

;(add-to-list 'eglot-server-programs '(lean4-mode . ("foo-language-server" "--args")))


;(use-package font-tool
;  :load-path "site-lisp/font-tool"
;  :demand)


;(use-package eaw
;  :load-path "site-lisp/eaw"
;  :init (eaw-fullwidth)
;  :ensure)

(use-package whitespace
  :load-path "site-lisp"
  :demand)

(use-package weight-helper
  :load-path "site-lisp"
  :config (setq weight-file (expand-file-name "~/Memo/weight.csv"))
  :demand)

;;(use-package benchmark-init
;;  :ensure
;;  :config (benchmark-init/activate))

;; (use-package smartparens
;;   :ensure t
;;   :demand
;;   :config
;;   (add-hook 'haskell-mode-hook #'smartparens-mode)
;;   (add-hook 'elm-mode-hook     #'smartparens-mode)
;  )

(use-package rainbow-delimiters
  :demand
  :config
  (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-mode-hook #'rainbow-delimiters-mode)
  )

(use-package open-junk-file
  :demand)

(use-package paredit-everywhere
  :demand
  :config
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


;; (use-package projectile
;;    :ensure t
;;    :config
;;    (add-hook 'haskell-mode-hook #'projectile-mode)
;;    (add-hook 'jdee-mode-hook    #'projectile-mode)
;;    (add-hook 'lean4-mode-hook #'projectile-mode)
;  )

;;(use-package malabar-mode
;;  :ensure t)

(use-package multiple-cursors
  :demand
  :config
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))


;(use-package term+
;  :defer t)
;; (use-package hindent
;;   :defer 3
;;   :init
;;   (add-to-list 'load-path "~/.local/bin")
;;   (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package which-key
  :demand
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

;; recent file mode
(progn
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 100))

(use-package helm
  :init
  :config
  (helm-mode 1)
  (define-key global-map (kbd "M-x")     'helm-M-x)

  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)
  (define-key global-map (kbd "M-r")     'helm-resume)
  (define-key global-map (kbd "C-M-h")   'helm-apropos)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (setq helm-ff-skip-boring-files t)
  (customize-set-variable 'helm-boring-file-regexp-list (add-to-list 'helm-boring-file-regexp-list "\\.agdai$"))
;   (add-to-list 'completion-ignored-extensions ".agdai")

  (setq helm-ff-newfile-prompt-p nil)
  )

(use-package helm-ls-git
  :demand
;;  :bind
;;  (("C-q C-l" . helm-ls-git-ls))
  :config
  (global-set-key (kbd "C-x C-d") #'helm-browse-project)
  )

;(use-package helm-projectile
;  :ensure t
;  :config
;  (helm-projectile-on))

;;(use-package isearch+
;;  :ensure)

;;(use-package mu4e
;;  :load-path "site-lisp/mu/mu4e")

;(use-package offlineimap
;  :ensure
;  :defer 3)
;(use-package )


(use-package elm-mode
  :demand
  :config
  (add-hook 'elm-mode-hook #'lsp)
  :mode "\\.elm")
(use-package elm-yasnippets
  :defer t)



;;(use-package lsp-ui
;;  :demand
;;  :commands lsp-ui-mode
;;  :config
;;;;  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;;;  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;  (define-key lsp-ui-imenu-mode-map (kbd "C-f") 'lsp-ui-imenu--next-kind)
;;  (define-key lsp-ui-imenu-mode-map (kbd "C-b") 'lsp-ui-imenu--prev-kind))


;; Historical haskell-mode settings
(cond
 (t
    (use-package eglot
      :ensure t
      :config
      (setq haskell-interactive-popup-errors nil)
      (define-key ctl-Q-keymap (kbd "C-e") 'flymake-goto-next-error)
      (custom-set-variables '(haskell-process-type 'cabal-repl))
      (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
      (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
      (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "lsp")))))
 (nil
  (progn
;;    (use-package flycheck
;;      :ensure t
;;      :init
;;      (global-flycheck-mode t))
;;    (use-package yasnippet
    ;;      :ensure t)
    (use-package lsp-haskell
      :demand
      :config
      (custom-set-variables '(lsp-haskell-server-path "haskell-language-server-wrapper"))
      (custom-set-variables '(lsp-haskell-server-args '()))
      ;;https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
      
      ;;(custom-set-variables '(haskell-process-type 'stack-ghci))
      
      ;; https://github.com/emacs-lsp/lsp-haskell
      (add-hook 'haskell-mode-hook #'lsp)
      (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
      (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

      (add-hook 'haskell-literate-mode-hook #'lsp)
;;      (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
;;      (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
      
      (setq lsp-prefer-flymake nil)
      
      ;; Comment/uncomment this line to see interactions between lsp client/server.
      ;;(interactive-haskell-mode t)
      ;;(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
      (add-hook 'haskell-mode-hook #'show-paren-mode)
      (setq lsp-log-io t)))))
;;(use-package haskell-snippets
(use-package subword
  :demand
  :diminish subword-mode
  :init
  (setq haskell-interactive-popup-errors nil)
  (add-hook 'haskell-mode-hook 'subword-mode))


;(use-package magit
;  :ensure
;  :if (display-graphic-p)
;  :init
;;  (add-hook 'magit-mode-hook 'hl-line-mode)
;  :config
;  (setenv "GIT_PAGER" ""))

(defun company-abort-and-delete-backward-char ()
  (interactive)
  (progn
    (company-abort)
    (delete-backward-char 1)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-h") #'company-abort-and-delete-backward-char))


(use-package volatile-highlights
  :demand
  :config
  (volatile-highlights-mode))


;; キーに登録する関数を返す関数
(defun local-switch-workspace (i)
  (let ((index i))
	       (lambda ()
		 (interactive)
		 (persp-switch (int-to-string index)))))

;; (use-package perspective
;;   :ensure t
;;   :config
;;   (progn
;;     (persp-mode 1)
;;     ;; ワークスペース生成
;;     (mapc (lambda (i)
;; 	    (persp-switch (int-to-string i)))
;; 	  (number-sequence 0 9))
;;     ;; キーバインドの登録を行う
;;     (mapc (lambda (i)
;; 	    (global-set-key (kbd (format "H-%d" i)) (local-switch-workspace i)))
;; 	  (number-sequence 0 9))
;;     ;; 最初のワークスペースは"1"に設定
;;     (persp-switch "1")))



(use-package reason-mode
  :demand)


(cond (nil
(use-package lingature
  :load-path "site-lisp/lingature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))))

(use-package protobuf-mode
  :demand)


;; Agda2 mode setup is performed at the end of the initalization
;; since .lagda.md should be before .md as a auto-mode-alist

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
	 (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)


; -- agda

(defun agda2-mode-path ()
  "Locate agda mode directory"
 (let* ((coding-system-for-read 'utf-8)
	(agda-mode-el (shell-command-to-string "agda-mode locate"))
	(agda-mode-el- (shell-command-to-string (expand-file-name "~/.local/bin/agda-mode locate"))))
   (cond ((file-exists-p agda-mode-el)
	  (file-name-directory agda-mode-el))
	 ((file-exists-p agda-mode-el-)
	  (file-name-directory agda-mode-el-))
	 (nil "~/.emacs.d/site-lisp/"))))


(defmacro use-package-agda2 (agda2-path)
  "Use use-package macro dynamically."
  `(use-package agda2-mode
      :load-path ,agda2-path
      :config
      (progn
	(define-key agda2-mode-map (kbd "C-c .") #'quail-show-key)
	(define-key agda2-mode-map (kbd "C-c /") #'agda-input-show-translations)
	(if (fboundp #'agda2-mode)
	    (add-to-list 'auto-mode-alist '("\\.lagda\\.md$" . agda2-mode))
	  (if (boundp 'agda2-mode-map)
	      (define-key agda2-mode-map (kbd "C-c C-i") #'agda2-insert-commented-region))))))
(setq agda2-path (agda2-mode-path))
;(use-package-agda2 (agda2-path))

(use-package go-mode
  :demand)


(set-face-attribute 'default nil
		    ;;:family "DejaVu Sans Mono"
		    :family "mononoki"
		    :height 200     ;20pt
		    :weight 'medium
		    :width  'normal)
(set-face-attribute 'default nil
		    :family "FiraCode"
		    :height 200     ;20pt
		    :weight 'medium
		    :width  'normal)
(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :height 160     ;20pt
		    :weight 'medium
		    :width  'normal)



; fix for DejaVu Sans Mono
;(set-fontset-font "fontset-default"
;		  (cons (decode-char 'ucs #x2982)
;			(decode-char 'ucs #x2982))
;		  "STIX")
;
;(load-file (let ((coding-system-for-read 'utf-8))
;                (shell-command-to-string "agda-mode locate")))


(defvar darwin-p (eq system-type 'darwin))
(make-variable-buffer-local 'darwin-p)
(defvar ns-p (eq window-system 'ns))
(make-variable-buffer-local 'ns-p)
(defvar mac-p (eq window-system 'mac))
(make-variable-buffer-local 'mac-p)
(defvar linux-p (eq system-type 'gnu/linux))
(make-variable-buffer-local 'linux-p)
(defvar cygwin-p (eq system-type 'cygwin))
(make-variable-buffer-local 'cygwin-p)
(defvar nt-p (eq system-type 'windows-nt))
(make-variable-buffer-local 'nt-p)
(defvar windows-p (or cygwin-p nt-p))
(make-variable-buffer-local 'windows-p)



(when (and darwin-p (or ns-p mac-p))
  (progn
    (set-fontset-font "fontset-default" nil
                      (font-spec :name "PlemolJP Console NFJ-20"))
;    (add-to-list 'default-frame-alist
; 		 '(font . "-*-*-*-*-*-*-16-*-*-*-m-*-iso10646-1"))
;    (add-to-list 'default-frame-alist
; 		 '(font . "-*-*-*-*-*-*-20-*-*-*-*-*-*-*"))
    (add-to-list 'default-frame-alist '(width . 113))
    (add-to-list 'default-frame-alist '(height . 50))
    (add-to-list 'default-frame-alist '(top . 0))
    (add-to-list 'default-frame-alist '(left . 0))
    ;(set-default line-spacing 0)
    (setq default-frame-alist
	  (append (list
		   '(font . "PlemolJP Console NFJ-20"))
		  default-frame-alist))
    (setq ns-alternate-modifier 'super
	  ns-command-modifier 'meta
	  ns-right-command-modifier 'super
	  ns-right-alternate-modifier 'hyper
    )))

(when mac-p
  (progn
    (mac-auto-ascii-mode 1)
    (x-focus-frame nil)
    ))


;; server start for emacs-client
(when window-system                       ; GUI時
  (require 'server)
  (unless (eq (server-running-p) 't)
    (server-start)

    (defun iconify-emacs-when-server-is-done ()
      (unless server-clients (iconify-frame)))

    ;; C-x C-cに割り当てる(好みに応じて)
    ;;(global-set-key (kbd "C-x C-c") 'server-edit)
    ;; M-x exitでEmacsを終了できるようにする
    ;;(defalias 'exit 'save-buffers-kill-emacs)
    ;; 起動時に最小化する
    ;;;(add-hook 'after-init-hook 'iconify-emacs-when-server-is-done)

    ;; 終了時にyes/noの問い合わせ
    ;;(setq confirm-kill-emacs 'yes-or-no-p)
    ))


(if (boundp 'mouse-wheel-tilt-scroll)
    (setq mouse-wheel-tilt-scroll t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'cabal-repl)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
