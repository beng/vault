(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(use-package magit
  :ensure t)

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save t)
;;   :config
;;   (package-initialize)
;;   (elpy-enable)
;;   (setenv "WORKON_HOME" "~/.pyenv/versions")
;;   (setq elpy-test-pytest-runner-command '("py.test" "-s"))
;;   (setq elpy-rpc-backend "jedi"))


;; (use-package py-autopep8
;;   :ensure t
;;   ;:after (elpy)
;;   :config
;;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (pyvenv-mode 1))
;; ;; elpy + pyenv
;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   (setenv "WORKON_HOME" "~/.pyenv/versions")
;;   (pyenv-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase .5)
  (setq company-idle-delay .5)
  (setq company-minimum-prefix-length 1))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package lsp-mode
  :hook (
	 ((python-mode c-mode) . lsp)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :commands lsp
  :custom
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024)))

(use-package company-lsp
  :config
  (setq lsp-prefer-capf t)
  (push 'company-lsp company-backends))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :hook
    (lsp-mode . lsp-ui-mode))
  ;; :config
  ;;   ;; disable documentation popup on hover
  ;;   (setq lsp-ui-doc-enable nil))



;; (use-package lsp-mode
;;   :commands (lsp)
;;   :hook
;;   (add-hook 'python-mode-hook #'lsp)
;;   :config
;;   (add-hook 'python-mode-hook #'lsp)
;;   (require 'lsp-clients)
;;   (use-package company-lsp
;;      :config
;;      (push 'company-lsp company-backends))
;;   (use-package lsp-ui
;;     :commands lsp-ui-mode
;;     :config
;;     ;; disable documentation popup on hover
;;     (setq lsp-ui-doc-enable nil)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package perspective
  :ensure t
  :config
  (persp-mode))

;; fixes issue with PATH and env vars not being
;; inherited correctly
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(cond
 ((eq window-system 'ns) ; macosx
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")))

;; Merlin configuration

;;(global-auto-complete-mode t)

;;(use-package merlin
;;  :ensure t)

;; this was a MASSIVE PITA to setup, took multiple hours and UTOP still doenst work great
;; guides that were used
;; https://discuss.ocaml.org/t/using-emacs-for-ocaml-development/726/4
;; https://github.com/seth/my-emacs-dot-d/blob/master/emacs-init.org
;; https://emacs.stackexchange.com/questions/12084/how-to-get-merlin-mode-to-work-in-emacs
;;
;; for loading various files and stuff
;; https://github.com/ocaml-community/utop/issues/4

(push "~/.opam/default/share/emacs/site-lisp/" load-path)

(with-eval-after-load 'merlin
  (use-package flycheck-ocaml
    :ensure t
    :hook
    ((reason-mode tuareg-mode caml-mode) . merlin-mode)
    :config
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)

    ;; Enable Flycheck checker
    (flycheck-ocaml-setup)))

(use-package tuareg
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
  (setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
  (autoload 'utop-minor-mode "utop" "Toplevel for OCaml" t)
  ;; (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (setq merlin-error-after-save nil))

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

(add-hook 'after-init-hook 'global-company-mode)

; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;; rust setup
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :hook
  (rust-mode . lsp)
  (rust-mode . (lambda () (setq tab-width 4)))
  :config
  (setq rust-format-on-save t))
  ; rust-lsp has indent at 8
  ;(setq rust-indent-offset tab-width)
;; (add-hook 'rust-mode-hook (lambda () (setq tab-width 4)))
;;
;; (defcustom rust-indent-offset 4
;;   "*Indent Rust code by this number of spaces."
;;   :group 'rust-mode)


(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode)
  (toml-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;; golang setup
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :ensure t
  :bind
  ("C-c C-j" . lsp-find-definition)
  ("C-c C-d" . lsp-describe-thing-at-point)

  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :init
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v && go test -v && go vet")
  (add-hook 'before-save-hook 'gofmt-before-save))

(let ((govet (flycheck-checker-get 'go-vet 'command)))
  (when (equal (cadr govet) "tool")
    (setf (cdr govet) (cddr govet))))

(use-package rjsx-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(use-package key-chord
  :ensure t
  :init
  :config
  (key-chord-mode 1))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;;(define-key evil-insert-state-map "jk" 'evil-normal-state)
  (setq key-chord-two-keys-delay .05)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

  ;; unbind M-. and M-, so that jump to definition works as expected
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "M-.") nil)
    (define-key evil-normal-state-map (kbd "M-,") nil))
  (evil-ex-define-cmd "q[uit]" 'kill-buffer)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ian/save-and-kill-this-buffer))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-company-use-tng nil)
  (evil-collection-init 'vterm)
  :config
  (evil-collection-init))

;;;;;;;;;;
;; terraform mode
;;;;;;;;;;
(use-package terraform-mode
  :ensure t
  :init
  (setq terraform-indent-level 4))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"

  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(counsel-M-x :which-key "counsel mx")
  "f"   '(counsel-find-file :which-key "find file")
  "b"   '(ivy-switch-buffer :which-key "switch buffers")
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wx"  '(delete-window :which-key "delete window")
  "es"  '(eshell :which-key "open eshell in fullscreen")
  "tff" '(toggle-frame-fullscreen :which-key "toggle frame fullscreen")
  "wr"  '(winner-redo :which-key "winner redo")
  "wu"  '(winner-undo :which-key "winner undo")
  "ww"  '(save-buffer :which-key "save buffer")
  "ss"  '(swiper :which-key "swiper search")
  "gh"  '(counsel-org-goto-all :which-key "counsel org go-to all")
  "ptt" '(python-pytest :which-key "python pytest")
  "ptf" '(python-pytest-file :which-key "python pytest file")
  "mg"  '(magit-status :which-key "magit status")))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-git-submodule-command nil)
  :init
  (setq projectile-completion-system 'ivy))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :ensure t)

;; used for autocomplete with certain languages like javascript
(use-package eglot)

(use-package company-box
  :diminish
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook (company-mode . company-box-mode))

(use-package winner
  :init (winner-mode t))

(use-package shell-pop
  :ensure t
  :bind ("C-'" . shell-pop)
  :custom
  ;(shell-pop-shell-type '("eshell" "eshell" (lambda () (eshell)))) (shell-pop-full-span t))
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda nil (vterm)))))

;; Followed the below links to get vterm installed
;; - https://develop.spacemacs.org/layers/+tools/shell/README.html
    ;; - https://stackoverflow.com/questions/40067547/glibtool-on-macbook
;; - used shellpop from here https://wolfecub.github.io/dotfiles/
(use-package vterm
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))

(add-hook 'after-make-frame-functions
          #'enable-doom-modeline-icons)


;; (use-package lsp-python-ms
;;   :ensure t
;;   ;;:after elpy
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook
;;   (python-mode . (lambda ()
;;                   (require 'lsp-python-ms)
;;                   (lsp))))
(use-package python-pytest
  :ensure t
  :after python
  :custom
  (python-pytest-arguments
   '("-s")
   '("--color=yes")))
    ;;"--failed-first"   ;; run the previous failed tests first
    ;;"--maxfail=5"))    ;; exit in 5 continuous failures in a run



(when (executable-find "ipython")
(setq python-shell-interpreter "ipython"))
(setq python-shell-interpreter-args "--simple-prompt -i")
(setq python-shell-unbuffered nil)
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-prompt-detect-enabled nil)
