(require 'package)
(require 'cl)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; bootstrap `use-package`
(unless (package-installed-p 'use-package)
`  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; use atom theme
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

;; magit - git plugin
(use-package magit
  :ensure t)

;; elpy - python development
(use-package elpy
  :ensure t
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save t)
  :config
  (package-initialize)
  (elpy-enable)
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (setq elpy-rpc-backend "jedi"))

;; autopep8
(use-package py-autopep8
  :ensure t
  :after (elpy))

;; elpy + pyenv
(use-package pyenv-mode
  :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; perspective mode
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
(use-package flycheck-rust
	     :ensure t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'company-mode)
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq rust-format-on-save t))
