;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name ""
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
(setq doom-font (font-spec :family "Monaco" :size 14)
      doom-big-font (font-spec :family "Monaco" :size 24)
      doom-big-font-increment 5
      doom-variable-pitch-font (font-spec :family "Monaco")
      doom-unicode-font (font-spec :family "Monaco"))

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

(after! good-scroll
  (good-scroll-mode 1))


(after! python
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (setq python-indent-offset 4
        python-shell-interpreter "ipython3"
        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
        python-shell-prompt-detect-enabled nil
        python-shell-prompt-detect-failure-warning nil
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8"
        flycheck-highlighting-mode "lines"
        flycheck-python-pycompile-executable "python3")

    ;; not sure if it's better to enable use this package or to use
    ;; (set-formatter!) like i previously was like so:
    ;;     (setq-hook! 'python-mode-hook +format-with-lsp nil)
    ;;     (set-formatter! 'python-mode "black -q" ))

  ;; https://gist.github.com/jordangarrison/8720cf98126a1a64890b2f18c1bc69f5
  (use-package! python-black
    :demand t
    :hook (python-mode . python-black-on-save-mode)
    :config
        ;;(set-formatter! 'python-mode #'python-black-buffer)
        (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
        (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
        (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)))



(after! rustic
  ;;(setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer))

(after! cargo
  (setq cargo-process--custom-path-to-bin "~/.cargo/bin/cargo"))

(after! rust
  ;;(setq rust-format-on-save t)
  (add-hook! :after rust-mode-hook #'lsp))
;;(add-hook! :after rust-mode-hook #'rust-enable-format-on-save))

(add-hook! rust-mode
           ;;(flycheck-rust-setup)
           (flycheck-mode)
           (cargo-minor-mode)
           (lsp)
           ;;(rust-enable-format-on-save)
           (map! :map rust-mode-map
                 "C-c C-f" #'rust-format-buffer))



;; display of certain characters and control codes to UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))


(add-hook 'term-exec-hook 'my-term-use-utf8)

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

;; disable line numbering. scrolling is very slow on a big monitor
;; and disabling line numbering has been a way to reduce lag
(setq display-line-numbers-type nil)

;; insert timestamp when status set to closed in org mode
(setq org-log-done 'time)

(after! evil
  (setq evil-escape-delay .5)
  (setq evil-escape-key-sequence "jj"))

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        ;;company-dabbrev-downcase nil
        company-tooltip-limit 20
        company-tooltip-minimum-width 15
        company-tooltip-align-annotations t))


(after! sh
  (add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode)))

;; https://github.com/hlissner/doom-emacs/issues/4320
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; https://github.com/akermu/emacs-libvterm/issues/313
(setq evil-insert-state-cursor '(bar "#00FF00")
      evil-visual-state-cursor '(box "#FF00FF")
      evil-normal-state-cursor '(box "#E2E8EF"))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; add some indentation on left side
(setq left-margin-width 2)
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/issues/4106
(setq-hook! 'web-mode-hook +format-with 'prettier-prettify)

;; reduce delay to show leader popup when pressing SPC
;; https://github.com/hlissner/doom-emacs/issues/1839
(require 'which-key)
(setq which-key-idle-delay 0.1)

(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)

; alias find-file to ff
(defun eshell/ff (&rest args)
  (apply #'find-file args))
