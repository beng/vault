;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Gelb"
      user-mail-address "ben@inviam.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
(setq doom-font (font-spec :family "Hack" :size 12)
      doom-big-font (font-spec :family "Hack" :size 24)
      doom-big-font-increment 5
      doom-variable-pitch-font (font-spec :family "Hack")
      doom-unicode-font (font-spec :family "Hack"))
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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Python virtual env
;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (setenv "WORKON_HOME" "~/.pyenv/versions")
;;   (pyvenv-mode 1))


(after! python
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"))
  (setenv "WORKON_HOME" "~/.pyenv/versions")

  (setq python-indent-offset 4
        python-shell-prompt-detect-enabled nil
        python-shell-interpreter-args "--simple-prompt -i"
        python-shell-unbuffered nil
        python-shell-prompt-detect-failure-warning nil
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8"))


(after! rustic
  (setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer))

;; (after! cargo
;;   (setq cargo-process--custom-path-to-bin "~/.cargo/bin/cargo"))

;; (after! rust
;;   ;;(setq rust-format-on-save t)
;;   (add-hook! :after rust-mode-hook #'lsp)
;;   (add-hook! :after rust-mode-hook #'rust-enable-format-on-save))

;; (add-hook! rust-mode
;;   (flycheck-rust-setup)
;;   (flycheck-mode)
;;   (cargo-minor-mode)
;;   (lsp)
;;   (rust-enable-format-on-save)
;;   (map! :map rust-mode-map
;;         "C-c C-f" #'rust-format-buffer))


(after! lsp-clients
  (lsp-register-client
   (make-lsp-client :new-connection
    (lsp-stdio-connection
        (expand-file-name
          "~/Documents/code/elixir-ls/language_server.sh"))
        :major-modes '(elixir-mode)
        :priority -1
        :server-id 'elixir-ls
        :initialized-fn (lambda (workspace)
            (with-lsp-workspace workspace
             (let ((config `(:elixirLS
                             (:mixEnv "dev"
                                     :dialyzerEnabled
                                     :json-false))))
             (lsp--set-configuration config)))))))


(setq flycheck-python-pycompile-executable "python3")

;; display of certain characters and control codes to UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))


(add-hook 'term-exec-hook 'my-term-use-utf8)
;;
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


;; insert timestamp when status set to closed in org mode
(setq org-log-done 'time)

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
