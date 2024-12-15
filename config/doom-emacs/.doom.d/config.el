;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



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
;; + `doom-variable-pitch-font[[id:0ad36096-ab7c-464a-8ca1-4b601d8761e2][test]]'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
(setq doom-font (font-spec :family "Fira Code Retina" :size 14)
      doom-big-font (font-spec :family "Fira Code Retina" :size 24)
      doom-big-font-increment 5
      doom-variable-pitch-font (font-spec :family "Fira Code Retina")
      doom-unicode-font (font-spec :family "FiraCode Nerd Font"))


(setq shell-file-name "/bin/zsh")

(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL"))
  (exec-path-from-shell-initialize))

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'macchiato) ;; mocha, macchiato, latte, frappe
(catppuccin-reload)

(setq
 ;; set undo limit to 10mb
 undo-limit 10000000
 ;; By default while in insert all changes are one big blob. Be more granular
 evil-want-fine-undo t
 auto-save-default t
 line-spacing 3
 lsp-diagnostics-provider :auto)

;; iterate through CamelCase words
(global-subword-mode 1)

;; new buffers open in org mode rather than fundamental mode
(setq-default major-mode 'org-mode)


;; (after! python
;;   (setenv "WORKON_HOME" "~/.pyenv/versions")
;;     (setq flycheck-enabled-checkers '(python-flake8 python-mypy))
;;   (setq flycheck-disabled-checkers '(python-pylint))
;;   (setq flycheck-python-flake8-executable "flake8")
;;   (setq python-indent-offset 4
;;         python-indent 4
;;         python-shell-interpreter "ipython3"
;;         python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
;;         python-shell-prompt-detect-enabled nil
;;         python-shell-prompt-detect-failure-warning nil
;;         flycheck-python-pylint-executable "pylint"
;;         flycheck-python-flake8-executable "flake8"
;;         flycheck-highlighting-mode "lines"
;;         flycheck-python-pycompile-executable "python3")

;; ;; https://gist.github.com/jordangarrison/8720cf98126a1a64890b2f18c1bc69f5
;; (use-package! python-black
;;   :demand t
;;   :hook (python-mode . python-black-on-save-mode)
;;   :config
;;   ;;(set-formatter! 'python-mode #'python-black-buffer)
;;   (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
;;   (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
;;   (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)))




;; display of certain characters and control codes to UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))


(add-hook 'term-exec-hook 'my-term-use-utf8)

;; ;; fixes issue with PATH and env vars not being
;; ;; inherited correctly
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; (cond
;;  ((eq window-system 'ns) ; macosx
;;   ;; Invoke login shells, so that .profile or .bash_profile is read
;;   (setq shell-command-switch "-lc")))

;; insert timestamp when status set to closed in org mode
(setq org-log-done 'time
      ;; hide * * for bold // for italics, etc
      org-hide-emphasis-markers t)

(after! evil
  (setq evil-escape-delay .5)
  (setq evil-escape-key-sequence "jj"))

(use-package! pyvenv
  :after python
  :config
  (pyvenv-tracking-mode 1))

(use-package! rg
  :config
  (rg-enable-default-bindings))

(defun my/consult-projectile-or-find ()
  "Use `consult-projectile-find-file` if in a project; otherwise, emulate its behavior using `consult-find` with ripgrep."
  (interactive)
  (if (projectile-project-p)
      ;; Use consult-projectile-find-file for Projectile projects
      (consult-projectile-find-file)
    (call-interactively #'find-file)))

;; (defun my/consult-projectile-or-find ()
;;   "Use `consult-projectile-find-file` if in a project; otherwise, use `consult-find`."
;;   (interactive)
;;   (if (projectile-project-p)
;;       (consult-projectile-find-file)
;;     ;; Fallback for non-project directories using ripgrep
;;     (let ((default-directory (or default-directory ".")))
;;       (let* ((rg-command "rg --files --hidden --glob '!.git/' --glob '!node_modules/' --glob '!venv/' .")
;;              (files (split-string (shell-command-to-string rg-command) "\n" t))
;;              ;; Use Marginalia to annotate file metadata
;;              (annotated-files
;;               (mapcar (lambda (file)
;;                         (propertize file 'consult--metadata (file-attributes file)))
;;                       files)))
;;         ;; Pass annotated files to consult--read
;;         (consult--read
;;          annotated-files
;;          :prompt "Find file: "
;;          :sort t
;;          :require-match t
;;          :category 'file
;;          :history 'file-name-history
;;          :lookup #'consult--lookup-candidate)))))

(setq grep-program "rg"
      grep-command "rg --color=auto --line-number --no-heading --smart-case --hidden")

(setq +default-search-backend 'rg)

(setq projectile-generic-command "rg --files --hidden --ignore-case --glob '!.git/' --glob '!node_modules/' --glob '!venv/'")

(setq counsel-rg-base-command
      "rg --line-number --no-heading --hidden --smart-case --glob '!.git/' --glob '!node_modules/' --glob '!venv/' %s")

(setq consult-grep-command
      "rg --null --line-buffered --color=always --max-columns=1000 --smart-case --no-heading --line-number --hidden . -e ARG")

(setq consult-ripgrep-command
      "rg --null --line-buffered --color=always --max-columns=1000 --smart-case --no-heading --line-number --hidden --glob '!.git/' --glob '!node_modules/' --glob '!venv/' . -e ARG")

(setq consult-find-command
      "rg --files --hidden --glob '!.git/' --glob '!node_modules/' --glob '!venv/' .")

(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)


(map! :leader
      :desc "Find file in project" "SPC" #'my/consult-projectile-or-find
      :desc "Find file in project" "f f" #'my/consult-projectile-or-find
      :desc "Switch project with consult" "p p" #'consult-projectile-switch-project
      :desc "Search project with ripgrep" "p s" #'consult-ripgrep
      (:prefix ("s" . "search")
       :desc "Ripgrep project" "r" #'consult-ripgrep
       :desc "Search line" "l" #'consult-line
       :desc "Search recent file" "f" #'consult-find))


;; Enable rg.el for extra functionality


;; (after! company
;;   (setq company-tooltip-limit 5
;; 	company-idle-delay 0.05
;;         company-minimum-prefix-length 2
;;         ;;company-dabbrev-downcase nil
;;         company-tooltip-minimum-width 80
;;         company-tooltip-align-annotations t)
;;   (add-to-list 'company-backends 'company-capf))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; BEGIN CUSTOMIZATION BORROWED FROM THIS CONFIG
;; ;; - https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany
;; ;; - https://www.soitflows.xyz/posts/my-doom-emacs-configuration/
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; use ripgrep as default for project search
;; (setq +ivy-project-search-engines '(rg))

;; (after! ivy
;;   (ivy-add-actions
;;    'counsel-M-x
;;    `(("h" +ivy/helpful-function "Helpful"))))


;; (after! counsel
;;   (setq counsel-evil-registers-height 20
;;         counsel-yank-pop-height 20
;;         counsel-org-goto-face-style 'org
;;         counsel-org-headline-display-style 'title
;;         counsel-org-headline-display-tags t
;;         counsel-org-headline-display-todo t))

;; (after! ivy
;;   (setq ivy-use-selectable-prompt t
;;         ivy-auto-select-single-candidate t
;;         ivy-rich-parse-remote-buffer nil
;;         +ivy-buffer-icons nil
;;         ivy-use-virtual-buffers nil
;;         ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
;;         ivy-height 20
;;         ivy-rich-switch-buffer-name-max-length 50))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END CUSTOMIZATION BORROWED FROM THIS CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(setq left-margin-width 4)
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/issues/4106
(setq-hook! 'web-mode-hook +format-with 'prettier-prettify)

;; reduce delay to show leader popup when pressing SPC
;; https://github.com/hlissner/doom-emacs/issues/1839
(require 'which-key)
(setq which-key-idle-delay 0.1)

(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)

(after! eshell
  (set-eshell-alias!
   "ff"   "find-file $1"
   "l"   "ls -lahtr"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "rg"  "rg --color=always $*"))

(setq lsp-eslint-auto-fix-on-save t)

;; set cursor shape in iterm/terminal to indicate inser/normal
;; mode for evil mode
;;(global-term-cursor-mode)

;; (defun native-comp-available-p () nil)

;; increae line height
(setq default-text-properties '(line-spacing 0.3 line-height 1.2))

;; (after! treesit
;;   (setq treesit-language-source-alist
;;         '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
;;           (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil))))

;; Disable doc help buffer at the bottom
;; was causing emacs to freeze
;; https://github.com/emacs-lsp/lsp-mode/issues/1223
(setq lsp-signature-auto-activate nil)

(use-package! mise
  :config
  (add-hook 'after-init-hook #'global-mise-mode))

(after! markdown-mode
  (setq markdown-command "grip"))

(after! vertico
  (vertico-mode)

  (setq vertico-scroll-margin 2
        vertico-resize t
        vertico-count 15)

  (after! marginalia
    (marginalia-mode)
    (setq marginalia-censor-variables nil)

    ;; Force Marginalia to always annotate file candidates
    (add-to-list 'marginalia-command-categories '(consult-projectile-find-file . file))
    (add-to-list 'marginalia-command-categories '(consult-file . file))

    ;; Custom annotation for files
    (defadvice! +marginalia--annotate-local-file-colorful (cand)
      "A colorful version of `marginalia--annotate-local-file` to display permissions and timestamps."
      :override #'marginalia--annotate-local-file
      (when-let (attrs (file-attributes (substitute-in-file-name
                                         (marginalia--full-candidate cand))
                                        'integer))
        (marginalia--fields
         ((marginalia--file-owner attrs) :width 12 :face 'marginalia-file-owner)
         ((marginalia--file-modes attrs)) ;; File permissions
         ((+marginalia-file-size-colorful (file-attribute-size attrs)) :width 7)
         ((+marginalia--time-colorful (file-attribute-modification-time attrs)) :width 12))))

    ;; Custom colorful file sizes
    (defun +marginalia-file-size-colorful (size)
      (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
             (color (if (< size-index 10000000) ; 10 MB
                        (doom-blend 'orange 'green size-index)
                      (doom-blend 'red 'orange (- size-index 1)))))
        (propertize (file-size-human-readable size) 'face (list :foreground color))))

    ;; Custom colorful timestamps
    (defun +marginalia--time-colorful (time)
      (let* ((seconds (float-time (time-subtract (current-time) time)))
             (color (doom-blend
                     (face-attribute 'marginalia-date :foreground nil t)
                     (face-attribute 'marginalia-documentation :foreground nil t)
                     (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0))))))) ;; Time decay coloring
        (propertize (marginalia--time time) 'face (list :foreground color))))))
;; (after! marginalia
;;   (marginalia-mode)
;;   (setq marginalia-censor-variables nil)
;;   ;;(add-to-list 'marginalia-command-categories '(consult-projectile-find-file . file))
;;   (setq marginalia-command-categories '((consult-find . file)
;;                                         (consult-projectile-find-file . file)))

;;   (defadvice! +marginalia--annotate-local-file-colorful (cand)
;;     :override #'marginalia--annotate-local-file
;;     (when-let (attrs (file-attributes (substitute-in-file-name
;;                                        (marginalia--full-candidate cand))
;;                                       'integer))
;;       (marginalia--fields
;;        ((marginalia--file-owner attrs) :width 12 :face 'marginalia-file-owner)
;;        ((marginalia--file-modes attrs))
;;        ((+marginalia-file-size-colorful (file-attribute-size attrs)) :width 7)
;;        ((+marginalia--time-colorful (file-attribute-modification-time attrs)) :width 12))))

;;   (defun +marginalia-file-size-colorful (size)
;;     (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
;;            (color (if (< size-index 10000000) ; 10 MB
;;                       (doom-blend 'orange 'green size-index)
;;                     (doom-blend 'red 'orange (- size-index 1)))))
;;       (propertize (file-size-human-readable size) 'face (list :foreground color))))

;;   (defun +marginalia--time-colorful (time)
;;     (let* ((seconds (float-time (time-subtract (current-time) time)))
;;            (color (doom-blend
;;                    (face-attribute 'marginalia-date :foreground nil t)
;;                    (face-attribute 'marginalia-documentation :foreground nil t)
;;                    (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0))))))) ;; Time decay coloring
;;       (propertize (marginalia--time time) 'face (list :foreground color))))))


;; (after! nerd-icons-completion
;;   (nerd-icons-completion-mode))

;; ;; Enable Nerd Icons in Dired Mode
;; (after! dired
;;   (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))


;; (use-package! marginalia
;;   :after vertico
;;   :config
;;   (marginalia-mode))

(use-package! nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package! nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
