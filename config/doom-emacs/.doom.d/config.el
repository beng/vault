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
      doom-unicode-font (font-spec :family "Fira Code Retina"))

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org-notes/org/")

(setq
 ;; set undo limit to 10mb
 undo-limit 10000000
 ;; By default while in insert all changes are one big blob. Be more granular
 evil-want-fine-undo t
 auto-save-default t
 line-spacing 3
 lsp-diagnostics-provider :auto)

;; (unless (string-match-p "^Power N/A" (battery))
;;   (display-battery-mode 1))

;; iterate through CamelCase words
(global-subword-mode 1)

;; new buffers open in org mode rather than fundamental mode
(setq-default major-mode 'org-mode)

(after! good-scroll
  (good-scroll-mode 1))

(after! python
  (setenv "WORKON_HOME" "~/.pyenv/versions")
    (setq flycheck-enabled-checkers '(python-flake8 python-mypy))
  (setq flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-python-flake8-executable "flake8")
  (setq python-indent-offset 4
        python-indent 4
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


;; TODO: this isnt loading correctly, only way
;; to make it work is to load it globally, not working
;; when i use ( after! prettier )
;;(add-hook 'after-init-hook #'global-prettier-mode)
;;
;;BG: 12/08/2021 DISABLED GLOBAL HOOK OF PRETTIER MODE as it was breaking
;;python indentation. if JS/react starts to break, its because of this!
;;to fix, renable this!
;;(add-hook 'after-init-hook #'global-prettier-mode)
(add-hook 'web-mode-hook #'prettier-mode)
(add-hook 'css-mode-hook #'prettier-mode)
(add-hook 'typescript-mode-hook #'prettier-mode)
;; (after! prettier
;;   (add-hook 'after-init-hook #'global-prettier-mode))
;;(add-hook! (global-prettier-mode)))

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
(setq display-line-numbers-type 'relative)

;; insert timestamp when status set to closed in org mode
;;
(setq org-log-done 'time
      ;; hide * * for bold // for italics, etc
      org-hide-emphasis-markers t)

(after! evil
  (setq evil-escape-delay .5)
  (setq evil-escape-key-sequence "jj"))

(after! company
  (setq company-tooltip-limit 5
	company-idle-delay 0.05
        company-minimum-prefix-length 2
        ;;company-dabbrev-downcase nil
        company-tooltip-minimum-width 80
        company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-capf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN CUSTOMIZATION BORROWED FROM THIS CONFIG
;; - https://github.com/Brettm12345/doom-emacs-literate-config/blob/master/config.org#completioncompany
;; - https://www.soitflows.xyz/posts/my-doom-emacs-configuration/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use ripgrep as default for project search
(setq +ivy-project-search-engines '(rg))

(after! ivy
  (ivy-add-actions
   'counsel-M-x
   `(("h" +ivy/helpful-function "Helpful"))))


(after! counsel
  (setq counsel-evil-registers-height 20
        counsel-yank-pop-height 20
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'title
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t))

(after! ivy
  (setq ivy-use-selectable-prompt t
        ivy-auto-select-single-candidate t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers nil
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-height 20
        ivy-rich-switch-buffer-name-max-length 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(global-term-cursor-mode)

;; (after! deft
;;   (setq deft-directory "~/Documents/notes/org-roam"
;;         deft-recursive t
;;         deft-use-filename-as-title nil
;;         deft-use-filter-string-for-filename t
;;         deft-extensions '("md" "txt" "org")
;;        deft-default-extension "org"))

(after! org
  ;; (add-hook! 'org-mode-hook #'org-superstar-mode)
  ;; (setq org-superstar-prettify-item-bullets t )
  ;; type shortcut, eg `<sh' and then hit Tab to expand the template
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("shell" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("python" . "src python"))
  (add-to-list 'org-structure-template-alist '("golang" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))


;; (after! org-roam
;;   (setq org-roam-completion-system 'ivy-mode
;;         org-roam-directory "~/Documents/org-notes/org-roam"
;;         org-roam-capture-templates
;;         '(("d" "default" plain
;;            "%?"
;;            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;            :unnarrowed t)
;;           ("l" "programming language" plain
;;            "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
;;            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;            :unnarrowed t)))
;;   (org-roam-db-autosync-mode))

;; org-roam before-hook throws an error and cant sync files to db.
;; ` (void-function native-comp-available-p)`
;; see issue below. this is a temp workaround
;; https://github.com/hlissner/doom-emacs/issues/5706
(defun native-comp-available-p () nil)

(after! tramp
  (setq tramp-inline-compress-start-size 1000)
  (setq tramp-copy-size-limit 10000)
  (setq vc-handled-backends '(Git))
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq projectile--mode-line "Projectile")
  (setq tramp-verbose 1))


(after! counsel-tramp
(add-hook 'counsel-tramp-pre-command-hook '(lambda () ;;(global-aggressive-indent-mode 0)
				     (projectile-mode 0)))
				     ;;(editorconfig-mode 0)))
(add-hook 'counsel-tramp-quit-hook '(lambda () ;;(global-aggressive-indent-mode 1)
			      (projectile-mode 1))))
			      ;;(editorconfig-mode 1))))

;; increae line height
(setq default-text-properties '(line-spacing 0.3 line-height 1.2))

(after! treesit
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil))))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))

;; Disable doc help buffer at the bottom
;; was causing emacs to freeze
;; https://github.com/emacs-lsp/lsp-mode/issues/1223
(setq lsp-signature-auto-activate nil)
