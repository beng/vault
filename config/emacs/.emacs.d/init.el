;;; load personal emacs configuration
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "packages")
(load-library "style")
(load-library "orgmode")
(load-library "aliases")

;; load 'customize' auto-generated variables
(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file)
(put 'downcase-region 'disabled nil)

;; increase garabage collection limits
(setq gc-cons-threshold 100000000)
(setq large-file-warning-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "https://orgmode.org/elpa")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; bootstrap `use-package`
(unless (package-installed-p 'use-package)
`  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;(require 'org)
;;(org-babel-load-file "~/.emacs.d/init.org")
