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
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
