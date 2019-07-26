;;; load personal emacs configuration
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "packages")
(load-library "style")
(load-library "orgmode")

;; load 'customize' auto-generated variables
(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file)
