;; enable custom font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#ABB2BF" :background "#282C34"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default)))
 '(org-agenda-files (quote ("~/Documents/personal/todo.org")))
 '(org-hide-leading-stars t)
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (eshell-prompt-extras ccls shell-pop company-box web-mode toml-mode eglot doom-themes all-the-icons-install-fonts nord-theme doom-modeline lsp-python-ms lsp-ui company-lsp lsp-mode reason counsel-projectile projectile general which-key key-chord yaml-mode terraform-mode evil rjsx-mode kivy-mode counsel cargo rust-mode racer exec-path-from-shell atom-one-dark-theme importmagic flycheck pyenv-mode elpy magit material-theme use-package)))
 '(python-shell-interpreter "python")
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm)))) t)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]]))
