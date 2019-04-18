;; allow fullscreen emacs
; https://crypt.codemancers.com/posts/2013-07-05-non-native-fullscreen-for-osx-on-emacs-24-dot-3/
(setq ns-use-native-fullscreen nil)
;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
;(toggle-frame-fullscreen)

;; disable toolbar
(tool-bar-mode -1)

;; enable global line numbers 
(global-linum-mode 1)

;; disable scrollbar
(toggle-scroll-bar -1)

;; "interactively do things" - no need to `tab` complete
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; hide splash screen and startup banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; global auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; highlight current line
(global-hl-line-mode 1)

;; highlight matching ()
(show-paren-mode 1)


;; show battery percentage
(display-battery-mode t)

;; ignore annoying ass bell
(setq ring-bell-function 'ignore)

;; add current directory + filename to framebar
(setq-default mode-line-buffer-identification
	      (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                              (cdr mode-line-buffer-identification)))))
