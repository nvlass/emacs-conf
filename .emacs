(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

(setq ispell-program-name "aspell")

(setq exec-path (append exec-path '("~/bin")))

;; temp
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "/opt/google/chrome/chrome")

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(defun clip-foo (x)
  (if (> x 100)
      100
    (if (< x 5)
	5
      x)))

(defun colorize-region ()
  (interactive)
  (ansi-color-apply-on-region (region-beginning) (region-end)))

(defun inc-transp ()
  (interactive)
  (let ((fr-p (frame-parameter (selected-frame) 'alpha)))
    (set-frame-parameter 
     (selected-frame) 
     'alpha 
     (list (clip-foo (+ (car fr-p) 5))
	   (clip-foo (+ (cadr fr-p) 5))))))

(defun dec-transp ()
  (interactive)
  (let ((fr-p (frame-parameter (selected-frame) 'alpha)))
    (set-frame-parameter 
     (selected-frame) 
     'alpha 
     (list (clip-foo (- (car fr-p) 5))
	   (clip-foo (- (cadr fr-p) 5))))))


(global-set-key (kbd "C-x <S-prior>") 'inc-transp)
(global-set-key (kbd "C-x <S-next>") 'dec-transp)

(set 'scheme-program-name "guile")

(setq browse-url-browser-function 'browse-url-safari)

(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Java
(add-hook 'java-mode-hook 
    (lambda ()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)
        (setq c-continued-statement-offset 4)
        (c-set-offset 'arglist-cont-nonempty 4)
        (linum-mode)
    ))

;; Clojure
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (linum-mode)
	    (rainbow-delimiters-mode)
	    (auto-complete-mode)))


(require 'clojure-mode)
;; compojure indenting stuff
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; Python
(add-hook 'python-mode-hook
    (lambda ()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)))


;; Go 
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(setq gofmt-command "goimports")

(defun go-mode-hook-n ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (auto-complete-mode 1)
  )

(add-hook 'go-mode-hook 'go-mode-hook-n)

;; Frame / buffer navigation
(defun right-win-or-frame ()
  (interactive)
  (if (windowp (window-in-direction 'right))
      (windmove-right)
    (other-frame +1)))

(defun left-win-or-frame ()
  (interactive)
  (if (windowp (window-in-direction 'left))
      (windmove-left)
    (other-frame -1)))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'left-win-or-frame)
(global-set-key (kbd "C-x <right>") 'right-win-or-frame)


;; Frame
;;(add-to-list 'default-frame-alist '(foreground-color . "navajowhite"))
;;(add-to-list 'default-frame-alist '(background-color . "black"))


;; JSON
(require 'json-mode)

(add-hook
 'json-mode-hook
 (lambda ()
   (hs-minor-mode)
   (global-set-key (kbd "C-c h") 'hs-hide-block)
   (global-set-key (kbd "C-c s") 'hs-show-block)
   ))


(winner-mode)

(add-hook
 'html-mode-hook
 (lambda ()
   (web-mode)))

;; use pandoc to html preview latex 
;; files
(defvar pandoc-command "pandoc")

(defun do-pandoc-preview () 
  (interactive)
  (let ((obn (get-buffer-create " *pandoc-preview")))
    (save-excursion  ;; probably unneccessary
      (shell-command 
       (concat pandoc-command " "
	       (shell-quote-argument buffer-file-name))
       obn)
      (browse-url-of-buffer obn))))


;; Latex mode highlighting
(add-hook 
 'latex-mode-hook
 (lambda ()
   (global-set-key (kbd "C-c p") 'do-pandoc-preview)
   (highlight-regexp "FIXME" 'hi-yellow)
   (highlight-regexp "SUPERFL" 'hi-pink)
   (highlight-regexp "PHRASING" 'hi-red-b)
   (highlight-lines-matching-regexp "TARG" 'hi-green)
   (highlight-lines-matching-regexp "MENTION" 'hi-blue)
   (highlight-regexp "\<CHECK\>.*\</CHECK\>")))



;; Standard
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))


;; revive.el stuff
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)



;; Cross-emacs copying through pipes
;; (defun copy-to-pipe ()
;;   (interactive)
;;   (when (file-exists-p "/tmp/f1")
;;     (write-region (buffer-substring (region-beginning) (region-end))
;; 		  nil "/tmp/f1" nil)))

;; (defun paste-from-pipe ()
;;   (interactive)
;;   (when (file-exists-p "/tmp/f1")
;;     (insert-file-contents "/tmp/f1")))

;; (when (file-exists-p "/tmp/f1")
;;   (global-set-key (kbd "C-c C-y") 'copy-to-pipe)
;;   (global-set-key (kbd "C-c M-y") 'paste-from-pipe))


;; Cider specific config for direct
;; backend connections
(require 'backend-cider)


;; Fullscreen mode
(when (executable-find "wmctrl")
  (defun toggle-full-screen ()
    "Toggle full screen"
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))

(global-set-key [f11] 'toggle-full-screen)

;; Clear *SQL* buffer (copied from somewhere)
(defun clear-sql-comint-out ()
  (interactive)
  ;; I am lazy, so it only works with *SQL* buffers
  ;; (and not renamed ones)
  (with-current-buffer (get-buffer "*SQL*")
    (delete-region (point-min)
		   (point-max))
    (comint-send-input)))

(global-set-key (kbd "C-c C-l C-l") 'clear-sql-comint-out)

;; Set fonts when in window mode
(when (not (eql (framep (selected-frame)) 't))
  (let ((fnt-name "Inconsolata 12"))
    (let ((ft (find-font (font-spec :name fnt-name))))
      (if (fontp ft)
	  (progn
	    (set-frame-font (find-font (font-spec :name fnt-name)))
	    (set-face-attribute 'default nil :height 110 :weight 'light :slant 'normal))
	(set-face-attribute 'default nil :height 110 :slant 'normal)))))


;; For when using JDEE
;; (setq jdee-server-dir (expand-file-name "~/usr/jdee-bundle-1.1-full.jar"))

;; Ensime and scala stuff
;; (setq ensime-sbt-command (expand-file-name "~/usr/local/opt/sbt@0.13/bin"))


(load-theme 'cyberpunk)
(put 'downcase-region 'disabled nil)

