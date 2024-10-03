(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))

;; sloppy
(defun filt-list-n (pred lst)
  (cond ((not lst)                '())
	((funcall pred (car lst)) (cons (car lst)
					(filt-list-n pred (cdr lst))))
	(t 		          (filt-list-n pred (cdr lst)))))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;; Download
;; "https://www.gnu.org/software/emacs/manual/info/elisp.info.gz"
(when (not (file-exists-p (expand-file-name "~/.emacs.d/elisp.info.gz")))
  (url-copy-file
   "https://www.gnu.org/software/emacs/manual/info/elisp.info.gz"
   "~/.emacs.d/elisp.info.gz"))

(global-set-key (kbd "C-x C-h C-e")
		#'(lambda ()
		    (interactive)
		    (info (expand-file-name "~/.emacs.d/elisp.info.gz"))))

(when (executable-find "zprint")
  (global-set-key (kbd "C-x C-h C-z")
		  #'(lambda ()
		      (interactive)
		      (shell-command-on-region
		       (region-beginning) (region-end)
		       "/home/nvlassopoulos/bin/zprint" (current-buffer) t))))

;; new additions
(hl-line-mode)


;;
(setq ispell-program-name "aspell")
(setq exec-path (append exec-path '("~/bin")))

(setq inferior-lisp-program "/usr/bin/sbcl")
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

;; temp?
(global-set-key (kbd "C-x C-h C-o") 'cider-repl-clear-buffer)


(set 'scheme-program-name "guile")

(setq browse-url-browser-function 'browse-url-firefox)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Check for packages missing from standard setup
(defvar *stdpackages*
  '((highlight-symbol . elpa)
    (ac-cider . elpa)
    (company . elpa)
    (cyberpunk-2019-theme . elpa)
    (cyberpunk-theme . elpa)
    (deadgrep . elpa)
    (elisp-slime-nav . elpa)
    ;; (elixir-mode . elpa)
    (geiser . elpa)
    ;; (cider . elpa)
    (clojure-mode . elpa)
    (json-mode . elpa)
    (magit . elpa)
    (markdown-mode . elpa)
    (paredit . elpa)
    (powerline . elpa)
    (railscasts-reloaded-theme . elpa)
    (rainbow-delimiters . elpa)
    (slime . elpa)
    (swiper . elpa)
    (web-mode . elpa)
    (with-editor . elpa)
    (flycheck-clj-kondo . melpa)
    ;; (zerdaork-theme . elpa)
    (zprint-mode . elpa)))

(let ((not-installed (filt-list-n #'(lambda (x)
				      (not (package-installed-p (car x))))
				  *stdpackages*)))
  (when (and not-installed
	     (yes-or-no-p "Missing packages. Install them now? "))
    (dolist (p not-installed)
      (package-install (car p) (cdr p)))))


;; Java
(add-hook 'java-mode-hook 
    (lambda ()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)
        (setq c-continued-statement-offset 4)
        (c-set-offset 'arglist-cont-nonempty 4)
        (line-number-mode)
    ))


;; thanks https://ericscrivner.me/2015/06/better-emacs-rainbow-delimiters-color-scheme/
;; better colors for dark (cyberpunk) themes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))


;; stuff for zero dark (HL paren is not showing well)
(defun red-hl-paren ()
  (interactive)
  (let ((class '((class color) (min-colors 89)))) ;; FIXME:
    (custom-theme-set-faces
     'zerodark
     `(show-paren-match ((,class (:foreground "#FF0000" :weight bold)))))))

(when (custom-theme-enabled-p 'zerodark)
  (red-hl-paren))


;; Stathis' config for highlight symbol
;; see EOF for exact code
(require 'highlight-symbol)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'scheme-mode-hook 'highlight-symbol-mode)
(add-hook 'cider-repl-mode-hook 'highlight-symbol-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-.") 'highlight-symbol-next)
(defun highlight-symbol-count (&optional symbol)
  "(Do not) Print the number of occurrences of symbol at point."
  (interactive))
;; 
(setq highlight-symbol-idle-delay 1)
(setq highlight-symbol-on-navigation-p 't)
(setq highlight-symbol-occurrence-message (quote (explicit)))


(defun clojure-set-find-var ()
  (local-set-key (kbd "M-.") 'cider-find-var))

;; Clojure
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (line-number-mode)
	    (show-paren-mode)
	    (rainbow-delimiters-mode)
	    (flycheck-mode)
	    ;; (auto-complete-mode)
	    (clojure-set-find-var)
	    (undo-tree-mode)
	    (company-mode)))


(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (company-mode)
	    (clojure-set-find-var)))

;; swiper
(require 'swiper)
(global-set-key (kbd "C-x C-h C-s") 'swiper)

(require 'clojure-mode)

(require 'flycheck-clj-kondo)

;; Python
(add-hook 'python-mode-hook
    (lambda ()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)))


;; Go 
;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq gofmt-command "goimports")
;; (defun go-mode-hook-n ()
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (auto-complete-mode 1)
;;   )
;; (add-hook 'go-mode-hook 'go-mode-hook-n)

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
;; In case it's missing, navajowhite is 0xFFDEAD
(add-to-list 'default-frame-alist '(foreground-color . "navajowhite"))
(add-to-list 'default-frame-alist '(background-color . "black"))


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


;; ORG mode local 
(add-hook 
 'org-mode-hook
 (lambda ()
   (highlight-regexp "NOTE" 'hi-pink)))

(org-babel-do-load-languages
 'org-babel-load-languages '((java . t)))

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
;; (autoload 'save-current-configuration "revive" "Save status" t)
;; (autoload 'resume "revive" "Resume Emacs" t)
;; (autoload 'wipe "revive" "Wipe Emacs" t)

;; (define-key ctl-x-map "S" 'save-current-configuration)
;; (define-key ctl-x-map "F" 'resume)
;; (define-key ctl-x-map "K" 'wipe)


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


;;; Standard projectile setup
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
  (with-current-buffer (get-buffer "*SQL: Postgres*")
    (delete-region (point-min)
		   (point-max))
    (comint-send-input)))

(global-set-key (kbd "C-c C-l C-l") 'clear-sql-comint-out)

(with-eval-after-load "sql"
  (setq sql-postgres-login-params
	(append sql-postgres-login-params
		'((port 15432 :default 5432)))))

;; Set fonts when in window mode
(when (not (eql (framep (selected-frame)) 't))
  (let ((fnt-name "Anonymous Pro 13"))
    (let ((ft (find-font (font-spec :name fnt-name))))
      (if (fontp ft)
	  (progn
	    (set-frame-font (find-font (font-spec :name fnt-name)))
	    (set-face-attribute 'default nil :height 130 :weight 'light :slant 'normal))
	(set-face-attribute 'default nil :height 130 :slant 'normal)))))


;; SlimeNavElisp -- to check
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook rainbow-delimiters-mode))
  (add-hook hook 'elisp-slime-nav-mode))


;; For when using JDEE
;; (setq jdee-server-dir (expand-file-name "~/usr/jdee-bundle-1.1-full.jar"))

;; Ensime and scala stuff
;; (setq ensime-sbt-command (expand-file-name "~/usr/local/opt/sbt@0.13/bin"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-input-method "greek"))



(load-theme 'railscasts-reloaded)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
