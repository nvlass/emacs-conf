(tool-bar-mode -1)

(load (expand-file-name "~/bin/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

(defun clip-foo (x)
  (if (> x 100)
      100
    (if (< x 5)
	5
      x)))

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
;(set 'scheme-program-name "kawa")

(add-to-list 'load-path "~/bin/elisp/")

(setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-url-elinks)

;; (setq synonyms-file "/home/nvlass/bin/mthesaur.txt")
;; (setq synonyms-cache-file "/home/nvlass/bin/mthesaur.txt.cache")
;; (require 'synonyms)

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; wordnik thesaurus stuff
;; (require 'json)
;; (defvar wordnik-token nil)

(require 'itail)


(require 'java-mode-indent-annotations)

(add-hook 'java-mode-hook 
    (lambda ()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)
        (setq c-continued-statement-offset 4)
        (c-set-offset 'arglist-cont-nonempty 4)
        (java-mode-indent-annotations-setup)
        (linum-mode)
    ))

(add-hook 'clojure-mode-hook
    'linum-mode
    'rainbow-delimiters-mode)

(add-hook 'python-mode-hook
    (lambda ()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)))

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


;; (require 'erlang)

(add-to-list 'default-frame-alist '(foreground-color . "navajowhite"))
(add-to-list 'default-frame-alist '(background-color . "black"))

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(set-face-attribute 'default nil :height 90)

(require 'json-mode)

(winner-mode)

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

(require 'uuid-gen)
(global-set-key (kbd "C-c C-u") 'uuid-gen)


(add-hook 'json-mode-hook
    (lambda ()
      (hs-minor-mode)
      (global-set-key (kbd "C-c h") 'hs-hide-block)
      (global-set-key (kbd "C-c s") 'hs-show-block)
      ))

(when (functionp 'powerline-default-theme)
  (powerline-default-theme))

;; swagger validate schema
(defvar swagger-uri 
  "https://raw.githubusercontent.com/swagger-api/swagger-spec/master/schemas/v2.0/schema.json")

(defun json-validate-buff ()
  (interactive)
  (let ((pbuff (current-buffer))
	(psize (buffer-size)))
    
    (let* ((wg-file (make-temp-file "/tmp/wgfoo"))
	   (sw-file (make-temp-file "/tmp/swfoo")))
      (with-temp-file wg-file
	(call-process "wget" 
		      nil 
		      (current-buffer)
		      nil 
		      swagger-uri 
		      "-O" "-" "--quiet")
	(buffer-string))
      (with-temp-file sw-file
	(insert-buffer-substring
	 pbuff 1 (+ psize 1)))
      (with-current-buffer (get-buffer-create "*swa-val*")
	(erase-buffer)
	(start-process "swa" "*swa-val*" "json-schema-val.sh" wg-file sw-file)))))

(global-set-key (kbd "C-c v j") 'json-validate-buff)

(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(when (executable-find "wmctrl")
  (defun toggle-full-screen ()
    "Toggle full screen"
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))

;; (setq browse-url-browser-function 'browse-url-firefox
;;       browse-url-new-window-flag  nil
;;       browse-url-firefox-new-window-is-tab t)

(global-set-key [f11] 'toggle-full-screen)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nodejs-repl-command "nodejs")
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.inaccess.com")
 '(smtpmail-smtp-service 25)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)

