(require 'cl) ; require Common Lisp

(setq home-dir (getenv "HOME"))

(load "package")
(package-initialize)

;; requires xscheme.elc file in /usr/share/emacs/<version>/lisp/
(load-library "xscheme")

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Org mode
(setq org-mode-package-dir (format "%s/.emacs.d/org/lisp" home-dir))
(add-to-list 'load-path org-mode-package-dir)
(require 'ox-confluence)

;;; Load Person .el files
(setq personal-lisp-files-dir "~/.emacs.d/lisp/")
(add-to-list 'load-path personal-lisp-files-dir)
(require 'java-getter-setter)

;; Java Mode Hooks
(add-hook 'java-mode-hook (lambda ()
                            (progn
                              (global-set-key (kbd "C-c j s") 'insert-single-get-and-set) ;; from java-getter-setter
                              (global-set-key (kbd "C-c j r") 'insert-get-and-set-multiple)))) ;; from java-getter-setter

(require 'ido)
;; Enable Ido Everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Tabs to spaces
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; Turn on column-number-mode
(setq column-number-mode t)

;; Backup file preferences
;; (setq make-backup-files nil) ; stop creating ~ files
(setq backup-directory-alist '((".*" . "~/.emacs_saves/backups")))
;;(setq auto-save-file-name-transforms '((".*" ,temporary-file-directory t)))
(setq backup-by-copying t)
;; Interesting Backup stuff
(setq delete-old-versions t) ;; If set to true, when saving a backup this deletes the old backup automatically
(setq kept-new-versions 6  ;; Number of newest versions to keep
      kept-old-versions 2 ;; Number of older versions to keep
      version-control t )

;; Show file name
(defun show-full-name ()
  "Show the full file path in the minibuffer"
  (interactive) ;; interactive allows the function to be called interactively, a.k.a. bound to a key-sequence
  (message (buffer-file-name)))
(global-set-key (kbd "C-c f n") 'show-full-name)

;; Insert pdb debug
;; should add a python-mode check here
(defun insert-python-debug-line ()
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(global-set-key (kbd "C-c C-d") 'insert-python-debug-line)


;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default c-basic-offset 4)

(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Color of selection
(set-face-attribute 'region nil :background "#004ed4" :foreground "#ffffff")

; Don't show the manual screen and scratch buffer message when emacs boots
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

; Don't type out the whole "yes" or "no" when emacs asks, make it one character
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c b") 'magit-blame)
(global-set-key (kbd "C-c f r") 'replace-string)

; Packages List
(defvar allareri/packages '(auto-complete
			    python-mode
			    magit
			    ample-zen-theme
			    clojure-mode
			    paredit
                            flymake-python-pyflakes
			    cider
                            projectile
                            go-mode
                            org)
  "Default packages")

(defun allareri/packages-installed-p ()
  (loop for pkg in allareri/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (allareri/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg allareri/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


; Load Theme
(load-theme 'ample-zen t)
(set-face-foreground 'minibuffer-prompt "green") ; Change prompt text color in minibuffer (where you enter commands, on the bottom...)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Linum Mode
(global-linum-mode 1)
(setq linum-format "%d ") ;; add one space of separation between line number and text

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; menu bar
(menu-bar-mode -1)

;; python-mode preferences
(add-hook 'python-mode-hook (lambda ()
			      (local-set-key (kbd "C-j") 'newline-and-indent)))

;; Projectile
(projectile-global-mode)
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(add-hook 'python-mode-hook 'projectile-mode)

;; clojure-mode preferences
(add-hook 'clojure-mode-hook #'paredit-mode)

;; cider-mode preferences (clojure)
(defun nolinum ()
  (global-linum-mode 0))
(add-hook 'cider-mode-hook #'nolinum)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; flake8
(autoload 'flymake-python-pyflakes-load "flymake-python-pyflakes" nil t)
(eval-after-load 'python
    '(add-hook 'python-mode-hook 'flymake-python-pyflakes-load))
(global-set-key (kbd "C-c C-e") 'flymake-popup-current-error-menu)

;; Go Lang
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (setq tab-width 4))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq yaml-indent 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends (quote (ascii html icalendar latex md odt confluence)))
 '(package-selected-packages
   (quote
    (dracula-theme nord-theme go-mode projectile cider flymake-python-pyflakes paredit clojure-mode ample-zen-theme magit python-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Open the man page for the current word at point, if it exists
(defun open-man-page (term)
  (interactive (list (current-word nil 't)))
  (man term))

(global-set-key (kbd "C-c m") 'open-man-page)

;; Define keybinding for org-toggle-link-display when in org-mode
(with-eval-after-load
    'org (define-key (current-global-map) (kbd "C-c s l") 'org-toggle-link-display))

;; Disable eldoc mode (python-help)
(global-eldoc-mode -1)

;; org-mode set Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (java . t)
   (C . t)
   (shell . t)))
