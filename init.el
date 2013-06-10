(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defmacro do-to-package-list (packageList &rest body)
  "Does something to each package of a package list"
  `(mapc (lambda (package) ,@body ) ,packageList)) 

(defun install-if-necessary (package)
  (or (package-installed-p package) 
      (package-install package)))

;; loads the listed packages, installing if necessary
(do-to-package-list '(magit rainbow-mode yasnippet package ido-vertical-mode ido-ubiquitous)
                         (install-if-necessary package)
                         (require package))

;;installs the following packages (without loading) if necessary
(do-to-package-list '(dired+ auctex color-theme)
                         (install-if-necessary package))

;;the base directory for git packages
(defvar init-git-directory "~/.emacs.d/git-packages/") 

;;the directory in which this git package would be installed
(cl-defun init-git-package-directory (package &optional (baseDir init-git-directory)) (file-truename  (concat baseDir (symbol-name package)))) 

;;requires packageName, fetching from git url if necessary
(defun require-or-git-clone (package url) 
  (let ((packageDir (init-git-package-directory package))) 
    (add-to-list 'load-path packageDir)
    (unless (require package nil 'noerror)
      (let* ((git (or (executable-find "git")
                      (error "Unable to find `git'")))
             (status  
              (call-process
               git nil nil nil "--no-pager" "clone" "-v" url packageDir)))
        (if (zerop status)
            (require package)
          (error "Couldn't clone %s from the Git repository: %s" package url))))))

;;evil
(require-or-git-clone 'evil "git://gitorious.org/evil/evil.git" )
(require-or-git-clone 'lalopmak-evil "https://github.com/lalopmak/lalopmak-evil" )

;;tango color theme
(require-or-git-clone 'color-theme-tangotango "https://github.com/juba/color-theme-tangotango")
(add-to-list 'custom-theme-load-path (init-git-package-directory 'color-theme-tangotango))
(load-theme 'tangotango t)



(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq
 el-get-sources
 '(el-get				; el-get is self-hosting
  kill-ring-ido)) 

;;adds the fetched el-get packages to load-path and requires them
(do-to-package-list el-get-sources
                         (add-to-list 'load-path (init-git-package-directory package "/home/yourname/.emacs.d/el-get/"))
                         (require package))                      

(el-get 'sync el-get-sources)

;; (require 'kill-ring-ido)
(global-set-key (kbd "M-y") 'kill-ring-ido)
(setq kill-ring-ido-shortage-length 24) 

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)


(yas--initialize)


;; (iswitchb-mode t)
(require 'ido)
(ido-mode 'both) ;; for buffers and files

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ido-case-fold  t                 ; be case-insensitive
      ido-enable-last-directory-history t ; remember last used dirs
      ido-record-commands t
      ido-max-work-directory-list 30   ; should be enough
      ido-max-work-file-list      50   ; remember many
      ido-use-virtual-buffers t 
      )

;; This tab override shouldn't be necessary given ido's default 
;; configuration, but minibuffer-complete otherwise dominates the 
;; tab binding because of my custom tab-completion-everywhere 
;; configuration.
(defmacro add-ido-hook (key hook)
  `(add-hook 'ido-setup-hook 
             (lambda () 
               (define-key ido-completion-map ,key ,hook))))

(add-ido-hook [tab] 'ido-next-match)
(add-ido-hook [up] 'ido-prev-match)
(add-ido-hook [down] 'ido-next-match)

(defun file-visited-by (name)
  "Returns name of file visited by buffer [name], or nil if it's not visiting any" 
  (buffer-file-name (get-buffer-create name)))

(defun active-and-not-visiting-file (name)
 (and (get-buffer name) (not (file-visited-by name)) )) 

(add-to-list 'ido-ignore-buffers 'active-and-not-visiting-file)


(ido-vertical-mode t)

(ido-ubiquitous-mode 1)
;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)


;; I want spaces for indentation
(setq-default indent-tabs-mode nil)

;;line numbers
(global-linum-mode t)

(set-scroll-bar-mode 'right)   ; replace 'right with 'left to place it to the left

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(evil-mode 1)

(show-paren-mode 1)

;;byte-compiles .el files upon save
(add-hook 'after-save-hook 
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

(require 'flymake)

(defun flymake-get-tex-args (file-name)
  (list "lualatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)	

;; ;;fix copy paste?
;; ;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
;; (setq x-select-enable-clipboard t)

;; ;; after mouse selection in X11, you can paste by `yank' in emacs
;; (setq x-select-enable-primary t)
