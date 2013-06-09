(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; loads the listed packages, installing if necessary
(mapc
 (lambda (package)
   (or (package-installed-p package) 
       (package-install package))
   (require package))
 '(magit rainbow-mode yasnippet package ido-vertical-mode ido-ubiquitous ))

;;installs the following packages (without loading) if necessary
(mapc
 (lambda (package)
   (or (package-installed-p package) 
       (package-install package)))
 '(dired+ auctex color-theme))

;;the base directory for git packages
(defvar init-git-directory "~/.emacs.d/git-packages/") 

;;the directory in which this git package would be installed
(defun init-git-package-directory (package) (file-truename  (concat init-git-directory (symbol-name package)))) 

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



(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)

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
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))



(yas--initialize)

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

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

(require 'flymake)

(defun flymake-get-tex-args (file-name)
  (list "lualatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)	


