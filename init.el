(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package) (package-initialize)

; I want spaces for indentation
(setq-default indent-tabs-mode nil)

;line numbers
(global-linum-mode t)

(set-scroll-bar-mode 'right)   ; replace 'right with 'left to place it to the left
;(set-face-background 'scroll-bar "red")


;(require 'color-theme)
;    (color-theme-initialize)
;    (color-theme-late-night)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)  
(evil-mode 1)

(add-to-list 'load-path "~/.emacs.d/evil/lalopmak-evil")
(require 'lalopmak-evil)

(require 'yasnippet) ;; not yasnippet-bundle

    (yas--initialize)

(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t
      ;;ido-everywhere t

 ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
)

(require 'ido-vertical-mode)
(ido-vertical-mode t)

;; Use ido everywhere
(require 'ido-ubiquitous)
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


;; This tab override shouldn't be necessary given ido's default 
;; configuration, but minibuffer-complete otherwise dominates the 
;; tab binding because of my custom tab-completion-everywhere 
;; configuration.
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

(require 'flymake)

(defun flymake-get-tex-args (file-name)
	(list "lualatex"
	      (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)	
