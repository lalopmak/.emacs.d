;;; License

;; This software is licensed under the CC0 1.0 Public Domain Declaration, as
;; released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.
;; This software comes with NO WARRANTIES OR GUARANTEES WHATSOEVER.


;; uncomment if using qwerty
;; (defvar lalopmak-layout-map 'colemak-to-qwerty)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defmacro do-to-package-list (packageList &rest body)
  "Does something to each package of a package list"
  `(mapc (lambda (package) ,@body ) ,packageList)) 

(defun install-if-necessary (package)
  "Installs a package if it is not already"
  (or (package-installed-p package) 
      (package-install package)))

;; loads the listed packages, installing if necessary
(do-to-package-list '(magit rainbow-mode yasnippet package ido-vertical-mode ido-ubiquitous linum-relative centered-cursor-mode edit-server)
                    (install-if-necessary package)
                    (require package))

;;installs the following packages (without loading) if necessary
(do-to-package-list '(dired+ auctex color-theme)
                    (install-if-necessary package))

;;;;;Package retrieval helpers

;;the base directory for online retrieved packages
(defvar online-packages-directory "~/.emacs.d/online-packages/") 

(cl-defun init-online-packages-directory (package &optional (baseDir online-packages-directory)) 
  "The directory in which this online package would be installed"
  (file-truename (concat baseDir 
                         (symbol-name package)))) 

(defun execute-process (processName &rest processArgs)
  "Executes a process with given args, all strings.  Returns status (check with zerop)"
  (let ((process (or (executable-find processName)
                            (error (concat "Unable to find " processName)))))
    (apply 'call-process
           process 
           nil
           nil
           nil
           processArgs)))


(defun fetch-online (fetcher &rest processArgs)
  "Loads and requires package, fetching with fetcher process if necessary"
  (apply 'execute-process fetcher processArgs))

(defun fetch-online-then-require (package url fetcher &rest processArgs)
  "Fetches something online using the fetcher process with processArgs, then requires the associated package"
  (if (zerop (apply 'fetch-online fetcher processArgs))
        (require package)
      (error "Couldn't fetch %s from %s" package url)))

(defun require-or-fetch-online (package packageDir url fetcher &rest processArgs)
  "Loads and requires package, fetching with fetcher process if necessary"
  (add-to-list 'load-path packageDir)
  (unless (require package nil 'noerror)
    (apply 'fetch-online-then-require package url fetcher processArgs)))
 
(defun require-and-fetch-online (package packageDir url fetcher &rest processArgs)
  "Fetches (unless already fetched), loads, and requires package."
  (add-to-list 'load-path packageDir)
  (if (file-exists-p packageDir)
      (require package)
    (apply 'fetch-online-then-require package url fetcher processArgs)))

 
(cl-defun require-or-git-clone (package url &optional (packageDir (init-online-packages-directory package))) 
  "Loads and requires packageName, cloning from git url if necessary"
  (require-or-fetch-online package packageDir url "git" "--no-pager" "clone" "-v" url (file-truename packageDir)))

(cl-defun require-and-git-clone (package url &optional (packageDir (init-online-packages-directory package))) 
  "Loads and requires packageName, cloning from git url if not already fetched"
  (require-and-fetch-online package packageDir url "git" "--no-pager" "clone" "-v" url (file-truename packageDir)))


(defun git-clone (url dir) 
  "Loads and requires packageName, cloning from git url if not already fetched"
  (fetch-online "git" "--no-pager" "clone" "-v" url (file-truename dir)))

;;;;;;;Packages retrieved via git

;;evil
(require-and-git-clone 'evil "git://gitorious.org/evil/evil.git" )
(require-and-git-clone 'lalopmak-evil "https://github.com/lalopmak/lalopmak-evil" )

;;tango color theme
(require-or-git-clone 'color-theme-tangotango "https://github.com/juba/color-theme-tangotango")
(add-to-list 'custom-theme-load-path (init-online-packages-directory 'color-theme-tangotango))
(load-theme 'tangotango t)


;;Snippets collection
(defvar init-snippets-dir "~/.emacs.d/snippets/")

(unless (file-exists-p init-snippets-dir) 
  ;;Clones the collection
  (git-clone "https://github.com/lalopmak/snippets" init-snippets-dir)

  ;;Copies them over to yasnippet directory
  (execute-process "ruby" (concat (file-name-as-directory init-snippets-dir) "update_snippets.rb")))



(cl-defun require-or-wget (package url &optional (packageDir (init-online-packages-directory package))) 
  "Loads and requires packageName, fetching with wget if necessary"
  (require-or-fetch-online package packageDir url "wget" url "-P" packageDir))

(cl-defun require-and-wget (package url &optional (packageDir (init-online-packages-directory package))) 
  "Loads and requires packageName, fetching with wget unless already fetched"
  (require-and-fetch-online package packageDir url "wget" url "-P" packageDir))


;;;;;;  Packages retrieved via wget
(require-and-wget 'prolog "http://bruda.ca/_media/emacs/prolog.el")



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
                    (add-to-list 'load-path (init-online-packages-directory package "~/.emacs.d/el-get/"))
                    (require package))                      

(el-get 'sync el-get-sources)

(global-set-key (kbd "M-y") 'kill-ring-ido)
(setq kill-ring-ido-shortage-length 24) 

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)


(yas--initialize)
(yas-global-mode 1)

;; (iswitchb-mode t)
(require 'ido)
(ido-mode 'both) ;; for buffers and files

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ido-case-fold  t                 ; be case-insensitive
      ido-enable-last-directory-history t ; remember last used dirs
      ido-record-commands t
      ido-max-work-directory-list 60   ; should be enough
      ido-max-work-file-list      100   ; remember many
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

(defun concat-with-space (head &rest tail)
  (if tail
      (concat head 
              " " 
              (apply 'concat-with-space tail))
    head))

(defvar latex-base-compile-command (concat-with-space "lualatex -interaction=nonstopmode"))

(defun latex-compile-command (file-name)
  (concat-with-space latex-base-compile-command file-name))

(defun flymake-get-tex-args (file-name)
  (list "lualatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)	

(cl-defun latex-compile (&optional (file-name buffer-file-name))
  (interactive)
  (compile (latex-compile-command file-name)))

(require 'reftex)
(setq reftex-plug-into-AUCTeX t)


(evil-ex-define-cmd "latex" 'latex-compile)
;;;;;;;;;;;;;;;;
;;Frame title setter
;;;;;;;;;;;;;;;;

;;how to insert emacs at the end of frame: pick one
(defvar emacs-title-format " @ emacs")
;; (defvar emacs-title-format 
;;   (concat " @ Emacs "
;;           emacs-version))

;;sets frame title: "filename (directory) [emacs-title-format]"
(setq frame-title-format
      '("%b " (:eval (if (buffer-file-name)  ;adds the (directory)
                         (concat "("
                                 (abbreviate-file-name (file-name-directory buffer-file-name)) 
                                 ")"))) 
        emacs-title-format))

;;;;;;;
;;Spell check
;;;;;;

(require-or-wget 'speck "http://www.emacswiki.org/emacs/download/speck.el")

;;;;;;;
;;Edit-Server (for text areas in browsers)
;;;;;;;

(edit-server-start)

(add-hook 'edit-server-edit-mode-hook
  (lambda()
    ;;Adds spell check to edit-server
    (speck-mode 1)
    ;;Sets size
    (if window-system
        (set-frame-size (selected-frame) 80 12))))

;;;;;;;
;;Behaviors
;;;;;;;

(defmacro init-activate-on-open ()
  "Commands we want to activate upon opening new file/buffer"
  `(progn (linum-mode t)
          (centered-cursor-mode t)))

;;Global mode for those same commands (because not all openings are covered by our advice)
(global-linum-mode t)
(global-centered-cursor-mode t)

;;;;Enable commands

;;M-x downcase-region
(put 'downcase-region 'disabled nil)

(defadvice ido-find-file (after init-new-found-file ())
  "Activates those commands upon opening file"
  (init-activate-on-open))

(defadvice ido-switch-buffer (after nint-new-buffer ())
  "Activate those commands upon switching buffer"
  (init-activate-on-open))

(column-number-mode 1)    ;  displays line and column number in status bar

;; (global-hl-line-mode 1) ; turn on highlighting current line
(delete-selection-mode 1) ; delete seleted text when typing
;; (transient-mark-mode 1) ; highlight text selection
;; (setq show-paren-style 'expression) ; highlight entire bracket expression



;;Sets clipboard to primary by default
;; (setq x-select-enable-clipboard nil)
;; (setq x-select-enable-primary t)

;; Disables highlight-copying in "insert mode"
(setq mouse-drag-copy-region nil)


;;doesn't seem to make a difference
;; (global-set-key "\C-w" 'clipboard-kill-region)
;; (global-set-key "\M-w" 'clipboard-kill-ring-save)

(global-set-key "\C-y" 'clipboard-yank)


(ad-activate-all) ;activates all advice

;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
 (define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

