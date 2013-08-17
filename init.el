;;; License

;; This software is licensed under the CC0 1.0 Public Domain Declaration, as
;; released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS",
;; WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
;; THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;; uncomment if using qwerty
;; (defvar lalopmak-layout-map 'colemak-to-qwerty)

;;This script calls programs: git, ruby, wget



(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

(package-initialize)

(defmacro do-to-package-list (packageList &rest body)
  "Does something to each package of a package list"
  `(mapc (lambda (package) ,@body ) ,packageList)) 

(defun install-if-necessary (package)
  "Installs a package if it is not already"
  (or (package-installed-p package) 
      (package-install package)))

;; loads the listed packages, installing if necessary
(do-to-package-list '(magit rainbow-mode yasnippet package ido-vertical-mode ido-ubiquitous linum-relative centered-cursor-mode edit-server ace-jump-mode imenu-anywhere markdown-mode nlinum
;;for clojure 
 auto-complete 
 paredit popup  rainbow-delimiters)
                    (install-if-necessary package)
                    (require package))

;;installs the following packages (without loading) if necessary
(do-to-package-list '(dired+ auctex color-theme undo-tree  clojure-mode nrepl ac-nrepl 
)
                    (install-if-necessary package))

;;evals our libraries
(load-file (expand-file-name "init-libraries.el" "~/.emacs.d/"))


;; rainbow delimiters
(global-rainbow-delimiters-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t)
 '(hl-line ((t (:inherit highlight :background ;#1f2f2f"
"#243434"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#556677"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#408000"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#003db4"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#819a00"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#5393b3"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#e69500"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#009a63"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(show-paren-match ((((class color) (background dark)) (:background "#999999")))))


;;;;;;;Packages retrieved via git

;;evil
(require-online-package-else-git-clone 'evil "git://gitorious.org/evil/evil.git" )

(require-online-package-else-git-clone 'surround "https://github.com/timcharper/evil-surround" )
(global-surround-mode 1)

;; (require-online-package-else-git-clone 'evil-nerd-commenter "https://github.com/redguardtoo/evil-nerd-commenter" )
(require-online-package-else-git-clone 'evil-nerd-commenter "https://github.com/lalopmak/evil-nerd-commenter" )

(define-key evil-normal-state-map "," 'evilnc-comment-operator)
(define-key evil-visual-state-map "," 'evilnc-comment-operator)
;;if local copy of undo-tree is required
;; (require-online-package-else-git-clone 'undo-tree "http://www.dr-qubit.org/git/undo-tree.git")

;; ;;symlinks undo-tree.el into evil if necessary
;; (let* ((undo-tree-concater (lambda (package) (concat (file-name-as-directory (init-online-packages-directory package))
;;                                                      "undo-tree.el")))
;;        (evil-undo-tree-file (funcall undo-tree-concater 'evil))
;;        (undo-tree-file (funcall undo-tree-concater 'undo-tree)))
;;   (unless (file-exists-p evil-undo-tree-file)
;;     (make-symbolic-link undo-tree-file evil-undo-tree-file)))
 

(unless-dir-exists-git-clone 'lalopmak-evil "https://github.com/lalopmak/lalopmak-evil" )
(require 'lalopmak-evil)
;; (require 'lalopmak-evil-mnemonic)

(evil-mode 1)


(require-online-package-else-git-clone 'stopwatch "https://github.com/lalopmak/stopwatch" )

(require-online-package-else-git-clone 'expand-region "https://github.com/magnars/expand-region.el" )

;;tango color theme
(require-else-git-clone 'color-theme-tangotango "https://github.com/juba/color-theme-tangotango")
(add-to-list 'custom-theme-load-path (init-online-packages-directory 'color-theme-tangotango))
(load-theme 'tangotango t)

(setq-default frame-background-mode 'dark)

;;Snippets collection
(defvar init-snippets-dir "~/.emacs.d/snippets/")

(unless (file-exists-p init-snippets-dir) 
  ;;Clones the collection
  (git-clone "https://github.com/lalopmak/snippets" init-snippets-dir)

  ;;Copies them over to yasnippet directory
  (execute-process "ruby" (concat (file-name-as-directory init-snippets-dir) "update_snippets.rb")))




;;;;;;  Packages retrieved via wget
(require-online-package-else-wget 'prolog "http://bruda.ca/_media/emacs/prolog.el")



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

(defun latex-word-count ()
  "Gets wordcount for latex file"
  (interactive)
  (save-buffer)
  (shell-command (concat "texcount \""
                         (buffer-file-name)
                         "\"")))

(evil-ex-define-cmd "wordcount" 'latex-word-count)

(defun flymake-get-tex-args (file-name)
  (list "lualatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)	

(cl-defun latex-compile (&optional (file-name buffer-file-name))
  (interactive)
  (save-buffer)
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
;;Ace jump
;;;;;;;

;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(setq ace-jump-mode-move-keys
      (list    ?n ?e ?i ?o ?h ?u ?y ?k ?a ?t ?d ?w ?f ?p ?l ?r ?s ?v  
            ;;?N ?E ?I ?O ?K ?U ?Y ?L
            ))
;;;;;;;
;;Spell check
;;;;;;

(require-else-wget 'speck "http://www.emacswiki.org/emacs/download/speck.el")

;;;;;;;;
;;record key frequencies
;;;;;;;;
(require-online-package-else-git-clone 'keyfreq "https://github.com/lalopmak/keyfreq")

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

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

(defvar init-centered-cursor nil, "Wheter or not we set centered cursor by default")
(defvar init-blinking-cursor t, "Whether or not the cursor should blink")

(defvar init-relative-mode nil, "Whether or not we start out with relative line numbers")

(defvar init-highlight-line t, "Whether or not to highlight current line")

;;cursor blinks every that number of seconds
(setq blink-cursor-interval 0.7)


(unless init-blinking-cursor (blink-cursor-mode 0))

;;;;;;;
;; Globalized nlinum mode, until they make official one
;;;;;;;
(define-globalized-minor-mode global-nlinum-mode nlinum-mode nlinum-on)
(defun nlinum-on () (unless (minibufferp) (nlinum-mode 1)))
(global-nlinum-mode 1)

;;;;;;;;
;;Init Mode: stuff to happen in every buffer
;;;;;;;;
(defun init-mode-on-new-buffer ()
  "Commands we want to activate upon opening new file/buffer"
  (nlinum-mode t)
  (if init-centered-cursor (centered-cursor-mode t)))

(define-minor-mode init-mode "Stuff to happen in every buffer")
(define-globalized-minor-mode global-init-mode init-mode init-mode-on-new-buffer)
(global-init-mode 1)
;;;;;;;;


;;Necessary since hooks don't seem to work in fundamental mode
;;======
(defadvice ido-find-file (after init-new-found-file ())
  "Activates those commands upon opening file"
  (init-mode-on-new-buffer))

(defadvice ido-switch-buffer (after init-new-buffer ())
  "Activate those commands upon switching buffer"
  (init-mode-on-new-buffer))
;;======

;;linum-relative starts on by default, toggle off if necessary
(unless init-relative-mode (linum-relative-toggle))

(if init-centered-cursor (global-centered-cursor-mode t))

;;soft line wrap by word at boundary
(global-visual-line-mode 1)

;;;;Enable commands

;;M-x downcase-region
(put 'downcase-region 'disabled nil)




(column-number-mode 1)    ;  displays line and column number in status bar

(if init-highlight-line (global-hl-line-mode 1)) ; turn on highlighting current line
(delete-selection-mode 1) ; delete seleted text when typing
;; (transient-mark-mode 1) ; highlight text selection
;; (setq show-paren-style 'expression) ; highlight entire bracket expression


;; Disables highlight-copying in "insert mode"
(setq mouse-drag-copy-region nil)

;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;doesn't seem to make a difference
;(global-set-key "\C-w" 'clipboard-kill-region)
;; (global-set-key "\M-w" 'clipboard-kill-ring-save)

(global-set-key "\C-y" 'clipboard-yank)

;;Makes sure the top of the clipboard is saved onto our kill ring beforehand
;;avoiding issue where it's never used due to an intermediate kill
(setq save-interprogram-paste-before-kill t)

;; If parent directory of write-file doesn't exist, prompt user for creating it
(add-hook 'before-save-hook
          (lambda ()
            (let ((parent (buffer-directory)))
                (when (and parent
                           (not (file-exists-p parent))
                           (y-or-n-p (concat "Directory " parent " does not exist. Create it?")))
                  (make-directory parent t)))))

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

;;Use yasnippet ido menu instead of inefficient prompt menus
(setq yas-prompt-functions '(yas-ido-prompt))

(put 'upcase-region 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/online-packages/lisptree")
(require 'lisptree)

(ad-activate-all) ;activates all advice


