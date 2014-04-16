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

(tool-bar-mode -1)

(require 'package)
(package-initialize)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

(defmacro do-to-package-list (packageList &rest body)
  "Does something to each package of a package list.

Current package assigned to variable 'package'."
  `(mapc (lambda (package) ,@body ) ,packageList))

(defun install-if-necessary (package)
  "Installs a package if it is not already"
  (or (package-installed-p package)
      (progn
        (unless (assoc package package-archive-contents)
          (package-refresh-contents))
        (package-install package))))

;; loads the listed packages, installing if necessary
(do-to-package-list '(magit rainbow-mode yasnippet package ido-vertical-mode ido-ubiquitous flx-ido linum-relative hlinum centered-cursor-mode edit-server edit-server-htmlize ace-jump-mode imenu-anywhere markdown-mode nlinum ag hy-mode latex-pretty-symbols anaphora combinators kmacro-decision key-chord ;;ww3m
;;for clojure
 auto-complete
 paredit popup  rainbow-delimiters grizzl fiplr)
                    (install-if-necessary package)
                    (require package))

;;installs the following packages (without loading) if necessary
(do-to-package-list '(dired+ auctex color-theme undo-tree clojure-mode nrepl ac-nrepl

assemblage-theme
)
                    (if (listp package)
                        ;;accepts lists like (downloaded-package required-package)
                        (progn (install-if-necessary (car package))
                               (require (cadr package)))
                      (install-if-necessary package)))

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


(defun add-modes-to-hooks (hooks modes)
  "Takes lists of hooks and modes and adds all modes to hooks"
  (dolist (hook hooks)
    (dolist (mode modes)
      (add-hook hook mode))))

(defun add-mode-to-hooks (hooks mode)
  "Takes list of hooks and a mode, adds mode to all hooks."
  (add-modes-to-hooks hooks (list mode)))

(defvar init-working-mode-hooks '(emacs-lisp-mode-hook
                                  clojure-mode-hook
                                  python-mode-hook
                                  haskell-mode-hook
                                  text-mode-hook
                                  latex-mode-hook
                                  LaTeX-mode-hook
                                  )
"Hooks for working modes (e.g. not git buffers)")

;;;;;;;Packages retrieved via git

;;evil
(require-online-package-else-git-clone 'evil "git://gitorious.org/evil/evil.git" )

(require-online-package-else-git-clone 'surround "https://github.com/timcharper/evil-surround" )
(global-surround-mode 1)


(require-online-package-else-git-clone 'evil-nerd-commenter "https://github.com/redguardtoo/evil-nerd-commenter" )
;; (require-online-package-else-git-clone 'evil-nerd-commenter "https://github.com/lalopmak/evil-nerd-commenter" )

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
;; (require 'lalopmak-evil-default-comparison)
;; (require 'lalopmak-evil-mnemonic)

(defun copy-register (source destination)
  "Copies content of source register to destination register"
  (interactive "*cSource Register: \ncDestination Register:")
  (set-register destination (get-register source)))


(evil-mode 1)

(setq key-chord-two-keys-delay .05) 
(key-chord-define-global "tn" 'evil-normal-state)
(key-chord-define-global "tw" 'evil-normal-state)
(key-chord-define-global "ft" 'delete-backward-char)
(key-chord-define-global "fn" 'delete-backward-char)
(key-chord-mode 1)

(require-online-package-else-git-clone 'stopwatch "https://github.com/lalopmak/stopwatch" )

(require-online-package-else-git-clone 'expand-region "https://github.com/magnars/expand-region.el" )

;;Color Themes
(require-else-git-clone 'obsidian-theme "https://github.com/mswift42/obsidian-theme")
(require-else-git-clone 'color-theme-tangotango "https://github.com/juba/color-theme-tangotango")
(require-else-git-clone 'soft-charcoal-theme "https://github.com/lalopmak/soft-charcoal-theme")

(add-to-list 'custom-theme-load-path
             (init-online-packages-directory 'color-theme-tangotango)
             (init-online-packages-directory 'obsidian-theme))

;; (load-theme 'obsidian t)
;; (load-theme 'tangotango t)
(load-theme 'soft-charcoal t)

(setq-default frame-background-mode 'dark)
(set-face-attribute 'default nil :height 120)

;;Snippets collection
(defvar init-snippets-dir "~/.emacs.d/snippets/")

(unless (file-exists-p init-snippets-dir)
  ;;Clones the collection
  (git-clone "https://github.com/lalopmak/snippets" init-snippets-dir)

  ;;Copies them over to yasnippet directory
  (execute-process "ruby" (concat (file-name-as-directory init-snippets-dir) "update_snippets.rb")))




;;;;;;  Packages retrieved via wget
(require-online-package-else-wget 'prolog "http://bruda.ca/_media/emacs/prolog.el")



;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")




;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; (setq
;;  el-get-sources
;;  '(el-get				; el-get is self-hosting
;;    ;; kill-ring-ido                     ; just gonna fork this
;;    ))

;; ;;adds the fetched el-get packages to load-path and requires them
;; (do-to-package-list el-get-sources
;;                     (add-to-list 'load-path (init-online-packages-directory package "~/.emacs.d/el-get/"))
;;                     (require package))

;; (el-get 'sync el-get-sources)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 100)

(setq recentf-auto-cleanup 'never) ;;apparently needed for tramp

(yas--initialize)
(yas-global-mode 1)

;; (iswitchb-mode t)
(require 'ido)
(ido-mode 'both) ;; for buffers and files

(setq ;; ido-max-prospects 9   ;how many matches in display string, needs to be just enough for max size
      ido-everywhere t
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ido-case-fold  t                 ; be case-insensitive
      ido-enable-last-directory-history t ; remember last used dirs
      ido-record-commands t
      ido-max-work-directory-list 60   ; should be enough
      ido-max-work-file-list      100   ; remember many
      ido-use-virtual-buffers t

      ido-enable-flex-matching t
      flx-ido-mode 1
      flx-ido-use-faces nil
      ;; ido-use-faces nil   ;to see flx highlights
      )

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration.
(defmacro add-ido-hook (key hook)
  `(add-hook 'ido-setup-hook
             (lambda ()
               (define-key ido-completion-map ,key ,hook))))

(defun init-ido-complete-tab ()
  "Ido-complete only if there's a common match.

Otherwise, goes to the next match (C-s)"
  (interactive)
  (if (and (stringp ido-common-match-string)
	   (stringp ido-text)
           (> (length ido-common-match-string) (length ido-text)))
      (ido-complete)
    (ido-next-match)))

(add-ido-hook [tab]
              'init-ido-complete-tab)
;; (add-ido-hook " " (lambda () (interactive (insert " "))))
(add-ido-hook " " 'ido-restrict-to-matches)
(add-ido-hook [?\C- ] (lambda () (interactive (insert " "))))

;;alternate mapping
;; (add-ido-hook " "
;;               'ido-complete-space)
;; (add-ido-hook [tab] 'ido-restrict-to-matches)
;; (add-ido-hook [?\C- ] (lambda () (interactive (insert " "))))

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

;;;;;;;;
;;fiplr/grizzl search
;;;;;;;;
;; (global-set-key (kbd "M-b") 'fiplr-find-file)
(define-key *grizzl-keymap* (kbd "C-s")   'grizzl-set-selection-1)
(define-key *grizzl-keymap* (kbd "<tab>")    'grizzl-set-selection-1)
(define-key *grizzl-keymap* (kbd "C-r") 'grizzl-set-selection+1) 


;; I want spaces for indentation
(setq-default indent-tabs-mode nil)


(set-scroll-bar-mode 'right)   ; replace 'right with 'left to place it to the left

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy t)
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
;; (add-hook 'LaTeX-mode-hook 'latex-unicode-simplified)

(cl-defun latex-compile (&optional (file-name buffer-file-name))
  (interactive)
  (save-buffer)
  (compile (latex-compile-command file-name)))

(require 'reftex)
(setq reftex-plug-into-AUCTeX t)


(evil-ex-define-cmd "latex" 'latex-compile)

;;;;;;;;;;;;;;;;
;;External Processes
;;;;;;;;;;;;;;;;

(lalopmak-evil-directory-process "nemo")
(lalopmak-evil-directory-process "thunar")

;;ranger (in x-terminal-emulator)
;; (lalopmak-evil-directory-process "rgr" nil nil "ranger" "ranger") ;buggy

(lalopmak-evil-directory-process "gnome-terminal"
  "--command=ranger \"%s\""
  t
  "lalopmak-evil-ranger"
  "ranger")

;;x-terminal-emulator
(lalopmak-evil-directory-process "gnome-terminal"
  "--working-directory="
  nil
  nil
  "shell")
(evil-ex-define-cmd "gnome-terminal" "shell")

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
;;record key frequencies (most run at init-delayed-startup-actions)
;;;;;;;;
(require-online-package-else-git-clone 'keyfreq "https://github.com/lalopmak/keyfreq")

;;;;;;;
;;Auto Complete
;;;;;;;

(global-auto-complete-mode t)

(require 'auto-complete-config)

;; (add-to-list 'ac-dictionary-directories "")
(ac-config-default)

;;;;;;;
;;kill-ring-ido
;;;;;;;

(require-online-package-else-git-clone 'kill-ring-ido "https://github.com/lalopmak/kill-ring-ido")
(global-set-key (kbd "M-y") 'kill-ring-ido)
(setq kill-ring-ido-shortage-length 24)


;;;;;;;
;;Edit-Server (for text areas in browsers) - see also init-delayed-startup-actions
;;;;;;;


(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)

(add-hook 'edit-server-edit-mode-hook
          (lambda()
            ;;Adds spell check to edit-server
            (speck-mode 1)
            ;; (setq auto-save-interval 20) ; 20 letters
            ;; (setq auto-save-timeout 10) ; ten idle seconds
            ;;Sets size
            (if window-system
                (set-frame-size (selected-frame) 80 12))))

(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;;;;;;;
;;Symbol prettification
;;;;;;;

;; (global-prettify-symbols-mode)

(defvar init-pretty-symbols '(("lambda" . ?λ)
                              ("->" . ?→)
                              ("<-" . ?←)
                              ("<=" . ?≤)
                              ("=>" . ?≥)
                              ("!=" . ?≠)
))

(defun init-add-pretty-symbols ()
  "Adds init's collection of pretty symbols to prettify-symbols-alist"
  (unless (boundp 'prettify-symbols-alist)
    (defvar-local prettify-symbols-alist nil
  "Alist of symbol prettifications.
Each element looks like (SYMBOL . CHARACTER), where the symbol
matching SYMBOL (a string, not a regexp) will be shown as
CHARACTER instead."))
  (dolist (replacement init-pretty-symbols)
    (push replacement prettify-symbols-alist)))

(add-mode-to-hooks init-working-mode-hooks
                   (lambda ()
                     (when (fboundp 'pretty-symbols-mode)
                       (pretty-symbols-mode))))

;; (add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
;; (add-hook 'clojure-mode-hook 'pretty-symbols-mode)
  ;; (prettify-symbols-mode)

;;;;;;;
;;multiple cursors
;;;;;;;

;; next symbol to C-i
;; (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
;; (global-set-key (kbd "H-i") 'mc/mark-next-like-this)

;; (global-set-key (kbd "C-n") 'mc/mark-previous-like-this)

;; (global-unset-key (kbd "C-/"))
;; (global-set-key (kbd "C-/") 'mc/mark-all-symbols-like-this)

;; (global-unset-key (kbd "C-<down-mouse-1>"))
;; (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)

;;;;;;;
;;Behaviors
;;;;;;;

(defvar init-centered-cursor nil, "Wheter or not we set centered cursor by default")
(defvar init-blinking-cursor t, "Whether or not the cursor should blink")
(defvar init-cursor-color "#ffffff", "Cursor color")

(defvar init-use-nlinum nil, "If nil, use linum.  Otherwise, use nlinum." )
(defvar init-linum-relative-mode t, "Whether or not we start out with relative line numbers")
(defvar init-linum-relative-show-current-line t, "Whether or not our relative line numbers should show the current line as absolute")
(defvar init-linum-current-line-foreground "red", "The foreground color of the current line")
(defvar init-linum-current-line-background nil, "The background color of the current line")

(defvar init-highlight-line-gui t, "Whether or not to highlight current line in a gui")
(defvar init-highlight-line-terminal nil, "Whether or not to highlight current line in a terminal")

(defvar init-use-header-for-notify-files-changed t, "Whether or not to popup header saying that file has been externally modified")
(defvar init-notify-files-changed-interval 5, "Time in seconds between checking for file changes")

(defvar default-frame-icon "/usr/share/icons/hicolor/scalable/apps/emacs24.svg", "Default icon to use for frames, ignored if nil")


;;cursor blinks every that number of seconds
(setq blink-cursor-interval 0.7)

(when init-cursor-color (set-cursor-color init-cursor-color))
(unless init-blinking-cursor (blink-cursor-mode 0))

;;;;;;
;; Delayed startup function
;;;;;;

(defvar init-delayed-startup-time 15, "Time in seconds to delay init-delayed-startup-actions")

(defun init-activate-keyfreq (&optional save-on-exit use-abs-times abs-times debugging)
  "Starts keyfreq-mode with the appropriate options."
  (interactive)
  (when use-abs-times (setq keyfreq-use-abs-times t))
  (when abs-times (setq keyfreq-autosave-abs-times abs-times))
  (when debugging (setq keyfreq-debugging debugging))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (when save-on-exit
    (keyfreq-save-on-exit-mode)))

(defun init-delayed-startup-actions ()
  "Actions to perform after a delay."
  (when (boundp 'server-name)
    ;; Edit server being started instead by
    ;;      alias editserver='emacsclient -a "" -s "Edit Server" -e "(edit-server-start)"'
    ;; (when (equal server-name "Edit Server")
    ;;   (edit-server-start))
    (cond ((equal server-name "User Server")
           (init-activate-keyfreq nil t '(0 20 40) t))
          ((equal server-name "Edit Server")
           (init-activate-keyfreq nil t '(10 30 50) t)))))

(when init-delayed-startup-time
  (run-at-time init-delayed-startup-time nil 'init-delayed-startup-actions))

;;;;;;;
;; Globalized nlinum mode, until they make official one
;;;;;;;

(defun nlinum-on () (unless (minibufferp) (nlinum-mode 1)))
(define-globalized-minor-mode global-nlinum-mode nlinum-mode nlinum-on)

(if init-use-nlinum
    (global-nlinum-mode 1)
  (global-linum-mode 1))

;;;;;;;;;;;;;;;;
;;Frame title and icon setter
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

(if default-frame-icon
    (add-to-list 'default-frame-alist '(icon-type . default-frame-icon)))



;;;;;;;;
;;Init Mode: stuff to happen in every buffer
;;;;;;;;
(defun init-mode-on-new-buffer ()
  "Commands we want to activate upon opening new file/buffer"
  (if (and init-highlight-line-gui
           (display-graphic-p))
      (hl-line-mode 1))
  (if (and init-highlight-line-terminal
           (not (display-graphic-p)))
      (hl-line-mode 1)) ; turn on highlighting current line
  (if init-use-nlinum
      (nlinum-mode t)
    (linum-mode t))
  (init-add-pretty-symbols)
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


(if init-centered-cursor (global-centered-cursor-mode t))

;;soft line wrap by word at boundary
(global-visual-line-mode 1)

;;;;Enable commands

;;M-x downcase-region
(put 'downcase-region 'disabled nil)

;;;Reloading buffer/checking for external modification
(global-set-key (kbd "<f5>") 'init-reload-buffer)

(if (not init-use-header-for-notify-files-changed)
    (run-at-time nil init-notify-files-changed-interval (lambda () (init-notify-files-changed nil)))
  (add-hook 'before-save-hook
            (lambda ()
              (setq header-line-format nil)))
  (run-at-time nil init-notify-files-changed-interval (lambda () (init-notify-files-changed t))))



(column-number-mode 1)    ;  displays line and column number in status bar

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

 (defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))

(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)

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

(when (file-exists-p  "~/.emacs.d/online-packages/lisptree")
  (add-to-list 'load-path "~/.emacs.d/online-packages/lisptree")
  (require 'lisptree))

(require 'tramp)
(setq tramp-default-method "scp")

(ad-activate-all) ;activates all advice


(defvar init-times-record "")

(defun init-record-current-time ()
  "Prints the current time into file."
  (interactive (list (read-file-name "Record File: ")))
  (let ((record (concat (current-time-string) "\n")))
    (setq init-times-record (concat init-times-record record))
    ))

(defvar init-timer nil)

(defun init-timer-restart ()
  (init-record-current-time)
(when init-timer (cancel-timer init-timer))
(setq init-timer (run-at-time 60 nil 'init-timer-restart) ) )

(init-timer-restart)

(define-coding-system-alias 'UTF-8 'utf-8)










(defvar init-linum-relative-format-string "%2d") 

(add-hook 'linum-before-numbering-hook 'init-linum-relative-get-format-string)

;; (defun init-linum-relative-get-format-string ()
;;   (let* ((width (length (number-to-string
;;                          (count-lines (point-min) (point-max)))))
;;          (format (concat "%" (number-to-string width) "d")))
;;     (setq init-linum-relative-format-string format)))

 
(defun init-linum-relative-get-format-string ()
  (let* ((width (max 2
                     (if init-linum-relative-show-current-line
                         (length (number-to-string (line-number-at-pos))) 
                       -1))) 
         (format (concat "%" (number-to-string width) "d")))
    (when init-linum-relative-mode
      (setq init-linum-relative-format-string format))))


(defvar init-linum-relative-current-line-number 0)

(setq linum-format 'init-linum-relative-relative-line-numbers)

(defun init-linum-relative-relative-line-numbers (line-number)

  (let ((line-number-display (if (or (not init-linum-relative-mode)
                                     (and init-linum-relative-show-current-line
                                          (eq line-number init-linum-relative-current-line-number)))
                                 line-number
                               (abs (- line-number init-linum-relative-current-line-number)))))
    (propertize (format init-linum-relative-format-string line-number-display) 'face 'linum
                'font-lock-face '(:foreground "red")))) 

(defadvice linum-update (around init-linum-relative-update)
  (let ((init-linum-relative-current-line-number (line-number-at-pos)))
    ad-do-it))

(ad-activate 'linum-update)

(defun init-linum-relative-toggle ()
  "Toggles relative line numbers."
  (interactive)
  ;;for some reason, can't just set it to (not (init-linum-relative-mode))
  (if init-linum-relative-mode
      (setq init-linum-relative-mode nil)
    (setq init-linum-relative-mode t))) 

;;linum relative toggle
(evil-ex-define-cmd "relative" 'init-linum-relative-toggle) 
(hlinum-activate)

(set-face-attribute 'linum-highlight-face
                    nil
                    :foreground init-linum-current-line-foreground 
                    :background init-linum-current-line-background
                    ) 
