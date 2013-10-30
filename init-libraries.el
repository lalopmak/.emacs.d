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


;;;;;Package retrieval helpers

;;the base directory for online retrieved packages
(defvar online-packages-directory "~/.emacs.d/online-packages/")

(cl-defun init-online-packages-directory (package &optional (baseDir online-packages-directory))
  "The directory in which this online package would be installed"
  (file-truename (concat (file-name-as-directory baseDir)
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

(defun execute-process-noblock (processName &rest processArgs)
  "Executes a process with given args, all strings.  Does not wait for PROCESSNAME to terminate; returns nil."
  (let ((process (or (executable-find processName)
                     (error (concat "Unable to find " processName)))))
    (apply 'call-process
           process
           nil
           0
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

(defun require-else-fetch (package packageDir url fetcher &rest processArgs)
  "Loads and requires package (possibly not online version), If package load fails, fetch online version."
  (add-to-list 'load-path packageDir)
  (unless (require package nil 'noerror)
    (apply 'fetch-online-then-require package url fetcher processArgs)))

(defun unless-dir-exists-fetch (packageDir fetcher &rest processArgs)
  "If packageDir doesn't exist, fetch from online."
  (add-to-list 'load-path packageDir)
  (unless (file-exists-p packageDir)
    (apply 'fetch-online fetcher processArgs)))

(defun require-online-package-else-fetch (package packageDir url fetcher &rest processArgs)
  "Fetches online version of package (unless already fetched), then loads and requires it."
  (add-to-list 'load-path packageDir)
  (if (file-exists-p packageDir)
      (require package)
    (apply 'fetch-online-then-require package url fetcher processArgs)))


;; git fetchers
(defun git-args (url dir)
  "Returns the list of args to a git call"
  (list "--no-pager" "clone" "-v" url (file-truename dir)))

(cl-defun require-else-git-clone (package url &optional (packageDir (init-online-packages-directory package)))
  "Loads and requires package (possibly not online version), If package load fails, git clone online version."
  (apply 'require-else-fetch package packageDir url "git" (git-args url packageDir)))

(cl-defun require-online-package-else-git-clone (package url &optional (packageDir (init-online-packages-directory package)))
  "Fetches online version of package (unless already fetched), then loads and requires it."
  (apply 'require-online-package-else-fetch package packageDir url "git" (git-args url packageDir)))

(cl-defun unless-dir-exists-git-clone (package url &optional (packageDir (init-online-packages-directory package)))
  "If packageDir doesn't exist, clone from git url."
  (apply 'unless-dir-exists-fetch packageDir "git" (git-args url packageDir)))

(defun git-clone (url dir)
  "Loads and requires packageName, cloning from git url if not already fetched"
  (apply 'fetch-online "git" (git-args url dir)))


;;;wget fetchers

(cl-defun require-else-wget (package url &optional (packageDir (init-online-packages-directory package)))
  "Loads and requires package (possibly not online version), If package load fails, wget online version."
  (require-else-fetch package packageDir url "wget" url "-P" packageDir))

(cl-defun require-online-package-else-wget (package url &optional (packageDir (init-online-packages-directory package)))
  "Fetches online version of package (unless already fetched), then loads and requires it."
  (require-online-package-else-fetch package packageDir url "wget" url "-P" packageDir))

(defun buffer-directory ()
  "Name of buffer's current directory, or nil if not a file"
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun init-reload-buffer ()
  "Reloads current buffer from file.  If init-use-header-for-notify-files-changed non-nil, clears header since we are saving."
  (interactive)
  (revert-buffer t t)
  (when init-use-header-for-notify-files-changed
    (setq header-line-format nil)))

(defmacro loop-frames-windows (args &rest code)
  "Executes CODE for every window in every frame.

ARGS is a tuple (frame-arg window-arg &optional buffer-arg)

The user-designated variable frame-arg will contain the current frame,
and be usable in CODE.  Similarly for window-arg and buffer-arg.

Example usage: (loop-frames-windows (frame window buffer) ...)"
  (declare (indent defun))
  (let* ((frame-arg (car args))
         (window-arg (cadr args))
         (buffer-arg (caddr args))
         (tail (if buffer-arg
                   `((let ((,buffer-arg (window-buffer ,window-arg)))
                       ,@code))
                 code)))
    `(loop for ,frame-arg in (frame-list)
           do (loop for ,window-arg in (window-list ,frame-arg)
                    do ,@tail))))

(defun init-notify-files-changed (&optional showheader)
  "With SHOWHEADER nil, causes emacs to check if file associated with each visible buffer has been externally modified.

With SHOWHEADER non-nil, also creates a header-line if this is so."
  (loop-frames-windows (frame window buffer)
    (set-buffer buffer)
    (cond ((not showheader) (verify-visited-file-modtime buffer))  ;;only trigger file recheck (side-effect)
          ((verify-visited-file-modtime buffer) (setq header-line-format nil))  ;; file has not been externally written to
          ;;file has been externally written to; show the header
          (t (let* ((long-filename (abbreviate-file-name (buffer-file-name buffer)))
                    (short-filename (file-name-nondirectory long-filename))
                    (init-reload-buffer-binding (car (where-is-internal 'init-reload-buffer)))
                    (init-reload-buffer-str (if init-reload-buffer-binding
                                                (key-description init-reload-buffer-binding)
                                              "M-x init-reload-buffer"))
                    (header (lambda (filename &optional shorten)
                              (format "%s. Press %s to reload%s"
                                      (propertize (format "%s%s %s"
                                                          (if shorten "" "The file ")
                                                          filename
                                                          (if shorten "changed" "has changed on disk"))
                                                  'face '(:foreground "#cc1122"))
                                      init-reload-buffer-str
                                      (if shorten "" " it"))))
                    (longheader-str (funcall header long-filename))
                    (medheader-str (funcall header short-filename))
                    (longheader-len (length longheader-str))
                    (medheader-len (length medheader-str))
                    (width (window-width window))
                    (header-str (cond ((> width longheader-len) longheader-str)
                                      ((> width medheader-len) medheader-str)
                                      (t (funcall header short-filename 'shorten)))))
               (setq header-line-format header-str))))))

;; (setq ido-max-prospects   ;how many matches in display string, needs to be just enough for max size
;;       (let ((max-height (or ido-max-window-height
;;                             max-mini-window-height)))
;;         (cond ((integerp max-height) max-height)
;;               ((display-graphic-p) (ceiling (* max-height 
;;                                                (/ (display-pixel-height)
;;                                                   (frame-char-height)))))
;;               ((t (ceiling (* max-height 
;;                               (display-pixel-height))))))))
