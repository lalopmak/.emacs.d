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

