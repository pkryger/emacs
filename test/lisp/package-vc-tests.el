;;; package-vc-tests.el --- Tests for package-vc -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Przemsyław Kryger <pkryger@gmail.com>
;; Keywords: package

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These tests focus on verifying post conditions for `package-vc'
;; operations on packages.  These tests install and load test packages
;; with a sample test implementation, resulting in modification of
;; numerous global variables, for example `load-history', `load-path',
;; `features', etc.  When run with `ert' it may contaminate current
;; Emacs session.  For this reason, tests execute their bodies in
;; `with-package-vc-tests-installed' (which see), that takes care of
;; cleaning up the environment.

;;; Code:

(require 'package-vc)
(require 'package)
(require 'vc-git)
(require 'vc)
(require 'cl-lib)
(require 'info)
(require 'ert-x)
(require 'ert)

(eval-and-compile
  ;; We evaluate the definition at compile-time as well, so that
  ;; `package-vc-test-deftest' can use the value.
  (defvar package-vc-tests-under-test
    '(test-package-1
      test-package-2
      test-package-3
      test-package-4
      test-package-5
      test-package-6
      test-package-7
      test-package-8
      test-package-9)))

(defvar package-vc-tests-preserve-temporary nil
  "When non-nil preserve temporary files produced by tests.
Each test produces a new temporary directory for each package under
test.  This leads to creation of [length of
`package-vc-tests-under-test'] times [number of tests executed]
temporary directories for each tests run.  When this variable is nil
then delete all temporary directories as soon as they are no longer
needed.  When this variable is a symbol, then preserve temporary
directories for the package that matches the symbol.  When this variable
is a list of symbol, then preserve temporary directories for each
package that matches symbol in the list.  When this variable is t then
preserve all temporary directories.  Tests create temporary directories
with `make-temp-file', which see.")

(defvar package-vc-tests-dir)
(defvar package-vc-tests-packages)
(defvar package-vc-tests-bundle)

;; TODO: add test for deleting packages, with asserting
;; `package-vc-selected-packages'

;; TODO: clarify `package-vc-install-all' behaviour with regards to
;; packages installed with `package-vc' but not stored in
;; `package-vc-selected-packages' i.e., packages from ELPAs

(defun package-vc-tests-add (suffix in-file &optional lisp-dir)
  "Create a new file from IN-FILE template updating SUFFIX in it.
When LISP-DIR is non-nil place the NAME file under LISP-DIR."
  (let* ((resource-dir (ert-resource-directory))
         (suffix (if (stringp suffix) suffix (format "%s" suffix)))
         (file (let ((file (replace-regexp-in-string
                            (rx (or "SUFFIX"
                                    (: "-v" digit (* "." (1+ digit)))
                                    (: ".in" string-end)) )
                            (lambda (mat)
                              (if (string= mat "SUFFIX") suffix ""))
                            in-file)))
                 (file-name-concat lisp-dir file))))
    (unless (zerop (call-process
                    "sed" (expand-file-name in-file resource-dir)
                    `(:file ,file) nil
                    (format "s/SUFFIX/%s/g" suffix)))
      (error "Failed to invoke M4 on %s" in-file))
    (vc-git-command nil 0 nil "add" ".")))

(defun package-vc-tests-create-bundle (suffix &optional lisp-dir)
  "Create a test package bundle with SUFFIX.
If LISP-DIR is non-nil place sources of the package in LISP-DIR."
  (let* ((name (format "test-package-%s" suffix))
         (repo-dir (expand-file-name (file-name-concat "repo" name)
                                     package-vc-tests-dir)))
    (make-directory (expand-file-name (or lisp-dir ".") repo-dir) t)
    (let ((default-directory repo-dir)
          (bundle-file (expand-file-name (format "%s.bundle" name)
                                         package-vc-tests-dir)))
      (vc-git-command nil 0 nil "init" "-b" "master")
      (package-vc-tests-add
       suffix "test-package-SUFFIX-lib-v0.1.el.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX-v0.1.el.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX.texi.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX-inc.texi.in" lisp-dir)
      ;; Place Makefile in root of the repository
      (package-vc-tests-add
       suffix "Makefile.in" nil)
      (vc-git-command nil 0 nil "commit" "-m" "First commit")
      (package-vc-tests-add
       suffix "test-package-SUFFIX-lib-v0.2.el.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX-v0.2.el.in" lisp-dir)
      (vc-git-command nil 0 nil "commit" "-m" "Second commit")
      (vc-git-command nil 0 nil
                      "bundle" "create" bundle-file "master")
      (list bundle-file (vc-git-working-revision nil)))))

(defun package-vc-tests-package-desc (package &optional installed)
  "Return descriptor of PACKAGE.
When INSTALLED is non-nil the descriptor will come from `package-alist'.
Otherwise the descriptor will be from `package-archive-contents'.  This
is to mimic `package-vc--read-package-desc'."
  (cadr (assoc package (if installed package-alist package-archive-contents)
               #'string=)))

(defun package-vc-tests-package-lisp-dir (pkg)
  "Return a Lisp directory of PKG."
  (and-let* ((checkout-dir (car (alist-get pkg package-vc-tests-packages))))
    (if-let* ((lisp-dir (cadr (alist-get pkg package-vc-tests-packages))))
        (expand-file-name lisp-dir checkout-dir)
      checkout-dir)))

(defun package-vc-tests-package-main-file (pkg)
  "Return a main file of PKG."
  (file-name-concat (package-vc-tests-package-lisp-dir pkg)
                    (format "%s.el" pkg)))

;; When a package source is being recompiled - for example as result of
;; `pakckage-vc-upgrade' or `package-vc-rebuild' - it is also reloaded
;; [1] to ensure that the most recent version of compiled code is
;; available to Emacs.  There are a few tests that add markers in
;; `load-history' before executing such functions.  And then follow up
;; tests use these markers to assert that expected package files are in
;; correct places in the `load-history'.
;;
;; [1] Only when a file has been loaded previously.

(defun package-vc-tests-load-history-marker (name)
  "Return a `load-history' marker with NAME."
  (expand-file-name (symbol-name name) package-vc-tests-dir))

(defun package-vc-tests-load-history-pattern (pkg type)
  "Return a regexp pattern for PKG's file of TYPE."
  (pcase type
    (:autoloads
     (rx (literal (file-name-concat
                   package-user-dir
                   (symbol-name pkg)
                   (format "%s-autoloads.el" pkg)))
         eos))
    (:main
     (rx (literal (package-vc-tests-package-main-file pkg)) eos))
    (:main-compiled
     (rx (literal (package-vc-tests-package-main-file pkg)) "c" eos))
    (:marker
     (regexp-quote (package-vc-tests-load-history-marker pkg)))))

(defun package-vc-tests-load-history-position (pkg type)
  "Return a PKG's file of TYPE position in `load-history'.
If TYPE is `:autoloads' return a position of a PKG autoloads file.
Otherwise, if TYPE is `:main' return a position of PKG main file (not
compiled).  Otherwise, if TYPE is `:main-compiled' return a position of
PKG compiled main file.  Otherwise, if TYPE is `:marker' return a
position of a marker PKG."
  (let ((pkg-file (package-vc-tests-load-history-pattern pkg type))
        (interesting-entry
         (rx string-start
             (literal (file-truename package-vc-tests-dir)))))
    (cl-position-if
     (lambda (file) (string-match pkg-file file))
     (mapcan
      (lambda (ent)
        (and (consp ent)
             (stringp (car ent))
             (let ((file-name (file-truename (car ent))))
               (and (string-match interesting-entry file-name)
                    (list file-name)))))
      load-history))))

(defun package-vc-tests-explain-load-history-position (pkg type)
  "Explain `package-vc-tests-load-history' failed for PKG of TYPE."
  (let ((pattern
         (concat "..."
                 (substring
                  (package-vc-tests-load-history-pattern pkg type)
                  (length (rx (literal package-vc-tests-dir))))))
        (reason
         (if-let* ((pos (package-vc-tests-load-history-position
                         pkg type)))
             `(found in load-history at pos ,pos)
           '(not found in load-history)))
        (entries
         (cl-loop
          with len = (length package-vc-tests-dir)
          for hist in load-history
          when (and (consp hist)
                    (stringp (car hist))
                    (string-prefix-p package-vc-tests-dir (car hist)))
          collect (concat "..." (substring (car hist) len)))))
    (append (list 'pattern pattern) reason (list entries))))

(put #'package-vc-tests-load-history-position
     'ert-explainer
     #'package-vc-tests-explain-load-history-position)

;; The following predicates and helper functions take an extra PKG
;; argument.  This is needed for ERT to print package name in case of
;; failure.

(defun package-vc-tests-elc-files (pkg)
  "Return elc files for PKG."
  (when-let* ((dir (package-vc-tests-package-lisp-dir pkg)))
    (directory-files dir nil (rx ".elc" string-end))))

(defun package-vc-tests-assert-elc (pkg)
  "Assert that PKG has correct .elc files in."
  (let* ((dir (package-vc-tests-package-lisp-dir pkg))
         (elc-files (should (package-vc-tests-elc-files pkg)))
         (autoloads-rx (rx (literal (format "%s-autoloads.elc" pkg))
                           string-end)))
    (should-not (cl-find-if (lambda (elc)
                              (string-match autoloads-rx elc))
                            elc-files))
    (dolist (elc-file elc-files)
      (delete-file (expand-file-name elc-file dir)))))

(defun package-vc-tests-assert-package-alist (pkg version)
  "Assert that PKG entry in `package-alist' have correct VERSION and dir."
  (let ((pkg-desc (should (cadr (assq pkg package-alist)))))
    (should (equal (file-name-as-directory
                    (expand-file-name (format "%s" pkg)
                                      package-user-dir))
                   (file-name-as-directory
                    (package-desc-dir pkg-desc))))
    (should (equal (list pkg version)
                   (list pkg (package-desc-version pkg-desc))))))

(defun package-vc-tests-reset-head (pkg)
  "Reset to HEAD^ checkout for PKG."
  (let ((default-directory (cadr (assoc pkg package-vc-tests-packages))))
    (vc-git-command nil 0 nil "reset" "--hard" "HEAD^")))

(defun package-vc-tests-package-head (pkg)
  "Return HEAD revisions of a PKG."
  (let ((default-directory (cadr (assoc pkg package-vc-tests-packages))))
    (vc-git-working-revision nil)))

(defmacro with-package-vc-tests-installed (pkg &rest body)
  "Eval BODY with PKG installed in a test environment."
  (declare (indent 1) (debug t))
  ;; git-bundle(1) produces test packages sources in bundle files, based
  ;; on skeleton files in directory package-vc-resources.  Before
  ;; executing body make sure that:
  ;;
  ;; - `package' has been initialised, and there are no
  ;;   `package-archives' defined
  `(let* ((package-archives (unless package--initialized
                              (let (package-archives)
                                (package-initialize)
                                (package-vc--archives-initialize))
                              nil))
          ;; - create a temporary location for packages and test files
          (package-vc-tests-dir
           (expand-file-name
            (make-temp-file "package-vc-tests-"
                            t
                            (format-time-string "-%Y%m%d.%H%M%S.%3N"))))
          ;; - packages are installed into a test directory
          (package-user-dir (expand-file-name "elpa"
                                              package-vc-tests-dir))
          ;; - define test packages, their checkout locations, lisp
          ;;   directories, and install functions
          (package-vc-tests-packages
           `(;; checkout and install with `package-vc-install' (on
             ;; ELPA)
             (test-package-1
              ,(expand-file-name "test-package-1" package-user-dir)
              nil
              package-vc-tests-install-from-elpa)
             ;; checkout and install with `package-vc-install' (not on
             ;; ELPA)
             (test-package-2
              ,(expand-file-name "test-package-2" package-user-dir)
              nil
              package-vc-tests-install-from-spec)
             ;; checkout with `package-vc-checktout' and install with
             ;; `package-vc-install-from-checkout' (on ELPA)
             (test-package-3
              ,(expand-file-name "test-package-3" package-vc-tests-dir)
              nil
              pakcage-vc-tests-checkout-from-elpa-install-from-checkout)
             ;; checkout with git and install with
             ;; `package-vc-install-from-checkout'
             (test-package-4
              ,(expand-file-name "test-package-4" package-vc-tests-dir)
              nil
              package-vc-tests-checkout-with-git-install-from-checkout)
             ;; sources in "lisp" sub directory, checkout and install
             ;; with `package-vc-install' (not on ELPA)
             (test-package-5
              ,(expand-file-name "test-package-5" package-user-dir)
              "lisp"
              package-vc-tests-install-from-spec)
             ;; sources in "lisp" sub directory, checkout with git and
             ;; install with `package-vc-install-from-checkout'
             (test-package-6
              ,(expand-file-name "test-package-6" package-vc-tests-dir)
              "lisp"
              package-vc-tests-checkout-with-git-install-from-checkout)

             ;; sources in "src" sub directory, checkout and install
             ;; with `package-vc-install' (on ELPA)
             (test-package-7
              ,(expand-file-name "test-package-7" package-user-dir)
              "src"
              package-vc-tests-install-from-elpa)
             ;; sources in "src" sub directory, checkout with
             ;; `package-vc-checktout' and install with
             ;; `package-vc-install-from-checkout' (on ELPA)
             (test-package-8
              ,(expand-file-name "test-package-8" package-vc-tests-dir)
              nil
              pakcage-vc-tests-checkout-from-elpa-install-from-checkout)
             ;; sources in "custom-dir" sub directory, checkout and
             ;; install with `package-vc-install' (on ELPA)
             (test-package-9
              ,(expand-file-name "test-package-9" package-user-dir)
              "custom-dir"
              package-vc-tests-install-from-elpa)))
          ;; - create a test package bundle
          (package-vc-tests-bundle
           (let* ((pkg-name (symbol-name ,pkg))
                  (suffix (and (string-match
                                (rx ?- (group (1+ (not ?-))) eos)
                                pkg-name)
                               (match-string 1 pkg-name))))
             (package-vc-tests-create-bundle
              suffix (cadr (alist-get ,pkg package-vc-tests-packages)))))
          ;; - find all packages that are present in a test ELPA
          (package-vc-tests-elpa-packages
           (cl-loop
            for (name _ _ fn) in package-vc-tests-packages
            when (memq
                  fn
                  '(package-vc-tests-install-from-elpa
                    pakcage-vc-tests-checkout-from-elpa-install-from-checkout))
            collect name))
          ;; - make test packages recognisable by `package' and
          ;;   `package-vc' internals:
          (package-archive-contents
           (mapcar
            (lambda (pkg)
              (list pkg
                    (package-desc-create
                     :name pkg
                     :version '(0 2)
                     :reqs '((emacs (30.1)))
                     :kind 'tar
                     :archive "test-elpa"
                     :extras
                     (list
                      '(:maintainer
                        ("Test Maintainer"
                         . "test-maintainer@test-domain.org"))
                      (cons :url  (car package-vc-tests-bundle))
                      (cons :commit (cadr package-vc-tests-bundle))
                      (cons :revdesc (substring
                                      (cadr package-vc-tests-bundle)
                                      0 12))))))
            package-vc-tests-elpa-packages))
          ;; Branch needs to be specified in a pkg-spec, as cloning from
          ;; a bundle won't checkout a default branch.
          (package-vc--archive-spec-alists
           (list
            (cons 'test-elpa
                  (mapcar
                   (lambda (pkg)
                     (let ((lisp-dir
                            (cadr (alist-get
                                   ,pkg package-vc-tests-packages))))
                       (append
                        (list pkg
                              :url (car package-vc-tests-bundle)
                              :branch "master"
                              :doc (let ((doc-file
                                          (format "%s.texi" ,pkg)))
                                     (if lisp-dir
                                         (file-name-concat lisp-dir
                                                           doc-file)
                                       doc-file))
                              :make (format "build-%s" ,pkg)
                              :shell-command (format
                                              "touch %s.cmd-build"
                                              ,pkg))
                        (and lisp-dir
                             (not (member lisp-dir '("lisp" "src")))
                             (list :lisp-dir lisp-dir)))))
                   package-vc-tests-elpa-packages))))
          (package-vc--archive-data-alist
           '((test-elpa :version 1 :default-vc Git)))
          ;; - `vc-guess-backend-url' is recognising bundles as `Git'
          ;;   repositories:
          (vc-clone-heuristic-alist
           `((,(rx "test-package-" (1+ digit) ".bundle" eos)
              . Git)
             ,@vc-clone-heuristic-alist))
          ;; - ensure that `package-alist' and
          ;;   `package-vc-selected-packages' are empty
          package-alist
          package-vc-selected-packages
          ;; - don't save any customization
          (user-init-file nil)
          ;; - FIXME: something sets `default-directory' to last
          ;;   checkout directory after `package-vc-checkout', which
          ;;   causes problems when this macro deletes the temporary
          ;;   directory after body execution.
          (default-directory package-vc-tests-dir))
     (unwind-protect
         (progn
           (funcall (or (caddr (alist-get ,pkg package-vc-tests-packages))
                        (lambda (pkg)
                          (ert-fail
                           (format
                            "Cannot find %s in package-vc-tests-packages"
                            pkg))))
                    ,pkg)
           ,@body)
       ;; Unbind package defined symbols, and remove package defined
       ;; features and entries from `load-path',`load-history', and
       ;; `Info-directory-list'.
       (let ((pattern (rx string-start (literal package-vc-tests-dir))))
         (dolist (entry load-history)
           (when-let* ((file (car-safe entry))
                       ((stringp file))
                       ((string-match pattern file)))
             (dolist (elt (cdr entry))
               (pcase elt
                 (`(defun . ,fun)
                  (fmakunbound fun))
                 (`(provide . ,feat)
                  (setq features (cl-remove feat features)))
                 ((and (pred symbolp)
                       (pred boundp))
                  (makunbound elt))))))
         (setq load-path (cl-remove-if
                          (lambda (path)
                            (and (stringp path)
                                 (string-match pattern path)))
                          load-path)
               load-history (cl-remove-if
                             (lambda (entry)
                               (and-let* ((path (car-safe entry))
                                          (_ (stringp path)))
                                 (string-match pattern path)))
                             load-history)
               Info-directory-list (cl-remove-if
                                    (lambda (dir)
                                      (and (stringp dir)
                                           (string-match pattern dir)))
                                    Info-directory-list)))
       (if (or (eq package-vc-tests-preserve-temporary t)
               (eq package-vc-tests-preserve-temporary ,pkg)
               (and (listp package-vc-tests-preserve-temporary)
                    (memq ,pkg package-vc-tests-preserve-temporary)))
           (message "package-vc-tests: preserving temporary directory %s"
                    package-vc-tests-dir)
         (delete-directory package-vc-tests-dir t)))))

(defun package-vc-tests-install-from-elpa (pkg)
  "Install PKG with `package-vc-install'."
  (push (list (package-vc-tests-load-history-marker 'install-begin))
        load-history)
  (should (eq t (package-vc-install pkg)))
  (push (list (package-vc-tests-load-history-marker 'install-end))
        load-history)
  (should-not (alist-get pkg package-vc-selected-packages
                         nil nil #'string=)))

(defun package-vc-tests-install-from-spec (pkg)
  "Install PKG with `package-vc-install' (not on ELPA)."
  (push (list (package-vc-tests-load-history-marker 'install-begin))
        load-history)
  ;; Branch needs to be specified in a pkg-spec, as cloning from a
  ;; bundle won't checkout a default branch.
  (should (eq t (package-vc-install `(,pkg
                                      :url ,(car package-vc-tests-bundle)
                                      :branch "master"))))
  (push (list (package-vc-tests-load-history-marker 'install-end))
        load-history)
  (should (equal (car package-vc-tests-bundle)
                 (plist-get (alist-get (symbol-name pkg)
                                       package-vc-selected-packages
                                       nil nil #'string=)
                            :url))))

(defun pakcage-vc-tests-checkout-from-elpa-install-from-checkout (pkg)
  "Install PKG with `package-vc-install-from-checkout'.
Make checkout with `package-vc-checkout'."
  (let ((checkout-dir (car (alist-get pkg package-vc-tests-packages))))
    (let ((buffer (package-vc-checkout (package-vc-tests-package-desc
                                        pkg)
                                       checkout-dir)))
      (should (bufferp buffer))
      (should (string-prefix-p (symbol-name pkg) (buffer-name buffer))))
    (push (list (package-vc-tests-load-history-marker 'install-begin))
          load-history)
    (should (eq t
                (package-vc-install-from-checkout checkout-dir)))
    (push (list (package-vc-tests-load-history-marker 'install-end))
          load-history)
    (should (equal (concat package-vc--url-scheme checkout-dir)
                   (plist-get (alist-get (symbol-name pkg)
                                         package-vc-selected-packages
                                         nil nil #'string=)
                              :url)))))

(defun package-vc-tests-checkout-with-git-install-from-checkout (pkg)
  "Install PKG with `package-vc-install-from-checkout'.
Make checkout with git(1)."
  (let ((checkout-dir (car (alist-get pkg package-vc-tests-packages))))
    (vc-git-clone  (car package-vc-tests-bundle)
                   checkout-dir
                   "master")
    (push (list (package-vc-tests-load-history-marker 'install-begin))
          load-history)
    (should (eq t
                (package-vc-install-from-checkout checkout-dir
                                                  (symbol-name pkg))))
    (push (list (package-vc-tests-load-history-marker 'install-end))
          load-history)
    (should (equal (concat package-vc--url-scheme checkout-dir)
                   (plist-get (alist-get (symbol-name pkg)
                                         package-vc-selected-packages
                                         nil nil #'string=)
                              :url)))))

(defmacro package-vc-tests-package-vc-async-wait (seconds count flags &rest body)
  "Wait up to SECONDS for COUNT async vc commands with FLAGS called by BODY.
Return nil on timeout or the value of last form in BODY."
  (declare (indent 3))
  (let ((count-sym (make-symbol "count"))
        (post-vc-command-sym (make-symbol "post-vc-command")))
    `(let* ((,count-sym ,count)
            (,post-vc-command-sym
             (lambda (command _ command-flags)
               ;; A crude filter for vc commands
               (when (and (equal command vc-git-program)
                          (cl-every (lambda (flag)
                                      (member flag command-flags))
                                    ,flags))
                 (decf ,count-sym)))))
       (add-hook 'vc-post-command-functions ,post-vc-command-sym 100)
       (unwind-protect
           (with-timeout (,seconds nil)
             (prog1 (progn ,@body)
               (while (/= ,count-sym 0)
                 (accept-process-output nil 0.01))))
         (remove-hook 'vc-post-command-functions ,post-vc-command-sym)))))

(defmacro package-vc-test-deftest (name args &rest body)
  (declare (debug (&define [&name "test@" symbolp]
			   sexp
			   def-body))
           (indent 2))
  (unless (length= args 1)
    (error "`package-vc' tests have to take a single argument"))
  (let ((file (or (macroexp-file-name) buffer-file-name))
        (tests '()))
    (dolist (pkg package-vc-tests-under-test)
      (let ((name (intern (format "package-vc-tests-%s/%s" name pkg))))
        (push
         `(cl-macrolet ((skip-unless (form) `(ert--skip-unless ,form)))
            (ert-set-test
             ',name
             (make-ert-test
              :name ',name
              :tags '(package-vc)
              :file-name ,file
              :body
              (lambda ()
                (let ((,(car args) ',pkg))
                  (with-package-vc-tests-installed ,(car args)
                    ,@body))
                nil))))
         tests)))
    `(progn ,@tests)))

(package-vc-test-deftest install-post-conditions (pkg)
  (let ((install-begin
           (should (package-vc-tests-load-history-position
                    'install-begin :marker)))
          (install-end
           (should (package-vc-tests-load-history-position
                    'install-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads))))
      (should (< install-end autoloads-pos install-begin))
      (should-not (package-vc-tests-load-history-position
                   pkg :main))
      (should-not (package-vc-tests-load-history-position
                   pkg :main-compiled)))
    (should (equal (package-vc--main-file
                    (package-vc-tests-package-desc pkg t))
                   (package-vc-tests-package-main-file pkg)))
    (should (equal (package-vc-commit
                    (package-vc-tests-package-desc pkg t))
                   (cadr package-vc-tests-bundle)))
    (package-vc-tests-assert-elc pkg)
    (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest require (pkg)
  (should (fboundp (intern (format "%s-func" pkg))))
  (should (autoloadp
           (symbol-function (intern (format "%s-func" pkg)))))
  (should (require pkg))
  (should (fboundp (intern (format "%s-func" pkg))))
  (should-not (autoloadp
               (symbol-function (intern (format "%s-func" pkg)))))
  (should-not (fboundp (intern (format "%s-old-func" pkg))))
  (should-not (package-vc-tests-load-history-position
               pkg :main))
  (let ((install-end
         (should (package-vc-tests-load-history-position
                  'install-end :marker)))
        (main-compiled-pos
         (should (package-vc-tests-load-history-position
                  pkg :main-compiled))))
    (should (< main-compiled-pos install-end))))

(package-vc-test-deftest upgrade (pkg)
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade (package-vc-tests-package-desc pkg t))
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-end))
          load-history)
    (should-not (package-vc-tests-load-history-position
                 pkg :main))
    (should-not (package-vc-tests-load-history-position
                 pkg :main-compiled))
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads))))
      (should (< upgrade-end autoloads-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should (autoloadp
               (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest upgrade-after-require (pkg)
  (should (require pkg))
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade (package-vc-tests-package-desc pkg t))
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-end))
          load-history)
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads)))
          (main-pos
           (should (package-vc-tests-load-history-position
                    pkg :main)))
          (main-compiled-pos
           (should (package-vc-tests-load-history-position
                    pkg :main-compiled))))
      (should (< upgrade-end autoloads-pos upgrade-begin))
      (should (< upgrade-end main-pos upgrade-begin))
      (should (< upgrade-end main-compiled-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should-not (autoloadp
                   (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest upgrade-all (pkg)
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade-all)
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-end))
          load-history)
    (should-not (package-vc-tests-load-history-position
                 pkg :main))
    (should-not (package-vc-tests-load-history-position
                 pkg :main-compiled))
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-all-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-all-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads))))
      (should (< upgrade-end autoloads-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should (autoloadp
               (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest upgrade-all-after-require (pkg)
  (should (require pkg))
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade-all)
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-end))
          load-history)
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-all-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-all-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads)))
          (main-pos
           (should (package-vc-tests-load-history-position
                    pkg :main)))
          (main-compiled-pos
           (should (package-vc-tests-load-history-position
                    pkg :main-compiled))))
      (should (< upgrade-end autoloads-pos upgrade-begin))
      (should (< upgrade-end main-pos upgrade-begin))
      (should (< upgrade-end main-compiled-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should-not (autoloadp
                   (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest rebuild (pkg)
  (package-vc-tests-reset-head pkg)
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-rebuild
     (package-vc-tests-package-desc pkg t))
    (let ((old-func (intern (format "%s-old-func" pkg))))
      (should (fboundp old-func))
      (should (autoloadp
               (symbol-function old-func))))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should (autoloadp
               (symbol-function func)))
      (should (equal "Old macro test"
                     (funcall func "test"))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 1)))

(package-vc-test-deftest rebuild-after-require (pkg)
  (should (require pkg))
  (package-vc-tests-reset-head pkg)
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-rebuild
     (package-vc-tests-package-desc pkg t))
    (let ((old-func (intern (format "%s-old-func" pkg))))
      (should (fboundp old-func))
      (should-not (autoloadp
                   (symbol-function old-func))))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should-not (autoloadp
                   (symbol-function func)))
      (should (equal "Old macro test"
                     (funcall func "test"))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 1)))

(package-vc-test-deftest prepare-patch (pkg)
  ;; Ensure `vc-prepare-patch' respects subject from function argument
  (let ((vc-prepare-patches-separately nil))
    (package-vc-prepare-patch (package-vc-tests-package-desc pkg t)
                              "test-subject"
                              (cdr package-vc-tests-bundle))
    (let ((message-buffer
           (should (get-buffer "*unsent mail to Test Maintainer*"))))
      (should (bufferp message-buffer))
      (switch-to-buffer message-buffer)
      (goto-char (point-min))
      (should
       (string-match
        pkg
        (rx
         "To: Test Maintainer <test-maintainer@test-domain.org>")
        (buffer-substring (point) (pos-eol))))
      (forward-line)
      (should
       (string-match
        pkg
        (rx "Subject: test-subject")
        (buffer-substring (point) (pos-eol))))
      (let (kill-buffer-query-functions)
        (kill-buffer message-buffer)))))

(package-vc-test-deftest log-incoming (pkg)
  (package-vc-tests-reset-head pkg)
      (should
       (package-vc-tests-package-vc-async-wait
           5 1 '("log" "--decorate")
         (package-vc-log-incoming (package-vc-tests-package-desc pkg t))
         t))
      (let ((incoming-buffer (get-buffer "*vc-incoming*"))
            (pattern (rx (literal
                          (substring
                           (cadr package-vc-tests-bundle)
                           0 7))
                         (one-or-more any)
                         "Second commit"
                         line-end)))
        (should (bufferp incoming-buffer))
        (switch-to-buffer incoming-buffer)
        (goto-char (point-min))
        (should
         (string-match
          pattern
          (buffer-substring (point) (pos-eol))))
        (let (kill-buffer-query-functions)
          (kill-buffer incoming-buffer))))

(package-vc-test-deftest pkg-spec-doc-make-shell-command (pkg)
  ;; Only `package-vc-install' runs make and shell command
  (skip-unless (eq (caddr (alist-get pkg package-vc-tests-packages))
                   #'package-vc-tests-install-from-elpa))
  (let ((package-vc-allow-build-commands t))
    (let ((checkout-dir (car (alist-get
                              pkg package-vc-tests-packages))))
      (should (file-exists-p
               (expand-file-name
                (format "%s.make-build" pkg)
                checkout-dir)))
      (should (file-exists-p
               (expand-file-name
                (format "%s.cmd-build" pkg)
                checkout-dir))))
    (should (cl-member-if (lambda (dir)
                            (and (stringp dir)
                                 (string-prefix-p package-vc-tests-dir
                                                  dir)))
                          Info-directory-list))
    (let ((info-file
           (expand-file-name (format "%s.info" pkg)
                             (car (alist-get
                                   pkg package-vc-tests-packages)))))
      (should (file-exists-p info-file))
      (ert-with-test-buffer
          (:name (format "*package-vc-tests: %s.info*" pkg))
        (insert-file-contents info-file)
        (goto-char (point-min))
        (should (re-search-forward
                 (format "First chapter for %s" pkg)))
        (should (re-search-forward
                 (format "Second chapter for %s" pkg)))))))

(provide 'package-vc-tests)

;;; package-vc-tests.el ends here
