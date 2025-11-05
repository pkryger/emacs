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
;; operations on packages.  Since installing a package changes the state
;; of running process these tests are ment to be executed in order, as
;; denoted by digits in their name.  Also in many cases the subsequent
;; test often depends on the passing of the previous test.

;; It is recommended to run this suite in a separate process, for
;; example, in root of Emacs source directory:

;;   ./src/emacs -batch \
;;       -l ../test/lisp/package-vc-tests.el \
;;       -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package-vc)
(require 'package)
(require 'vc-git)
(require 'vc)
(require 'cl-lib)
(require 'ert-x)
(require 'ert)

(defvar package-vc-tests-dir nil)
(defvar package-vc-tests-packages nil)
(defvar package-vc-tests-bundles nil)

(defun package-vc-tests-checkin (name suffix in commit-msg &optional lisp-dir)
  "For package NAME copy IN file as main file.
After copying update SUFFIX in the file and check it in with COMMIT-MSG.
If LISP-DIR is non-nil place the file under LISP-DIR."
  (let ((resource-dir (ert-resource-directory))
        (main-file (format "%s.el" (if lisp-dir
                                       (expand-file-name name lisp-dir)
                                     name)))
        (suffix (if (stringp suffix) suffix (format "%s" suffix))))
    (copy-file (expand-file-name in resource-dir) main-file t)
    (with-temp-buffer
      (insert-file-contents main-file)
      (goto-char (point-min))
      (while (search-forward "SUFFIX" nil t)
        (replace-match suffix))
      (write-file main-file))
    (vc-git-command nil 0 nil "add" ".")
    (vc-git-command nil 0 nil "commit" "-m" commit-msg)))

(defun package-vc-tests-create-bundle (suffix &optional lisp-dir)
  "Create a test package bundle with SUFFIX.
If LISP-DIR is non-nil place sources of the package in LISP-DIR."
  (let* ((name (format "test-package-%s" suffix))
         (src-dir (expand-file-name (format "src/%s" name)
                                    package-vc-tests-dir)))
    (make-directory (if lisp-dir
                       (expand-file-name lisp-dir src-dir)
                     src-dir)
                    :parents)
    (let ((default-directory src-dir)
          (bundle-file (expand-file-name  (format "%s.bundle" name)
                                          package-vc-tests-dir)))
      (vc-git-command nil 0 nil "init" "-b" "master")
      (package-vc-tests-checkin
       name suffix "test-package-v0.1.el.in" "First commit" lisp-dir)
      (package-vc-tests-checkin
       name suffix "test-package-v0.2.el.in" "Second commit" lisp-dir)
      (vc-git-command nil 0 nil
                      "bundle" "create" bundle-file "master")
      (list (intern name)
            bundle-file (vc-git-working-revision nil)))))

(defmacro with-package-vc-tests-enviroment (&rest body)
  "Eval BODY with test environment."
  (declare (debug t))
  ;; Test packages sources are stored in bundle files produced by
  ;; git-bundle(1) and are stored in directory package-vc-resources.
  ;; Before executing body make sure that:
  `(let* (package-archives
          ;; - temporary location for packages and test files is ready
          (package-vc-tests-dir
           (or package-vc-tests-dir
               (setq package-vc-tests-dir
                     (make-temp-file "package-vc-tests-"
                                     t
                                     (format-time-string "-%Y%m%d.%H%M%S")))))
          ;; - packages are installed into a test directory
          (package-user-dir (expand-file-name "elpa"
                                              package-vc-tests-dir))
          ;; - create test package bundles if necessary and define test
          ;;   packages and their checkout locations
          (package-vc-tests-bundles
           (or package-vc-tests-bundles
               (setq package-vc-tests-bundles
                     (mapcar (lambda (suffix)
                               (package-vc-tests-create-bundle
                                suffix (and (< 4 suffix) "lisp")))
                             '(1 2 3 4 5 6)))))
          (package-vc-tests-packages
           `(;; checkout and install with `package-vc-install' (on
             ;; ELPA)
             (test-package-1
              . ,(expand-file-name "test-package-1"
                                   package-user-dir))
             ;; checkout and install with `package-vc-install' (not on
             ;; ELPA)
             (test-package-2
              . ,(expand-file-name "test-package-2"
                                   package-user-dir))
             ;; checkout with `package-vc-checktout' and install with
             ;; `package-vc-install-from-checkout' (on ELPA)
             (test-package-3
              . ,(expand-file-name "test-package-3"
                                   package-vc-tests-dir))
             ;; checkout with git and install with
             ;; `package-vc-install-from-checkout'
             (test-package-4
              . ,(expand-file-name "test-package-4"
                                   package-vc-tests-dir))
             ;; sources in "lisp" sub directory, checkout and install
             ;; with `package-vc-install'
             (test-package-5
              . ,(expand-file-name "test-package-5"
                                   package-user-dir))
             ;; sources in "lisp" sub directory, checkout with git and
             ;; install with `package-vc-install-from-checkout'
             (test-package-6
              . ,(expand-file-name "test-package-6"
                                   package-vc-tests-dir))

             ;; TODO: a package with source files in a non-standard
             ;; :lisp-dir, a custom Makefile and non-standard :doc
             ;; (both methods of installation)
             ))
          ;; - test packages are recognised by `package' and
          ;;   `package-vc' internals:
          (package-archive-contents
           (list
            (let ((bundle (alist-get 'test-package-1
                                     package-vc-tests-bundles)))
              (list 'test-package-1
                    (package-desc-create
                     :name 'test-package-1
                     :version '(0 2)
                     :reqs '((emacs (30.1)))
                     :kind 'tar
                     :archive "test-elpa"
                     :extras
                     (list
                      (cons :url  (car bundle))
                      (cons :commit (cadr bundle))
                      (cons :revdesc (substring (cadr bundle) 0 12))))))
            (let ((bundle (alist-get 'test-package-3
                                     package-vc-tests-bundles)))
              (list 'test-package-3
                    (package-desc-create
                     :name 'test-package-3
                     :version '(0 2)
                     :reqs '((emacs (30.1)))
                     :kind 'tar
                     :archive "test-elpa"
                     :extras
                     (list
                      (cons :url  (car bundle))
                      (cons :commit (cadr bundle))
                      (cons :revdesc (substring (cadr bundle) 0 12))))))))
          (package-vc--archive-spec-alists
           (list
            (list 'test-elpa
                  (list 'test-package-1
                        :url (car (alist-get 'test-package-1
                                             package-vc-tests-bundles))
                        :branch "master")
                  (list 'test-package-3
                        :url (car (alist-get 'test-package-3
                                             package-vc-tests-bundles))
                        :branch "master"))))
          (package-vc--archive-data-alist
           (list
            (list 'test-elpa :version 1 :default-vc 'Git)))
          ;; - `vc-guess-backend-url' is recognising bundles as `Git'
          ;;   repositories:
          (vc-clone-heuristic-alist
           (cons
            (cons (rx "test-package-" (one-or-more digit) ".bundle"
                      string-end)
                  'Git)
            vc-clone-heuristic-alist)))
     ;; - `package' has been initialised:
     (should package--initialized)

     ,@body))


;; TODO: add test for deleting packages, with asserting
;; `package-vc-selected-packages'

;; TODO: clarify `package-vc-install-all' behaviour with regards to
;; packages installed with `package-vc' but not stored in
;; `package-vc-selected-packages' i.e., packages from ELPAs

(defun package-vc-tests-package-desc (package &optional installed)
  "Return descriptor of PACKAGE.
When INSTALLED is non-nil the descriptor will come from `package-alist'.
Otherwise the descriptor will be from `package-archive-contents'.  This
is to mimic `package-vc--read-package-desc'."
  (cadr (assoc (if (stringp package) package (symbol-name package))
               (if installed package-alist package-archive-contents)
               #'string=)))

(defun package-vc-tests-package-lisp-dir (pkg)
  "Return a Lisp directory of PKG."
  (when-let* ((checkout-dir (alist-get pkg package-vc-tests-packages)))
    (cond
     ((member pkg '(test-package-5 test-package-6))
      (expand-file-name "lisp" checkout-dir))
     (t checkout-dir))))

(defun package-vc-tests-package-main-file (pkg)
  "Return a main file of PKG."
  (format "%s/%s.el" (package-vc-tests-package-lisp-dir pkg) pkg))


;; When a package source is being recompiled - for example as result of
;; `pakckage-vc-upgrade' or `package-vc-rebuild' - it is also reloaded
;; [1] to ensure that the most recent version of compiled code is
;; available to Emacs.  There are a few tests that add markers in
;; `load-history' before executing such functions.  And then follow up
;; tests use these markers to assert that expected package files are in
;; correct places in the `load-history'.
;;
;; [1] Only when a file has been loaded previously.
(defun package-vc-tests-load-history-position (pkg type)
  "Return a PKG's position in `load-history'.
If TYPE is `:autoloads' return a position of a PKG autoloads file.
Otherwise, if TYPE is `:main' return a position of PKG main file (not
compiled).  Otherwise, if TYPE is `:main-compiled' return a position of
PKG compiled main file.  Otherwise, if TYPE is `:marker' return a
position of a marker PKG."
  (let ((pkg-file (pcase type
                    (:autoloads
                     (rx (literal
                          (format "%s/%s/%s-autoloads.el"
                                  package-user-dir pkg pkg))
                         string-end))
                    (:main
                     (rx
                      (literal
                       (package-vc-tests-package-main-file pkg))
                      string-end))
                    (:main-compiled
                     (rx
                      (literal
                       (package-vc-tests-package-main-file pkg))
                      "c"
                      string-end))
                    (:marker
                     (rx "/" (literal (format "%s" pkg))))))
        (interesting-entry
         (rx string-start
             (literal (file-truename package-vc-tests-dir)))))
    (cl-position-if
     (lambda (file)
       (string-match pkg-file file))
     (cl-remove-if-not
      (lambda (file-name)
        (string-match interesting-entry file-name))
      (mapcar
       #'file-truename
       (cl-remove-if-not
        #'stringp
        (mapcar #'car load-history)))))))

(defun package-vc-tests-assert-delete-elc ()
  "Assert that .elc files are in expected directories and delete them.
When ALL is non nil, check all packages under test."
  (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
    (let* ((dir (package-vc-tests-package-lisp-dir pkg))
           (elc-files (directory-files dir nil (rx ".elc" string-end)))
           (autoloads-rx (rx
                          (literal (format "%s-autoloads.el" pkg))
                          string-end)))
      (should (equal (format "%s: has elc-files" dir)
                     (format "%s: %s elc-files"
                             dir (if elc-files "has" "has no"))))
      (should-not (cl-find-if (lambda (elc)
                                (string-match autoloads-rx elc))
                              elc-files))
      (dolist (elc-file elc-files)
        (delete-file (expand-file-name elc-file dir))))))

(defun package-vc-tests-assert-package-alist (version)
  "Assert that entries in `package-alist' have correct VERSION and dir."
  (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
    (let ((pkg-desc (should (cadr (assq pkg package-alist)))))
      (should (equal (file-name-as-directory
                      (expand-file-name (format "%s" pkg)
                                        package-user-dir))
                     (file-name-as-directory
                      (package-desc-dir pkg-desc))))
      (should (equal (list pkg version)
                     (list pkg (package-desc-version pkg-desc)))))))

(defun package-vc-tests-reset-heads ()
  "Reset to HEAD^ checkouts `package-vc-tests-packages'."
  (pcase-dolist (`(,_ . ,dir) package-vc-tests-packages)
    (let ((default-directory dir))
      (vc-git-command nil 0 nil "reset" "--hard" "HEAD^"))))

(defun package-vc-tests-packages-heads ()
  "Return HEAD revisions of `package-vc-tests-packages'."
  (mapcar (lambda (pkg-checkout-dir)
            (let ((default-directory (cdr pkg-checkout-dir)))
              (cons (car pkg-checkout-dir)
                    (vc-git-working-revision nil))))
          package-vc-tests-packages))

(ert-deftest package-vc-tests-000-install ()
  (package-initialize)
  (package-vc--archives-initialize)
  (eval
   '(with-package-vc-tests-enviroment
     (push (list (format "%s/install-begin" package-vc-tests-dir))
           load-history)
     (package-vc-install 'test-package-1)
     (should-not (alist-get "test-package-1"
                            package-vc-selected-packages
                            nil nil #'string=))

     (let ((bundle (car (alist-get 'test-package-2
                                   package-vc-tests-bundles))))
       (package-vc-install `(test-package-2
                             :url ,bundle
                             :branch "master"))
       (should (equal bundle
                      (plist-get (alist-get "test-package-2"
                                            package-vc-selected-packages
                                            nil nil #'string=)
                                 :url))))

     (let ((checkout-dir (expand-file-name "test-package-3"
                                           package-vc-tests-dir)))
       (package-vc-checkout (package-vc-tests-package-desc
                             'test-package-3)
                            checkout-dir)
       (package-vc-install-from-checkout checkout-dir)
       (should (equal (format "file://%s" checkout-dir)
                      (plist-get (alist-get "test-package-3"
                                            package-vc-selected-packages
                                            nil nil #'string=)
                                 :url))))

     (let ((checkout-dir (expand-file-name "test-package-4"
                                           package-vc-tests-dir)))
       (shell-command
        (format "git clone -b master %s %s"
                (car (alist-get 'test-package-4
                                package-vc-tests-bundles))
                checkout-dir))
       (package-vc-install-from-checkout checkout-dir "test-package-4")
       (should (equal (format "file://%s" checkout-dir)
                      (plist-get (alist-get "test-package-4"
                                            package-vc-selected-packages
                                            nil nil #'string=)
                                 :url))))

     (let ((bundle (car (alist-get 'test-package-5
                                   package-vc-tests-bundles))))
       (package-vc-install `(test-package-5
                             :url ,bundle
                             :branch "master"))
       (should (equal bundle
                      (plist-get (alist-get "test-package-5"
                                            package-vc-selected-packages
                                            nil nil #'string=)
                                 :url))))

     (let ((checkout-dir (expand-file-name "test-package-6"
                                           package-vc-tests-dir)))
       (shell-command
        (format "git clone -b master %s %s"
                (car (alist-get 'test-package-6
                                package-vc-tests-bundles))
                checkout-dir))
       (package-vc-install-from-checkout checkout-dir "test-package-6")
       (should (equal (format "file://%s" checkout-dir)
                      (plist-get (alist-get "test-package-6"
                                            package-vc-selected-packages
                                            nil nil #'string=)
                                 :url))))

     (push (list (format "%s/install-end" package-vc-tests-dir))
           load-history)

     (package-vc-tests-assert-package-alist '(0 2))
     (package-vc-tests-assert-delete-elc))))

(ert-deftest package-vc-tests-001-main-file ()
  (eval
   '(with-package-vc-tests-enviroment
     (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
       (should (equal (package-vc--main-file
                       (package-vc-tests-package-desc pkg t))
                      (package-vc-tests-package-main-file pkg)))))))

(ert-deftest package-vc-tests-002-commit ()
  (eval
   '(with-package-vc-tests-enviroment
     (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
       (let ((commit (package-vc-commit
                      (package-vc-tests-package-desc pkg t))))
         (should (equal (format "%s: has commit" pkg)
                        (format "%s: %s commit"
                                pkg (if commit "has" "has no"))))
         (should-not (equal (format "%s: unknown commit" pkg)
                            (format "%s: %s commit" pkg commit))))))))

(ert-deftest package-vc-tests-003-load-history-after-install ()
  (eval
   '(with-package-vc-tests-enviroment
     (let ((install-begin
            (should (package-vc-tests-load-history-position
                     'install-begin :marker)))
           (install-end
            (should (package-vc-tests-load-history-position
                     'install-end :marker))))
       (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
         (let ((autoloads-pos
                (should (package-vc-tests-load-history-position
                         pkg :autoloads))))
           (should (equal (format "%s: autoloads-pos between %s and %s"
                                  pkg install-end install-begin)
                          (format "%s: autoloads-pos %s %s and %s"
                                  pkg
                                  (if (< install-end
                                         autoloads-pos
                                         install-begin)
                                      "between"
                                    "is not between")
                                  install-end install-begin)
                          )))
         (should-not (package-vc-tests-load-history-position
                      pkg :main))
         (should-not (package-vc-tests-load-history-position
                      pkg :main-compiled)))))))

(defmacro package-vc-tests-package-vc-upgrade-wait (seconds count &rest body)
  "Wait up to SECONDS for COUNT packages upgrading BODY.
Return nil on timeout or the value of last form in BODY."
  (declare (indent 2))
  `(letrec ((packages-count ,count)
            (post-vc-command
             (lambda (command _ flags)
               ;; A crude filter for vc commands
               (when (and (equal command "git")
                          (string-prefix-p "*vc-git" (buffer-name)))
                 (decf packages-count)))))
     (add-hook 'vc-post-command-functions post-vc-command 100)
     (unwind-protect
         (with-timeout (,seconds nil)
           (prog1
               (progn ,@body)
             (while (/= packages-count 0)
               (sleep-for 0.1))))
       (remove-hook 'vc-post-command-functions post-vc-command))))

(ert-deftest package-vc-tests-004-upgrade-all ()
  (eval
   '(with-package-vc-tests-enviroment
     (push (list (format "%s/upgrade-all-begin" package-vc-tests-dir))
           load-history)
     (let ((heads (package-vc-tests-packages-heads)))
       (package-vc-tests-reset-heads)
       (should
        (package-vc-tests-package-vc-upgrade-wait
            5 (length package-vc-tests-packages)
          (package-vc-upgrade-all)
          t))
       (should (equal heads
                      (package-vc-tests-packages-heads))))
     (push (list (format "%s/upgrade-all-end" package-vc-tests-dir))
           load-history)
     (package-vc-tests-assert-package-alist '(0 2))
     (package-vc-tests-assert-delete-elc))))

(ert-deftest package-vc-tests-005-load-history-after-upgrade-all ()
  (eval
   '(with-package-vc-tests-enviroment
     (let ((upgrade-all-begin
            (should (package-vc-tests-load-history-position
                     'upgrade-all-begin :marker)))
           (upgrade-all-end
            (should (package-vc-tests-load-history-position
                     'upgrade-all-end :marker))))
       (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
         (let ((autoloads-pos
                (should (package-vc-tests-load-history-position
                         pkg :autoloads))))
           (should (equal (format "%s: autoloads-pos between %s and %s"
                                  pkg upgrade-all-end upgrade-all-begin)
                          (format "%s: autoloads-pos %s %s and %s"
                                  pkg
                                  (if (< upgrade-all-end
                                         autoloads-pos
                                         upgrade-all-begin)
                                      "between"
                                    "is not between")
                                  upgrade-all-end upgrade-all-begin)))
           (should-not (package-vc-tests-load-history-position
                        pkg :main))
           (should-not (package-vc-tests-load-history-position
                        pkg :main-compiled))))))))

(ert-deftest package-vc-tests-006-require ()
  (eval
   '(with-package-vc-tests-enviroment
     (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
       (should (fboundp (intern (format "%s-func" pkg))))
       (should (autoloadp
                (symbol-function (intern (format "%s-func" pkg)))))
       (should (require pkg))
       (should (fboundp (intern (format "%s-func" pkg))))
       (should-not (autoloadp
                    (symbol-function (intern (format "%s-func" pkg)))))
       (should-not (fboundp (intern (format "%s-old-func" pkg)))))
     (let ((upgrade-all-end
            (should (package-vc-tests-load-history-position
                     'upgrade-all-end :marker))))
       (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
         (let ((main-pos (should (package-vc-tests-load-history-position
                                  pkg :main))))
           (should (equal (format "%s: main-pos less than %s"
                                  pkg upgrade-all-end)
                          (format "%s: main-pos %s than %s"
                                  pkg
                                  (if (< main-pos
                                         upgrade-all-end)
                                      "less"
                                    "is not less")
                                  upgrade-all-end))))
         (should-not (package-vc-tests-load-history-position
                      pkg :main-compiled)))))))

(ert-deftest package-vc-tests-007-upgrade ()
  (eval
   '(with-package-vc-tests-enviroment
     (push (list (format "%s/upgrade-begin" package-vc-tests-dir))
           load-history)
     (let ((heads (package-vc-tests-packages-heads)))
       (package-vc-tests-reset-heads)
       (should
        (package-vc-tests-package-vc-upgrade-wait
            5 (length package-vc-tests-packages)
          (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
            (package-vc-upgrade
             (package-vc-tests-package-desc pkg t))
            (should (fboundp (intern (format "%s-func" pkg))))
            (should-not (autoloadp
                         (symbol-function
                          (intern (format "%s-func" pkg)))))
            (should-not (fboundp (intern (format "%s-old-func" pkg)))))
          t))
       (should (equal heads
                      (package-vc-tests-packages-heads))))
     (push (list (format "%s/upgrade-end" package-vc-tests-dir))
           load-history)
     (package-vc-tests-assert-package-alist '(0 2))
     (package-vc-tests-assert-delete-elc))))

(ert-deftest package-vc-tests-008-load-history-after-upgrade ()
  (eval
   '(with-package-vc-tests-enviroment
     (let ((upgrade-begin
            (should (package-vc-tests-load-history-position
                     'upgrade-begin :marker)))
           (upgrade-end
            (should (package-vc-tests-load-history-position
                     'upgrade-end :marker))))
       (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
         (let ((autoloads-pos
                (should (package-vc-tests-load-history-position
                         pkg :autoloads)))
               (main-pos
                (should (package-vc-tests-load-history-position
                         pkg :main)))
               (main-compiled-pos
                (should (package-vc-tests-load-history-position
                         pkg :main-compiled))))
           (should
            (equal (format "%s: autoloads-pos between %s and %s"
                                  pkg upgrade-end upgrade-begin)
                          (format "%s: autoloads-pos %s %s and %s"
                                  pkg
                                  (if (< upgrade-end
                                         autoloads-pos
                                         upgrade-begin)
                                      "between"
                                    "is not between")
                                  upgrade-end upgrade-begin)))
           (should
            (equal (format "%s: main-pos between %s and %s"
                                  pkg upgrade-end upgrade-begin)
                          (format "%s: main-pos %s %s and %s"
                                  pkg
                                  (if (< upgrade-end
                                         main-pos
                                         upgrade-begin)
                                      "between"
                                    "is not between")
                                  upgrade-end upgrade-begin)))
           (should
            (equal (format "%s: main-compiled-pos between %s and %s"
                                  pkg upgrade-end upgrade-begin)
                          (format "%s: main-compiled-pos %s %s and %s"
                                  pkg
                                  (if (< upgrade-end
                                         main-compiled-pos
                                         upgrade-begin)
                                      "between"
                                    "is not between")
                                  upgrade-end upgrade-begin)))))))))

(ert-deftest package-vc-tests-009-rebuild ()
  (eval
   '(with-package-vc-tests-enviroment
     (package-vc-tests-reset-heads)
     (let ((heads (package-vc-tests-packages-heads)))
       (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
         (package-vc-rebuild
          (package-vc-tests-package-desc pkg t))
         (should (fboundp (intern (format "%s-func" pkg))))
         (should-not (autoloadp
                      (symbol-function
                       (intern (format "%s-func" pkg)))))
         (should (fboundp (intern (format "%s-old-func" pkg))))
         (should-not (autoloadp
                      (symbol-function
                       (intern (format "%s-old-func" pkg))))))
       (should (equal heads
                      (package-vc-tests-packages-heads))))
     (package-vc-tests-assert-package-alist '(0 1))
     (package-vc-tests-assert-delete-elc))))

(ert-deftest package-vc-tests-010-prepare-patch ()
  (eval
   '(with-package-vc-tests-enviroment
     (pcase-dolist (`(,pkg . ,dir) package-vc-tests-packages)
       (cl-letf* ((call-count 0)
                  ((symbol-function #'package-maintainers)
                   (lambda (&rest _)
                     "test-maintainers"))
                  ((symbol-function #'vc-prepare-patch)
                   (lambda (addressee subject revisions)
                     (should (equal (file-name-as-directory
                                     default-directory)
                                    (file-name-as-directory dir)))
                     (should (equal "test-maintainers" addressee))
                     (should (equal "test-subject" subject))
                     (should (equal "test-revisions" revisions))
                     (cl-incf call-count))))
         (package-vc-prepare-patch (package-vc-tests-package-desc pkg t)
                                   "test-subject"
                                   "test-revisions")
         (should (eql 1 call-count)))))))

(ert-deftest package-vc-tests-011-log-incoming ()
  (eval
   '(with-package-vc-tests-enviroment
     (pcase-dolist (`(,pkg . ,dir) package-vc-tests-packages)
       (cl-letf* ((call-count 0)
                  ((symbol-function #'vc-log-incoming)
                   (lambda ()
                     (interactive)
                     (should (equal (file-name-as-directory
                                     default-directory)
                                    (file-name-as-directory dir)))
                     (cl-incf call-count))))
         (package-vc-log-incoming (package-vc-tests-package-desc pkg t))
         (should (eql 1 call-count)))))))

(provide 'package-vc-tests)

;;; package-vc-tests.el ends here
