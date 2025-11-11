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

;; These tests load test packages with some test implementation, as well
;; as `load-history' is being modified.  This may contaminate the
;; running Emacs instance.  This, compounded with a need to run all
;; tests in the order it is recommended to run this suite in a separate
;; process, for example, in root of Emacs source directory:

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
  ;; - `package' is initialised, and there are no `package-archives'
  `(let* ((package-archives (unless package--initialized
                              (let (package-archives)
                                (package-initialize)
                                (package-vc--archives-initialize))
                              nil))
          ;; - temporary location for packages and test files is ready
          (package-vc-tests-dir
           (or package-vc-tests-dir
               (setq package-vc-tests-dir
                     (expand-file-name
                      (make-temp-file "package-vc-tests-"
                                      t
                                      (format-time-string "-%Y%m%d.%H%M%S"))))))
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
           (mapcar
            (lambda (pkg)
              (let ((bundle (alist-get pkg
                                       package-vc-tests-bundles)))
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
                          ("Test Maintainer" . "test-maintainer@test-domain.org"))
                        (cons :url  (car bundle))
                        (cons :commit (cadr bundle))
                        (cons :revdesc (substring (cadr bundle) 0 12)))))))
            '(test-package-1 test-package-3)))
          (package-vc--archive-spec-alists
           (list
            (cons 'test-elpa
                  (mapcar
                   (lambda (pkg)
                     (list pkg
                           :url (car (alist-get pkg
                                                package-vc-tests-bundles))
                           :branch "master"))
                   '(test-package-1 test-package-3)))))
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

;; The following predicates and helper functions take an extra PKG
;; argument.  This is needed for ERT to print package name in case of
;; failure.

(defun package-vc-tests-valid-commit-p (_pkg commit)
  "Return non-nil when COMMIT is a valid commit."
  (and (stringp commit)
       (not (string= commit "unknown"))))

(defun package-vc-tests-in-strict-order-p (_pkg &rest args)
  "Return non-nil when ARGS are in strict order."
  (apply #'< args))

(defun package-vc-tests-match-p (_pkg regexp string)
  "Return non-nil when REGEXP matches STRING."
  (string-match regexp string))

(defun package-vc-tests-buffer-p (_pkg obj)
  "Return non-nil when OBJ is a buffer."
  (bufferp obj))

(defun package-vc-tests-elc-files (pkg)
  "Return elc files for PKG."
  (when-let* ((dir (package-vc-tests-package-lisp-dir pkg))
              (elc-files (directory-files
                          dir nil (rx ".elc" string-end))))
    elc-files))

(defun package-vc-tests-assert-delete-elc ()
  "Assert that .elc files are in expected directories and delete them.
When ALL is non nil, check all packages under test."
  (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
    (let* ((dir (package-vc-tests-package-lisp-dir pkg))
           (elc-files (should (package-vc-tests-elc-files pkg)))
           (autoloads-rx (rx
                          (literal (format "%s-autoloads.elc" pkg))
                          string-end)))
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
  (with-package-vc-tests-enviroment
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
     (should (equal (concat package-vc--url-scheme checkout-dir)
                    (plist-get (alist-get "test-package-3"
                                          package-vc-selected-packages
                                          nil nil #'string=)
                               :url))))

   (let ((checkout-dir (expand-file-name "test-package-4"
                                         package-vc-tests-dir)))
     (vc-git-clone
      (car (alist-get 'test-package-4
                      package-vc-tests-bundles))
      checkout-dir
      "master")
     (package-vc-install-from-checkout checkout-dir "test-package-4")
     (should (equal (concat package-vc--url-scheme checkout-dir)
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
     (vc-git-clone
      (car (alist-get 'test-package-6
                      package-vc-tests-bundles))
      checkout-dir
      "master")
     (package-vc-install-from-checkout checkout-dir "test-package-6")
     (should (equal (concat package-vc--url-scheme checkout-dir)
                    (plist-get (alist-get "test-package-6"
                                          package-vc-selected-packages
                                          nil nil #'string=)
                               :url))))

   (push (list (format "%s/install-end" package-vc-tests-dir))
         load-history)

   (package-vc-tests-assert-package-alist '(0 2))
   (package-vc-tests-assert-delete-elc)))

(ert-deftest package-vc-tests-001-main-file ()
  (with-package-vc-tests-enviroment
   (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
     (should (equal (package-vc--main-file
                     (package-vc-tests-package-desc pkg t))
                    (package-vc-tests-package-main-file pkg))))))

(ert-deftest package-vc-tests-002-commit ()
  (with-package-vc-tests-enviroment
   (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
     (should (package-vc-tests-valid-commit-p
              pkg
              (package-vc-commit
               (package-vc-tests-package-desc pkg t)))))))

(ert-deftest package-vc-tests-003-load-history-after-install ()
  (with-package-vc-tests-enviroment
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
         (should (package-vc-tests-in-strict-order-p
                  pkg
                  install-end
                  autoloads-pos
                  install-begin)))
       (should-not (package-vc-tests-load-history-position
                    pkg :main))
       (should-not (package-vc-tests-load-history-position
                    pkg :main-compiled))))))

(defmacro package-vc-tests-package-vc-async-wait (seconds count flags &rest body)
  "Wait up to SECONDS for COUNT async vc commands with FLAGS called by BODY.
Return nil on timeout or the value of last form in BODY."
  (declare (indent 3))
  (let ((count-sym (make-symbol "count"))
        (post-vc-command-sym (make-symbol "post-vc-command")))
    `(letrec ((,count-sym ,count)
              (,post-vc-command-sym
               (lambda (command _ command-flags)
                 ;; A crude filter for vc commands
                 (when (and (equal command "git")
                            (cl-every (lambda (flag)
                                        (member flag command-flags))
                                      ,flags))
                   (decf ,count-sym)))))
       (add-hook 'vc-post-command-functions ,post-vc-command-sym 100)
       (unwind-protect
           (with-timeout (,seconds nil)
             (prog1
                 (progn ,@body)
               (while (/= ,count-sym 0)
                 (accept-process-output nil 0.01))))
         (remove-hook 'vc-post-command-functions ,post-vc-command-sym)))))

(ert-deftest package-vc-tests-004-upgrade-all ()
  (with-package-vc-tests-enviroment
   (push (list (format "%s/upgrade-all-begin" package-vc-tests-dir))
         load-history)
   (let ((heads (package-vc-tests-packages-heads)))
     (package-vc-tests-reset-heads)
     (should
      (package-vc-tests-package-vc-async-wait
          5 (length package-vc-tests-packages) '("pull")
        (package-vc-upgrade-all)
        t))
     (should (equal heads
                    (package-vc-tests-packages-heads))))
   (push (list (format "%s/upgrade-all-end" package-vc-tests-dir))
         load-history)
   (package-vc-tests-assert-package-alist '(0 2))
   (package-vc-tests-assert-delete-elc)))

(ert-deftest package-vc-tests-005-load-history-after-upgrade-all ()
  (with-package-vc-tests-enviroment
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
         (should (package-vc-tests-in-strict-order-p
                  pkg
                  upgrade-all-end
                  autoloads-pos
                  upgrade-all-begin))
         (should-not (package-vc-tests-load-history-position
                      pkg :main))
         (should-not (package-vc-tests-load-history-position
                      pkg :main-compiled)))))))

(ert-deftest package-vc-tests-006-require ()
  (with-package-vc-tests-enviroment
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
         (should (package-vc-tests-in-strict-order-p
                  pkg
                  main-pos
                  upgrade-all-end)))
       (should-not (package-vc-tests-load-history-position
                    pkg :main-compiled))))))

(ert-deftest package-vc-tests-007-upgrade ()
  (with-package-vc-tests-enviroment
   (push (list (format "%s/upgrade-begin" package-vc-tests-dir))
         load-history)
   (let ((heads (package-vc-tests-packages-heads)))
     (package-vc-tests-reset-heads)
     (should
      (package-vc-tests-package-vc-async-wait
          5 (length package-vc-tests-packages) '("pull")
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
   (package-vc-tests-assert-delete-elc)))

(ert-deftest package-vc-tests-008-load-history-after-upgrade ()
  (with-package-vc-tests-enviroment
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
         (should (package-vc-tests-in-strict-order-p
                  pkg
                  upgrade-end
                  autoloads-pos
                  upgrade-begin))
         (should (package-vc-tests-in-strict-order-p
                  pkg
                  upgrade-end
                  main-pos
                  upgrade-begin))
         (should (package-vc-tests-in-strict-order-p
                  pkg
                  upgrade-end
                  main-compiled-pos
                  upgrade-begin)))))))

(ert-deftest package-vc-tests-009-rebuild ()
  (with-package-vc-tests-enviroment
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
   (package-vc-tests-assert-delete-elc)))

(ert-deftest package-vc-tests-010-prepare-patch ()
  (let (vc-prepare-patches-separately)
    ;; Ensure `vc-prepare-patch' respects subject from function
    (with-package-vc-tests-enviroment
     (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
       (package-vc-prepare-patch (package-vc-tests-package-desc pkg t)
                                 "test-subject"
                                 (cdr (alist-get
                                       pkg package-vc-tests-bundles)))
       (let ((message-buffer
              (get-buffer "*unsent mail to Test Maintainer*")))
         (should (equal (format "%s: message-buffer" pkg)
                        (format "%s: %s"
                                pkg (if (bufferp message-buffer)
                                        "message-buffer"
                                      "no message-buffer"))))
         (switch-to-buffer message-buffer)
         (goto-char (point-min))
         (should
          (package-vc-tests-match-p
           pkg
           (rx
            "To: Test Maintainer <test-maintainer@test-domain.org>")
           (buffer-substring (point) (pos-eol))))
         (forward-line)
         (should
          (package-vc-tests-match-p
           pkg
           (rx "Subject: test-subject")
           (buffer-substring (point) (pos-eol))))
         (let (kill-buffer-query-functions)
           (kill-buffer message-buffer)))))))

(ert-deftest package-vc-tests-011-log-incoming ()
  (with-package-vc-tests-enviroment
   (pcase-dolist (`(,pkg . ,_) package-vc-tests-packages)
     (should
      (package-vc-tests-package-vc-async-wait
          5 1 '("log" "--decorate")
        (package-vc-log-incoming (package-vc-tests-package-desc pkg t))
        t))
     (let ((incoming-buffer (get-buffer "*vc-incoming*"))
           (pattern (rx (literal
                         (substring
                          (cadr (alist-get pkg
                                           package-vc-tests-bundles))
                          0 7))
                        (one-or-more any)
                        "Second commit"
                        line-end)))
       (should (package-vc-tests-buffer-p pkg incoming-buffer))
       (switch-to-buffer incoming-buffer)
       (goto-char (point-min))
       (should
        (package-vc-tests-match-p
         pkg
         pattern
         (buffer-substring (point) (pos-eol))))
       (let (kill-buffer-query-functions)
         (kill-buffer incoming-buffer))))))

(provide 'package-vc-tests)

;;; package-vc-tests.el ends here
