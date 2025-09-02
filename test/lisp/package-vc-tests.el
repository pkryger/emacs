;;; package-vc-tests.el --- Tests for package-vc -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Przemsyław Kryger <pkryger@Gail.com>
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
(require 'ert)

(defvar package-vc-tests-dir
  (make-temp-file "package-vc-tests-" t (format-time-string "-%Y%m%d.%H%M%S")))

;; Test packages sources are stored in bundle files produced by git-bundle(1)
;; and are stored in directory package-vc-resources.  Make sure that:
;;
;; - tests know path to the directory:
(defvar package-vc-tests-resources-dir
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "package-vc-resources"))
;; - test packages are recognised by `package' and `package-vc' internals:
(setq package-archives nil)
(package-initialize)
(push (list 'test-package-1
            (package-desc-create :name 'test-package-1
                                 :version '(0 2)
                                 :reqs '((emacs (30.1)))
                                 :kind 'tar
                                 :archive "test-elpa"
                                 :extras (list
                                          `(:url . ,(format "%s/test-package-1.bundle"
                                                            package-vc-tests-resources-dir))
                                          '(:commit . "7b8e3322055287ef1580432014de3a2d5f383d79")
                                          '(:revdesc . "7b8e33220552"))))
      package-archive-contents)
(push (list 'test-package-3
            (package-desc-create :name 'test-package-3
                                 :version '(0 2)
                                 :reqs '((emacs (30.1)))
                                 :kind 'tar
                                 :archive "test-elpa"
                                 :extras (list
                                          `(:url . ,(format "%s/test-package-3.bundle"
                                                            package-vc-tests-resources-dir))
                                          '(:commit . "7176b647c4f021f811fb7cf27f288694a0ab997d")
                                          '(:revdesc . "7176b647c4f0"))))
      package-archive-contents)
(push (list 'test-elpa
            (list 'test-package-1
                  :url (format "%s/test-package-1.bundle"
                               package-vc-tests-resources-dir)
                  :branch "master")
            (list 'test-package-3
                  :url (format "%s/test-package-3.bundle"
                               package-vc-tests-resources-dir)
                  :branch "master"))
      package-vc--archive-spec-alists)
(push (list 'test-elpa :version 1 :default-vc 'Git)
      package-vc--archive-data-alist)
;; - `vc-guess-backend-url' is recognising bundles as `Git' repositories.
(push (cons (rx-to-string `(seq ,package-vc-tests-resources-dir "/"
                                (one-or-more any) ".bundle"
                                string-end))
            'Git)
      vc-clone-heuristic-alist)

(setq package-user-dir (expand-file-name "elpa" package-vc-tests-dir))

(defvar package-vc-tests-packages
  `(;; checkout and install with `package-vc-install' (on ELPA)
    (test-package-1 . ,(expand-file-name "test-package-1" package-user-dir))
    ;; checkout and install with `package-vc-install' (not on ELPA)
    (test-package-2 . ,(expand-file-name "test-package-2" package-user-dir))
    ;; checkout with `package-vc-checktout' and install with
    ;; `package-vc-install-from-checkout' (on ELPA)
    (test-package-3 . ,(expand-file-name "test-package-3" package-vc-tests-dir))
    ;; checkout with git and install with `package-vc-install-from-checkout'
    (test-package-4 . ,(expand-file-name "test-package-4" package-vc-tests-dir))
    ;; TODO: a package with source files in lisp/ directory (both methods of
    ;; installation)

    ;; TODO: a package with source files in a non-standard :lisp-dir (both
    ;; methods of installation)
    ))

;; TODO: add test for deleting packages, with asserting
;; `package-vc-selected-packages'

;; TODO: clarify `package-vc-install-all' behaviour with regards to packages
;; installed with `package-vc' but not stored in `package-vc-selected-packages'
;; i.e., packages from ELPAs

(defun package-vc-tests-package-desc (package &optional installed)
  "Return descriptor of PACKAGE.
When INSTALLED is non-nil the descriptor will come from `package-alist'.
Otherwise the descriptor will be from `package-archive-contents'.  This
is to mimic `package-vc--read-package-desc'."
  (cadr (assoc (if (stringp package) package (symbol-name package))
               (if installed package-alist package-archive-contents)
               #'string=)))

(defun package-vc-tests-package-main-file (pkg-checkout-dir)
  "Return a main file of PKG-CHECKOUT-DIR."
  (format "%s/%s.el" (cdr pkg-checkout-dir) (car pkg-checkout-dir)))

(defun package-vc-tests-load-history-position (pkg type)
  "Return a PKG's position in `load-history'.
If TYPE is `:autoloads' return a position of a PKG autoloads file.
Otherwise, if TYPE is `:main' return a position of PKG main file (not
compiled).  Otherwise, if TYPE is `:main-compiled' return a position of
PKG compiled main file.  Otherwise, if TYPE is `:marker' return a
position of a marker PKG."
  (let ((pkg-file (pcase type
                    (:autoloads
                     (rx-to-string
                      `(seq ,(format "%s/%s/%s-autoloads.el"
                                     package-user-dir pkg pkg)
                            string-end)))
                    (:main
                     (rx-to-string
                      `(seq ,(format "%s"
                                     (package-vc-tests-package-main-file
                                      (assoc pkg package-vc-tests-packages)))
                            string-end)))
                    (:main-compiled
                     (rx-to-string
                      `(seq ,(format "%s"
                                     (package-vc-tests-package-main-file
                                      (assoc pkg package-vc-tests-packages)))
                            "c"
                            string-end)))
                    (:marker
                     (rx-to-string `(seq "/" ,(format "%s" pkg))))))
        (interesting-entry
         (rx-to-string `(seq string-start
                             ,(file-truename package-vc-tests-dir)))))
    (cl-position-if
     (lambda (file)
       (string-match pkg-file file))
     (cl-remove-if-not (lambda (file-name)
                         (string-match interesting-entry file-name))
                       (mapcar #'file-truename
                               (cl-remove-if-not #'stringp
                                                 (mapcar #'car load-history)))))))

(defun package-vc-tests-assert-delete-elc ()
  "Assert that .elc files are in expected directories and delete them.
When ALL is non nil, check all packages under test."
  (dolist (pkg-checkout-dir package-vc-tests-packages)
    (let* ((dir (cdr pkg-checkout-dir))
           (elc-files (directory-files dir nil (rx ".elc" string-end)))
           (autoloads-rx (rx-to-string
                          `(seq ,(format "%s" (car pkg-checkout-dir))
                                "-autoloads.elc"
                                string-end))))
      (should-not (equal (cons dir elc-files)
                         (list dir)))
      (should-not (cl-find-if (lambda (elc)
                                (string-match autoloads-rx  elc))
                              elc-files))
      (dolist (elc-file elc-files)
        (delete-file (expand-file-name elc-file dir))))))

(defun package-vc-tests-assert-package-alist (version)
  "Assert that entries in `package-alist' have correct VERSION and dir."
  (dolist (pkg (mapcar #'car package-vc-tests-packages))
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
  (mapcar (lambda (pkg-checkout-dir)
            (let ((default-directory (cdr pkg-checkout-dir)))
              (vc-git-command nil 0 nil "reset" "--hard" "HEAD^")))
          package-vc-tests-packages))

(defun package-vc-tests-packages-heads ()
  "Return HEAD revisions of `package-vc-tests-packages'."
  (mapcar (lambda (pkg-checkout-dir)
            (let ((default-directory (cdr pkg-checkout-dir)))
              (cons (car pkg-checkout-dir)
                    (vc-git-working-revision nil))))
          package-vc-tests-packages))

(ert-deftest package-vc-tests-000-install ()
  (package-refresh-contents)
  (package-vc--archives-initialize)
  (push (list (format "%s/install-begin" package-vc-tests-dir))
        load-history)
  (package-vc-install 'test-package-1)
  (should-not (alist-get "test-package-1" package-vc-selected-packages
                         nil nil #'string=))

  (let ((bundle (format "%s/test-package-2.bundle" package-vc-tests-resources-dir)))
    (package-vc-install `(test-package-2
                          :url ,bundle
                          :branch "master"))
   (should (equal bundle
                  (plist-get (alist-get "test-package-2"
                                        package-vc-selected-packages
                                        nil nil #'string=)
                             :url))))

  (let ((checkout-dir (expand-file-name "test-package-3" package-vc-tests-dir)))
    (package-vc-checkout (package-vc-tests-package-desc 'test-package-3)
                         checkout-dir)
    (package-vc-install-from-checkout checkout-dir)
    (should (equal (format "file://%s" checkout-dir)
                   (plist-get (alist-get "test-package-3"
                                         package-vc-selected-packages
                                         nil nil #'string=)
                              :url))))

  (let ((checkout-dir (expand-file-name "test-package-4" package-vc-tests-dir)))
    (shell-command
     (format "git clone -b master %s/test-package-4.bundle %s"
             package-vc-tests-resources-dir
             checkout-dir))
    (package-vc-install-from-checkout checkout-dir "test-package-4")
    (should (equal (format "file://%s" checkout-dir)
                   (plist-get (alist-get "test-package-4"
                                         package-vc-selected-packages
                                         nil nil #'string=)
                              :url))))

  (push (list (format "%s/install-end" package-vc-tests-dir))
        load-history)

  (package-vc-tests-assert-package-alist '(0 2))
  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-001-main-file ()
 (dolist (pkg-checkout-dir package-vc-tests-packages)
   (should (equal (package-vc--main-file
                   (package-vc-tests-package-desc (car pkg-checkout-dir) t))
                  (package-vc-tests-package-main-file pkg-checkout-dir)))))

(ert-deftest package-vc-tests-002-commit ()
 (dolist (pkg-checkout-dir package-vc-tests-packages)
   (let ((pkg (car pkg-checkout-dir))
         (commit (package-vc-commit
                  (package-vc-tests-package-desc (car pkg-checkout-dir) t))))
     (should-not (equal (cons pkg commit)
                        (list pkg)))
     (should-not (equal (list pkg "unknown")
                        (list pkg commit))))))

(ert-deftest package-vc-tests-003-load-history-after-install ()
  (let ((install-begin (should (package-vc-tests-load-history-position
                                'install-begin :marker)))
        (install-end (should (package-vc-tests-load-history-position
                              'install-end :marker))))
    (dolist (pkg (mapcar #'car package-vc-tests-packages))
      (let ((autoloads-pos
             (should (package-vc-tests-load-history-position pkg :autoloads))))
        (should (< install-end autoloads-pos install-begin)))
      (should-not (package-vc-tests-load-history-position pkg :main))
      (should-not (package-vc-tests-load-history-position pkg :main-compiled)))))

(defmacro package-vc-tests-package-vc-upgrade-wait (seconds count &rest body)
  "Wait up to SECONDS for COUNT packages upgrading BODY.
Return nil on timeout or non nil otherwise."
  (declare (indent 2))
  `(letrec ((packages-count ,count)
            (post-vc-command (lambda (command _ flags)
                               ;; A crude filter for vc commands
                               (when (and (equal command "git")
                                          (string-prefix-p "*vc-git" (buffer-name)))
                                 (cl-decf packages-count)))))
     (add-hook 'vc-post-command-functions post-vc-command 100)
     (unwind-protect
         (progn
           ,@body
           (catch 'done
             (dotimes (i (* 10 ,seconds))
               (sleep-for 0.1)
               (when (eql packages-count 0)
                 (throw 'done t)))))
       (remove-hook 'vc-post-command-functions post-vc-command))))

(ert-deftest package-vc-tests-004-upgrade-all ()
  (push (list (format "%s/upgrade-all-begin" package-vc-tests-dir))
        load-history)
  (let ((heads (package-vc-tests-packages-heads)))
    (package-vc-tests-reset-heads)
    (should
     (package-vc-tests-package-vc-upgrade-wait
         5 (length package-vc-tests-packages)
       (package-vc-upgrade-all)))
    (should (equal heads
                   (package-vc-tests-packages-heads))))
  (push (list (format "%s/upgrade-all-end" package-vc-tests-dir))
        load-history)
  (package-vc-tests-assert-package-alist '(0 2))
  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-005-load-history-after-upgrade-all ()
  (let ((upgrade-all-begin (should (package-vc-tests-load-history-position
                                'upgrade-all-begin :marker)))
        (upgrade-all-end (should (package-vc-tests-load-history-position
                              'upgrade-all-end :marker))))
    (dolist (pkg (mapcar #'car package-vc-tests-packages))
      (let ((autoloads-pos
             (should (package-vc-tests-load-history-position pkg :autoloads))))
        (should (< upgrade-all-end autoloads-pos upgrade-all-begin))
        (should-not (package-vc-tests-load-history-position pkg :main))
      (should-not (package-vc-tests-load-history-position pkg :main-compiled))))))

(ert-deftest package-vc-tests-006-require ()
  (dolist (pkg (mapcar #'car package-vc-tests-packages))
    (should (fboundp (intern (format "%s-func" pkg))))
    (should (autoloadp (symbol-function (intern (format "%s-func" pkg)))))
    (should (require pkg))
    (should (fboundp (intern (format "%s-func" pkg))))
    (should-not (autoloadp (symbol-function (intern (format "%s-func" pkg)))))
    (should-not (fboundp (intern (format "%s-old-func" pkg)))))
  (let ((upgrade-all-end (should (package-vc-tests-load-history-position
                              'upgrade-all-end :marker))))
    (dolist (pkg (mapcar #'car package-vc-tests-packages))
      (let ((main-pos (should (package-vc-tests-load-history-position pkg :main))))
        (should (< main-pos upgrade-all-end)))
      (should-not (package-vc-tests-load-history-position pkg :main-compiled)))))

(ert-deftest package-vc-tests-007-upgrade ()
  (push (list (format "%s/upgrade-begin" package-vc-tests-dir))
        load-history)
  (let ((heads (package-vc-tests-packages-heads)))
    (package-vc-tests-reset-heads)
    (should
     (package-vc-tests-package-vc-upgrade-wait
         5 (length package-vc-tests-packages)
       (dolist (pkg (mapcar #'car package-vc-tests-packages))
         (package-vc-upgrade
          (package-vc-tests-package-desc pkg t))
         (should (fboundp (intern (format "%s-func" pkg))))
         (should-not (autoloadp (symbol-function (intern (format "%s-func" pkg)))))
         (should-not (fboundp (intern (format "%s-old-func" pkg)))))))
    (should (equal heads
                   (package-vc-tests-packages-heads))))
  (push (list (format "%s/upgrade-end" package-vc-tests-dir))
        load-history)
  (package-vc-tests-assert-package-alist '(0 2))
  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-008-load-history-after-upgrade ()
  (let ((upgrade-begin (should (package-vc-tests-load-history-position
                                'upgrade-begin :marker)))
        (upgrade-end (should (package-vc-tests-load-history-position
                              'upgrade-end :marker))))
    (dolist (pkg (mapcar #'car package-vc-tests-packages))
      (let ((autoloads-pos
             (should (package-vc-tests-load-history-position pkg :autoloads)))
            (main-pos
             (should (package-vc-tests-load-history-position pkg :main)))
            (main-compiled-pos
             (should (package-vc-tests-load-history-position pkg :main-compiled))))
        (should (< upgrade-end autoloads-pos upgrade-begin))
        (should (< upgrade-end main-pos upgrade-begin))
        (should (< upgrade-end main-compiled-pos upgrade-begin))))))

(ert-deftest package-vc-tests-009-rebuild ()
  (package-vc-tests-reset-heads)
  (let ((heads (package-vc-tests-packages-heads)))
    (dolist (pkg (mapcar #'car package-vc-tests-packages))
      (package-vc-rebuild
       (package-vc-tests-package-desc pkg t))
      (should (fboundp (intern (format "%s-func" pkg))))
      (should-not (autoloadp (symbol-function (intern (format "%s-func" pkg)))))
      (should (fboundp (intern (format "%s-old-func" pkg))))
      (should-not (autoloadp (symbol-function (intern (format "%s-old-func" pkg))))))
    (should (equal heads
                   (package-vc-tests-packages-heads))))
  (package-vc-tests-assert-package-alist '(0 1))
  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-010-prepare-patch ()
  (dolist (pkg-checkout-dir package-vc-tests-packages)
    (cl-letf* ((call-count 0)
               ((symbol-function #'package-maintainers)
                (lambda (&rest _)
                  "test-maintainers"))
               ((symbol-function #'vc-prepare-patch)
                (lambda (addressee subject revisions)
                  (should (equal (file-name-as-directory default-directory)
                                 (file-name-as-directory (cdr pkg-checkout-dir))))
                  (should (equal "test-maintainers" addressee))
                  (should (equal "test-subject" subject))
                  (should (equal "test-revisions" revisions))
                  (cl-incf call-count))))
      (package-vc-prepare-patch (package-vc-tests-package-desc
                                 (car pkg-checkout-dir)
                                 t)
                                "test-subject"
                                "test-revisions")
      (should (eql 1 call-count)))))

(ert-deftest package-vc-tests-011-log-incoming ()
  (dolist (pkg-checkout-dir package-vc-tests-packages)
    (cl-letf* ((call-count 0)
               ((symbol-function #'vc-log-incoming)
                (lambda ()
                  (interactive)
                  (should (equal (file-name-as-directory default-directory)
                                 (file-name-as-directory (cdr pkg-checkout-dir))))
                  (cl-incf call-count))))
      (package-vc-log-incoming (package-vc-tests-package-desc
                                (car pkg-checkout-dir) t))
      (should (eql 1 call-count)))))

(provide 'package-vc-tests)

;;; package-vc-tests.el ends here
