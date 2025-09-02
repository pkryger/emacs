;;; package-vc-tests.el --- Tests for package-vc -*- lexical-binding:t -*-

;;; Code:

(require 'package-vc)
(require 'package)
(require 'vc-git)
(require 'cl-lib)
(require 'ert)

(defvar package-vc-tests-dir
  (make-temp-file "package-vc-tests-" t (format-time-string "-%Y%m%d.%H%M%S")))

(setq package-user-dir (expand-file-name "elpa" package-vc-tests-dir))

(defvar package-vc-tests-packages
  `(;; checkout and install with `package-vc-install' (on ELPA)
    (diminish . ,(expand-file-name "diminish" package-user-dir))
    ;; checkout and install with `package-vc-install' (not on ELPA)
    (ultra-scroll . ,(expand-file-name "ultra-scroll" package-user-dir))
    ;; checkout with `package-vc-checktout' and install with
    ;; `package-vc-install-from-checkout'
    (dash . ,(expand-file-name "dash" package-vc-tests-dir))
    ;; checkout with git and install with `package-vc-install-from-checkout'
    (basic-stats . ,(expand-file-name "basic-stats.el" package-vc-tests-dir))
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

(defun package-vc-tests-assert-delete-elc ()
  "Assert that .elc files are in expected directories and delete them.
When ALL is non nil, check all packages under test."
  (dolist (pkg-checkout-dir package-vc-tests-packages)
    (let* ((dir (cdr pkg-checkout-dir))
           (elc-files (directory-files dir nil (rx ".elc" string-end))))
      (should-not (equal (cons dir elc-files)
                         (list dir)))
      (dolist (elc-file elc-files)
        (delete-file elc-file)))))

(defun package-vc-tests-packages-heads (reset)
  "Return HEAD revisions of `package-vc-tests-packages'.
When RESET is non-nil also reset to a previous version."
  (mapcar (lambda (pkg-checkout-dir)
            (let ((default-directory (cdr pkg-checkout-dir)))
              (prog1
                  (cons (car pkg-checkout-dir)
                        (vc-git-working-revision nil))
                (when reset
                  (vc-git-command nil 0 nil "reset" "--hard" "HEAD^")))))
          package-vc-tests-packages))

(ert-deftest package-vc-tests-000-install ()
  (package-refresh-contents)
  (package-vc--archives-initialize)
  (package-vc-install 'diminish)
  (should-not (alist-get "diminish" package-vc-selected-packages
                         nil nil #'string=))

  (package-vc-install '(ultra-scroll
                        :url "https://github.com/jdtsmith/ultra-scroll.git"))
  (should (equal "https://github.com/jdtsmith/ultra-scroll.git"
                 (plist-get (alist-get "ultra-scroll"
                                       package-vc-selected-packages
                                       nil nil #'string=)
                            :url)))

  (let ((checkout-dir (expand-file-name "dash" package-vc-tests-dir)))
    (package-vc-checkout (package-vc-tests-package-desc 'dash)
                         checkout-dir)
    (package-vc-install-from-checkout checkout-dir)
    (should (equal (format "file://%s" checkout-dir)
                   (plist-get (alist-get "dash"
                                         package-vc-selected-packages
                                         nil nil #'string=)
                              :url))))

  (let ((checkout-dir (expand-file-name "basic-stats.el" package-vc-tests-dir)))
    (shell-command
     (format "git clone https://github.com/pkryger/basic-stats.el.git %s"
             checkout-dir))
    (package-vc-install-from-checkout checkout-dir "basic-stats")
    (should (equal (format "file://%s" checkout-dir)
                   (plist-get (alist-get "basic-stats"
                                         package-vc-selected-packages
                                         nil nil #'string=)
                              :url))))

  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-001-main-file ()
 (dolist (pkg-checkout-dir package-vc-tests-packages)
   (should (equal (package-vc--main-file
                   (package-vc-tests-package-desc (car pkg-checkout-dir) t))
                  (format "%s/%s.el"
                          (cdr pkg-checkout-dir)
                          (car pkg-checkout-dir))))))

(ert-deftest package-vc-tests-002-commit ()
 (dolist (pkg-checkout-dir package-vc-tests-packages)
   (let ((pkg (car pkg-checkout-dir))
         (commit (package-vc-commit
                  (package-vc-tests-package-desc (car pkg-checkout-dir) t))))
     (should-not (equal (cons pkg commit)
                        (list pkg)))
     (should-not (equal (list pkg "unknown")
                        (list pkg commit))))))

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

(ert-deftest package-vc-tests-003-upgrade-all ()
  (let ((heads (package-vc-tests-packages-heads t)))
    (should
     (package-vc-tests-package-vc-upgrade-wait
         5 (length package-vc-tests-packages)
       (package-vc-upgrade-all)))
    (should (equal heads (package-vc-tests-packages-heads nil))))
  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-004-upgrade ()
  (let ((heads (package-vc-tests-packages-heads t)))
    (should
     (package-vc-tests-package-vc-upgrade-wait
         5 (length package-vc-tests-packages)
       (dolist (pkg-checkout-dir package-vc-tests-packages)
    (package-vc-upgrade
     (package-vc-tests-package-desc (car pkg-checkout-dir) t)))))
    (should (equal heads (package-vc-tests-packages-heads nil))))
  (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-005-rebuild ()
  (dolist (pkg-checkout-dir package-vc-tests-packages)
    (package-vc-rebuild
     (package-vc-tests-package-desc (car pkg-checkout-dir) t)))
 (package-vc-tests-assert-delete-elc))

(ert-deftest package-vc-tests-006-prepare-patch ()
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

(ert-deftest package-vc-tests-007-log-incoming ()
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
