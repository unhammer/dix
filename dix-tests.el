;;; dix-tests.el --- tests for dix.el

;; Copyright (C) 2016 Kevin Brubeck Unhammer

;;; Commentary:

;; TODO

;;; Code:

(require 'ert)
(require 'dix)

(ert-deftest dix-test-schemas-nil ()
  "Tests whether `dix-schemas' DTRT when defcustom unset."
  (let ((old-rng-schema-locating-files rng-schema-locating-files))
    (let ((dix-schema-locating-files nil))
      (dix-schemas)
      (should (member (concat (file-name-directory (find-lisp-object-file-name #'dix-schemas nil))
                              (if (file-exists-p "/usr/share/lttoolbox/dix.rnc")
                                  "schemas.xml"
                                "local-schemas.xml"))
                      rng-schema-locating-files)))
    (setq rng-schema-locating-files old-rng-schema-locating-files)))

(ert-deftest dix-test-schemas-set ()
  "Tests whether `dix-schemas' DTRT when defcustom set."
  (let ((old-rng-schema-locating-files rng-schema-locating-files))
    (let ((dix-schema-locating-files '("foo.xml")))
      (dix-schemas)
      (should (member "foo.xml" rng-schema-locating-files)))
    (setq rng-schema-locating-files old-rng-schema-locating-files)))

(ert-deftest dix-test-nearest ()
  "Tests whether `dix-nearest' DTRT."
  (should (equal (dix-nearest 3 nil 1 2 3 7 6 5 1)
                 5))
  (should (equal (dix-nearest 3 nil 1 2 3 4 5 6 7)
                 4))
  (should (equal (dix-nearest 3 nil 1 2 3)
                 nil))
  (should (equal (dix-nearest 3 t 1 2 3)
                 2))
  (should (equal (dix-nearest 3 t 1 2 4)
                 2)))

(provide 'dix-tests)

;;;============================================================================

;;; dix-tests.el ends here
