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
