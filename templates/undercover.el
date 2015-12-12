;; Here is the code you need to setup coverage:
;; for ert: insert this in "test/test-helper.el" before to require your package
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))

;; for ecukes: insert this in "features/support/env.el" before to require your package
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:report-file "/tmp/undercover-report.json"))
