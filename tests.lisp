(in-package :cl-user)
(defpackage liql-test
    (:use :cl :prove :liql))
(in-package :liql-test)

;;;FIXME: This test suite assumes that the warflagger database is present. Local use only!

(plan 4)

;;Basic usage
(ok (integerp (grab-one (liql 'author.id))))
(ok (integerp (car (grab-column (liql '(1 2 3 4 5 6 7 8 9 10) 'opinion.author 'opinion)))))
(ok (integerp (grab-one (liql 'opinion.author 'opinion))))
(ok (stringp (getf (grab-plist (liql 'author)) :value)))

;;And clause

(is (grab-one (liql '(4 5 6) 'opinion '(6 7 8) 'opinion 'opinion.id)) 6)
(is (grab-one (liql '(4 5 6) 'opinion '(6 7 8) 'opinion)) 6)
(is (grab-one (liql '(4 5 6) 'opinion '(6 7 8) 'opinion.id)) 6)
(is (grab-column (liql '(4 5 6) 'opinion '(6 7 8) 'opinion)) '(6))

(finalize)

