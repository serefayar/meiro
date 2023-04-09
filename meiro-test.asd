(defsystem meiro-test
  :author "Seref R. Ayar"
  :license "MIT"
  :depends-on (:meiro
               :rove)
  :components ((:module "t"
                :components
                ((:file "url")
                 (:file "route")
                 (:file "meiro"))))
  :description "Test system for meiro"
  :perform (test-op (op c) (symbol-call :rove :run c)))
