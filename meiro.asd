(defsystem "meiro"
  :version "0.1.0"
  :author "Seref R. Ayar"
  :license "MIT"
  :depends-on (:cl-ppcre
               :quri)
  :components ((:module "src"
                :components
                ((:file "url")
                 (:file "route")
                 (:file "meiro"))))
  :description ""
  :in-order-to ((test-op (test-op meiro-test))))
