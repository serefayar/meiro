(defpackage meiro.t.route
  (:use :cl
        :rove)
  (:import-from :meiro.route
                :route
                :route-pattern
                :route-params))

(in-package :meiro.t.route)


(deftest route-initialize-test
  (testing "route init: slot pattern should not be nil"
    (let* ((r (make-instance 'meiro.route:route
                 :method :get
                 :url "/path"))
          (pattern (slot-value r 'meiro.route::pattern))
          (params (slot-value r 'meiro.route::params)))
      (ok (not (null pattern)))
      (ok (and
           (not (null params))
           (equalp (list :path #()) params)))))

  (testing "route init: given path params, slot path-params should not be nil"
    (let* ((r (make-instance 'meiro.route:route
                 :method :get
                 :url "/path/:param/sub/:sub"))
           (pattern (slot-value r 'meiro.route::pattern))
           (params (slot-value r 'meiro.route::params)))
      (ok (not (null pattern)))
      (ok (and
           (not (null params))
           (equalp (list :path #("param" "sub")) params))))))

(deftest match-route-test
  (let ((r (make-instance 'meiro.route:route
                 :method :get
                 :url "/path/:param/sub/:sub")))
    (testing "should return nil if there is no match"
      (ok (null (meiro.route:match-route r "/not-found" :get "?a=1&b=2"))))

    (testing "should return nil for method-matched-p if the url matched but the method has not"
      (let ((ret (meiro.route:match-route r "/path/1/sub/2" :post "?a=1&b=2")))
        (ok (eq nil (second ret)))
        (ok (not (null (first ret))))
        (ok (eq nil (third ret)))))

    (testing "should return all  if the url fully matched"
      (let ((ret (meiro.route:match-route r "/path/1/sub/2" :get "?a=1&b=2")))
        (ok (not (null (first ret))))
        (ok (not (null (second ret))))
        (ok (not (null (third ret))))))))


(deftest equal-route-test
  (testing "should not be equal if url not matched"
    (ok (not (meiro.route:equal-route
              (make-instance 'meiro.route:route
                             :method :get
                             :url "/path/:param/sub/:sub")
              (make-instance 'meiro.route:route
                             :method :get
                             :url "/path/:param/sub")))))
  (testing "should not be equal if method not matched"
    (ok (not (meiro.route:equal-route
              (make-instance 'meiro.route:route
                             :method :get
                             :url "/path/:param/sub/:sub")
              (make-instance 'meiro.route:route
                             :method :post
                             :url "/path/:param/sub/:sub")))))

  (testing "should signal conflicted-route error for equal url and method"
    (ok (signals (meiro.route:equal-route
                  (make-instance 'meiro.route:route
                                 :method :get
                                 :url "/path/:param/sub/:sub")
                  (make-instance 'meiro.route:route
                                 :method :get
                                 :url "/path/:param/sub/:sub"))
            'meiro.route::conflicted-route))))
