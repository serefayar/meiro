(defpackage meiro.t.meiro
  (:use :cl
        :rove)
  (:import-from :meiro)
  (:import-from :meiro.route))

(in-package :meiro.t.meiro)

(deftest union-routes-test
  (let ((rl1 (list (make-instance 'meiro.route:route
                                  :method :post
                                  :url "/")))
        (rl2 (list (make-instance 'meiro.route:route
                                  :method :post
                                  :url "/"))))
    (testing "given the same route it should signal conflictied route error"
      (ok (signals (meiro::union-routes rl1 rl2) 'meiro.route::conflicted-route)))))


(deftest map-routes-test
  (testing "should return routes as route instances in a set"
    (let ((r (meiro::map-routes (list "/"
                                      :get (list :handler nil)
                                      :post (list :handler nil)))))
      (ok (= 2 (length r)))
      (ok (string= "/" (slot-value (first r) 'meiro.route::url)))
      (ok (eq :post (slot-value (second r) 'meiro.route::method))))))


(deftest collect-routes-test
  (testing "should return routes as route instances in a set"
    (let ((r (meiro::collect-routes (list (list "/"
                                                :get (list :handler nil)
                                                :post (list :handler nil))
                                          (list "/path"
                                                :get (list :handler nil)
                                                :post (list :handler nil))))))
      (ok (= 4 (length r)))
      (ok (string= "/" (slot-value (first r) 'meiro.route::url)))
      (ok (string= "/path" (slot-value (third r) 'meiro.route::url))))))


(deftest find-routes-test
  (let ((routes (meiro::collect-routes (list (list "/"
                                                   :get (list :handler nil)
                                                   :post (list :handler nil))
                                             (list "/path"
                                                   :get (list :handler nil))))))
    (testing "should return nil if no route found"
      (ok (eq nil (meiro::find-routes routes (list :path-info "/not-found")))))

    (testing "when fully matched route found, it should be in the returning list"
      (let* ((rs (meiro::find-routes routes (list :path-info "/" :request-method :get)))
            (r (caar rs)))
        (ok (= 2 (length rs))) ;; it should be two, :get and :post
        (ok (not (null r)))
        (ok (string= "/" (slot-value r 'meiro.route::url)))
        ;; method should be matched
        (ok (not (null (second (car rs)))))))

    (testing "when route found without method matched, it should be in the returning list"
      (let* ((rs (meiro::find-routes routes (list :path-info "/path" :request-method :post)))
            (r (caar rs)))
        (ok (not (null r)))
        (ok (string= "/path" (slot-value r 'meiro.route::url)))
        (ok (null (second (car rs))))))))


(deftest dispatch-test
  (let* ((some-handler (lambda (i) (declare (ignore i)) '(200 (:content-type "text/plain") ("found"))))
         (routes (meiro::collect-routes (list (list "/"
                                                    :get (list :handler some-handler)
                                                    :post (list :handler nil))
                                              (list "/path"
                                                    :get (list :handler nil))))))

    (testing "should return not-found when no route found for url"
      (let ((r (meiro::dispatch routes
                                (list :path-info "/not-found" :request-method :get)
                                meiro:*default-handlers*)))
        (ok (eq r (funcall (getf meiro:*default-handlers* :not-found) nil)))))

    (testing "should return method-not-allowed when route found for url but method not matched"
      (let ((r (meiro::dispatch routes
                                (list :path-info "/path" :request-method :post)
                                meiro:*default-handlers*)))
        (ok (eq r (funcall (getf meiro:*default-handlers* :method-not-allowed) nil)))))

    (testing "should return not-acceptable when route found for url, method matched but handler is nil"
      (let ((r (meiro::dispatch routes
                                (list :path-info "/path" :request-method :get)
                                meiro:*default-handlers*)))
        (ok (eq r (funcall (getf meiro:*default-handlers* :not-acceptable) nil)))))

    (testing "should return response when route found for url, method matched and handler is not nil"
      (let ((r (meiro::dispatch routes
                                (list :path-info "/" :request-method :get)
                                meiro:*default-handlers*)))
        (ok (eq r (funcall some-handler nil)))))
    ))


(deftest router-test
  "")
