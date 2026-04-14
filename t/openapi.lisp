(defpackage meiro.t.openapi
  (:use :cl
        :rove)
  (:import-from :meiro)
  (:import-from :meiro.openapi))

(in-package :meiro.t.openapi)


(deftest generate-openapi-basic-test
  (testing "should generate a basic openapi spec from routes"
    (let* ((routes (meiro:router
                    (list (list "/users"
                                :get (list :handler (lambda (e) (declare (ignore e)) '(200 () ("ok"))))))))
           (spec (meiro.openapi:generate-openapi routes
                                                  :title "Test API"
                                                  :version "1.0.0")))
      (ok (not (null spec)))
      (ok (string= "Test API" (meiro.openapi::spec-title spec)))
      (ok (string= "1.0.0" (meiro.openapi::spec-version spec)))
      (ok (= 1 (length (meiro.openapi::spec-operations spec)))))))


(deftest generate-openapi-with-path-params-test
  (testing "should auto-detect path parameters from URL"
    (let* ((routes (meiro:router
                    (list (list "/users/:id"
                                :get (list :handler (lambda (e) (declare (ignore e)) '(200 () ("ok"))))))))
           (spec (meiro.openapi:generate-openapi routes :title "Test" :version "1.0"))
           (op (first (meiro.openapi::spec-operations spec)))
           (params (meiro.openapi::operation-parameters op)))
      (ok (= 1 (length params)))
      (ok (string= "id" (meiro.openapi::param-name (first params))))
      (ok (eq :path (meiro.openapi::param-in (first params))))
      (ok (meiro.openapi::param-required-p (first params))))))


(deftest generate-openapi-with-metadata-test
  (testing "should include openapi metadata from route definition"
    (let* ((routes (meiro:router
                    (list (list "/users/:id"
                                :get (list :handler (lambda (e) (declare (ignore e)) '(200 () ("ok")))
                                           :openapi (list :summary "Get user by ID"
                                                          :tags (list "users")
                                                          :parameters (list (list :name "id"
                                                                                   :in :path
                                                                                   :schema (list :type "integer")
                                                                                   :description "User ID"))
                                                          :responses (list (list 200 :description "User found")
                                                                           (list 404 :description "User not found"))))))))
           (spec (meiro.openapi:generate-openapi routes :title "Test" :version "1.0"))
           (op (first (meiro.openapi::spec-operations spec))))
      (ok (string= "Get user by ID" (meiro.openapi::operation-summary op)))
      (ok (equal '("users") (meiro.openapi::operation-tags op)))
      (ok (= 1 (length (meiro.openapi::operation-parameters op))))
      (ok (= 2 (length (meiro.openapi::operation-responses op)))))))


(deftest openapi-to-json-test
  (testing "should convert spec to valid JSON alist"
    (let* ((routes (meiro:router
                    (list (list "/users"
                                :get (list :handler (lambda (e) (declare (ignore e)) '(200 () ("ok")))
                                           :openapi (list :summary "List users"
                                                          :responses (list (list 200 :description "Success"))))))))
           (spec (meiro.openapi:generate-openapi routes :title "Test API" :version "1.0.0"))
           (json (meiro.openapi:openapi-to-json spec)))
      (ok (assoc "openapi" json :test #'string=))
      (ok (string= "3.0.3" (cdr (assoc "openapi" json :test #'string=))))
      (ok (assoc "info" json :test #'string=))
      (ok (assoc "paths" json :test #'string=)))))


(deftest convert-url-to-openapi-path-test
  (testing "should convert meiro URL pattern to OpenAPI path format"
    (ok (string= "/users/{id}" (meiro.openapi::convert-url-to-openapi-path "/users/:id")))
    (ok (string= "/users/{id}/posts/{post-id}"
                 (meiro.openapi::convert-url-to-openapi-path "/users/:id/posts/:post-id")))
    (ok (string= "/static" (meiro.openapi::convert-url-to-openapi-path "/static")))))


(deftest openapi-handler-test
  (testing "should create a handler that returns JSON response"
    (let* ((routes (meiro:router
                    (list (list "/test"
                                :get (list :handler (lambda (e) (declare (ignore e)) '(200 () ("ok"))))))))
           (spec (meiro.openapi:generate-openapi routes :title "Test" :version "1.0"))
           (handler (meiro.openapi:openapi-handler spec))
           (response (funcall handler nil)))
      (ok (= 200 (first response)))
      (ok (string= "application/json" (getf (second response) :content-type)))
      (ok (stringp (first (third response)))))))
