(defpackage meiro.t.url
  (:use :cl
        :rove)
  (:import-from :meiro))

(in-package :meiro.t.url)


(deftest test-parse-url
  (testing "parse url should return url with regex rules for params"
    (multiple-value-bind (re params) (meiro.url::parse-url "/path/:path/sub/:sub")
      (ok (string= re "^/path/([\\w-/]+)/sub/([\\w-/]+)$"))
      (ok (equalp params #("path" "sub")))))
  (testing "parse url should return same url if no path params"
    (multiple-value-bind (re params) (meiro.url::parse-url "/path/123/sub/321")
      (ok (string= re "^/path/123/sub/321$"))
      (ok (equalp params #())))))

(deftest test-parse-qs
  (testing "parse qs should return query string plist"
    (ok (equal (list :param1 "1" :param2 "2")
               (meiro.url::parse-qs "?param1=1&param2=2"))))
  (testing "parse qs should return nil if it is empty"
    (ok (equal nil
               (meiro.url::parse-qs "")))))
