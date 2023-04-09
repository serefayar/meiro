(defpackage :meiro.route
  (:use :cl)
  (:import-from :meiro.url
                :parse-url
                :parse-qs))

(in-package :meiro.route)


(define-condition conflicted-route (error)
  ((url :initarg :url
        :reader cr-url)
   (method :initarg :method
           :reader cr-method))
  (:report (lambda (condition stream)
     (format stream "Conflicted route found for \"~a: ~a\"" (cr-method condition) (cr-url condition)))))


(defclass route ()
  ((name
    :initarg :name
    :initform nil
    :accessor route-name)
   (url
    :initarg :url
    :accessor route-url)
   (method
    :initarg :method
    :initform :GET
    :accessor route-method)
   (handler
    :initarg :handler
    :accessor route-handler)
   (pattern
    :initform nil
    :accessor route-pattern)
   (params
    :initform nil
    :accessor route-params)))

(defmethod initialize-instance :after ((route route) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (re path-params) (parse-url url)
    (setf (route-pattern route) re
          (route-params route) (list :path path-params))))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t)
    (format stream "~s ~s"
            (route-method route)
            (route-url route))))


(defmethod match-route ((route route) url method query-string)
  (multiple-value-bind (route-match-p values)
      (ppcre:scan-to-strings (route-pattern route) url)
    (when route-match-p
        (if (eql method (route-method route))
            ;; both url and method matched
            (loop :with query-params = (parse-qs query-string)
                  :for key :across (getf (route-params route) :path)
                  :for val :across values
                  :appending (list (intern (string-upcase key) :keyword) val) :into path-params
                  :finally
                     (return (list route-match-p
                                   t
                                   (list :path path-params
                                         :query query-params))))
            ;; method not matched
            (list route-match-p nil nil)))))


(defmethod equal-route ((route1 route) (route2 route))
  (let* ((url (route-url route1))
         (method (route-method route1))
         (is-equal (and (string= url (route-url route2))
                        (eql method (route-method route2)))))
    (when is-equal
      (error 'conflicted-route :url url :method method))
    is-equal))
