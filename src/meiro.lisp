(defpackage meiro
  (:use :cl)
  (:import-from :meiro.route
                :route
                :match-route
                :equal-route
                :route-handler)
  (:export :router
           :clack-handler
           :create-fallback-handler))

(in-package :meiro)


(defun union-routes (routes1 routes2)
  (union routes1 routes2 :test #'equal-route))


(defun map-routes (route)
  (let ((url (first route))
        (defs (rest route)))
    (loop :for (method def) :on defs :by 'cddr
          :collect  (make-instance 'route
                                   :url url
                                   :handler (getf def :handler)
                                   :method method))))

(defun collect-routes (routes)
  (reduce #'union-routes
          (mapcar #'map-routes routes)))



(defun find-routes (routes env)
  (destructuring-bind (&key request-method path-info query-string &allow-other-keys) env
    (reduce (lambda (acc route)
              (let ((rm (match-route route path-info request-method query-string)))
                (if (first rm)
                    (destructuring-bind (route-match-p method-match-p params) rm
                      (when route-match-p
                        (destructuring-bind (&key path query) params
                          (cons (list route
                                      method-match-p
                                      (append
                                       (when path (list :path-params path))
                                       (when query (list :query-params query))
                                       env))
                                acc))))
                    acc)
                ))
            routes
            :initial-value '())))



(defun dispatch (routes env fallback-handlers)
  (let* ((routes-found (find-routes routes env))
         (route-matched (find-if (lambda (rf) (second rf)) routes-found))
         (route (first route-matched))
         (response (cond
                     ((not routes-found) (getf fallback-handlers :not-found))
                     ((not route-matched) (getf fallback-handlers :method-not-allowed))
                     ((null (route-handler route)) (getf fallback-handlers :not-acceptable))
                     (t (route-handler route)))))
    (funcall response env)))


(defun create-fallback-handler (&optional &key not-found
                                            method-not-allowed
                                            not-acceptable)
  ;; do 'or' them here for better readability
  (list :not-found (or not-found
                       (constantly '(404 (:content-type "text/plain") ("not found"))))
        :method-not-allowed (or method-not-allowed
                                (constantly '(405 (:content-type "text/plain") ("method not allowed"))))
        :not-acceptable (or not-acceptable
                            (constantly '(406 (:content-type "text/plain") ("not acceptable"))))))


(defun router (routes)
  (collect-routes routes))


(defun clack-handler (router &optional fallback-handler options)
  (declare (ignore options)) ; ignore it for now
  (lambda (env)
    (dispatch router env (or fallback-handler
                             (create-fallback-handler)))))
