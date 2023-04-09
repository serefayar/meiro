(defpackage :meiro.url
  (:use :cl)
  (:import-from :quri
                :url-decode)
  (:local-nicknames (:ppcre :cl-ppcre))
  (:export :parse-url
           :parse-qs))

(in-package :meiro.url)


(defun parse-url (url)
  (loop :with l = (ppcre:split ":([\\w-]+)" url :with-registers-p t :omit-unmatched-p t)
        :for (path name) :on l :by #'cddr
        :collect path :into re
        :when name
          :collect "([\\w-/]+)" into re
        :and
          :collect name :into params
        :finally
           (return (values (format nil "^~{~A~}$" re)
                           (make-array (length params) :initial-contents params)))))


(defun parse-qs (query-string)
  (let (result-list)
    (ppcre:do-scans (ms me reg-start reg-end "[(\?|\&)]?([^=]+)\=([^&#]+)" query-string
                     nil
                     :start 0 :end (length query-string))
      (push (map 'list
                 (lambda (rs re)
                   (if rs
                       (url-decode (subseq query-string rs re))
                       nil))
                 reg-start
                 reg-end)
            result-list))
    (reduce (lambda (acc i)
              (list* (intern (string-upcase (first i)) :keyword) (second i) acc))
            result-list
            :initial-value '())))
