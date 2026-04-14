(defpackage :meiro.openapi
  (:use :cl)
  (:import-from :meiro.route
   :route
                :route-url
   :route-method
                :route-params
   :route-openapi-metadata)
  (:export :openapi-parameter
   :openapi-request-body
           :openapi-response
   :openapi-operation
           :openapi-spec
   :generate-openapi
           :openapi-to-json
   :openapi-handler))

(in-package :meiro.openapi)


(defclass openapi-parameter ()
  ((name
    :initarg :name
    :accessor param-name)
   (in
    :initarg :in
    :initform :path
    :accessor param-in)
   (description
    :initarg :description
    :initform nil
    :accessor param-description)
   (required
    :initarg :required
    :initform nil
    :accessor param-required-p)
   (schema
    :initarg :schema
    :initform '(:type "string")
    :accessor param-schema)
   (example
    :initarg :example
    :initform nil
    :accessor param-example)))


(defclass openapi-request-body ()
  ((description
    :initarg :description
    :initform nil
    :accessor body-description)
   (required
    :initarg :required
    :initform t
    :accessor body-required-p)
   (content
    :initarg :content
    :initform nil
    :accessor body-content)))


(defclass openapi-response ()
  ((status
    :initarg :status
    :accessor response-status)
   (description
    :initarg :description
    :accessor response-description)
   (content
    :initarg :content
    :initform nil
    :accessor response-content)
   (headers
    :initarg :headers
    :initform nil
    :accessor response-headers)))


(defclass openapi-operation ()
  ((route
    :initarg :route
    :accessor operation-route)
   (operation-id
    :initarg :operation-id
    :initform nil
    :accessor operation-id)
   (summary
    :initarg :summary
    :initform nil
    :accessor operation-summary)
   (description
    :initarg :description
    :initform nil
    :accessor operation-description)
   (tags
    :initarg :tags
    :initform nil
    :accessor operation-tags)
   (parameters
    :initarg :parameters
    :initform nil
    :accessor operation-parameters)
   (request-body
    :initarg :request-body
    :initform nil
    :accessor operation-request-body)
   (responses
    :initarg :responses
    :initform nil
    :accessor operation-responses)
   (deprecated
    :initarg :deprecated
    :initform nil
    :accessor operation-deprecated-p)
   (security
    :initarg :security
    :initform nil
    :accessor operation-security)))


(defclass openapi-spec ()
  ((openapi-version
    :initform "3.0.3"
    :accessor spec-openapi-version)
   (title
    :initarg :title
    :accessor spec-title)
   (version
    :initarg :version
    :accessor spec-version)
   (description
    :initarg :description
    :initform nil
    :accessor spec-description)
   (servers
    :initarg :servers
    :initform nil
    :accessor spec-servers)
   (operations
    :initform nil
    :accessor spec-operations)
   (components
    :initarg :components
    :initform nil
    :accessor spec-components)
   (tags
    :initarg :tags
    :initform nil
    :accessor spec-tags)))



;;; Helpers

(defun plist-to-alist (plist)
  "Convert a plist to an alist with string keys."
  (loop for (key value) on plist by #'cddr
        collect (cons (string-downcase (symbol-name key)) value)))


(defun keyword-to-string (keyword)
  "Convert a keyword to lowercase string."
  (string-downcase (symbol-name keyword)))


(defun parse-schema (schema-plist)
  "Convert a schema plist to alist format."
  (when schema-plist
    (plist-to-alist schema-plist)))


(defun parse-content (content-plist)
  "Parse content specification to alist format."
  (when content-plist
    (loop for (media-type spec) on content-plist by #'cddr
          collect (cons media-type
                        (when spec
                          (let ((schema (getf spec :schema)))
                            (when schema
                              (list (cons "schema" (parse-schema schema))))))))))


;;; Building OpenAPI objects from route metadata

(defun make-parameter-from-plist (plist)
  "Create an openapi-parameter from a plist definition."
  (make-instance 'openapi-parameter
                 :name (getf plist :name)
                 :in (or (getf plist :in) :path)
                 :description (getf plist :description)
                 :required (or (getf plist :required)
                               (eq (getf plist :in) :path))
                 :schema (or (getf plist :schema) '(:type "string"))
                 :example (getf plist :example)))


(defun make-response-from-plist (plist)
  "Create an openapi-response from a plist definition.
   PLIST format: (status :description desc :content content-spec)"
  (let ((status (first plist))
        (props (rest plist)))
    (make-instance 'openapi-response
                   :status status
                   :description (getf props :description)
                   :content (getf props :content))))


(defun make-request-body-from-plist (plist)
  "Create an openapi-request-body from a plist definition."
  (when plist
    (make-instance 'openapi-request-body
                   :description (getf plist :description)
                   :required (if (member :required plist)
                                 (getf plist :required)
                                 t)
                   :content (getf plist :content))))


(defun make-operation-from-route (route)
  "Create an openapi-operation from a route with openapi metadata."
  (let* ((metadata (route-openapi-metadata route))
         (path-param-names (getf (route-params route) :path))
         (user-params (mapcar #'make-parameter-from-plist
                              (getf metadata :parameters)))
         (user-param-names (mapcar #'param-name user-params))
         (auto-params (loop for name across (or path-param-names #())
                            unless (member name user-param-names :test #'string=)
                              collect (make-instance 'openapi-parameter
                                                     :name name
                                                     :in :path
                                                     :required t))))
    (make-instance 'openapi-operation
                   :route route
                   :operation-id (getf metadata :operation-id)
                   :summary (getf metadata :summary)
                   :description (getf metadata :description)
                   :tags (getf metadata :tags)
                   :parameters (append auto-params user-params)
                   :request-body (make-request-body-from-plist
                                  (getf metadata :request-body))
                   :responses (mapcar #'make-response-from-plist
                                      (getf metadata :responses))
                   :deprecated (getf metadata :deprecated)
                   :security (getf metadata :security))))


;;; JSON Serialization

(defgeneric openapi-to-json (object)
  (:documentation "Convert an OpenAPI object to JSON-compatible alist."))


(defmethod openapi-to-json ((param openapi-parameter))
  (let ((result (list (cons "name" (param-name param))
                      (cons "in" (keyword-to-string (param-in param)))
                      (cons "required" (if (param-required-p param) t :false))
                      (cons "schema" (parse-schema (param-schema param))))))
    (when (param-description param)
      (push (cons "description" (param-description param)) result))
    (when (param-example param)
      (push (cons "example" (param-example param)) result))
    (nreverse result)))


(defmethod openapi-to-json ((body openapi-request-body))
  (let ((result (list (cons "required" (if (body-required-p body) t :false)))))
    (when (body-description body)
      (push (cons "description" (body-description body)) result))
    (when (body-content body)
      (push (cons "content" (parse-content (body-content body))) result))
    (nreverse result)))


(defmethod openapi-to-json ((response openapi-response))
  (let ((result (list (cons "description" (or (response-description response) "")))))
    (when (response-content response)
      (push (cons "content" (parse-content (response-content response))) result))
    (nreverse result)))


(defmethod openapi-to-json ((op openapi-operation))
  (let ((result nil))
    (when (operation-responses op)
      (push (cons "responses"
                  (loop for resp in (operation-responses op)
                        collect (cons (write-to-string (response-status resp))
                                      (openapi-to-json resp))))
            result))
    (when (operation-request-body op)
      (push (cons "requestBody" (openapi-to-json (operation-request-body op))) result))
    (when (operation-parameters op)
      (push (cons "parameters"
                  (mapcar #'openapi-to-json (operation-parameters op)))
            result))
    (when (operation-security op)
      (push (cons "security" (operation-security op)) result))
    (when (operation-deprecated-p op)
      (push (cons "deprecated" t) result))
    (when (operation-tags op)
      (push (cons "tags" (coerce (operation-tags op) 'list)) result))
    (when (operation-description op)
      (push (cons "description" (operation-description op)) result))
    (when (operation-summary op)
      (push (cons "summary" (operation-summary op)) result))
    (when (operation-id op)
      (push (cons "operationId" (operation-id op)) result))
    result))


(defun convert-url-to-openapi-path (url)
  "Convert Meiro URL pattern to OpenAPI path format.
   /users/:id -> /users/{id}"
  (ppcre:regex-replace-all ":([\\w-]+)" url "{\\1}"))


(defun group-operations-by-path (operations)
  "Group operations by their URL path."
  (let ((paths (make-hash-table :test 'equal)))
    (dolist (op operations)
      (let* ((route (operation-route op))
             (path (convert-url-to-openapi-path (route-url route)))
             (method (keyword-to-string (route-method route))))
        (unless (gethash path paths)
          (setf (gethash path paths) nil))
        (push (cons method (openapi-to-json op))
              (gethash path paths))))
    paths))


(defmethod openapi-to-json ((spec openapi-spec))
  (let ((paths-hash (group-operations-by-path (spec-operations spec)))
        (paths-alist nil))
    (maphash (lambda (path methods)
               (push (cons path methods) paths-alist))
             paths-hash)
    (let ((result (list (cons "openapi" (spec-openapi-version spec))
                        (cons "info"
                              (let ((info (list (cons "title" (spec-title spec))
                                                (cons "version" (spec-version spec)))))
                                (when (spec-description spec)
                                  (push (cons "description" (spec-description spec)) info))
                                (nreverse info)))
                        (cons "paths" (nreverse paths-alist)))))
      (when (spec-servers spec)
        (push (cons "servers" (spec-servers spec)) (cdr (last result))))
      (when (spec-components spec)
        (push (cons "components" (spec-components spec)) (cdr (last result))))
      (when (spec-tags spec)
        (push (cons "tags" (spec-tags spec)) (cdr (last result))))
      result)))



;;; Public API

(defun generate-openapi (routes &key title version description servers components tags)
  "Generate an OpenAPI specification from a list of routes.
   ROUTES should be a list of route objects (output of meiro:router).
   Returns an openapi-spec object."
  (let ((spec (make-instance 'openapi-spec
                             :title (or title "API")
                             :version (or version "1.0.0")
                             :description description
                             :servers servers
                             :components components
                             :tags tags)))
    (setf (spec-operations spec)
          (mapcar #'make-operation-from-route routes))
    spec))


(defun alist-p (obj)
  "Check if OBJ is an alist with string keys."
  (and (listp obj)
       (not (null obj))
       (every (lambda (x) (and (consp x) (stringp (car x)))) obj)))


(defun convert-value (value)
  "Recursively convert a value for JSON encoding."
  (cond
    ((null value) :null)
    ((eq value t) t)
    ((eq value :false) :false)
    ((stringp value) value)
    ((numberp value) value)
    ((hash-table-p value) value)
    ((alist-p value)
     (alist-to-hash-table value))
    ((listp value)
     (mapcar #'convert-value value))
    (t value)))


(defun alist-to-hash-table (alist)
  "Recursively convert an alist to a hash table for yason encoding."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht)
            (convert-value (cdr pair))))))


(defun spec-to-json-string (spec &optional (stream nil))
  "Convert an openapi-spec to a JSON string."
  (yason:with-output (stream :indent t)
    (yason:encode (alist-to-hash-table (openapi-to-json spec)))))


(defun openapi-handler (spec)
  "Create a Clack handler that serves the OpenAPI JSON specification."
  (lambda (env)
    (declare (ignore env))
    (list 200
          (list :content-type "application/json")
          (list (with-output-to-string (s)
                  (spec-to-json-string spec s))))))
