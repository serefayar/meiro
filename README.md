# meiro

Meiro is an URL routing library targeting to [Clack](http://clacklisp.org/).


## STATUS 

***Pre-alpha, in design and prototyping phase.***

    
## Installation

```common-lisp
;; from "local-projects"
(ql:quickload :meiro)
```

## Usage

```common-lisp
(ql:quickload :clack)
(ql:quickload :meiro)

(defun get-user (env)
  "Handler with path-params and query-params injected into env"
  (destructuring-bind (&key path-params query-params &allow-other-keys) env
    (declare (ignore query-params))
    (let ((id (getf path-params :id)))
      `(200 (:content-type "application/json")
            (,(format nil "{\"id\": ~a}" id))))))

(defun list-users (env)
  (declare (ignore env))
  '(200 (:content-type "application/json") ("[{\"id\": 1}, {\"id\": 2}]")))

;; Define routes with OpenAPI metadata
(defparameter *routes*
  `(("/users"
     :get (:handler ,#'list-users
           :openapi (:summary "List all users"
                     :tags ("users")
                     :responses ((200 :description "Success")))))
    ("/users/:id"
     :get (:handler ,#'get-user
           :openapi (:summary "Get user by ID"
                     :tags ("users")
                     :parameters ((:name "id"
                                   :in :path
                                   :schema (:type "integer")
                                   :description "User ID"))
                     :responses ((200 :description "User found")
                                 (404 :description "Not found")))))))

;; Create router
(defparameter *router* (meiro:router *routes*))

;; Generate OpenAPI spec
(defparameter *openapi-spec*
  (meiro.openapi:generate-openapi *router*
                                   :title "User API"
                                   :version "1.0.0"))

;; Create Clack app with OpenAPI endpoint
(defparameter *app*
  (meiro:clack-handler
    (meiro:router
      (append *routes*
              (list (list "/openapi.json"
                          :get (list :handler (meiro.openapi:openapi-handler *openapi-spec*))))))
    (meiro:create-fallback-handler
      :not-found (constantly '(404 (:content-type "application/json") ("{\"error\": \"not found\"}"))))))

(clack:clackup *app* :address "0.0.0.0" :port 3000)
;; GET /users         -> List users
;; GET /users/123     -> Get user 123
;; GET /openapi.json  -> OpenAPI spec
```

## Inspired Projects
 * [myway](https://github.com/fukamachi/myway) 
 * [reitit](https://github.com/metosin/reitit) for route definition

## TODO

- [ ] stabilize the api
- [ ] more test
- [ ] performance optimization
- [ ] better documentation

## Author

* Seref R. Ayar

## Copyright

Copyright (c) 2023 Seref R. Ayar

## License

Licensed under the MIT License.
