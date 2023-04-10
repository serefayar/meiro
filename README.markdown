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

(use-package :clack)
(use-package :meiro)

(defun some-handler (env)
  "we have two new keys in env plist, path-params and query-params"
  (destructuring-bind (&key path-params query-params &allow-other-keys) env
     (declare (ignore path-params query-params))
     '(200 (:content-type "text/plain") ("Hello, Meiro!")))) 

;; meiro router
(defparameter *app*
  (meiro:clack-handler
    (meiro:router
      (list (list "/"
                  :get (list :name "home" 
                             :handler #'some-handler))
            (list "/found/:found/sub/:sub"
                  :get (list :handler #'some-handler))
            (list "/not-there"
                  :get (list :handler nil)
                  :post (list :handler #'some-handler))))
    ;; overridee fallback handler for not acceptable (url and method matched but handler is nil. for example: GET "/not-there")
    (meiro:create-fallback-handler
      :not-acceptable (constantly '(406 (:content-type "text/plain") ("it is not acceptable"))))))



(clack:clackup
         *app*
         :address "0.0.0.0"
         :port 3000)
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
