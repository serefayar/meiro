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
  (destructuring-bind (&key path-params query-params &allow-other-keys) env
     (declare (ignore path-params query-params))
     '(200 (:content-type "text/plain") ("Hello, Meiro!")))) 

;; meiro router
(defparameter *app*
  (router
    (list (list "/"
                 :get (list :handler #'some-handler))
          (list "/found/:found/sub/:sub"
                 :get (list :handler #'some-handler))
          (list "/not-there"
	             :get (list :handler nil)
                 :post (list :handler #'some-handler)))))



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
- [ ] performance optimization
- [ ] better documentation

## Author

* Seref R. Ayar

## Copyright

Copyright (c) 2023 Seref R. Ayar

## License

Licensed under the MIT License.