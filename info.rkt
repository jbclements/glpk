#lang setup/infotab

(define collection 'multi)

#|undeclared dependency detected
raco setup:   for package: "glpk"
raco setup:   on packages:
raco setup:    "base"
raco setup:    "typed-racket-lib"
raco setup:   on packages for build:
raco setup:    "racket-doc"
raco setup:    "scribble-lib"
raco setup:    "scribble-math"
raco setup:    "typed-racket-more"
|#
(define deps
  (list "base"
        "typed-racket-lib"))

(define build-deps
  (list "racket-doc"
        "scribble-lib"
        "scribble-math"
        "typed-racket-more"))

