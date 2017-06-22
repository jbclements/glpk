#lang typed/racket

(provide lp-solve)

;; this formulation of the problem comes from glpk.pdf. You need
;; to express your problem using this formulation.

;; An Objective function is a constant plus a linear combination
;; of structural variables, plus a constant.
;; So, for instance, the objective function 3x + 4y - 7z + 9
;; would be represented as  '(9 (3 x) (4 y) (-7 z))
(define-type Objective (Pairof Real LinearCombination))

;; represents a linear combination of variables
(define-type LinearCombination (Listof (List Real Symbol)))

;; A direction of 'max indicates that the objective function
;; is to be maximized. A direction of 'min indicates that the
;; objective function is to be minimized
(define-type Direction (U 'max 'min))

;; a Constraint sets an auxiliary variable equal to a constant
;; plus a linear combination of structural variables.
;; So, for instance, the constraint
;; a = 3x + 4y - 7z would be represented as
;; '(a (3 x) (4 y) (-7 z))
(define-type Constraint
  (Pairof Symbol LinearCombination))

;; a Bound contains a lower and an upper limit on a variable,
;; either auxiliary or structural. Bounds can be infinite or
;; finite.
(define-type Bound (List Symbol BoundNum BoundNum))
(define-type BoundNum (U 'posinf 'neginf Real))

;; A Solution includes a value for the objective function
;; and an association list mapping structural
;; variable names to values.
(define-type Solution (List Float
                            (Listof (List Symbol Float))))

;; A FailCode indicates why the solver did not return a solution.
(define-type FailCode
  (U 'GLP_EBADB   ;; invalid basis
     'GLP_ESING   ;; singular matrix
     'GLP_ECOND   ;; ill-conditioned matrix
     'GLP_EBOUND  ;; invalid bounds
     'GLP_EFAIL   ;; solver failed
     'GLP_EOBJLL  ;; objective lower limit reached
     'GLP_EOBJUL  ;; objective upper limit reached
     'GLP_EITLIM  ;; iteration limit exceeded
     'GLP_ETMLIM  ;; time limit exceeded
     'GLP_ENOPFS  ;; no primal feasible solution
     'GLP_ENODFS  ;; no dual feasible solution
     'GLP_EROOT   ;; root LP optimum not provided
     'GLP_ESTOP   ;; search terminated by application
     'GLP_EMIPGAP ;; relative mip gap tolerance reached
     'GLP_ENOFEAS ;; no primal/dual feasible solution
     'GLP_ENOCVG  ;; no convergence
     'GLP_EINSTAB ;; numerical instability
     'GLP_EDATA   ;; invalid data
     'GLP_ERANGE  ;; result out of range
     ))

(define-type BoundsSym (U 'GLP_FR 'GLP_LO 'GLP_UP 'GLP_DB 'GLP_FX))


(require (only-in typed/racket/unsafe unsafe-require/typed))

(unsafe-require/typed "lib.rkt"
               [#:opaque Problem glpk-problem?])

(require/typed "lib.rkt"
               [glp_term_out ((U 'GLP_ON 'GLP_OFF) -> Symbol)]
               [glp_create_prob (-> Problem)]
               [glp_set_obj_dir (Problem (U 'GLP_MIN
                                            'GLP_MAX)
                                         -> Void)]
               [glp_add_rows (Problem Natural -> Natural)]
               [glp_add_cols (Problem Natural -> Natural)]
               [glp_set_row_name (Problem Natural Bytes -> Void)]
               [glp_set_col_name (Problem Natural Bytes -> Void)]
               [glp_set_row_bnds (Problem Natural BoundsSym
                                          Float Float -> Void)]
               [glp_set_col_bnds (Problem Natural BoundsSym
                                          Float Float -> Void)]
               [glp_set_obj_coef (Problem Natural Float -> Void)]
               [glp_simplex (Problem False -> (U 'success FailCode))]
               [glp_get_obj_val (Problem -> Float)]
               [glp_get_col_prim (Problem Natural -> Float)]
               [M_MAX Natural]
               [N_MAX Natural]
               [NNZ_MAX Natural]
               [glp-load-matrix (Problem (Listof
                                          (List Natural Natural Flonum))
                                         -> Void)]
               )

;; pulled these limits out of the source code. They really 

;; the type for lp-solve is derived from the formulation of the
;; linear programming problem as described in glpk.pdf.
(: lp-solve
   (Objective Direction (Listof Constraint) (Listof Bound)
              -> (U FailCode Solution)))
(define (lp-solve objective direction constraints bounds)
  (unless (< 0 (length constraints) M_MAX)
    (raise-argument-error 'lp-solve
                          "list of constraints with length in 1..M_MAX"
                          2 objective direction constraints bounds))
  (define aux-vars (map (ann car (Constraint -> Symbol))
                        constraints))
  (ensure-distinct "auxiliary" aux-vars)
  (define struct-vars (remove-duplicates
                       (apply
                        append
                        (map constraint-rhs-vars
                             constraints))))
  (unless (< 0 (length struct-vars) N_MAX)
    (raise-argument-error
     'lp-solve
     "problem with more than zero and fewer than N_MAX structural vars"
     2 objective direction constraints bounds))
  (check-no-overlap aux-vars struct-vars)
  (define all-vars (append aux-vars struct-vars))
  (define bounded-vars (map (inst first Symbol Any)
                            bounds))
  (check-all-present all-vars bounded-vars
                     "variables ~e have no provided bounds")
  (define optimized-vars (map (inst second Any Symbol Any)
                              (cdr objective)))
  (check-all-present optimized-vars struct-vars
                     "variables ~e in objective fn are not constrained")
  (ensure-distinct
   "objective"
   (map (inst cadr Any Symbol Any) (cdr objective)))
  ;; turn the pairs around to allow assoc to work:
  (define objective-assoc : (Listof (Pairof Symbol Real))
    (map (λ ([pr : (List Real Symbol)]) : (Pairof Symbol Real)
           (cons (second pr) (first pr)))
         (cdr objective)))
  (glp_term_out 'GLP_OFF)
  (define prob (glp_create_prob))
  (glp_set_obj_dir prob
                   (cond [(eq? direction 'max) 'GLP_MAX]
                         [(eq? direction 'min) 'GLP_MIN]))  
  (row/col-setup prob aux-vars bounds 'row)
  (row/col-setup prob struct-vars bounds 'col)
  (glp_set_obj_coef prob 0 (real->double-flonum (car objective)))
  ;; FIXME check that uninitialized coefficients are set to 0.0
  (lin-comb-map
   struct-vars
   (cdr objective)
   (λ ([j : Natural] [val : Flonum]) : Void
     (glp_set_obj_coef prob j val))
   "objective")
  (define nonzero-matrix-tuples
    (apply
     append
     (for/list : (Listof (Listof (List Natural Natural Flonum)))
      ([constraint (in-list constraints)]
       [i : Natural
          (ann (in-range 1 (add1 (length constraints)))
               (Sequenceof Natural))])
      (lin-comb-map
       struct-vars
       (cdr constraint)
       (λ ([j : Natural] [val : Flonum]) : (List Natural Natural Flonum)
         (list i j val))
       "constraint"))))
  (when (< NNZ_MAX (length nonzero-matrix-tuples))
    (error 'lp-solve
           "problem has more than NNZ_MAX nonzero constraint coefficients"))
  (glp-load-matrix prob nonzero-matrix-tuples)
  (match (glp_simplex prob #f)
    ['success
     (list
      (glp_get_obj_val prob)
      (for/list : (Listof (List Symbol Float))
        ([struct-var (in-list struct-vars)]
         [i : Natural
            (ann (in-range 1 (add1 (length struct-vars)))
                 (Sequenceof Natural))])
        (list struct-var (glp_get_col_prim prob i))))]
    [fail-code
     (cast fail-code FailCode)]))

;; given the list of structural variables and a LinearCombination
;; (without repeats)
;; and a function, call the function once for each of the structural
;; variables, passing in the index and the corresponding coefficient
;; (0.0 if the variable doesn't occur in the linear combination).
(: lin-comb-map
   (All (T) ((Listof Symbol) LinearCombination (Natural Flonum -> T)
                             String
                             -> (Listof T))))
(define (lin-comb-map vars lin-comb fn kind)
  (ensure-distinct
   kind
   (map (inst cadr Any Symbol Any) lin-comb))
  ;; turn the pairs around to allow assoc to work:
  (define lin-comb-assoc : (Listof (Pairof Symbol Real))
    (map (λ ([pr : (List Real Symbol)]) : (Pairof Symbol Real)
           (cons (second pr) (first pr)))
         lin-comb))
  (for/list : (Listof T)
    ([var (in-list vars)]
     [j : Natural (ann (in-range 1 (add1 (length vars)))
                       (Sequenceof Natural))]
     #:when (assoc var lin-comb-assoc))
    (match (assoc var lin-comb-assoc)
      [(cons _ n)
       (fn j (real->double-flonum n))])))

;; given the problem and
;; a list of auxiliary (row) or structural (column) variables,
;; and the list of all the bounds, and whether this is for rows
;; or columns, set up the rows or columns appropriately
(define (row/col-setup [prob : Problem]
                       [vars : (Listof Symbol)]
                       [bounds : (Listof Bound)]
                       [row/col : (U 'row 'col)]) : Void
  (cond [(eq? row/col 'row)
         (glp_add_rows prob (length vars))]
        [else
         (glp_add_cols prob (length vars))])
  (for ([name (in-list vars)]
        [i : Natural (in-range 1 (add1 (length vars)))])
    (define namebytes (string->bytes/utf-8
                       (symbol->string name)))
    (ensure-short namebytes)
    (cond [(eq? row/col 'row)
           (glp_set_row_name prob i namebytes)]
          [else
           (glp_set_col_name prob i namebytes)])
    (define (set-row/col-bound [type : BoundsSym]
                               [lo : Float]
                               [hi : Float])
      (cond [(eq? row/col 'row)
             (glp_set_row_bnds prob i type lo hi)]
            [else
             (glp_set_col_bnds prob i type lo hi)]))
    (match (assoc name bounds)
      [(and (list _ _ 'neginf) badbound)
       (error 'lp-solve
              "neginf not allowed as upper bound: ~e"
              badbound)]
      [(and (list _ 'posinf _) badbound)
       (error 'lp-solve
              "posinf not allowed as lower bound: ~e"
              badbound)]
      [(list _ 'neginf 'posinf)
       (set-row/col-bound 'GLP_FR 0.0 0.0)]
      [(list _ 'neginf n)
       ;; n can't be posinf or neginf by earlier checks:
       (set-row/col-bound 'GLP_UP
                          0.0
                          (real->double-flonum (cast n Real)))]
      ;; lo can't be neginf or posinf now
      [(list _ n 'posinf)
       ;; n can't be posinf or neginf by earlier checks:
       (set-row/col-bound 'GLP_LO
                          (real->double-flonum (cast n Real))
                          0.0)]
      ;; neither hi nor lo can be either posinf or neginf now
      [(list _ lo hi)
       ;; TR can't figure out the invariants, must cast:
       (define lonum (cast lo Real))
       (define hinum (cast hi Real))
       (cond [(< lonum hinum)
              (set-row/col-bound 'GLP_DB
                                 (real->double-flonum lonum)
                                 (real->double-flonum hinum))]
             [(= lonum hinum)
              (set-row/col-bound 'GLP_FX
                                 (real->double-flonum lonum)
                                 (real->double-flonum hinum))]
             [else
              (error 'lp-solve
                     "low bound ~v higher than high bound ~v"
                     lonum hinum)])]
      [#f (error 'lp-solve
                 ;; we already checked for this...
                 "internal error 594b0085 bound not found")])))



;; given a byte-string, ensure it's less than 256 bytes
(define (ensure-short [b : Bytes]) : Void
  (when (< 255 (bytes-length b))
    (raise-argument-error
     'ensure-short
     "var name less than 256 characters"
     0 b)))

;; given a constraint, return a list of its rhs (structural) vars.
(define (constraint-rhs-vars [c : Constraint]) : (Listof Symbol)
  (map (ann second ((List Real Symbol) -> Symbol))
       (cdr c)))

;; check that two sets of symbols (aux & struct vars) don't overlap
(define (check-no-overlap [aux-vars : (Listof Symbol)]
                          [struct-vars : (Listof Symbol)]) : Void
  (define overlap (set-intersect (list->set aux-vars)
                                 (list->set struct-vars)))
  (unless (set-empty? overlap)
    (error
     'check-no-overlap
     "auxiliary variables ~e appear in the RHS of some constraint"
     overlap)))

;; check that every element of the first list (inner) appears
;; in the second list (cover)
(define (check-all-present [inner : (Listof Symbol)]
                           [cover : (Listof Symbol)]
                           [error-fmt-string : String]) : Void
  (define unbounded-vars (remove* cover inner))
  (unless (empty? unbounded-vars)
    (error 'check-all-present
           error-fmt-string
           unbounded-vars)))

;; given a label and a list of symbols, signal an error
;; if there are any duplicates
(define (ensure-distinct [kind : String]
                         [varnames : (Listof Symbol)]) : Void
  (match (check-duplicates varnames)
    [#f (void)]
    [other (error 'check-distinct
                  "duplicate ~a variable name: ~e"
                  kind
                  other)]))

(module+ test
  (require typed/rackunit)
;; example:
;; 480_csc + 480_cpe          < 8 ;; 8 sections offered
;; 490_csc + 490_cpe + 490_se < 6 ;; 6 sections offered
;;           431_cpe + 431_se < 2 ;; 2 sections offered
;; 480_csc + 490_csc = 10 ;; these students need 10 sections of TE
;; 480_cpe + 490_cpe + 431_cpe = 2 ;; these need 2 sections of TE
;; 490_se  + 431_se = 2 ;; these need 2 sections of TE
;; actually just want a feasible solution...
  (check-equal?
   (lp-solve '(0 (1 490_se)) 'max
             '((480_extra (1 480_csc) (1 480_cpe))
               (490_extra (1 480_csc) (1 490_cpe) (1 490_se))
               (431_extra (1 431_cpe) (1 431_se))
               (csc_tes (1 480_csc) (1 490_csc))
               (cpe_tes (1 480_cpe) (1 490_cpe) (1 431_cpe))
               (se_tes (1 490_se) (1 431_se)))
             '((480_extra 0 8)
               (490_extra 0 6)
               (431_extra 0 2)
               (csc_tes 10 10)
               (cpe_tes 2 2)
               (se_tes 2 2)
               (480_csc 0 posinf) (480_cpe 0 posinf)
               (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
               (431_cpe 0 posinf) (431_se 0 posinf)))
   '(2.0
     ((480_csc 4.0)
      (480_cpe 2.0)
      (490_cpe 0.0)
      (490_se 2.0)
      (431_cpe 0.0)
      (431_se 0.0)
      (490_csc 6.0))))

(check-exn #px"duplicate auxiliary variable name: '480_extra"
           (λ ()
             (lp-solve '(0 (1 431_se)) 'max
                       '((480_extra (-1 480_csc) (-1 480_cpe))
                         (480_extra (-1 480_csc) (-1 490_cpe) (-1 490_se))
                         (431_extra (-1 431_cpe) (-1 431_se)))
                       '((480_extra 0 posinf)
                         (490_extra 0 posinf)
                         (431_extra 0 posinf)
                         (480_csc 0 posinf) (480_cpe 0 posinf)
                         (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                         (431_cpe 0 posinf) (431_se 0 posinf)))))

  (check-exn #px"auxiliary variables.*appear in the RHS of some con"
             (λ ()
               (lp-solve '(0 (1 431_se)) 'max
                         '((480_extra (-1 480_csc) (-1 480_cpe))
                           (490_extra (-1 480_csc) (-1 490_cpe) (-1 490_se))
                           (431_extra (-1 431_cpe) (-1 431_se)
                                      (3 490_extra)))
                         '((480_extra 0 posinf)
                           (490_extra 0 posinf)
                           (431_extra 0 posinf)
                           (480_csc 0 posinf) (480_cpe 0 posinf)
                           (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                           (431_cpe 0 posinf) (431_se 0 posinf)))))

  (check-exn #px"variables.*have no provided bounds"
             (λ ()
               (lp-solve '(0 (1 431_se)) 'max
                         '((480_extra (-1 480_csc) (-1 480_cpe))
                           (490_extra (-1 480_csc) (-1 490_cpe) (-1 490_se))
                           (431_extra (-1 431_cpe) (-1 431_se)))
                         '((480_extra 0 posinf)
                           (490_extra 0 posinf)
                           (480_csc 0 posinf) (480_cpe 0 posinf)
                           (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                           (431_cpe 0 posinf) (431_se 0 posinf)))))

  (check-exn
   #px"variables.*in objective fn are not constrained"
   (λ ()
     (lp-solve '(0 (1 ZZZ)) 'max
               '((480_extra (-1 480_csc) (-1 480_cpe))
                 (490_extra (-1 480_csc) (-1 490_cpe) (-1 490_se))
                 (431_extra (-1 431_cpe) (-1 431_se)))
               '((480_extra 0 posinf)
                 (490_extra 0 posinf)
                 (431_extra 0 posinf)
                 (480_csc 0 posinf) (480_cpe 0 posinf)
                 (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                 (431_cpe 0 posinf) (431_se 0 posinf)))))

  (check-exn
   #px"list of constraints with length in"
   (λ ()
     (lp-solve '(0 (1 480_extra)) 'max
               '()
               '((480_extra 0 posinf)
                 (490_extra 0 posinf)
                 (431_extra 0 posinf)
                 (480_csc 0 posinf) (480_cpe 0 posinf)
                 (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                 (431_cpe 0 posinf) (431_se 0 posinf))))))



