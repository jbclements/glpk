#lang typed/racket

(require/typed "lib.rkt"
               [glp_term_out ((U 'GLP_ON 'GLP_OFF) -> Symbol)]
               [#:opaque Problem glpk-problem?]
               [glp_create_prob (-> Problem)]
               [glp_set_obj_dir (Problem (U 'GLP_MIN
                                            'GLP_MAX)
                                         -> Void)]
               [glp_add_rows (Problem Natural -> Void)]
               [M_MAX Natural])

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
;; a = 3x + 4y - 7z + 9 would be represented as
;; '(a 9 (3 x) (4 y) (-7 z))
(define-type Constraint
  (Pairof Symbol (Pairof Real LinearCombination)))

;; a Bound contains a lower and an upper limit on a variable,
;; either auxiliary or structural. Bounds can be infinite or
;; finite.
(define-type Bound (List Symbol BoundNum BoundNum))
(define-type BoundNum (U 'posinf 'neginf Real))

;; A Solution includes a value for the objective function,
;; an association list mapping auxiliary variable names
;; to values, and an association list mapping structural
;; variable names to values.
(define-type Solution (List Float
                            (Listof (List Symbol Float))
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
  (glp_term_out 'GLP_OFF)
  (define prob (glp_create_prob))
  (glp_set_obj_dir prob
                   (cond [(eq? direction 'max) 'GLP_MAX]
                         [(eq? direction 'min) 'GLP_MIN]))
  (glp_add_rows prob (length constraints))
  
  (error 'lp-solve
         "unimplemented"))

'(
;s4:   glp_add_rows(lp, 3);
(glp_add_rows lp 3)
;s5:   glp_set_row_name(lp, 1, "p");
(glp_set_row_name lp 1 "p")
;s6:   glp_set_row_bnds(lp, 1, GLP_UP, 0.0, 100.0);
(glp_set_row_bnds lp 1 'GLP_UP 0.0 100.0)
;s7:   glp_set_row_name(lp, 2, "q");
;s8:   glp_set_row_bnds(lp, 2, GLP_UP, 0.0, 600.0);
(glp_set_row_name lp 2 "q")
(glp_set_row_bnds lp 2 'GLP_UP 0.0 600.0)
;s9:   glp_set_row_name(lp, 3, "r");
;s10:  glp_set_row_bnds(lp, 3, GLP_UP, 0.0, 300.0);
(glp_set_row_name lp 3 "r")
(glp_set_row_bnds lp 3 'GLP_UP 0.0 300.0)
;s11:  glp_add_cols(lp, 3);
(glp_add_cols lp 3)
;s12:  glp_set_col_name(lp, 1, "x1");
;s13:  glp_set_col_bnds(lp, 1, GLP_LO, 0.0, 0.0);
(glp_set_col_name lp 1 "x1")
(glp_set_col_bnds lp 1 'GLP_LO 0.0 0.0)
;s14:  glp_set_obj_coef(lp, 1, 10.0);
(glp_set_obj_coef lp 1 10.0)
;s15:  glp_set_col_name(lp, 2, "x2");
;s16:  glp_set_col_bnds(lp, 2, GLP_LO, 0.0, 0.0);
;s17:  glp_set_obj_coef(lp, 2, 6.0);
(glp_set_col_name lp 2 "x2")
(glp_set_col_bnds lp 2 'GLP_LO 0.0 0.0)
(glp_set_obj_coef lp 2 6.0)
;s18:  glp_set_col_name(lp, 3, "x3");
;s19:  glp_set_col_bnds(lp, 3, GLP_LO, 0.0, 0.0);
;s20:  glp_set_obj_coef(lp, 3, 4.0);
(glp_set_col_name lp 3 "x3")
(glp_set_col_bnds lp 3 'GLP_LO 0.0 0.0)
(glp_set_obj_coef lp 3 4.0)

;s21: ia[1]=1,ja[1]=1,ar[1]=
;s22: ia[2]=1,ja[2]=2,ar[2]=
;s23: ia[3]=1,ja[3]=3,ar[3]=
;s24:  ia[4] = 2, ja[4] = 1, ar[4] = 10.0; /* a[2,1] = 10 */
;s25: ia[5]=3,ja[5]=1,ar[5]= 2.0;/*a[3,1]= 2*/
;s26: ia[6]=2,ja[6]=2,ar[6]= 4.0;/*a[2,2]= 4*/
;1If you just need to solve LP or MIP instance, you may write it in MPS or CPLEX LP format and then use the GLPK stand-alone solver to obtain a solution. This is much less time-consuming than programming in C with GLPK API routines.
;1.0;/*a[1,1]= 1*/ 1.0;/*a[1,2]= 1*/ 1.0;/*a[1,3]= 1*/
; 11
;s27: ia[7]=3,ja[7]=2,ar[7]= 2.0;/*a[3,2]= 2*/
;s28: ia[8]=2,ja[8]=3,ar[8]= 5.0;/*a[2,3]= 5*/
;s29: ia[9]=3,ja[9]=3,ar[9]= 6.0;/*a[3,3]= 6*/
;s30:  glp_load_matrix(lp, 9, ia, ja, ar);
;; first element of each array is ignored, too weird...
(glp_load_matrix lp 9
                 (vector->cblock (vector 0 1 1 1 2 3 2 3 2 3) _int)
                 (vector->cblock (vector 0 1 2 3 1 1 2 2 3 3) _int)
                 (vector->cblock (vector 0.0
                                         1.0 1.0 1.0
                                         10.0 2.0 4.0
                                         2.0 5.0 6.0)
                                 _double))
;s31:  glp_simplex(lp, NULL);
(glp_simplex lp #f)
;s32:  z = glp_get_obj_val(lp);
(define z (glp_get_obj_val lp))
;s33:  x1 = glp_get_col_prim(lp, 1);
;s34:  x2 = glp_get_col_prim(lp, 2);
;s35:  x3 = glp_get_col_prim(lp, 3);
(define x1 (glp_get_col_prim lp 1))
(define x2 (glp_get_col_prim lp 2))
(define x3 (glp_get_col_prim lp 3))
(printf "\nz = ~v; x1 = ~v; x2 = ~v; x3 = ~v\n"
        z x1 x2 x3))

;; given a constraint, return a list of its rhs (structural) vars.
(define (constraint-rhs-vars [c : Constraint]) : (Listof Symbol)
  (map (ann second ((List Real Symbol) -> Symbol))
       (cddr c)))

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
;; 480_csc + 490_csc = 10 ;; these students need 5 sections of TE
;; 480_cpe + 490_cpe + 431_cpe = 2 ;; these need 2 sections of TE
;; 490_se  + 431_se = 2 ;; these need 2 sections of TE
;; actually just want a feasible solution...
#;(lp-solve '(0 (1 431_se)) 'max
          '((480_extra 8 (-1 480_csc) (-1 480_cpe))
            (490_extra 6 (-1 480_csc) (-1 490_cpe) (-1 490_se))
            (431_extra 2 (-1 431_cpe) (-1 431_se)))
          '((480_extra 0 posinf)
            (490_extra 0 posinf)
            (431_extra 0 posinf)
            (480_csc 0 posinf) (480_cpe 0 posinf)
            (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
            (431_cpe 0 posinf) (431_se 0 posinf)))

(check-exn #px"duplicate auxiliary variable name: '480_extra"
           (λ ()
             (lp-solve '(0 (1 431_se)) 'max
                       '((480_extra 8 (-1 480_csc) (-1 480_cpe))
                         (480_extra 6 (-1 480_csc) (-1 490_cpe) (-1 490_se))
                         (431_extra 2 (-1 431_cpe) (-1 431_se)))
                       '((480_extra 0 posinf)
                         (490_extra 0 posinf)
                         (431_extra 0 posinf)
                         (480_csc 0 posinf) (480_cpe 0 posinf)
                         (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                         (431_cpe 0 posinf) (431_se 0 posinf)))))

  (check-exn #px"auxiliary variables.*appear in the RHS of some con"
             (λ ()
               (lp-solve '(0 (1 431_se)) 'max
                         '((480_extra 8 (-1 480_csc) (-1 480_cpe))
                           (490_extra 6 (-1 480_csc) (-1 490_cpe) (-1 490_se))
                           (431_extra 2 (-1 431_cpe) (-1 431_se)
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
                         '((480_extra 8 (-1 480_csc) (-1 480_cpe))
                           (490_extra 6 (-1 480_csc) (-1 490_cpe) (-1 490_se))
                           (431_extra 2 (-1 431_cpe) (-1 431_se)))
                         '((480_extra 0 posinf)
                           (490_extra 0 posinf)
                           (480_csc 0 posinf) (480_cpe 0 posinf)
                           (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
                           (431_cpe 0 posinf) (431_se 0 posinf)))))

  (check-exn
   #px"variables.*in objective fn are not constrained"
   (λ ()
     (lp-solve '(0 (1 ZZZ)) 'max
               '((480_extra 8 (-1 480_csc) (-1 480_cpe))
                 (490_extra 6 (-1 480_csc) (-1 490_cpe) (-1 490_se))
                 (431_extra 2 (-1 431_cpe) (-1 431_se)))
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



