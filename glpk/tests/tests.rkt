#lang typed/racket

(module+ test
  (require glpk
           typed/rackunit)
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
                 (431_cpe 0 posinf) (431_se 0 posinf)))))

  (require glpk
         typed/rackunit)
  
;; simple infeasible test
(check-equal?
 (lp-solve
  '(0 (1 x))
  'max
  '((sum (1 x) (1 y)))
  '((sum 0 1)
    (x 5 posinf)
    (y 7 posinf)))
 '(bad-status GLP_NOFEAS)))

