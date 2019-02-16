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
   (lp-solve '(0) 'max
             '((480_total (1 480_csc) (1 480_cpe))
               (490_total (1 490_csc) (1 490_cpe) (1 490_se))
               (431_total (1 431_cpe) (1 431_se))
               (csc_tes (1 480_csc) (1 490_csc))
               (cpe_tes (1 480_cpe) (1 490_cpe) (1 431_cpe))
               (se_tes (1 490_se) (1 431_se)))
             '((480_total 0 8)
               (490_total 0 6)
               (431_total 0 2)
               (csc_tes 10 10)
               (cpe_tes 2 2)
               (se_tes 2 2)
               (480_csc 0 posinf) (480_cpe 0 posinf)
               (490_csc 0 posinf) (490_cpe 0 posinf) (490_se 0 posinf)
               (431_cpe 0 posinf) (431_se 0 posinf)))
   ;; this is the one generated, hand-checked that it's feasible:
   '(good
     #f
     (0.0
      ((480_csc 8.0)
       (480_cpe 0.0)
       (490_csc 2.0)
       (490_cpe 2.0)
       (490_se 2.0)
       (431_cpe 0.0)
       (431_se 0.0)))))

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
   #px"variables.*in objective fn are not structural"
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
 '(bad-status GLP_NOFEAS #f))

(define (simple-sum [clause : (List Symbol (Listof Symbol))]) : Constraint
  (match clause
    [(list tgt sum-of)
     (cons tgt (map (λ ([var : Symbol]) ;; : (List Real Symbol)
                      (list 1 var))
                    sum-of))]))

(define (simple-sums [clauses : (Listof (List Symbol (Listof Symbol)))])
  (map simple-sum clauses))

(check-equal?
 (simple-sums
  '((csc101-offered (bogus1-csc101 bogus2-csc101))))
 '((csc101-offered (1 bogus1-csc101) (1 bogus2-csc101))))
  
;; bogus1-csc101 + bogus2-csc101 > 4.5
;; bogus1-csc202 + bogus2-csc202 > 4.5
;; bogus1-csc101 + bogus1-csc202 = 4
;; bogus2-csc101 + bogus2-csc202 = 5
(check-equal?
 (mip-solve '(0) 'max
            (simple-sums
             '((csc101-offered (bogus1-csc101 bogus2-csc101))
               (csc202-offered (bogus1-csc202 bogus2-csc202))
               (bogus1-secns (bogus1-csc101 bogus1-csc202))
               (bogus2-secns (bogus2-csc101 bogus2-csc202))))
            '((csc101-offered 4.5 posinf)
              (csc202-offered 4.5 posinf)
              (bogus1-secns 4 4)
              (bogus2-secns 5 5)
              (bogus1-csc101 0 posinf)
              (bogus1-csc202 0 posinf)
              (bogus2-csc101 0 posinf)
              (bogus2-csc202 0 posinf))
            '(bogus1-csc101 bogus1-csc202
                            bogus2-csc101 bogus2-csc202))
 '(bad-result GLP_ENOPFS #f))

  
  (check-equal?
   (mip-solve '(0 (1 smith-extra) (1 martinez-extra)) 'max
              '((csc101-offered (1 smith-csc101) (1 martinez-csc101))
                (csc202-offered (1 smith-csc202) (1 martinez-csc202))
                (smith-secns (1 smith-csc101)
                             (1 smith-csc202)
                             (1 smith-extra))
                (martinez-secns (1 martinez-csc101)
                                (1 martinez-csc202)
                                (1 martinez-extra)))
              '((csc101-offered 4.5 posinf)
                (csc202-offered 4.5 posinf)
                (smith-secns 9 9)
                (martinez-secns 9 9)
                (smith-csc101 0 posinf)
                (smith-csc202 0 posinf)
                (smith-extra 0 posinf)
                (martinez-csc101 0 posinf)
                (martinez-csc202 0 posinf)
                (martinez-extra 0 posinf))
              '(smith-csc101 smith-csc202
                             martinez-csc101 martinez-csc202))
   ;; regression, there are many correct answers:
   '(good
     #f
     (8.0
      ((smith-csc101 4.0)
       (martinez-csc101 1.0)
       (smith-csc202 5.0)
       (martinez-csc202 0.0)
       (smith-extra 0.0)
       (martinez-extra 8.0)))))


  )

