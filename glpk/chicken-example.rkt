#lang racket

(require glpk)

#;(lp-solve
 '(0 (1 a) (1 k) (1 cb) (1 cp) (1 ck))
 'max
 '((fb (1 a) (2 k) (1 cb))
   (fp (1 a) (1 k) (1 cp))
   (fk (2 a) (1 ck)))
 '((fb 0 30)
   (fp 0 20)
   (fk 0 50)
   (a 0 posinf)
   (k 0 posinf)
   (cb 0 posinf)
   (cp 0 posinf)
   (ck 0 posinf)))

#;(lp-solve
 '(0 (8 a) (10 k) (1/2 cb) (1/2 cp) (1/2 ck))
 'max
 '((fb (1 a) (2 k) (1 cb))
   (fp (1 a) (1 k) (1 cp))
   (fk (2 a) (1 ck)))
 '((fb 0 30)
   (fp 0 20)
   (fk 0 50)
   (a 0 posinf)
   (k 0 posinf)
   (cb 0 posinf)
   (cp 0 posinf)
   (ck 0 posinf)))

#;(lp-solve
 '(0 (8 a) (10 k) (2 cb) (2 cp) (2 ck))
 'max
 '((fb (1 a) (2 k) (1 cb))
   (fp (1 a) (1 k) (1 cp))
   (fk (2 a) (1 ck))
   (z (-1 a) (1 ak) (1 ac))
   (excessak (1 ak) (-1 k))
   (excessac (1 ac) (-1 cb) (-1 cp) (-1 ck)))
 '((z 0 0)
   (ak 0 posinf)
   (ac 0 posinf)
   (excessak 0 posinf)
   (excessac 0 posinf)
   (fb 0 30)
   (fp 0 20)
   (fk 0 50)
   (a 0 posinf)
   (k 0 posinf)
   (cb 0 posinf)
   (cp 0 posinf)
   (ck 0 posinf)))
