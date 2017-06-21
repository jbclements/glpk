#lang racket

(require "lib.rkt"
         ffi/unsafe)

(glp_term_out 'GLP_OFF)
#|/* sample.c */
#include <stdio.h>
#include <stdlib.h>
#include <glpk.h>
int main(void)
{     glp_prob *lp;
      int ia[1+1000], ja[1+1000];
      double ar[1+1000], z, x1, x2, x3;
s1:   lp = glp_create_prob();
|#
(define lp (glp_create_prob))
;s2:   glp_set_prob_name(lp, "sample");
(glp_set_prob_name lp "sample")
;s3:   glp_set_obj_dir(lp, GLP_MAX);
(glp_set_obj_dir lp 'GLP_MAX)
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
        z x1 x2 x3)
;s36:  printf("\nz = %g; x1 = %g; x2 = %g; x3 = %g\n",
;         z, x1, x2, x3);
;s37:  glp_delete_prob(lp);
;return 0;
;}
;/* eof */
