#lang scribble/manual

@title[#:style (with-html5 manual-doc-style)]{GLPK: The Gnu Linear Programming Kit}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require scribble-math/dollar)

@(require (for-label racket "main.rkt"))

This collection provides a simple racket interface to the
@link["https://www.gnu.org/software/glpk/"]{Gnu Linear Programming Kit},
by Andrew O. Makhorin,
allowing you to solve linear optimization problems.

This package does not include that library; you'll need to install it yourself,
using the package manager of your choice, or by building it from
source.

Note that you may need to configure Racket's search path to allow
it to find the installed library; search the raco documentation
for @racket['lib-search-dirs].

The GLPK library comes with many bells and whistles, including dual
simplex, Mixed Integer Programming (MIP), and other related problems.
Right now, this library does not support any of those bells and whistles;
it just allows basic simplex solving.

It's worth mentioning that GLPK does have one "interesting" design
choice; when you supply arguments to GLPK that don't make sense—for
instance, trying to supply a name for a column that doesn't exist—it
simply prints a message to @tt{stderr} and then
calls @tt{exit(1)}. That is: your racket (or DrRacket)
process will simply halt. This is not the behavior I think I would
have chosen, but we're stuck with it.
I believe I have designed and implemented the
@racket[lp-solve] function in such a way that this should not
be possible. Nevertheless, it's something that users of the library
should be aware of.

@section{The Linear Programming problem}

The linear programming problem can be formulated as follows: given a
set of constraints over a set of variables, and a function to be maximized
(or minimized), find the set of values for the variables that maximize
(or minimize) the function.

Okay, so what kind of constraints are possible? Well, each constraint
consists of a single equality, of the form

@$${a = \sum_{i} K_i x_i}

... where @${a} is what's called an "auxiliary" variable. These
auxiliary variables must occur only once each, on the left-hand
side of the corresponding constraint.

The other variables, the @${x_i}, are "structural" variables,
or "problem" variables, corresponding to unknown quantities in the
problem statement.

The objective function is a linear combination of structural variables.
It may be either maximized or minimized, as you like.

Along with these constraints, each variable, both structural and
auxiliary, comes with a pair of (possibly infinite) bounds. So,
for instance, you can specify that auxiliary variable @${b} ranges
between @${-10} and @${143.2}.

An example appears below.

@defmodule[glpk]

@defproc[(lp-solve [objective objective?] [direction (or/c 'max 'min)]
                   [constraints (listof constraint?)]
                   [bounds (listof bound?)])
         (or/c (list/c flonum? (listof (list/c symbol? flonum?)))
               symbol?)]{
 Solves a linear programming problem.

 Both the objective and the constraints make use of a "linear combination"
 form:

 @racketblock[
lin-comb? = (listof (list/c symbol? real?))]

 ... representing a linear combination of structural variables.

 The objective function includes a constant term and a linear
  combination of structural variables:

  @racketblock[
objective? = (pair/c real? lin-comb?)]

  The constraints each include the name of an auxiliary variable
  and a linear combination of structural variables:

  @racketblock[
constraint? = (pair/c symbol? lin-comb?)]

  Finally, the set of bounds provides bounds for both the auxiliary
  and structural variables. Each bound contains the name of a variable,
  and a low and high boundary. The low boundary can be @racket['neginf],
  indicating no lower bound, and the high boundary can be
  @racket['posinf], indicating no upper bound. The lower and upper
  bound can be equal, indicating that the corresponding variable is
  fixed.

  You must provide bounds for every auxiliary and structural variable.

  @racketblock[
 bound?    = (list/c symbol? lo-bound? hi-bound?)
 lo-bound? = (or/c 'neginf real?)
 hi-bound? = (or/c 'posinf real?)]

  The result is a list containing the maximal (or minimal) value of
  the objective function, along with a list of lists mapping structural
  variables to the values that produce that optimal value, unless
  no solution is possible, in which case a symbol representing
  an error code is returned.

  @codeblock|{
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
}|

  Yikes! Let's see an example.

  Okay, let's say you're trying to figure out whether you have
  enough food for your picnic. In particular, you're buying
  hamburgers, slices of bread, and pickles. You have three kinds
  of guests: Children, Adults, and Chickens.

  Each adult wants one slice of bread, a patty, and two pickles.
  Each child wants two slices of bread, and a patty. Each chicken
  wants *either* one slice of bread, one patty, or one pickle.

  So, if we have 30 slices of bread, 20 patties, and 50 pickles,
  how many guests can we invite?

  Let's make up some variables. We'll use @${k} for the number
  of children, @${a} for the number of adults, @${c_b}
  for the chickens eating a slice of bread, @${c_p} for the
  chickens eating a patty, and @${c_k} for the chickens eating
  a pickle. For our auxiliary variables, we'll use @${f_b}
  for the number of slices of bread eaten, @${f_p} for the number
  of patties eaten, and @${f_k} for the number of pickles eaten.

  @$${f_b = a + 2k + c_b }
  @$${f_p = a + k + c_p}
  @$${f_k = 2a + c_k}

  Here are the bounds we'll use:
  
  @$${0 < f_b < 30}
  @$${0 < f_p < 20}
  @$${0 < f_k < 50}
  @$${0 < a}
  @$${0 < k}
  @$${0 < c_b}
  @$${0 < c_p}
  @$${0 < c_k}


  Finally, our objective function: maximize @${f}:

  @$${
f = a + k + c_b + c_p + c_k
  }

  Here's the whole thing as a call to lp_solve:
  
@codeblock|{
(lp-solve
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
 }|

You may not be surprised by the result:

@code|{'(100.0 ((a 0.0) (k 0.0) (cb 30.0) (cp 20.0) (ck 50.0)))}|

In other words, you can invite a hundred guests, if they're all
chickens.  Well, that's fine, but what if we decide that kids are
the best (worth 10), adults are second best (worth 8), and chickens
are only worth 1/2?

Here's the call:

@codeblock|{
(lp-solve
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
}|

... and the result:

@code|{'(195.0 ((a 10.0) (k 10.0) (cb 0.0) (cp 0.0) (ck 30.0)))}|

In other words: 10 adults, 10 kinds, and a pile of chickens to
vacuum up the leftover pickles.

We can add arbitrary further constraints on this: each chicken
must be chaperoned by an adult, each chicken must be chaperoned
by an adult, no adult can chaperone both a child and a chicken,
etc. etc.

Nifty!

  
}
