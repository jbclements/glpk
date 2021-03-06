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

Here's a list, taken from the GLPK documentation:

@itemlist[
@item{    primal and dual simplex methods}
@item{    primal-dual interior-point method}
@item{    branch-and-cut method}
@item{    translator for GNU MathProg} ]

Right now, this library supports two of these modes: basic primal simplex
solving, and mixed integer programming (MIP) using the branch-and-cut method.
It's not hard to extend this library on an as-needed basis, to support more
of GLPK's functionality.

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
should be aware of. [See section @secref["err"]

@section{Typed Racket}

The outward-facing modules of this library are written using Typed
Racket, and should play nicely with the rest of your Typed Racket code.

@section{The Linear Programming problem}

The linear programming problem can be formulated as follows: given a
set of linear constraints over a set of variables, and a function to be maximized
(or minimized), find the set of values for the variables that maximize
(or minimize) the function.


Okay, so what kind of constraints are possible? Well, each constraint
consists of a single equality, of the form

@$${a = \sum_{i} K_i x_i}

... where the @${K_i} are real numbers, and
@${a} is what's called an "auxiliary" variable. These
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

@defproc[(lp-solve [objective objective?]
                   [direction (or/c 'max 'min)]
                   [constraints (listof constraint?)]
                   [bounds (listof bound?)]
                   [#:terminal-output terminal-output? boolean?])
         (list/c symbol?
                 (or/c symbol? false?)
                 (list/c flonum? (listof (list/c symbol? flonum?))))]{
 Solves a linear programming problem.

 Both the objective and the constraints make use of a "linear combination"
 form:

 @racketblock[
lin-comb? = (listof (list/c real? symbol?))]

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

  You may specify @racket[#:terminal-output] as @racket[true] to obtain
  output on (yes actually) stdout. This is included for completeness, but
  is generally more useful in mixed integer programming, described below.

 The result is a list containing a symbol that indicates either success
 (@racket['good]) or one of two kinds of failure, and then a symbol which
 in case of failure conveys information about the nature of the failure,
 and then either a solution or false. The reason for this "flattened"
 structure which can carry both failure and success simultaneously is
 that for certain kinds of failure in MIP (see below), we wish to provide
 both failure and a best-so-far solution.

 A solution is represented as a list containing the optimal value of
 the objective function, along with a list of lists mapping structural
 variables to the values that produce that optimal value.

 When the first symbol in the result is @racket['bad-result], the
 second element is a @racket[FailCode] (definition below). When the first symbol
 in the result is @racket['bad-status], the second element is a
 @racket[SolutionStatus] (also defined below).

  Yikes! Let's see an example.

  @subsection{An Example}

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

@code|{'(good #f (100.0 ((a 0.0) (k 0.0) (cb 30.0) (cp 20.0) (ck 50.0))))}|

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

@code|{'(good #f (195.0 ((a 10.0) (k 10.0) (cb 0.0) (cp 0.0) (ck 30.0))))}|

In other words: 10 adults, 10 kids, and a pile of chickens to
vacuum up the leftover pickles.

We can add arbitrary further constraints on this: each chicken
must be chaperoned by an adult, each chicken must be chaperoned
by an adult, no adult can chaperone both a child and a chicken.

To model this, we divide adults into adults chaperoning kinds
(@racket[ak]) and adults chaperoning chickens (@racket[ac]).
We could replace @racket[a] entirely, but it's easier just
to require that @racket[a] is the sum of @racket[ac] and
@racket[ak]. Also, let's bump up the desirability of chickens,
just to get a more interesting result:

@codeblock|{
(lp-solve
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
   (ck 0 posinf)))}|

The result:

@codeblock|{
'(good
  #f
  (200.0
   ((a 20.0)
    (k 0.0)
    (cb 10.000000000000002)
    (cp 0.0)
    (ck 9.999999999999998)
    (ak 0.0)
    (ac 20.0))))}|

That is: 20 adults, all chaperoning chickens. 10 of
the chickens get bread, 10 of the chickens get pickles.

Nifty!


}

@defproc[(mip-solve [objective objective?]
                    [direction (or/c 'max 'min)]
                    [constraints (listof constraint?)]
                    [bounds (listof bound?)]
                    [integer-vars (listof symbol?)]
                    [#:terminal-output terminal-output? boolean?]
                    [#:time-limit time-limit (or/c false? natural?)])
         (or/c (list/c symbol?
                       (or/c symbol? #f)
                       (or/c (list/c flonum? (listof (list/c symbol? flonum?)))
                             #f)))]{

Performs mixed-integer programming.

The Mixed-Integer-Programming solver is an extension of the linear programming
solver, and the problems that it solves are an extension of linear programming problems.
Specifically, in a mixed integer programming problem, some of the solution variables
can be labeled as integer variables, whose values must be integers.

This is indicated in the interface using an additional input, a list of structural
variables whose values must be integers.

Mixed integer programming is ... well, a lot harder than simple linear programming.
In fact, the first step in MIP is to solve the corresponding linear programming problem,
where the variables are all allowed to take on non-integer variables. The branch-and-cut
algorithm then attempts to find a related solution where the specified structural
variables have integer values.

This problem is NP-hard, so ... it can take a while. This motivates the addition of three
interface elements. First, the @racket[#:time-limit] argument accepts a number of milliseconds,
indicating how long to run before giving up. Second (making the previous one useful), the
result of a timeout includes a "best solution so far" along with the @racket['GLP_ETMLIM]
failure result. Third, the @racket[#:terminal-output] option is genuinely useful in the
case of mixed integer programming, because it provides information every few seconds (ten?)
on the progress of the search; the gap between the best known solution and the best solution
that might be possible, the number of possibilities left to explore, etc.

Unfortunately, the GLPK library is configured by default to provide this output on stdout,
which isn't terribly natural for a DrRacket program, since the output appears on the terminal
that you started from DrRacket... if you actually started DrRacket from a terminal at all.

Fortunately, GLPK provides @racket{glp_term_hook()} for exactly this purpose, so you can
redirect output to a more sane location.

Unfortunately, I haven't implemented support for this. Sorry!

In fact, I found that I've moved toward running my GLPK MIP problems more or less exclusively
in the terminal, both in order to see the output and also because interrupting the search
usually involves some rather violent process actions (control-backslash, for instance).

It would also be great to provide support for the various tuning options for the integer
solver; I haven't done this, but it would be less than an hour's work; if you need any of
these, let me know and I'd be happy to add them.

But wait... let's have an example.

@subsection{An Integer Example}

Here's a simple example: we need at least 4.5 sections of csc101 and 202. We have two instructors,
smith and martinez, each of whom can teach 9 sections. Can we staff both of our classes? Yes.

Here's the call. We're trying to maximize their unused sections:

@codeblock|{
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
   }|

Here's the solution that GLPK comes up with (one of many possible equivalent
ones:

@codeblock|{
'(good
     #f
     (8.0
      ((smith-csc101 4.0)
       (martinez-csc101 1.0)
       (smith-csc202 5.0)
       (martinez-csc202 0.0)
       (smith-extra 0.0)
       (martinez-extra 8.0))))
}|

}

@defidform[#:kind "syntax" FailCode]{
  A code that indicates the nature of a failure. Defined as follows:

@codeblock|{
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

}

@defidform[SolutionStatus]{
 A symbol that indicates the status of the solution. Defined as follows:
                          
@codeblock|{
(define-type SolutionStatus
  (U 'GLP_UNDEF
     'GLP_FEAS
     'GLP_INFEAS
     'GLP_NOFEAS
     'GLP_OPT
     'GLP_UNBND))}|


}


@section[#:tag "err"]{GLPK Error Handling}

EDIT: I'm now aware that GLPK includes a @tt{glp_error_hook} function
that allows the installation of a hook function that is called before
the library halts execution; the documentation suggests the use of
@tt{setjmp/longjmp} to escape if a user wishes not to exit. 

If I understand the internals of Racket correctly, making use of this
would require separately compiling a C stub that establishes a jump
buffer and uses setjmp before calling into each GLPK library function.
This stub would have to be compiled for every platform separately,
and I frankly don't have enough interest to implement something
like this now.... Sad face.
