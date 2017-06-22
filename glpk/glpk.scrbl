#lang scribble/manual

@title{GLPK: The Gnu Linear Programming Kit}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@;{(require (for-label racket
                     "cmd-line.rkt"
                     "char-model.rkt"
                     "example-model.rkt"))}

This collection provides a simple racket interface to the
Gnu Linear Programming Kit (@link["FIXME"]{FIXME}), allowing
you to solve linear optimization problems.

It does not include that library; you'll need to install it yourself,
using the package manager of your choice, or by building it from
source.

The GLPK library comes with many bells and whistles, including dual
simplex, Mixed Integer Programming (MIP), and other related problems.
Right now, this library does not support any of those bells and whistles;
it just allows basic simplex solving.

@section{The Linear Programming problem}

The linear programming problem can be formulated as follows: given a
set of constraints over a set of variables, and a function to be maximized
(or minimized), find the set of values for the variables that maximize
(or minimize) the function.

Okay, so what kind of constraints are possible? Well, 

@verbatim{
a = Ax_1 + Bx_2 + ...
b = Cx_1 + 

}


This collection produces passwords using models built on a corpus
of source text. It manages to guarantee generation of passwords
with known entropy by building markov models whose transitions are
guided by huffman trees, allowing the use of variable numbers of
bits of entropy for each transition.

To run it, use

@commandline{raco molis-hai}

It also has a number of command-line arguments that you can use to control
the password generation process.

@section{Source Text}

You can specify the source text using the @tt{-t} flag. A megabyte of text
is a reasonable size. I've included the text of Dickens' @emph{A Tale of
Two Cities}, and this is used by default. Extremely long source texts will
take a while to build models from, and extremely short source texts (e.g.,
@racket["banana"] will work, but will generate extremely long passwords.

@section{Order}

The tool builds a markov model mapping character-based n-grams to possible next
letters. By default, it uses order 2. That is, the model will take a look at
every instance of @racket["pr"], for instance, and see what could follow it.

You can also specify a larger order. If you specify order 3, then it will
build its model using strings of length 3 as states; in this case, it would
compute, e.g., what follows @racket["pro"] (and every other 3-letter string that occurs
in the source text).

Larger models produce passwords that are more characteristic of the source
text. Surprisingly, this isn't usually such a great thing. Here are some 56-bit
passwords of order 2:

@verbatim{
Yess ott was whoc
ming to mys, ale, and ar
and the factinand cale
toner gre,' Throur and w
dis whishe coullows ing
lationsir.' Arge,--l
inesperept his calty 
he led, againeved--th
}

And here are some 56-bit passwords of order 8:

@verbatim{
took horse for Dover; and then stood smoking, too! replied: My child, this world 
out of these footsteps destination and by the wall where he
and alluvial mud, under such thought madame, drawing themselves through her f
by Lucie at her sole reliance, under a bank overhanging about. There, said he, i
in such differed principle. And look at, am I? Why don't you be afraid of, for he stooped, and leth
the nose became much more wicked foreign woman; what plate as to matter. But when I ask your
did a great Stilton cheese, until to-morrow. For to-morrow! Now, a judiciously sensibility she wound about he
young ladies made to stand up against, and notice, when two dusty men passionate, loving, thankful that if s
}

The second set are clearly more Dickensian... but would make terrible passwords! I mean, yes,
you'd probably be the only girl on the block to have "Stilton cheese, until tomorrow" in your
password, but at the end of the day, I bet you'd be happier typing "Yess ott was whoc" when you
lift the lid on your laptop.

In fact, some people might even prefer the order-1 passwords, which look like this (again, 56-bit):

@verbatim{
arither tshelones,
a ffty,'l g wed
maged he tof hed ored
arerer pen le wagh
hemishemat. t wher
I engutsir.'sin
s ton toticould the
waped t If h inge t
}

Well, maybe not. Order-2 seems like a nice compromise.

@section{Entropy}

The generator uses a list of booleans to generate a password. Each character's generation consumes zero
or more booleans from this list. There is a bijection between the set of booleans of some length @racket[n]
and the set of passwords generated (assuming a fixed order and source text).

These booleans are generated by drawing randomness from @filepath{/dev/urandom} (sorry, Windows folks, tell
me how to get random bits on your platform).

What this means is that passwords generated by this system are guaranteed to have the required entropy.
That is, if I generate a 56-bit password this system, it's guaranteed to have 56 bits of entropy, even
though the attacker knows your source text and what order you've chosen.

For more about security and how it all works, take a look at my preprint on @link["http://arxiv.org/abs/1502.07786"]{ArXiv}.

@section{Performance}

Currently, the tool simply builds the model every time it's called. This may
seem inefficient, but the fact is that for order-2 models on 1-megabyte
texts, it only takes a few seconds on my machine.

Larger models take longer to build, but they're also huge, and would take up
lots of space on disk, and aren't really very useful (as noted above).
If there's really any demand for it, it wouldn't be
hard to add a means to compile source texts for fast password generation.
Finally, this tool usually isn't something that you run every day; waiting
30 seconds for password generation probably isn't a big deal.

@section{Security}

These passwords are as secure as the bits that come from @filepath{/dev/urandom}. In fact,
there's a bijection between them.

@section{Other Stuff}

The library contains lots of other stuff that I explored in the process of creating
this; there's some code for extracting the text of e-mails, and for html processing,
and for word-based generation, etc. etc. etc. Hopefully, it's all fairly easy to read.

@section{Using molis hai as a Library}

Naturally, you can also call this code as a racket library.

@section{Character Model}

@defmodule[molis-hai/char-model]{
The character model is the ordinary mode of operation for molis hai. Specifically, a character
model maps bits to password, and is built from the character n-grams from a specified source.
}

@defproc[(build-char-model [order natural?] [text string?]) model?]{
Given an order and a text, builds a character model.
}

@defproc[(generate-char-pwd [model model?] [bits (listof boolean?)]) string?]{
Given a model and a list of bits, return the corresponding password.
}

@section{Example Model}

@defmodule[molis-hai/example-model]{
Defines a few base models to use if you don't want to build your own.
}

@defthing[atotc-model model?]{
A character model built from Dickens' @emph{A Tale Of Two Cities}, using order 2.
}

@defthing[base-model model?]{
A character model of order 0, using a small fixed set of characters. This is essentially
the same as choosing a password using a sequence of random characters; there's nothing
innovative about this model, but it's good for purposes of comparison.
}

