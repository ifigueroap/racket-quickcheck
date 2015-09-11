#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     racket/contract
                     quickcheck
		     rackunit/quickcheck))

@(define qc-eval (make-base-eval))
@(qc-eval '(require racket/list
                    quickcheck
		    rackunit/quickcheck))

@title{Quickcheck}

@author["Mike Sperber" "Ismael Figueroa"]

This manual provides documentation for a Racket implementation
of Quickcheck, a library that tests the specification of a program
with randomly generated test inputs.

The original @hyperlink["http://www.cse.chalmers.se/~rjmh/QuickCheck/" "Quickcheck"]
is a library for Haskell.

@defmodule[quickcheck]

@section{Quickstart}

@subsection{Installation}

If you have not installed the package yet, run the following command from
the command-line:

  @commandline{raco pkg install quickcheck}

Note that this package will require Racket v6.0 or newer to install.

Alternatively, you may use the GUI package manager from DrRacket to install
the package.

@subsection{Running tests}

To use Quickcheck, you first write a specification for a program as a
property in ordinary Racket code. For example, the following is a property
that expresses the English specification ``the @racket[string->number] function
produces numbers when given strings'':

@interaction[#:eval qc-eval
  (define string->number-returns-number
    (property ([str arbitrary-string])
      (number? (string->number str))))
]

Given such a property, we can run random tests using @racket[quickcheck]:

@interaction[#:eval qc-eval
  (quickcheck string->number-returns-number)
]

You may have already guessed that this property will be easily falsified, since
most strings do not actually parse as numbers.

Next, let's write a property that we expect will actually succeed. For example,
the following encodes the English specification ``given two lists of integers,
the result of @racket[append]ing them will have the same length as the sum
of the original list lengths'':

@interaction[#:eval qc-eval
  (define append-and-length-agree
    (property ([lst-1 (arbitrary-list arbitrary-integer)]
               [lst-2 (arbitrary-list arbitrary-integer)])
      (= (+ (length lst-1) (length lst-2))
         (length (append lst-1 lst-2)))))
]

Testing the property reveals that it holds up:

@interaction[#:eval qc-eval
  (quickcheck append-and-length-agree)
]

@section{Running checks}

@defproc[(quickcheck [prop testable?]) void?]{
  Test the given @racket[prop], generating new test cases if @racket[prop]
  specifies a generator.

  Prints the result of the check, including a counterexample for the property
  if one was found.

  @examples[#:eval qc-eval
    (quickcheck (property ((str arbitrary-string))
                  (string=? str (list->string (string->list str)))))
  ]
}

@defproc[(quickcheck-results [prop testable?])
         (values integer? stamps (or/c boolean? result?))]{
  Like @racket[quickcheck], except it will return three values instead:

  @itemlist[
    @item{an integer representing the number of tests run,}
    @item{a list of ...,}
    @item{either @racket[#t] if the tests were successful, @racket[#f]
          if the arguments were exhausted, or an instance of the
          @racket[result] structure type on a test failure.}
  ]

  @examples[#:eval qc-eval
    (define-values (ntests stamps ok?)
      (quickcheck-results (property ((str arbitrary-string))
                            (string=? str (list->string (string->list str))))))
    ntests
    (first stamps)
    ok?
  ]
}

@defproc[(check [config config?] [prop testable?]) void?]{
  Runs a check like @racket[quickcheck] using the given @racket[config] as
  a configuration.

  Prints the result of the check, including a counterexample for the property
  if one was found.
}

@defproc[(check-results [config config?] [prop testable?]) result?]{
  Like @racket[check], except it will return three result values instead of
  printing the results. See the @racket[quickcheck] function for an explanation
  of the return values.
}

@section{Configuration, results, and utilities}

@defstruct[config ([max-test number?]
                   [max-fail number?]
                   [size (-> integer? integer?)]
                   [print-every (-> integer? (listof any/c) any)])]{
  A structure type that represents configurations for the test process.
  An instance of this type must be supplied when calling the
  @racket[check] or @racket[check-results] functions.

  The @racket[max-test] field represents the maximum number of succeeding
  tests that will be run. The @racket[max-fail] field represents the maximum
  number of tests that can be run with no result before the checker terminates.

  The @racket[size] field should be a function of
  one argument that produces the test size given the current test
  number. The @racket[print-every] field should be a function that
  takes the test number and the generated arguments and is called for
  its side effect.

  The @racket[quickcheck] and @racket[quickcheck-results] functions use
  a default config where the test count is @racket[100], the test size
  for test @racket[n] is @racket[(+ 3 (quotient n 2))], the max fail
  count is ten times the test count, and nothing is printed. The default
  config can be adjusted to run different numbers of tests with
  @racket[with-small-test-count], @racket[with-medium-test-count], and
  @racket[with-large-test-count].
}

@defform[(with-small-test-count body ...)]{
  Within @racket[body ...], the number of test cases used by the
  @racket[quickcheck] functions is @racket[100].
  @examples[#:eval qc-eval
    (with-small-test-count
      (quickcheck (property ((str arbitrary-string))
                    (string=? str (list->string (string->list str))))))
  ]
}

@defform[(with-medium-test-count body ...)]{
  Within @racket[body ...], the number of test cases used by the
  @racket[quickcheck] functions is @racket[1000].
  @examples[#:eval qc-eval
    (with-medium-test-count
      (quickcheck (property ((str arbitrary-string))
                    (string=? str (list->string (string->list str))))))
  ]
}

@defform[(with-large-test-count body ...)]{
  Within @racket[body ...], the number of test cases used by the
  @racket[quickcheck] functions is @racket[10000].
  @examples[#:eval qc-eval
    (with-large-test-count
      (quickcheck (property ((str arbitrary-string))
                    (string=? str (list->string (string->list str))))))
  ]
}

@defform[(with-test-count test-count-expr body ...)]{
  Within @racket[body ...], the number of test cases used by teh
  @racket[quickcheck] functions is @racket[test-count-expr].
  @examples[#:eval qc-eval
    (with-test-count 42
      (quickcheck (property ((str arbitrary-string))
                    (string=? str (list->string (string->list str))))))
  ]
}

@defstruct[result ([ok (or/c null #t #f)]
                   [stamp (listof string?)]
                   [argument-list (listof any/c)])]{
  Represents a single result from a test. The @racket[ok] field is
  @racket[#t] on success, @racket[#f] on failure, and @racket[null] if
  there is no test result.

  The @racket[stamp] field represents the labels that were relevant to
  this test execution. The @racket[argument-list] is a list of the
  values generated for checking this test case.
}

@defform[(property ([id gen/arb-expr] ...) body0 body ...)
         #:contracts ([gen/arb-expr (or/c arbitrary? generator?)])]{
  Constructs a testable property for functions like @racket[quickcheck].

  The @racket[id]s are bound to the result of the given @racket[gen/arb-expr]s
  inside the body expressions. The body expressions are used as the bodies
  of a predicate function that will be run with newly generated values from the
  specified generators or arbitraries.

  @examples[#:eval qc-eval
    (property ((str arbitrary-string))
      (string=? str (list->string (string->list str))))
  ]
}

@defproc[(property? [val any/c]) boolean?]{
  Returns @racket[#t] if @racket[val] is a property for testing, otherwise
  returns @racket[#f].
}

@defproc[(testable? [val any/c]) boolean?]{
  Returns @racket[#t] if @racket[val] is a value that can be tested with
  functions like @racket[quickcheck]. Returns @racket[#f] otherwise.

  Values that can be tested are the following:

  @itemlist[
    @item{boolean values (@racket[#t] and @racket[#f]),}
    @item{instances of the @racket[result] structure type,}
    @item{instances of the @racket[property] structure type,}
    @item{or instances of the @racket[generator] structure type.}
  ]
}

@section{Integration with RackUnit}

By default @racket[quickcheck] simply displays whether the property check
results in success or, in case of failure, it prints the arguments that falsify
the property along with some other metadata. This is not helpful for (semi-)
automatic testing as it is done using RackUnit. Fortunately it is possible to
combine the best of both worlds.

@defmodule[rackunit/quickcheck]

@defform[(check-property prop)]{ Like @racket[quickcheck] but a RackUnit
exception is raised if the property fails. The exception is raised using
@racket[fail-check] and it is augmented with additional data: the number of
tests performed, the stamps, and the arguments that falsify the property.

For example, let us check the previous @racket[string->number-returns-number]
property:

@interaction[#:eval qc-eval
(check-property string->number-returns-number)
]
}

@defform[(check-property/config config prop)]{
Similar to @racket[check-property] but taking a specific @racket[config] object.
}

@defform[(add-property-check-info ((name value) ...))]{ Adds specific
@racket[check-info] data when checking a property, using either
@racket[check-property] or @racket[check-property/config]. Its usage is like
@racket[with-check-info] but it does not define a new scope. Instead, it
modifies an internal parameter that is cleared at every usage of
@racket[check-property] or @racket[check-property/config].

The purpose of this form is to improve error messages when a property
fails. For instance, to compare whether two functions are observationally
equivalent for arbitrary arguments, we can define the following form using
@racket[add-property-check-info] to state the expected and actual values inside
the declaration of a property:

@racketblock+eval[#:eval qc-eval
(define-syntax-rule (conforms-to f g ([name gen] ...))
    (property ([name gen] ...)
              (let ([expected (f name ...)]
                    [actual (g name ...)])
                (add-property-check-info (['expected expected]
                                          ['actual actual]))
                (equal? expected actual))))

(define (f n) (+ n 1))
(define (g n) (- n 1))
]

@interaction[#:eval qc-eval
(check-property (conforms-to f g ([n arbitrary-integer])))
]

Crucially, every time the property is tested with a different set of arguments
@racket[add-property-check-info] overrides the previous data.

}

@section{Generators}

@defstruct[generator ([proc (-> integer? random-generator? any/c)])]{
  Represents a source of values for randomly testing a given property
  with functions like @racket[quickcheck].

  The @racket[proc] value should be a function that accepts an integer
  representing the size of the value, a random number generator, and
  returns a value for testing.
}

@defproc[(choose-integer [lower integer?] [upper integer?])
         generator?]{
  Produces a generator that returns integers between @racket[lower] and @racket[upper].
}

@defproc[(choose-real [lower real?] [upper real?])
         generator?]{
  Produces a generator that returns real numbers between @racket[lower] and
  @racket[upper].
}

@defthing[choose-ascii-char generator?]{
  A generator that returns ASCII characters.
}

@defthing[choose-ascii-letter generator?]{
  A generator that returns ASCII letter characters.
}

@defthing[choose-printable-ascii-char generator?]{
  A generator that returns ASCII characters that have a printed
  representation.
}

@defproc[(choose-char [lower char?] [upper char?])
         generator?]{
  Produces a generator that returns characters with integer values between
  @racket[lower] and @racket[higher].
}

@defproc[(choose-list [elem-gen generator?] [size integer?])
         generator?]{
  Produces a generator that returns lists of the given @racket[size] and
  with elements generated with @racket[elem-gen].
}

@defproc[(choose-vector [elem-gen generator?] [size integer?])
         generator?]{
  Produces a generator that returns vectors of the given @racket[size] and
  with elements generated with @racket[elem-gen].
}

@defproc[(choose-string [char-gen generator?] [size integer?])
         generator?]{
  Produces a generator that returns strings of the given @racket[size] and
  with elements generated with @racket[elem-gen], which must
  generate characters.
}

@defproc[(choose-symbol [char-gen generator?] [size integer?])
         generator?]{
  Produces a generator that returns symbols of the given @racket[size] and
  with elements generated with @racket[elem-gen], which must
  generate characters.
}

@defproc[(choose-one-of [opts (listof any/c)]) generator?]{
  Produces a generator that returns one of the values in the list
  @racket[opts].
}

@defproc[(choose-mixed [promises (listof (promise/c generator?))])
         generator?]{
  Produces a generator that returns returns values from one of the generator
  promises, forcing the promise when it does so.
}

@defproc[(choose-with-frequencies [freqs (listof (cons/c integer? generator?))])
         generator?]{
  Produces a generator that returns a value from one of the given generators,
  weighing each generator by the matching integer in @racket[freqs].
}

@defproc[(generator-unit [val any/c]) generator?]{
  Produces a generator that always returns the given @racket[val].
}

@defproc[(generator-bind [gen generator?] [k (-> any/c generator?)])
         generator?]{
  Produce a generator from the result of calling @racket[k] on the value
  generated from the generator @racket[gen].
}

@defproc[(generator-sequence [gens (listof generator?)]) generator?]{
  Given the list of generators @racket[gens], produce a generator that
  produces values from them in sequence.
}

@defproc[(sized [f (-> integer? generator?)]) generator?]{
  Produces a generator from a function @racket[f] that constructs a generator
  given an integer representing a value's size.
}

@defstruct[arbitrary ([gen generator?] [trans (-> any/c generator? generator?)])]{
  Represents a source of randomly generated values, except where the values
  are filtered by the function @racket[trans] to produce a narrower set of
  new values.
}

@defthing[arbitrary-boolean arbitrary?]{
  Generates a boolean value.
}

@defthing[arbitrary-char arbitrary?]{
  Generates a character.
}

@defthing[arbitrary-ascii-char arbitrary?]{
  Generates an ASCII character.
}

@defthing[arbitrary-printable-ascii-char arbitrary?]{
  Generates a printable ASCII character.
}

@defthing[arbitrary-integer arbitrary?]{
  Generates an integer.
}

@defthing[arbitrary-natural arbitrary?]{
  Generates a non-negative integer.
}

@defthing[arbitrary-rational arbitrary?]{
  Generates a rational number.
}

@defthing[arbitrary-real arbitrary?]{
  Generates a real number.
}

@defproc[(arbitrary-mixed
          [pred+promises (listof (cons/c (-> any/c any/c) (promise/c arbitrary?)))])
         arbitrary?]{
  Produces a arbitrary generator given a list matching up predicates to promises
  that produce arbitrary generators.
}

@defproc[(arbitrary-one-of [eql? (any/c any/c -> any/c)]
                           [vals (listof any/c)])
         arbitrary?]{
  Produces an arbitrary generator that generates values from the list of values @racket[vals].
  The values are filtered by the equality function @racket[eql?].
}

@defproc[(arbitrary-pair [fst arbitrary?] [rst arbitrary?])
         arbitrary?]{
  Produces an arbitrary generator that generates pairs of values drawn from the
  @racket[fst] and @racket[rst] generators respectively.
}

@defproc[(arbitrary-list [elem arbitrary?]) arbitrary?]{
  Produces an arbitrary generator that generates lists in which the element values
  are drawn from @racket[elem].
}

@defproc[(arbitrary-vector [elem arbitrary?]) arbitrary?]{
  Produces an arbitrary generator that generates vectors in which the element values
  are drawn from @racket[elem].
}

@defproc[(arbitrary-tuple [elem arbitrary?] ...) arbitrary?]{
  Produces an arbitrary generator that generates constant-length lists in which the
  element values are drawn from the generators @racket[elem]s in order.
}

@defproc[(arbitrary-record [constructor procedure?]
                           [accessors (listof procedure?)]
                           [elem arbitrary?] ...)
         arbitrary?]{
  Produces an arbitrary generator that generates values of some structure type
  given a constructor procedure and accessor procedures for that structure type.
  The values passed to the constructor are drawn from the generators @racket[elem]s.
}

@defthing[arbitrary-string arbitrary?]{
  Generates a string.
}

@defthing[arbitrary-ascii-string arbitrary?]{
  Generates an ASCII string.
}

@defthing[arbitrary-printable-ascii-string arbitrary?]{
  Generates an ASCII string consisting of characters with printable
  representations.
}

@defthing[arbitrary-symbol arbitrary?]{
  Generates a symbol.
}

@defproc[(arbitrary-procedure [result arbitrary?] [arg arbitrary?] ...)
         arbitrary?]{
  Generates a procedure that takes arguments drawn from the generators
  @racket[arg]s and which produces a value drawn from the generator @racket[result].
}

@section{Operations on properties}

@defform[(==> bool-expr prop)]{
  Represents implication for testable properties.

  If @racket[bool-expr] is @racket[#t], equivalent to @racket[prop].
  Otherwise, produces a property that returns no result.
}

@defproc[(label [str string?] [test testable?]) generator?]{
  Labels the given @racket[test] with a @racket[str] label to help
  identify what portion of the property is failing.
}

@defform[(classify really? label-expr testable-expr)]{
  Labels the result of @racket[testable-expr] with the
  value of @racket[label-expr] if @racket[really?] evaluates to
  @racket[#t]. Otherwise just returns the result of
  @racket[testable-expr].
}

@defform[(trivial really? testable-expr)]{
  Like @racket[classify], but always uses the label @racket["trivial"].
}

@defproc[(collect [lbl any/c] [test testable?]) generator?]{
  Labels the given @racket[test] with the written form of the
  value @racket[lbl]. Similar to the @racket[label] function.
}

@section{Random number generation}

@defproc[(make-random-generator [s1 number?] [s2 number?])
         random-generator?]{
  Constructs a random number generator, given two seeds
  @racket[s1] and @racket[s2].
}

@defproc[(random-generator? [val any/c]) boolean?]{
  Returns @racket[#t] if @racket[val] is a random number generator
  constructed by @racket[make-random-generator] and returns
  @racket[#f] in all other cases.
}

@defproc[(random-generator-next [rand random-generator?])
         (values integer? random-generator?)]{
  Produce a random integer and a random number generator.
}

@defproc[(random-generator-split [rand random-generator?])
         (values random-generator? random-generator?)]{
  Splits the given random number generator and returns two
  random number generators.
}

@defproc[(random-integer [rg random-generator?]
                         [low integer?]
                         [high integer?])
         integer?]{
  Returns a random integer between the bounds @racket[low] and
  @racket[high].
}

@defproc[(random-real [rg random-generator?]
                      [low real?]
                      [high real?])
         real?]{
  Returns a random real number between the bounds @racket[low] and
  @racket[high].
}
