module Data.Article exposing (text)


text =
    """
[image "caption:Camperdown Elm Tree, Prospect Park Brooklyn" https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg]

# Fault-Tolerant Parsing

The goal of this article is to explain  how one can implement a fault-tolerant parser for a simple markup language which we will call [b L1].
To put the notion of fault-tolerance in context, recall that the task of a  parser is to read source text in some language, then convert it to an abstract syntax tree (AST).  Classical parsers act like a dumb pipe, consuming the source text and producing the AST in one go, but bailing out when an error is encountered. Such behavior is not suited for interactive language editors, be they for programming languages or markup languages.  In an interactive environoment, the source text will pass in and out of error states, potentially  on each keystroke. A fault-tolerant parser should in all cases return an abstract syntax tree that makes sense.  This means that (a) most of the text is parsed as one expects, e.g., a new error at the halfway point does not necessarily invalidate the latter half which had already been correctly parsed in previous edits; (b) error text is converted into a valid node of the AST which displays the text in question, signals that it is an error, and gives some insight into the nature of the error.


The strategy for fault-tolerant parsing discussed here is based
on (1) Matt Griffith's project
[link "mdgriffith/elm-markup" https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/],
in which he introduced the notion of [i TextCursor] and (2)
the domain-specific parser developed by the team at
 [link "Brillant.org" https://brilliant.org] led by Rob Simmons.

The Camperdown parser can be configured for applications ranging from Markdown-style languages to a kind of mini-LaTeX to interactive story-telling (see XXX).  The aim here is to present the main ideas of Camperdown in a simple yet nontrivial context that will be helpful both on its own and as a warmup to understanding and using Camperdown itself. The  codebase for [b L1] is small, with the core `textCursor` module, the largest of the bunch,  weighing in at just over 200 lines of code. Here are a few sentences in [b L1]:

: (a) `This [highlight is [b not] a very good] test.`

: (b) `Pythagoras said that $a^2 + b^2 = c^2. Wow! What a dude!!`


These are rendered

:(a) This [highlight is [b not] a very good] test.

:(b) Pythagoras said that $a^2 + b^2 = c^2. Wow! What a dude!!


## The Main Ideas

The two strategies that go into designing a fault-tolerant parser [i isolation] and the use a [i TextCursor] to scan the source text in such a way even if errors are detected, a valid AST can be constructed.

### Isolation

A first strategy is to divide the document into pieces in some way, e.g. A, B, C, D, ... that can be parsed separately.  Then an error in B, for example, will not affect the parsing of the subsequent parts C, D, etc.  This strategy has another advantage.  If the user edits part B, only that part need be re-parsed and re-rendered.  This kind of partitioning of the work provides large speed improvements.  The goal is always the same — instant feedback for the user.

For [b L1], we use an especially simple algorithm for partitioning the text: pieces are contiguous set of lines separated by two or more newlines.  One can imagine more complex schemes, but this one will do here.



### Text Cursor

To explain the  scanning/cursor idea, let us consider the two snippets

:(a) `This [i is] a [b real] test!`

and

:(b) `This [i is a [b real] test!`

They are rendered as

:(a) This [i is] a [b real] test!

and

:(b) This [i is a [b real] test

In the second case the error is called out;  moreover, the word [i real] is
still rendered in bold.  Ergo, the parser was able to continue after the error, providing us with as good  a result as can be expected.

### The Inner Workings (Happy Path)

To understand the scan/parse process, we consider an isomorphic but simpler example, the string `a [x b] [y c] d`.  As we scan from left to right, with the scannner head represented by `^`, we try to recognize and parse as man subunits as possible, keeping them ready in case they are needed to fit all the pieces of the puzzle together at a later time.  The first line in the image below is the starting state, with the pointer to the left of the data.  Unprocessed data is highlighted in green.  At step (1), the pointer moves to the right to just before the left bracket, which is one of the two delimiters in [b L1], The text `a` is saved in `cursor.text`, which is highlighted in yellow.  At step (2) the scanner has encountered an opening for an [b L1] element.  It knows that `a` is completely processed; it is therefore saved in `cursor.complete`, which is highlighted in blue.  At the same time, the  left bracket, which is in an unfulfilled state, is pushed onto the stack (magenta) for later use.

[image https://noteimages.s3.amazonaws.com/parser-L1-1.png]


At step (3), the pointer moves to the next delimiter, placing the intervening text in the `.content` field of the item on top of the stack.  At step (4) something new happens.  The parser recognizes the right bracket which can close the data on the top of the stack. It appends the right bracket to the content of the top of the stack, parses it, and pushes it the list `cursor.parsed` (cyan).  This is the POP operation.  In step (5), a blank space is moved into `cursor.text`.  Not so interesting.

The next move of the pointer brings us to an opening bracket.  We know what to do. PUSH in step (6), then ADD text in (7), and finally POP once again in (8). Note that the function of ADD depends on whether the stack is empty or not.  Thus in (9), unprocessed text is transferred to `cursor.text`.

-

The stack in now empty, indicating no errors.
There is one last operation, COMMIT.  In the the case of an empty stack, its role is to put the various parts of the cursor in the correct order —

: `cursor.complete ++ cursor.parsed ++ (parse cursor.text)`

The very last element is a parsed version of `cursor.text`, and `++` is concatentation of lists.



### The Inner Workings (Unhappy Path)

Of course, it may happen that there are errors, as in the text

: `a [x b [y c] d`

The key fact is that the opening bracket of `[x b` was never closed.  There are multiple solutions to the problem of turnng this into valid text, among which are `a [x] b [y c] d` and `a [x b] [y c] d`.  We have to pick a solution and a general rule for finding it.  Fortunately, the parser has collected enough information to do this. As it enters the commit phase, it knows the initial position of the never-closed  data `[x b`.  It can therefore assemble the following:

: `c.complete ++ c.parsed ++ ((c.stack)) ++ (parse c.text)`

where `((c.stack))` means [i make a  valid element whose text comes from] `c.stack`.  The image below shows what the parser knows as it enters the commit phase.

[image https://noteimages.s3.amazonaws.com/parser-L1-2.png]




## References

1. [link "Matt Griffith, elm-markup" https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/)]

2. [link "Discussion on Elm discourse" https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262]

3. [link "Error recovery with parser combinators"  "https://eyalkalderon.com/blog/nom-error-recovery/"]

"""
