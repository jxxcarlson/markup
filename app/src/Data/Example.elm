module Data.Example exposing (text)


text =
    """


# The L1 Markup Language

[i by James Carlson, jxxcarlson@gmail.com ]


[b L1] is a markup language with a syntax somewhat like Lisp, but with square brackets instead of parentheses. It is similar to and linguistically a descendant of [link "CaYaTeX" https:jxxcarlson.github.io/app/cayatex], work of this author and Nicholas Yang, but with more robust error-handling abilities.

In L1, we say `[b bold text]` to make bold text, and for italic, we say `[i italic text]`. Elements can be nested as in `[i italic text is very [b bold]]`, which renders as

:[i italic text is very [b bold]]

The idea is a that the first part `f` of the expression ` [f a b c ...]` is a function,  `a`, `b`, `c`, ... are its arguments, and the expression itself is a function application.  An element like `italic` in `[i italic text]` auto-evaluates to `italic`, that is, it is self-quoted.

| heading2 Conveniences

In addition to the basic syntax, there are conveniences for common constructs.
Titles and section headings, for example, can be done as in Markdown:

` # The L1 Markup Language`

Such a heading must be preceded and followed by a blank line.

Conveniences can always be written in the standard way, e.g., `[heading1 The L1 Markup Language]`.

| heading3 Items


Items are a convenience used to indent text:

`:This is a test. [red I repeat: a test!].`

which renders as

:This is a test. [red I repeat: a test!].

Items must be preceded and followed by a blank line.  The colon symbol must be in first position.

| heading3 Blocks

A [i block element] is an alternate way of writing an element without having to worry about it being closed by a right bracket `]`.  However, this method has much better error-handling properties.
Here is an indentation block:

|| codeblock
| indent
This is a test.
I repeat: [i a test!]

It is rendered as below:

| indent
This is a test.
I repeat: [i a test!]

Note the pipe symbol `|` in first position, that is, at the left margin.  Because the pipe symbol cannot start a block element unless it is first position,  one can still say things like `a = (b|c)`. A  block consists of its first line, which names the block, and its body, which consists of non-blank lines followed by a blank line. The first line always has form `|name` with no space between `|` and `name.`

### Verbatim blocks

Below is a block for displayed math text.  Note the double pipe symbol.  It is used for [i verbatim]
blocks.  These are blocks whose body is not parsed, i.e., are passed on directly to the renderer.

|| codeblock
|| mathblock
\\int_0^1 x^n dx
  =
\\frac{1}{n+1}

It is rendered as

|| mathblock
\\int_0^1 x^n dx
 =
\\frac{1}{n+1}


For inline mathematics, one still has the familiar `$a^2 + b^2 = c^2$`, which renders as $a^2 + b^2 = c^2$.



Verbatim blocks are also used for code:

|| codeblock
|| codeblock
import sys
"""
        ++ String.fromChar '\u{00A0}'
        ++ """
capital = float(sys.argv[1])
...

which is rendered as

|| codeblock
import sys
"""
        ++ String.fromChar '\u{00A0}'
        ++ """
capital = float(sys.argv[1])
rate = float(sys.argv[2])/100.0
years = int(sys.argv[3])
"""
        ++ String.fromChar '\u{00A0}'
        ++ """
for i in range(0, years):
  capital = (1 + rate)*capital
  print "%3d %8.0f" % (i, capital)

Verbatim blocks begin with a double pipe `||` and in all other respects are like ordinary blocks.
[b Caveat:] When you have multiple paragraphs in a verbatim block, as above, the "blank" lines must contain at least one space.


For inline code, one can use backticks, just as in Markdown:

|| codeblock
`a[i] = 1.`

The rendered form is `a[i] = 1`




## More Examples

### Images

[image width:80 placement:left https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

[image width:200 placement:left https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]

[image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]



## Errors

Look at the example below, where the source text is labeled (1) and the rendered version is labeled (2).  There should be a right bracket after [i real]. The error is flagged in rendered text, but the subsequent italicized text is unaffected.  This error-tolerance is a feature which [b L1] derives from Camperdown (see the [i Article] tab).

(1) `This [i is] a [b real test! [i Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien.]`


(2) This [i is] a [b real test! [i Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in augue eget felis rhoncus ullamcorper sed pulvinar sapien.]

## Lisp-like functions

The text `[fontRGB 255 0 255 foo]` renders as
[fontRGB 255 0 255 foo].  Think of `fontRGB` and a function whose arguments here are the elements of the list `[255 0 255 foo]`.  Functions, or more properly, functional expressions, can be nested, as in  `[fontRGB 255 0 255 foo [b bar]]` which renders as [fontRGB 255 0 255 foo [b bar]].

##  Markdown-type stuff

Below are some Markdown-like examples.   Compare the source and rendered text to see what is going on.

### Links

Use the model below for links:

|| codeblock
[link "NYT" "https://nytimes.com"]

:[link "NYT" "https://nytimes.com"]

### Colors:

|| codeblock
 This is [red red meat].  [gray (We shouldn't eat so much)]

:This is [red red meat].  [gray (We shouldn't eat so much.)]



"""
