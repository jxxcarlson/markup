module Data.MiniLaTeXTest exposing (text)


text =
    """
his is some text. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math.

This is some text. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math.

THIS IS SOME MATH:
$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
$$

This is some code:

```
   a[1] = 1
   .
   b[i] = 2

This is a quote:

\\begin{quotation}
   Regular languages are rather inexpressive,
   but they work great for lexers. On the opposite
   side of expressivity spectrum are Turing machines.
   For them, we also have a number of meta-languages
   (like Rust), which work great for humans.
   It’s interesting that a Turing machine is
   equivalent to a finite state machine with
   a pair of stacks: to get two stacks from a tape,
   cut the tape in half where the head is. Moving
   the head then corresponds to popping from one
   stack and pushing to another.
\\end{quotation}



*by James Carlson*

"""
