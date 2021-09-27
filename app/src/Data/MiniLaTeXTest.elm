module Data.MiniLaTeXTest exposing (text)


text =
    """
\\title{MiniLaTeX Test}

\\section{Macros}

This is a very \\strong{bold} move.

Yes, a \\strong{\\red{very}} bold move indeed! \\italic{(Note that macros are composing properly).}

Pythagoras sez that $a^2 + b^2 = c^2$.

\\section{Math blocks}

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
$$

This is some code:

```
   a[1] = 1

   b[i] = 2
   c[i] = b[i] + 1


\\section{Environments}


This is a quotation:

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


\\section{Errors}

The text below has a two errors:

```
   \\begin{foo}
      ho ho ho!
   \\end{bar}

Here is how it is rendered:

\\begin{foo}
   ho ho ho!
\\end{bar}


"""
