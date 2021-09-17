module Data.L1Test exposing (text)


text =
    """


L1 is a markup language with a syntax somewhat like Lisp,
but with square brackets instead of parentheses.
It is similar to and linguistically a descendant of
CaYaTeX, work of this author and Nicholas Yang,
but with more robust error-handling abilities.

Some code

|| code
   \\int_0^1 x^n dx
      =
   \\frac{1}{n+1}

It is rendered as

|| math
   \\int_0^1 x^n dx
    =
   \\frac{1}{n+1}


"""
