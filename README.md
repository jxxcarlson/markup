# Markup

The function `Markup.API.compile` will transform source text
in any one of three markup languages (L1, Markdown, MiniLaTeX)
to `Html msg`:

```elm
compile : Language -> Int -> Settings 
       -> List String -> List (Element msg)
```

where 

```elm
type Language
    = L1
    | Markdown
    | MiniLaTeX
    
type alias Settings =
    { width : Int }
```

## Example 1  

Let 

```
source1 = 
   """
\\section{Calculus homework review}

Be \\bold{sure} to understand this formula:

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
$$
```

Then

```elm
compile MiniLaTeX 0 {width = 500} (String.lines source1)
```
will compile the source text to `Html msg`.

## Example 2


Let

```
source2 =

"""
# Calculus homework review

Be **sure** to understand this formula:

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
```

This text compiles to `Html msg` using

```elm
compile Markdown 0 {width = 500} (String.lines source2)
```

