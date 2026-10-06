# Make Portable File Names

Make Portable File Names

## Usage

``` r
make_fn_portable(
  fn,
  vb = options::opt("vb"),
  replace_regex = "[ &\\!\\)\\(\\}\\{\\[\\]\\+\\=@#\\$%\\^\\*]",
  replacement_char = "_"
)
```

## Arguments

- fn:

  Databrary party ID

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- replace_regex:

  A character string. A regular expression to capture the "non-portable"
  characters in fn.

- replacement_char:

  A character string. The character(s) that will replace the
  non-portable characters.

## Value

A "cleaned" portable file name
