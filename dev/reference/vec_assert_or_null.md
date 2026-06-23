# Assert an argument has known prototype and/or size or is NULL

Assert an argument has known prototype and/or size or is NULL

## Usage

``` r
vec_assert_or_null(
  x,
  ptype = NULL,
  size = NULL,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  A vector argument to check.

- ptype:

  Prototype to compare against. If the prototype has a class, its
  [`vec_ptype()`](https://vctrs.r-lib.org/reference/vec_ptype.html) is
  compared to that of `x` with
  [`identical()`](https://rdrr.io/r/base/identical.html). Otherwise, its
  [`typeof()`](https://rdrr.io/r/base/typeof.html) is compared to that
  of `x` with `==`.

- size:

  A single integer size against which to compare.

- arg:

  Name of argument being checked. This is used in error messages. The
  label of the expression passed as `x` is taken as default.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

Either throws an error or returns `x`, invisibly.
