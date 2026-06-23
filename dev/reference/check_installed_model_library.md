# Check if PKPDsim model library is installed

Check if PKPDsim model library is installed

## Usage

``` r
check_installed_model_library(model, call = rlang::caller_env())
```

## Arguments

- model:

  Character. Name of an installed PKPDsim model library.

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

## Value

This function is called for its side effects and returns `NULL` if the
PKPDsim model library is installed or returns an error otherwise.
