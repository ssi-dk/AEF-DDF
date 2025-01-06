# Linting
This folder contains a [lintr](https://lintr.r-lib.org/) `.lintr` configuration file.
This file enforces the [`diseasy` coding standard](https://github.com/ssi-dk/diseasy/wiki/diseasy-coding-standard) by
a mixture of configuring existing `lintr` linters and adding custom linters
(see files `0_linters.R` and `test-0_linters`).

## Adding additional exclusions
The `.lintr` configuration file is location where one would normally exclude certain files from linting.

We however, normally distribute this `.lintr` file to repositories using a "sync" GitHub action, which means
that updates to the configuration file are always updated across all repos in the loop.

To allow a per-repository exclusion of certain files from linting, this `.lintr` configuration file checks for the
existence of a `.lintr_exclusions.R` file and adds exclusion from this file during linting.

Example:
```r
list(
  "file_which_should_not_be_linted_by_linter" = list(
    "linter_to_skip_for_file" = Inf
  )
)
```

## Change to `nolint:` statements
In addition, the `lintr` configuration file defines a change to the detection of `nolint:` statements.

Example:
```r
## Before -- period required
# Valid use
Var_1 = 1 # nolint: assignment_linter.
#   Including the period means that linting works as expected and we still get lints from `object_name_linter`

# Invalid use
Var_2 = 2 # nolint: assignment_linter
#   Omitting the trailing period means that all linting is now disabled on this line.
#   This is "working as intended" https://github.com/r-lib/lintr/issues/2374


## Now -- period optional
# Valid use
Var_1 = 3 # nolint: assignment_linter.
Var_2 = 3 # nolint: assignment_linter
#   In both cases, only the assignment linter is disabled.
```

## Additional linters added to the linting system
### `nolint_position_linter`
Checks that all "nolint:" statements begin at character 121.

### `nolint_line_length_linter`
Checks that no line exceeds 120 characters (ignoring "nolint:" statements).

### `non_ascii_linter`
Checks that the code only contains ASCII characters.

### `param_and_field_linter`
Checks that R6 documentation of "@param" and "@field" objects adhere to the `mlr3` style.
I.e. it checks for the existence of a data_type and carriage return.

```r
#' @param variable (`data_type`)\cr
#'   Description of variable
```

## Adding paths to .Rbuildignore
If you use the linters, it is a good idea to add the following paths to to your `.Rbuildignore`
```
^R/0_linters\.R$
^tests/testthat/test-0_linters\.R$
```

This will prevent the linters from being bundled with your package since they are primarily a development tool.
If you support older version of R in your package (R < 4.2), then adding these lines is required to maintain backwards compatibility.

