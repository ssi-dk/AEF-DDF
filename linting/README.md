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
