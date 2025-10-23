# Github workflows for R-packages
In this folder will find a series of github workflows that you can copy to the `.github/workflows` directory of your
R-package.
These workflows calls the reusable workflows (which are stored in this repo' `.github/workflows` folder).

Below is an overview of the workflows available to you.


## all-workflows.yaml
This workflow calls is a convenience wrapper for all the individual CI workflows stored in this repo.
If you do not want granular control of the workflows being run, you can copy this single file to your repo and let us
curate the selection of workflows being run on your package.

This workflow has some overall settings that needs to be set via arguments:

| Arguments        | Description                             | Default     | Example       |
|------------------|-----------------------------------------|-------------|---------------|
| `skip`           | Workflows to skip                       | styler      | lint,document |
| `main_branches`  | Main branches to trigger on             | main,master | main,develop  |
| `use-containers` | Should docker containers be created?    | true        | false         |

The argument `use-containers` controls the creation of docker images.
By default, the workflow will bulid docker images and use these images in the pipeline to speed up the process, but
this may cause issues in some cases.

Continue reading to learn what the individual workflows do.

If these workflows take arguments, they can also be passed to `all_workflows` which will then pass it to the appropriate
workflows.


## lint.yaml
#### Trigger
Changes to any file in the `R/` or `man/` folders.

#### Function
Checks for style issues in your code base.

#### Outcome
The workflow will produce a list of lints (i.e. suspected style issues) detected in your code base.

Click on the workflow and look for the "Lint" tab to see the detected issues.

#### Exit status
If any lints are produced, the workflow will give an error.


## R-CMD-check.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to any file in the `R/`, `tests/`, `man/` or `vignette/`
folders.

#### Function
Runs `rcmdcheck::rcmdcheck` on your package using various operating systems (ubuntu, windows, macOS).

> [!IMPORTANT]
> To conserve resources, these R-CMD-checks will not run the tests.
> These tests are instead being run by code-coverage.yaml

Furthermore, the checks are run on ubuntu using the previous release of R (`oldrel-1`) and without loading the packages specified in `Suggests:`.
> [!IMPORTANT]
> The check for R-CMD-checks without dependencies will not run the tests to check for potential issues.

#### Outcome
The workflow will post the output of `rcmdcheck::rcmdcheck`.

Click on the workflow and look for the "Run r-lib/actions/check-r-package@v2" tab to see the output.

| Arguments        | Description                      | Default                                     | Example                       |
|------------------|----------------------------------|---------------------------------------------|-------------------------------|
| `rcmdcheck-args` | Arguments passed to R-CMD-check. | c("--no-manual", "--as-cran", "--no-tests") | c("--no-manual", "--as-cran") |


#### Exit status
If any issues are found, the workflow will give an error.


## code-coverage.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to the `DESCRIPTION`, or any file in the `R/` or
`tests/` folders.

#### Function
Determine the degree of [code coverage](https://en.wikipedia.org/wiki/Code_coverage) in your code base.

#### Outcome
Code coverage results are uploaded to [codecov.io](https://about.codecov.io/).
This enables you to add the code coverage badge to your `README.md`.

| Arguments            | Description                                              | Default     | Example       |
|----------------------|----------------------------------------------------------|-------------|---------------|
| `schemas`            | Schemas to create (optional)                             | ''          | test,test.one |
| `backend_exclude`    | Data base backends to skip (optional)                    | ''          | postgres      |
| `check_postgres_logs`| Should PostgreSQL logs be checked for errors? (optional) | true        | false         |

> [!NOTE]
> PostgreSQL logs errors to a log file during the tests. Checking these for errors can help detect uncaught
> errors in your test workflow.

#### Exit status
If any tests fail to run, the workflow will give an error.

> [!IMPORTANT]
> To conserve resources, only the `code-coverage` workflow is configured with data bases.
> This means that your `R-CMD-check` may not fail when `code-coverage` fails.
> If this is the case, the cause is likely a data base specific issue in your code base.


## styler.yaml
#### Trigger
Changes to any file in the `R/` or `vignettes` folder.

#### Function
Runs `styler::style_pkg()` on your package

#### Outcome
If this workflow changes any of the `.R` or `.Rmd` files, it will commit the updated
code base to the repository.

> [!IMPORTANT]
> To use this workflow, you need to have configure GitHub
> [secret](https://docs.github.com/en/actions/security-guides/using-secrets-in-github-actions) called "GH_PAT" that
> contains a [personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)
> which has write access to the repository.


## document.yaml
#### Trigger
Changes to any file in the `R/` folder.

#### Function
Renders the `roxygen` documentation for your package.

#### Outcome
If this generate changes the `.Rd` files, it will commit the updated documentation to the repository.

> [!IMPORTANT]
> To use this workflow, you need to have configure GitHub
> [secret](https://docs.github.com/en/actions/security-guides/using-secrets-in-github-actions) called "GH_PAT" that
> contains a [personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)
> which has write access to the repository.


## pkgdown.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to any file in the `R/`, `man/` or `vignette/` folders.

Special triggering on creation of a `release`.

#### Function
Generates the `pkgdown` documentation for your package.

> [!IMPORTANT]
> This workflow runs additional formatting of the pkgdown documentation.
> Specifically, it adds auto-linking to R6 object
> - `?MyR6Class` is rendered without the `?` and links to `MyR6Class`.
> - `?MyR6Class$method()` is rendered as `method()` and links to the method of `MyR6Class`.
> - `?MyR6Class$active_binding` is rendered as `active_binding` and links to the active binding section of `MyR6Class`.
>
> Notably, these changes are only done on Github.com.
> When rendering the documentation locally, the code blocks will not be changed.

#### Outcome
The output of `pkgdown` is stored as an artifact on github.

If the trigger is a `release`, the `pkgdown` output is deployed to the github page of the repository.
Alternatively, if the package is a pre-release state (i.e. the version number is less than "0.1") then the
`pkgdown` output is deployed when the workflow runs on one of the `main_branches`.

| Arguments            | Description                                              | Default     | Example       |
|----------------------|----------------------------------------------------------|-------------|---------------|
| `auto_link_r6`       | Should R6 references be auto-linked? (optional)          | true        | false         |
| `main_branches`      | Main branches to trigger on (optional).                  | main,master | production    |


## render-readme.yaml
#### Trigger
Changes to `README.Rmd`.

#### Function
Knits `README.md` from `README.Rmd`.

#### Outcome
If this generates changes to `README.md`, it will commit the updated README to the repository.

> [!IMPORTANT]
> To use this workflow, you need to have configure GitHub
> [secret](https://docs.github.com/en/actions/security-guides/using-secrets-in-github-actions) called "GH_PAT" that
> contains a [personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)
> which has write access to the repository.


## spell-checker.yaml
#### Trigger
Changes to `DESCRIPTION` or any file in the `man/` or `vignette/` folders.

#### Function
Runs `spelling::spell_check_package` on the code base.

#### Outcome
The workflow will post the output of `spelling::spell_check_package`.

Click on the workflow and look for the "Run Spelling Check test" tab to see the output.

#### Exit status
If any spelling mistakes are found, the workflow will give an error.


## update-lockfile.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to the `DESCRIPTION`, or any file in the `R/` or
`tests/` folders.

#### Function
Runs `pak::lockfile_create()` on the code base and commits results.

#### Outcome
The function will update the `pak.lock` file of the repository match the testing environment.

> [!IMPORTANT]
> To use this workflow, you need to have configure GitHub
> [secret](https://docs.github.com/en/actions/security-guides/using-secrets-in-github-actions) called "GH_PAT" that
> contains a [personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)
> which has write access to the repository.


## update-cache.yml
#### Trigger


## benchmark.yaml
#### Trigger
Manually triggered on Github.com.

#### Function
Runs `data-raw/benchmarks.R` across all test backends and compiles benchmarks to a single data object.

#### Outcome
Benchmark data compiled to `inst/extdata/benchmarks.rds`.

| Arguments            | Description                                              | Default     | Example       |
|----------------------|----------------------------------------------------------|-------------|---------------|
| `backend_exclude`    | Data base backends to skip (optional)                    | ''          | postgres      |
