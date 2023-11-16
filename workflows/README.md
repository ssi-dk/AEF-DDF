# Github workflows for R-pacakges
In this folder will find a series of github workflows that you can copy to the `.github/workflows` directory of your
R-package.
These workflows calls the reusable workflows (which are stored in this repo' `.github/workflows` folder).

Below is an overview of the workflows available to you.
## all_workflows.yaml
This workflow calls is a convenience wrapper for all the individual workflows stored in this repo.
If you do not want granular control of the workflows being run, you can copy this single file to your repo and let us
curate the selection of workflows being run on your package.

Continue reading to learn what the individual workflows do.

If these workflows take arguments, they can also be passed to `all_workflows` which will then pass it to the approriate
workflows.

#### Arguments
`skip`: Comma-seperated string naming workflows to skip


## lint.yaml
#### Trigger
Changes to any file in the `R/` or `man/` folders.

#### Function
Checks for style issues in your package.

#### Outcome
If issues any are found, the workflow will give an error.


## R-CMD-check.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to any file in the `R/`, `tests/`, `man/` or `vignette/`
folders.

#### Function
Runs `R-CMD-check` on your package using various operating systems (ubuntu, windows, macOS).

#### Outcome
If issues any are found, the workflow will give an error.


## code-coverage.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to any file in the `R/` or `tests/` folders.

#### Function
Determine the degree of [code coverage](https://en.wikipedia.org/wiki/Code_coverage) in your code base.

#### Outcome
Code coverage results are uploaded to [codecov.io](https://about.codecov.io/).
This enables you to add the code coverage badge to your `README.md`.

#### Arguments
`schema_creation`: String passed to the data base backend to create the required schemas

#### NOTE
To conserve resoures, only the `code-coverage` workflow is configured with data bases.
This means that your `R-CMD-check` may not fail when `code-coverage` fails.
If this is the case, the cause is likely a data base specific issue in your code base.


## document.yaml
#### Trigger
Changes to any file in the `R/` folder.

#### Function
Renders the `roxygen` documentation for your package.

#### Outcome
If this generate changes the `.Rd` files, it will commit the updated documentation to the repository.


## pkgdown.yaml
#### Trigger
Pushes or pull requests to the main branch if there are changes to any file in the `R/`, `man/` or `vignette/` folders.

Special triggering on creation of a `release`

#### Function
Generates the `pkgdown` documentation for your package

#### Outcome
The output of `pkgdown` is stored as an artifact on github.

If the trigger is a `release`, the `pkgdown` output is deployed to the github page of the repository.


## render-readme.yaml
#### Trigger
Changes to `README.Rmd`.

#### Function
Knits `README.md` from `README.Rmd`.

#### Outcome
If this generates changes to `README.md`, it it will commit the updated README to the repository.