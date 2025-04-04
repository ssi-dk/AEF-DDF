# Test setup
This folder contains a [testthat](https://testthat.r-lib.org/) `helper-setup.R` file designed to work with the GitHub workflows stored in this repo.

`helper-setup.R` provdes the `get_test_conns()` function.

The workflows configure a number of database backends and set environment variables that this `get_test_conns()` function picks up and uses to connect to the databases.

The `get_test_conns()` function will detect whether it is running locally (on your machine) or remotely (as part of Github CI) and connect to data bases accordingly.

See section [Configuring data base connections](#configuring-data-base-connections)

> [!CAUTION]
> You should always be careful when writing data base connection information directly to the `helper-setup.R` file.
> This is a file that is tracked by git and made publicly available when pushed to the remote repository.
> To prevent security issues, connection to sensitive data bases should be configured through environment
> variables.


# Configuring data base connections

Connection to data bases in `get_test_conns()` consists of three parts
1) `conn_list`: a named list of connection drivers,
2) `conn_args`: a named list of connection arguments,
2) `conn_post_connect`: a named list of commands to execute after connecting.

`get_test_conns()` first detects whether or not it is running locally.

If it is run locally, it uses the connection information hard coded into `get_test_conns()` in conjunction with the environment variable `CONN_ARGS_JSON`.
As the name suggests, this environment variable contains arguments to the connections stored in an `JSON` format.

To setup your local testing using `setup.R`, you first have to add the connection drivers you wish to test to the `conn_list` variable, and ensure sufficient connection arguments are stored in `conn_arg` and `CONN_ARGS_JSON` so that connections can be made to the data bases.

To set the variables in `CONN_ARGS_JSON`, use either your `.Rprofile` or equivalent file and write the environment variable in this file.

> [!CAUTION]
> Make sure that this file is not tracked by git in the project.
> Ideally, this information should be stored in your user folder.


If configured correctly, you can now use the function `get_test_conns()` in your package tests to get a list of connections to the available data bases.


## CONN_ARGS_JSON

To set the environment variable `CONN_ARGS_JSON`, you can use the `Sys.setenv()` function in R to write the connection arguments to the environment.

This must be a character string in the JSON format.

Example:
```
Sys.setenv("CONN_ARGS_JSON" = '{"MSSQL": {"driver": "SQL Server", "server": "localhost", "database": "my_MSSQL_db", "trusted_connection": "true"}}')
```

To check that it is valid JSON, use `jsonlite`:
```
jsonlite::fromJSON(Sys.getenv("CONN_ARGS_JSON"))
```
This should returns a named, nested list.


## Adding paths to .Rbuildignore
If you use the rd test file, it is a good idea to add the following paths to to your `.Rbuildignore`
```
^tests/testthat/test-0_rd_files\.R$
```

This will prevent the tests from being bundled with your package since they are primarily a development tool.
If you support older version of R in your package (R < 4.0), then adding these lines is required to maintain backwards compatibility.
