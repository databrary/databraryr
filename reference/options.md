# databraryr Options

Internally used, package-specific options. All options will prioritize R
options() values, and fall back to environment variables if undefined.
If neither the option nor the environment variable is set, a default
value is used.

## Checking Option Values

Option values specific to `databraryr` can be accessed by passing the
package name to `env`.

    options::opts(env = "databraryr")

    options::opt(x, default, env = "databraryr")

## Options

- vb:

  default:

  :   FALSE

  option:

  :   databraryr.vb

  envvar:

  :   R_DATABRARYR_VB (evaluated if possible, raw string otherwise)

## See also

options getOption Sys.setenv Sys.getenv
