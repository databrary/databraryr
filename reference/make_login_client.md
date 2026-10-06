# Log In To Databrary.org.

Log In To Databrary.org.

## Usage

``` r
make_login_client(
  email = NULL,
  password = NULL,
  store = FALSE,
  overwrite = FALSE,
  vb = options::opt("vb"),
  service = KEYRING_SERVICE,
  rq = NULL
)
```

## Arguments

- email:

  Databrary account email address.

- password:

  Databrary password (not recommended as it will displayed as you type)

- store:

  A boolean value. If TRUE store/retrieve credentials from the system
  keyring/keychain.

- overwrite:

  A boolean value. If TRUE and store is TRUE, overwrite or update stored
  credentials in keyring/keychain.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- service:

  A character label for stored credentials in the keyring. Default is
  "databrary"

- rq:

  An `httr2` request object. Defaults to NULL.

## Value

Logical value indicating whether log in is successful or not.

## Examples

``` r
if (FALSE) { # interactive()
make_login_client() # Queries user for email and password interactively.
}
# \donttest{
if (FALSE) { # \dontrun{
# The following shows how to use credentials that have been stored previously.

make_login_client(email = "you@provider.com", store = TRUE)
} # }
# }
```
