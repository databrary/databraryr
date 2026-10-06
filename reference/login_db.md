# Log In To Databrary.org.

Log In To Databrary.org.

## Usage

``` r
login_db(
  email = NULL,
  password = NULL,
  client_id = NULL,
  client_secret = NULL,
  store = FALSE,
  overwrite = FALSE,
  service = KEYRING_SERVICE,
  vb = options::opt("vb")
)
```

## Arguments

- email:

  Databrary account email address.

- password:

  Databrary password (not recommended as it will displayed as you type)

- client_id:

  OAuth2 client identifier.

- client_secret:

  OAuth2 client secret.

- store:

  A boolean value. If TRUE store/retrieve credentials from the system
  keyring/keychain.

- overwrite:

  A boolean value. If TRUE and store is TRUE, overwrite/ update stored
  credentials in keyring/keychain.

- service:

  A character label for stored credentials in the keyring. Default is
  `org.databrary.databraryr`.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

## Value

Logical value indicating whether log in is successful or not.

## Examples

``` r
if (FALSE) { # interactive()
login_db() # Queries user for email and password interactively.
}
# \donttest{
if (FALSE) { # \dontrun{
# The following shows how to use credentials that have been stored previously.

login_db(email = "you@provider.com", store = TRUE)

} # }
# }
```
