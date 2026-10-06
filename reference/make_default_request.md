# Set base request defaults for Databrary API.

Creates an `httr2` request with the package's default options, including
base URL, user agent, Accept header, and timeout tuned for the Django
API.

## Usage

``` r
make_default_request(
  with_token = TRUE,
  refresh = TRUE,
  vb = options::opt("vb")
)
```

## Arguments

- with_token:

  Should the request include an OAuth2 `Authorization` header? Defaults
  to `TRUE` since all API calls now require authentication.

- refresh:

  When `with_token = TRUE`, determines whether to refresh the cached
  token if it is near expiry. Defaults to `TRUE`.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

## Value

An `httr2_request` object configured for the Databrary API.

## Examples

``` r
make_default_request(with_token = FALSE)
#> <httr2_request>
#> GET https://api.databrary.org
#> Headers:
#> * Accept: "application/json"
#> Body: empty
#> Options:
#> * useragent     : "databraryr/1.0.0.9000"
#> * timeout_ms    : 5000
#> * connecttimeout: 0
#> Policies:
#> * retry_max_tries        : 3
#> * retry_on_failure       : FALSE
#> * retry_failure_threshold: Inf
#> * retry_failure_timeout  : 30
#> * retry_realm            : "api.databrary.org"
```
