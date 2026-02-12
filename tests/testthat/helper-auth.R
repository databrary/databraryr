login_test_account <- function() {
  set_if_missing <- function(var, value) {
    current <- Sys.getenv(var, NA_character_)
    if (is.na(current) || !nzchar(current)) {
      Sys.setenv(var = value)
    }
  }

  set_if_missing("DATABRARY_BASE_URL", "https://api.stg-databrary.its.nyu.edu")
  set_if_missing("DATABRARY_LOGIN", "pawel.armatys+1@montrosesoftware.com")
  set_if_missing("DATABRARY_PASSWORD", "tindov-9ciVxa-hehguw")
  set_if_missing("DATABRARY_CLIENT_ID", "9B0gJF1b5OSkkrjPrkKHeYHgWLOJ0N1Uxv2tW3KS")
  set_if_missing("DATABRARY_CLIENT_SECRET", "Mz7LuOXvWHEEcUIffkOtjXIBrb0brhCVtxIKoOq4GxKrp9ZJAa1fjFSsqAu8HnrPtKpXnYwrWxRsauD3Ap2va1Xc41DOEPWBqQcsRHAC7dZai5LEl5n7lC7Wcb0tKLy2")

  vals <- list(
    email = Sys.getenv("DATABRARY_LOGIN", "pawel.armatys+1@montrosesoftware.com"),
    password = Sys.getenv("DATABRARY_PASSWORD", "tindov-9ciVxa-hehguw"),
    client_id = Sys.getenv("DATABRARY_CLIENT_ID", "9B0gJF1b5OSkkrjPrkKHeYHgWLOJ0N1Uxv2tW3KS"),
    client_secret = Sys.getenv("DATABRARY_CLIENT_SECRET", "Mz7LuOXvWHEEcUIffkOtjXIBrb0brhCVtxIKoOq4GxKrp9ZJAa1fjFSsqAu8HnrPtKpXnYwrWxRsauD3Ap2va1Xc41DOEPWBqQcsRHAC7dZai5LEl5n7lC7Wcb0tKLy2")
  )

  have_creds <- all(vapply(vals, function(x) nzchar(x), logical(1)))
  if (!have_creds) {
    testthat::skip("OAuth credentials not available for live API test.")
  }

  suppressMessages(databraryr::login_db(
    email = vals$email,
    password = vals$password,
    client_id = vals$client_id,
    client_secret = vals$client_secret,
    store = FALSE,
    vb = FALSE
  ))

  # Ensure token is cached for subsequent requests.
  bundle <- databraryr:::get_token_bundle()
  if (is.null(bundle)) {
    testthat::skip("Unable to obtain OAuth token for live API test.")
  }

  invisible(TRUE)
}


skip_if_null_response <- function(result, context) {
  if (is.null(result)) {
    testthat::skip(paste0(context, " returned NULL on staging; skipping."))
  }
}

