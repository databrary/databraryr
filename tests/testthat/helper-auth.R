login_test_account <- function() {
  required_vars <- c(
    "DATABRARY_LOGIN",
    "DATABRARY_PASSWORD",
    "DATABRARY_CLIENT_ID",
    "DATABRARY_CLIENT_SECRET"
  )

  missing <- vapply(required_vars, function(v) {
    val <- Sys.getenv(v, "")
    !nzchar(val)
  }, logical(1))

  if (any(missing)) {
    testthat::skip(paste0(
      "Missing env vars for live API test: ",
      paste(required_vars[missing], collapse = ", "),
      ". See README for required environment variables."
    ))
  }

  if (!nzchar(Sys.getenv("DATABRARY_BASE_URL", ""))) {
    Sys.setenv(DATABRARY_BASE_URL = "https://api.stg-databrary.its.nyu.edu")
  }

  vals <- list(
    email = Sys.getenv("DATABRARY_LOGIN"),
    password = Sys.getenv("DATABRARY_PASSWORD"),
    client_id = Sys.getenv("DATABRARY_CLIENT_ID"),
    client_secret = Sys.getenv("DATABRARY_CLIENT_SECRET")
  )

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
