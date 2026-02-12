# Enumerations mirroring constants exposed by the Django backend.

#' @noRd
get_permission_levels_enums <- function() {
  list(
    volume_access_levels = c(
      "superuser",
      "owner",
      "investigator",
      "read write",
      "read only",
      "read only shared",
      "read only public",
      "read only overview",
      "none"
    )
  )
}

#' @noRd
get_release_levels_enums <- function() {
  list(
    levels = list(
      list(
        code = "private",
        description = "This content is not shared and is restricted to collaborators."
      ),
      list(
        code = "authorized_users",
        description = paste0(
          "This content is restricted to authorized Databrary users and ",
          "may not be redistributed in any form."
        )
      ),
      list(
        code = "learning_audiences",
        description = paste0(
          "This content is restricted to authorized Databrary users, who may ",
          "use clips or images from it in presentations for informational or ",
          "educational purposes. Such presentations may be videotaped or ",
          "recorded and those videos or recordings may then be made available ",
          "to the public via the internet (e.g., YouTube)."
        )
      ),
      list(
        code = "public",
        description = "This content is available to the public."
      )
    )
  )
}
