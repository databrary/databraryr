#' Load Package-wide Constants into Local Environment
#'
#'
# DATABRARY_BASE_URL <- Sys.getenv("DATABRARY_BASE_URL", "https://api.stg-databrary.its.nyu.edu")
DATABRARY_BASE_URL <- Sys.getenv("DATABRARY_BASE_URL", "https://api.databrary.org")

API_ACTIVITY_SUMMARY <- "/statistics/summary/"
API_GROUPED_FORMATS <- "/grouped-formats/"
API_USERS <- "/users/"
API_USER_DETAIL <- "/users/%s/"
API_USER_VOLUMES <- "/users/%s/volumes/"
API_USER_SPONSORSHIPS <- "/users/%s/sponsorships/"
API_USER_AFFILIATES <- "/users/%s/affiliates/"
API_USER_AVATAR <- "/users/%s/avatar/"
API_USERS_HISTORY <- "/users/%s/history/"
API_INSTITUTIONS_LIST <- "/institutions/"
API_INSTITUTIONS <- "/institutions/%s/"
API_INSTITUTION_AFFILIATES <- "/institutions/%s/affiliates/"
API_INSTITUTION_AVATAR <- "/institutions/%s/avatar/"
API_VOLUMES <- "/volumes/"
API_VOLUME_DETAIL <- "/volumes/%s/"
API_VOLUME_TAGS <- "/volumes/%s/tags/"
API_VOLUME_LINKS <- "/volumes/%s/links/"
API_VOLUME_FUNDINGS <- "/volumes/%s/fundings/"
API_VOLUME_COLLABORATORS <- "/volumes/%s/collaborators/"
API_VOLUME_COLLABORATOR_DETAIL <- "/volumes/%s/collaborators/%s/"
API_VOLUME_HISTORY <- "/volumes/%s/history/"
API_VOLUME_SESSIONS <- "/volumes/%s/sessions/"
API_VOLUME_FOLDERS <- "/volumes/%s/folders/"
API_VOLUME_RECORDS <- "/volumes/%s/records/"
API_VOLUME_RECORD_DETAIL <- "/volumes/%s/records/%s/"
API_SESSION_DETAIL <- "/volumes/%s/sessions/%s/"
API_SESSION_FILES <- "/volumes/%s/sessions/%s/files/"
API_SESSION_FILE_DETAIL <- "/volumes/%s/sessions/%s/files/%s/"
API_FILES_DOWNLOAD_LINK <- "/volumes/%s/sessions/%s/files/%s/download-link/"
API_SESSION_DOWNLOAD_LINK <- "/volumes/%s/sessions/%s/download-link/"
API_SESSION_CSV_DOWNLOAD_LINK <- "/volumes/%s/sessions/%s/csv-download-link/"
API_FOLDER_DETAIL <- "/volumes/%s/folders/%s/"
API_FOLDER_FILES <- "/volumes/%s/folders/%s/files/"
API_FOLDER_FILES_DETAIL <- "/volumes/%s/folders/%s/files/%s/"
API_FOLDER_DOWNLOAD_LINK <- "/volumes/%s/folders/%s/download-link/"
API_FOLDER_FILE_DOWNLOAD_LINK <- "/volumes/%s/folders/%s/files/%s/download-link/"
API_VOLUME_DOWNLOAD_LINK <- "/volumes/%s/download-link/"
API_VOLUME_CSV_DOWNLOAD_LINK <- "/volumes/%s/csv-download-link/"
API_SEARCH_VOLUMES <- "/search/volumes/"
API_SEARCH_USERS <- "/search/users/"
API_SEARCH_INSTITUTIONS <- "/search/institutions/"
API_FUNDERS <- "/funders/"
API_FUNDER_DETAIL <- "/funders/%s/"
API_TAG_DETAIL <- "/tags/%s/"
API_CATEGORIES <- "/categories/"
API_CATEGORY_DETAIL <- "/categories/%s/"

RETRY_LIMIT <- 3
RETRY_WAIT_TIME <- 1  # seconds
RETRY_BACKOFF <- 2  # exponential backoff
REQUEST_TIMEOUT <- 5 # seconds
REQUEST_TIMEOUT_VERY_LONG <- 600


OAUTH_TOKEN_URL <- sprintf("%s/o/token/", DATABRARY_BASE_URL)
OAUTH_TEST_URL <- sprintf("%s/oauth2/test/", DATABRARY_BASE_URL)

USER_AGENT <- Sys.getenv("USER_AGENT", "SRW$*Kxy2nYdyo4LozoGV#i6LvH/")
KEYRING_SERVICE <- 'org.databrary.databraryr'
