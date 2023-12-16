# Endpoints

DATABRARY_API <- "https://nyu.databrary.org/api"
LOGIN <- "https://nyu.databrary.org/api/user/login"
LOGOUT <- "https://nyu.databrary.org/api/user/logout"
GET_VOL_BY_ID <-
  "https://nyu.databrary.org/api/volume/%s?access&citation&links&funding&top&tags&excerpts&comments&records&containers=all&metrics&state"
GET_SESSIONS_IN_VOL <-
  "https://nyu.databrary.org/api/volume/%s?records&containers=all"
GET_ACTIVITY_DATA <-
  "https://nyu.databrary.org/api/activity"
CREATE_SLOT <-
  "https://nyu.databrary.org/api/volume/%s/slot"
CREATE_UPLOAD_FLOW <-
  "https://nyu.databrary.org/api/volume/%s/upload"
UPLOAD_CHUNK <- "https://nyu.databrary.org/api/upload"
CREATE_FILE_FROM_FLOW <-
  "https://nyu.databrary.org/api/volume/%s/asset"
UPDATE_SLOT <- "https://nyu.databrary.org/api/slot/%s"
QUERY_SLOT <-
  "https://nyu.databrary.org/api/slot/%s/-?records&assets&excerpts&tags&comments"
SESSION_CSV <- "https://nyu.databrary.org/volume/%s/csv"
API_CONSTANTS <- "https://nyu.databrary.org/api/constants"
DOWNLOAD_FILE <-
  "https://nyu.databrary.org/slot/%s/-/asset/%s/download"
GET_PARTY_BY_ID <- "https://nyu.databrary.org/api/party/%s"
GET_CONSTANTS <- "https://nyu.databrary.org//api/constants"
GET_PARTY_AVATAR <- "https://nyu.databrary.org/party/%s/avatar"
GET_SESSION_CSV <- "https://nyu.databrary.org/volume/%s/csv"
  
# Authentication parameters
USER_AGENT <-
  "databraryr (https://cran.r-project.org/package=databraryr)"
KEYRING_SERVICE <- 'org.databrary.databraryr'

# httr2 request parameters
RETRY_LIMIT <- 3
RETRY_WAIT_TIME <- 1  # seconds
RETRY_BACKOFF <- 2  # exponential backoff
REQUEST_TIMEOUT <- 5 # seconds

ROG_EMAIL <- Sys.getenv("DATABRARY_LOGIN")
ROG_PW <- keyring::key_get(service = KEYRING_SERVICE, username = ROG_EMAIL)