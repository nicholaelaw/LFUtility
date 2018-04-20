#' Get List of Github User
#'
#' @param FILE CSV file containing a list of user name and associated email name.
#' Default location is \code{./vault_users/gh_users.csv}
#'
#' @return User list as `data.table`
#' @export getGHUserList
getGHUserList <- function(FILE) {
  if (missing(FILE)) {
    filePath <- normalizePath('./vault_users/gh_users.csv')
  } else {
    filePath <- normalizePath(FILE)
  }
  is.readable(filePath)

  result <- fread(input = filePath, stringsAsFactors = FALSE)
}


addPayload <- function(DATA = './data/', VAULT, GH_USERS) {
  if (dir.exists(normalizePath('../LFDT2'))) {

  } else {

  }

}

updatePayload <- function(DATA, VAULT, GH_USERS) {

}



createPayload <- function(DATA = './data', VAULT = './payload', USERS) {
  dataDir  <- file.path(DATA)
  vaultDir <- file.path(VAULT)
  if (!dir.exists(vaultDir)) {dir.create(vaultDir)}

  config     <- fromJSON(txt = file.path(DATA, 'config.json'))
  ghUserList <- getGHUserList(USERS)
  userList   <- list_users(vault = vaultDir)

  for (i in 1L:length(ghUserList)) {
    if (!any(userList == ghUserList[i, github_email])) {
      add_github_user(
        github_user = ghUserList[i, github_user],
        email       = ghUserList[i, github_email],
        vault       = vaultDir,
        i           = ghUserList[i, index])
    }
  }
}

