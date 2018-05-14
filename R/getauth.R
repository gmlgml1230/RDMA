#' get RDMA
#' @export
#' @importFrom RCurl getURL


getauth = function(clientid, clientsecret, developertoken) {
  if(!exists('credentials')){
    c.id <- clientid
    if(c.id == ""){
      return(print('You have to provide a Client ID from the Adwords API Project for native apps.'))
    }
    else {
      credentials <- data.frame(c.id)
      credentials$c.secret <- clientsecret
      credentials$auth.developerToken <- developertoken
    }
  }

  if(exists('credentials')){
    url <- paste('https://accounts.google.com/o/oauth2/auth?',
                 'client_id=', credentials$c.id, '&',
                 'response_type=code&',
                 'scope=https%3A%2F%2Fadwords.google.com%2Fapi%2Fadwords%2F&',
                 'redirect_uri=urn:ietf:wg:oauth:2.0:oob&',
                 'access_type=offline&',
                 'approval_prompt=force', sep='', collapse='')
    cert <- system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")
    RCurl::getURL(url, cainfo=cert, ssl.verifypeer = FALSE)
    browseURL(url)
  }
  credentials
}
