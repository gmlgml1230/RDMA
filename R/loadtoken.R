#' get RDMA
#' @export
#' @importFrom rjson fromJSON


loadtoken = function(credlist) {
  opts = list(verbose=T, ssl.verifypeer = FALSE)
  a = rjson::fromJSON(RCurl::postForm("https://accounts.google.com/o/oauth2/token",
                                      .opts=opts, code=credlist$c.token,
                                      client_id=credlist$c.id,
                                      client_secret=credlist$c.secret,
                                      redirect_uri="urn:ietf:wg:oauth:2.0:oob",
                                      grant_type="authorization_code", 
                                      style="POST"))
  
  if (length(a) == 1) {
    print('You need to update the token - run doAuth()')
  } else {
    a$timeStamp <- as.numeric(Sys.time())
  }
  
  a
  
}
