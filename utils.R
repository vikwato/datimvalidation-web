  require(datimvalidation)
  options(shiny.maxRequestSize=30*1024^2)
  DHISLogin<-function(baseurl, username, password) {
    httr::set_config(httr::config(http_version = 0))
    baseurl<-"http://127.0.0.1:8080/"
    url <- URLencode(URL = paste0(baseurl, "api/me"))
    #Logging in here will give us a cookie to reuse
    r <- httr::GET(url ,
                   httr::authenticate(username, password),
                   httr::timeout(60))
    if(r$status != 200L){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  