## When the Shibboleth Single Sign-on is used, login.shibboleth() and login.moodle.shibboleth(),
## replace the equivalent basic auth functions. So, if prompted to use login(), use these ones.

login.shibboleth <- function(){ # Login to ESSE3 via Shibboleth

    G <- SIENA
    .internetOK()

    ## Check endpoint
    if(is.null(G$esse3url)) stop("Hi there. You forgot to set the Esse3 endpoint variable. Read the docs, please.")
    G$esse3url <- sub("/$", "", G$esse3url)
    message("Log in to ", G$esse3url)

    ## Set global credentials
    if(is.null(G$e3Credents)) G$e3Credents <- charToRaw(.cred.prompt()) 
    credlst  <- .cred.split(G$e3Credents)
    creds.post <- paste0("j_username=",  curl_escape(credlst$user),
                         "&j_password=", curl_escape(credlst$pw),
                         "&_eventId_proceed=") # <- this empty field is necessary
    creds.post  <-  charToRaw(creds.post)
    
    ## Esse3 shibboleth login url
    e3logurl <- paste0(G$esse3url, "/auth/Logon.do?cod_lingua=eng") # will redir to SSO site
    h <- new_handle()
  
    ## Get redirected SSO page with sign-in form (usually a different site)
    message('.', appendLF = FALSE)
    SSOresp <- curl_fetch_memory(e3logurl, h)
    form <- xml_find_all(read_html(rawToChar(SSOresp$content)), "//form")
    SSOsubmitLink <- xml_attr(form, "action") # the link is relative! 
    if(!length(SSOsubmitLink)) stop("\nUnable to find the address of login form at ", e3logurl)
  
    ## Extract SSO site url from location headers 
    multihds <- parse_headers(rawToChar(SSOresp$headers), multiple = TRUE)
    locs <- lapply(multihds, function(hds) grep("^Location: +http", hds, value = TRUE))
    loc <- sub("^Location: +", "", tail(unlist(locs), 1))
    SSOendpoint <- regmatches(loc, regexpr("https://.+?/", loc))    
    SSOpostUrl <- paste0(SSOendpoint, SSOsubmitLink)

    ## Post Credentials
    message('.', appendLF = FALSE)
    handle_setopt(h,  postfields = creds.post, followlocation = FALSE)
    SSOresp <- curl_fetch_memory(SSOpostUrl, h)
    if(!length(SSOresp$content)) stop("\nCredentials might be wrong. Run setCreds()")

    ## Get the "continue" form data, and simulate click to continue ('cause we have no scripts)
    form <- xml_find_all(read_html(rawToChar(SSOresp$content)), "//form") 
    if(!length(form)) stop("Unable to post credentials to\n", SSOpostUrl)
    relaystate <- xml_attr(xml_find_all(form, ".//input[@name='RelayState']"), "value") 
    samlresp <- xml_attr(xml_find_all(form, ".//input[@name='SAMLResponse']"), "value") 
    samlresp <- curl_escape(samlresp)
    postData <- charToRaw(paste0("RelayState=", relaystate, "&SAMLResponse=", samlresp, "&_eventId_proceed="))

    ## Submit "continue"
    message('.', appendLF = FALSE)
    handle_setopt(h, postfields = postData, followlocation = FALSE)
    logurl <- xml_attr(form, "action")
    logresp <- curl_fetch_memory(logurl, h)

    ## Test Shibboleth and Esse3
    if(!length(grep("_shibsession_", handle_cookies(h)$name))){
        stop("\nI did not get expected cookies at ", logurl)
    } else {
        message("\nYou are now logged in to Shibboleth Single Sign-on.")
    }
    ## We not log to ESSE3 with Shibboleth cookies. We are redirected to ESSE3 home (AreaDocente.do)
    message('...')
    handle_setopt(h, followlocation = TRUE)
    esse3resp <- curl_fetch_memory(e3logurl, h)  
    checkLogin(esse3resp, first = TRUE)

    ## On success return handle
    handle_setopt(h, followlocation = TRUE) # in case we change above
    G$e3Handle <- h
}

login.moodle.shibboleth <- function(){ # Login to Moodle via Shibboleth
### SSO is not used, just a second curl connection with the same creds set by setCreds.shibboleth()
    
    G <- SIENA
    .internetOK()
  
    ## Check endpoint
    if(is.null(G$moodleurl)) stop("Hi there. You forgot to set the Moodle endpoint variable. Read the docs, please.")
    G$moodleurl <- sub("/$", "", G$moodleurl)
    message("Log in to ", G$moodleurl)

    ## Set global credentials. Because of SSO we use G$e3Creds.post here too
    if(is.null(G$e3Credents)) G$e3Credents <- charToRaw(.cred.prompt()) 
    credlst  <- .cred.split(G$e3Credents)
    creds.post <- paste0("j_username=",  curl_escape(credlst$user),
                         "&j_password=", curl_escape(credlst$pw),
                         "&_eventId_proceed=") # <- this empty field is necessary
        
    ## Moodle login url   
    moodlogurl <- paste0(G$moodleurl, "/auth/shibboleth/index.php") # will redir to SSO site
    h <- new_handle()
  
    ## Get redirected SSO page with sign-in form (usually a different site)
    message('.', appendLF = FALSE)
    SSOresp <- curl_fetch_memory(moodlogurl, h)
    form <- xml_find_all(read_html(rawToChar(SSOresp$content)), "//form")
    SSOsubmitLink <- xml_attr(form, "action") # the link is relative! 
    if(!length(SSOsubmitLink)) stop("\nUnable to find the address of login form at ", logurl)
  
    ## Extract SSO site url from location headers 
    multihds <- parse_headers(rawToChar(SSOresp$headers), multiple = TRUE)
    locs <- lapply(multihds, function(hds) grep("^Location: +https", hds, value = TRUE))
    loc <- sub("^Location: +", "", tail(unlist(locs), 1))
    SSOendpoint <- regmatches(loc, regexpr("https://.+?/", loc))    
    SSOpostUrl <- paste0(SSOendpoint, SSOsubmitLink)

    ## Post Credentials   
    handle_setopt(h,  postfields = creds.post, followlocation = FALSE)
    message('.', appendLF = FALSE)
    SSOresp <- curl_fetch_memory(SSOpostUrl, h)
    if(!length(SSOresp$content)) stop("\nCredentials might be wrong. Run setCreds()")
    
    ## Get the "continue" form data, and simulate click to continue ('cause we have no scripts)
    form <- xml_find_all(read_html(rawToChar(SSOresp$content)), "//form") 
    if(!length(form)) stop("Unable to post credentials to\n", SSOpostUrl)
    relaystate <- xml_attr(xml_find_all(form, ".//input[@name='RelayState']"), "value") 
    samlresp <- xml_attr(xml_find_all(form, ".//input[@name='SAMLResponse']"), "value") 
    samlresp <- curl_escape(samlresp)
    postData <- charToRaw(paste0("RelayState=", relaystate, "&SAMLResponse=", samlresp, "&_eventId_proceed="))
    
    ## Submit "continue"
    message('.', appendLF = FALSE)
    handle_setopt(h, postfields = postData, followlocation = FALSE)
    logurl <- xml_attr(form, "action")
    logresp <- curl_fetch_memory(logurl, h)

    ## Test Shibboleth and Moodle
    if(!length(grep("_shibsession_", handle_cookies(h)$name))){
        stop("\nI did not get expected cookies at ", logurl)
    } else {
        message("\nYou are now logged in to Shibboleth Single Sign-on.")
    }

    ## Now /auth/shibboleth/index.php redirects and logs to the service
    message('...')
    handle_setopt(h, followlocation = TRUE)
    moodresp <- curl_fetch_memory(moodlogurl, h)
    checkLogin.moodle(moodresp, first = TRUE)

    ## On success return handle
    handle_setopt(h, followlocation = TRUE) # in case we change above
    G$moodHandle <- h # yes, we use 2 handles, despite Shibboleth, for a robot, two seems more convenient
}


### Please do not touch the strng below
### nC2L35TPRsbXlqUw9Ku$GV@wrzz34$Kk*eSw#Q8LlbafywtE*bT3QGn%
