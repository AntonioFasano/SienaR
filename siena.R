## Originally intended to upload Testmacs results to ESSE3. Now it can work with Moodle.

## Browse data:
##   login() >
##   getCourses() > setCourse(Entry) > getSchedules() > setSched(Entry) >
##   getSched.details, getSched.studs(), getPending()

## Stats:
##   getSummary(); getPending()

## Add sittings
## sitID <- addSitting(courseName, examdate, examtime, bookStart, bookEnd, results, examtype, examdesc, examnote)
## addCommittee(courseName, examdate, committee, sitID)
## findMember(course, examdate, member)

## Post grades from Testmacs or Moodle (see moodle.R)
## setSched(Entry) > postGrades(read.csv(grades.csv))
## cmp_csvesse3()

## Testmacs note:
## grade.R sets a NA grade for missing exam-IDs, which means a failed exam (INSUFFICENTE), mapped as 0 by ESSE3 


#################################################################
##                           Globals                           ##
#################################################################

SIENA <- new.env()  #Init global environment

## User 
SIENA$esse3url  <- NULL
SIENA$workdir  <- NULL

## Status pars
SIENA$e3Credents <-  NULL # ESSE3 credentials as user:pass
SIENA$e3Handle <- NULL
SIENA$e3CoursePars <- NULL # List of vectors of pars to query a course, ordered by course entry
SIENA$Courses <- NULL
SIENA$Schedules <- NULL
SIENA$CurCourse <- NULL
SIENA$CurSchedule <- NULL
SIENA$CurSchedule.e3 <- NULL  # ESSE3 sitting ID 
SIENA$mainpath <-  sys.calls()[[1]] [[2]]
SIENA$Auth <- list(Esse3 = "Basic Access Authentication")
SIENA$LoginFnc <- ".login.basic" 

#################################################################
##                          Libraries                          ##
#################################################################

library('curl')
library('xml2')
library('stringr')


##################################################################
##                      Manage Credentials                      ##
##################################################################

authtype <- function() {
    G <- SIENA
    message(paste(names(G$Auth), G$Auth, sep=": ", collapse = ". "), ".")
}

login <- function(){ # login to ESSE3
    G <- SIENA    
    do.call(G$LoginFnc, if(is.null(formals())) list() else as.list(sys.call())[-1])
}

.login.basic <- function(){ # login to ESSE3 with basic access authentication.

    G <- SIENA
    .internetOK()

    ## Check endpoint
    if(is.null(G$esse3url)) stop("Hi there. You forgot to set the ESSE3 endpoint variable. Read the docs, please.")
    G$esse3url <- sub("/$", "", G$esse3url)
    message("Log in to ", G$esse3url)

    ## Set global credentials 
    if(is.null(G$e3Credents)) G$e3Credents <- charToRaw(.cred.prompt()) 
    credlst  <- .cred.split(G$e3Credents)
    userfld <- "/WS/DataSet[@LocalEntityName='LDATA']/Row[@Num='1']/u"
    passfld <- "/WS/DataSet[@LocalEntityName='LDATA']/Row[@Num='1']/p"
    creds.post  <- paste0(curl_escape(userfld), "=", curl_escape(credlst$user), "&",
                          curl_escape(passfld), "=", curl_escape(credlst$pw))
    
    ## Set and post credentials to ESSE3 login url
    h <- new_handle()
    logurl <- paste0(G$esse3url, "/FormAuthSubmit.do?cod_lingua=eng")
    handle_setopt(h, postfields=creds.post)
    resp <- curl_fetch_memory(logurl, h)

    ## Test 
    checkLogin(resp, first=TRUE)

    ## On success return handle 
    G$e3Handle <- h

    ## Reinitialise.
    if(!is.null(G$CurCourse)) {
        message("Looks like you were already using the Exam Management. Reinitialising to ", G$Courses$Course[G$CurCourse])         
        getCourses(prompt = FALSE)
        getSchedules(prompt = FALSE)
    }
}

.cred.prompt <- function(){
        cred.col <- ""
        message("Your account as \"username:password\" or Enter to exit.")
        cat("Response: ") # in ESS based on `comint-password-prompt-regexp'
        cred.col <- readLines(file("stdin"), 1) 
        if(!nzchar(cred.col)) stop("Stopped by user.") else cred.col
}

.cred.split <- function(credents){  ## Extract components from colon cred string
    credents <-  rawToChar(credents)
    cred.v   <- str_split(credents, ":")[[1]]
    user     <- cred.v[1]
    pw       <- paste(cred.v[-1], collapse=":" ) # in case of colon in pass
    list(user = user, pw = pw)
}

setCreds <- function(){ # Ask and set ESSE3 creds.     
    G <- SIENA
    G$e3Credents <- charToRaw(.cred.prompt())
}

.internetOK <- function()
    if(!has_internet()) stop("Internet is currently down")

checkLogin  <- function(resp, first=FALSE){
    testkey <- "Logout.do?menu_opened_cod"    
    succ <- grepl(testkey, rawToChar(resp$content), fixed = TRUE)
    if(succ) {
        if(first) message("You are now logged in to ESSE3.")
    } else {
        if(first) {
            stop("Endpoint or credentials might be wrong. Check and run setCreds()")
        } else {
            stop("Connection expired. Use login()")
        }
    }
}

#################################################################
##                       Request Helpers                       ##
#################################################################

.query <- function(page, ...){ # Get the HTML content of an ESSE3 instructor page via

    G <- SIENA
    if(is.null(G$e3Handle)) stop("Please, login().")
    baseurl=paste0(G$esse3url, "/auth/docente/")
    url.path <- paste0(baseurl, page)
    url.full <- if(length(paste(...))) paste0(url.path, "?", paste(..., sep="&")) else url.path 
    #url=paste0(baseurl, page, "?", paste(..., sep="&"))
    resp <- curl_fetch_memory(url.full, G$e3Handle)
    checkLogin(resp)
    html <- read_html(resp$content)
    if(!xml_length(xml_find_all(html, "//div[@class='l-header']")))
        stop("There is a navigation error. \nThis can happen after the connection expires. To fix it,  select courses and schedules again.")

    ## TODO .query always imply read_html(), better to call it here recycling previous call above
    rawToChar(resp$content)

}

.query.split <- function(page, ...){ # Like .query() but split outputs at new lines for better grepping
    content <- .query(page, ...)
    strsplit(content, "\n")[[1]]
}

.query.post <- function( # Equivalent of query for post verbs, but returning full response
                       page, postfields,
                       encode=TRUE, follow = FALSE, reset = TRUE){

    G <- SIENA
    
    if(is.null(G$e3Handle)) stop("Please, login().")
    baseurl <- paste0(G$esse3url, "/auth/docente/") 
    url <- paste0(baseurl, page)

    handle_reset(G$e3Handle)
    if(encode) postfields <-  .encpostfields(postfields)  # encode as cURL --data-raw string     
    options <- list(post = TRUE, postfields = postfields, followlocation = FALSE)
    handle_setopt(G$e3Handle, .list = options)    
    resp <- curl_fetch_memory(url, G$e3Handle)

    ## Parse status code of first rederict. Curl gives the last 
    firstreq.hds <- parse_headers(resp$headers, multiple = TRUE)[[1]]
    fstatus <- firstreq.hds[1]
    firstreq.status <- regmatches(fstatus, regexec("^HTTP/.+ ([[:digit:]]+) ", fstatus))[[1]][2]

    ## In case of a login problem (session expired), we get a 200 status code (no 50x nor 300x redirect to login page)
    if(grepl("20[[:digit:]]", firstreq.status))  checkLogin(resp)
        
    ## If we get a 50x we need to check the code logic. Sometimes a getSchedules() before the post will do.  
    if(grepl("50[[:digit:]]", firstreq.status))
        stop (fstatus, "\nPerhaps some functionB was executed before the necessary functionA.")    
    is.redirect <- grepl("30[[:digit:]]", firstreq.status)
    lochead <- grep("^Location:", firstreq.hds, value=TRUE)
    if(is.redirect && !nzchar(lochead))
        stop("Despite status ", firstreq.status, " was returned, unable to find the new location.") 
    newloc <- if(length(lochead)) strsplit(lochead, ": ")[[1]][2] else NA   

    ## Below "last" would be misleading when follow = FALSE, so we use NAs in these cases
    list(content = strsplit(rawToChar(resp$content), "\n")[[1]],
         headers = parse_headers(resp$headers, multiple = TRUE), # list with a char vector for each redirect
         first.statuscode = firstreq.status,
         last.statuscode  = ifelse(follow, resp$status_code, NA),
         is.redirect = is.redirect, 
         first.redirect = newloc,
         last.url  = ifelse(follow, resp$url, NA) # possibly a redirect
         )
     
}

.comnPostdata <- function(){ # Common filled filed for posting, related to the current course

    G <- SIENA
    
    if(is.null(G$Courses)) getCourses(prompt = FALSE)
    p <- G$e3CoursePars[[G$CurCourse]]
    progrid   <- p[['CDS_ID']] # "10224"
    yearid    <- p[['AA_ID']]  # "2020"
    courseid  <- p[['AD_ID']]  # "22602"

    c(AA_ID = yearid, CDS_ID = progrid, AD_ID = courseid)
}


.encpostfields <- function(postvec){# Format named char vector of post fields similar to cURL --data-raw
    
    enames <- curl:::curl_escape(names(postvec))
    evals  <- curl:::curl_escape(postvec)
    paste0(enames, '=', evals, collapse="&")
}
## .encpostfields(c(`Can you add an 'email' or 'username'?` = "mrbean@email.com"))

.parseInputs  <- function(form) { # Parse  form input tags
        vals <- xml_attr(xml_find_all(form, "./input[@value]"), "value")
        nams <- xml_attr(xml_find_all(form, "./input[@name]"), "name")
        names(vals) <-  nams
        vals
}
    
.getin <- function(fld){ # Extract course elements stored by .parseInputs
    G <- SIENA
    val <- G$e3CoursePars[[G$CurCourse]][fld]
    if(fld == "APP_ID") val <- G$CurSchedule.e3 # This field is set manually by setSched()
    paste0(fld, "=", val) 
}

.urlHost <- function(url) # Extract http[s]://www.example.com *without* trailing slash
  regmatches(url, regexpr("http[s]{0,1}://[^/]+", url))

.urlIsAbsolute <- function(url) length(.urlHost(url)) > 0

.replayForm <- function( # Replay a post form 
                          form,  # form xml2 node,
                          resp,  # response header to get post url
                          handle # to get stored possible cookies
                          ){  
  inputs <- xml_find_all(form, ".//input[not(@type='submit')]")
  flds <- curl_escape(xml_attr(inputs, "name"))
  vals <- xml_attr(inputs, "value")
  vals[is.na(vals)] <- ""
  vals <- curl_escape(vals)
  postfields <-paste(paste(flds, vals, sep="="), collapse = "&")
  postfields.opt <- list(post = TRUE, postfields = postfields)  
  submitLink <- xml_attr(form, "action")
  url <- if(.urlIsAbsolute(submitLink)) submitLink else paste0(.urlHost(resp$url), submitLink)
  handle_setopt(handle, .list = postfields.opt)
  curl_fetch_memory(url, handle = handle)
}


##################################################################
##                        Exam Schedules                        ##
##################################################################

getCourses <- function( # Get available courses, assign a sequential entry number, collect course internal web data
                       prompt = TRUE # Prompt to use setCourse()
                       ){ 

    G <- SIENA
    gencal <-  .query("CalendarioEsami/ListaAttivitaCalEsa.do")

    ## Get course HTML table as text data frame
    html <- read_html(gencal)
    course.tab <- xml_find_all(html, "//table[@class='detail_table']")
    if(! length(course.tab)) stop("There seems to be no courses associated with current credentials.")
    courses <- .htmlTab2Array(course.tab)

    ## Convert table to DF
    #courses <- as.data.frame(t(sapply(course.tab, sapply, xml_text)))
    courses <- courses[-1, c(3,1,2)]
    names(courses) <- c("Entry", "Course", "Program")
    courses$Program <- gsub(",.D.M. 270/2004", "", courses$Program)
    courses$Entry <- as.integer(factor(courses$Course, unique(courses$Course)))
    if(prompt) {
        print(courses , row.names=FALSE)
        message("\nUse setCourse(Entry) to select one.\n")
    }
    
    ## Store courses
    rawnames <- unique(courses$Course)
    G$Courses <- data.frame(
        Course= tools::toTitleCase(tolower(gsub(" \\[[[:digit:]]+\\]", "", rawnames))),
        Entry= seq_along(rawnames),
        ESSE3ID = sapply(
            regmatches(rawnames, gregexec("\\[([[:digit:]]+)\\]", rawnames)),
            `[`, 2) 
    )

    ## Extract course data from links
    courses.raw <-  .htmlTab2Array(course.tab, toText = FALSE)
    links <- unique(lapply(courses.raw, "[[", 3)[-1])
    forms <- lapply(links, xml_find_all, "./form")
    G$e3CoursePars <- setNames(lapply(forms, .parseInputs), G$Courses$Entry)
    invisible(G$Courses)
    
}

setCourse <- function( # Set current course
                      entry # Course entry number as from getCourses
                      ){
    G <- SIENA
    if(is.null(G$Courses)) stop("Please, run getCourses() first")
    G$CurCourse <- entry
    G$Courses$Course[entry]
}

setCourse.infer <- function(course){ # Convert a badly formatted string to a course entry number and set it current.
### If the course is already an entry number, just set this entry as current 
### Course entry numbers are those from getSchedules()). 
        
    G <- SIENA
    
    if(is.null(G$Courses)) getCourses(prompt = FALSE)
    
    if(class(course) == "numeric")
        entrynum <- course
    else {
        rxname <- paste0("^", toupper(gsub(" +", " ",  trimws(course))), "$")
        entrynum  <- grep(rxname, toupper(G$Courses$Course))
        if(length(entrynum)==0) stop("Unable to find course named: ", course)
    }
    G$CurCourse <- entrynum
    
}

getSchedules <- function( # Get exam schedules (sessions) and internal schedule ID
                         prompt = TRUE # Prompt to use setSched() 
                         ){ 
    ## Note the schedule ID is different for different courses 

    G <- SIENA
    if(is.null(G$CurCourse)) stop("Please, run getCourses(), setCourse() first")   

    scheds  <- .getSchedules()
    G$Schedules <- scheds
   
    ## Show
    if(prompt) {
        message(sprintf("Schedules for %s:", G$Courses[G$CurCourse, "Course"]))
        if(!nrow(scheds)) {
            message("No scheduled exam found!")
        } else {
            print(data.frame(Schedules=scheds$dateEU, Entry=seq_along(scheds$dateEU)), row.names=FALSE)
            message("\nUse setSched(entry) to select one.\n")
        }
    } 
    invisible(scheds)    
}

.getSchedules <- function(){ # getSchedules() workhorse

    G <- SIENA

    ## Consistency 
    if(is.null(G$CurCourse)) stop("Please, set course first")

    ## Get Schedules html table 
    schedules.url <- "CalendarioEsami/ElencoAppelliCalEsa.do"
    schedules <- .query(schedules.url, .getin("AA_ID"), .getin("CDS_ID"), .getin("AD_ID"))
    html <- read_html(schedules)
    scheds.htab <- xml_find_all(html, "//table[@class='detail_table']")

    if(!length(scheds.htab)) return(data.frame(desc = "", esse3SchedID = "", datetime = as.POSIXct(0), dateEU = "", enrol.status = "", enrol.num = 0, enrol.green = TRUE, graded.status = "", graded.num = 0, graded.green = TRUE, recorded.status ="", recorded.num = 0, recorded.green = TRUE)[-1,])
    
    ## Get table headers and body
    schedArray <- .htmlTab2Array(scheds.htab, toText = FALSE)
    headers <- schedArray[[1]]
    headers <- sapply(headers, xml_text)
    grid <- schedArray[-1]

    ## Function to parse a schedule row. Header have often the same name, usually the first apply
    E <- new.env() # to store each schedule row by reference

    ## Functions to parse the current E$row schedule
    text <- function(header) xml_text(E$row[[which(headers == header)[1]]], trim = TRUE)
    a.text <- function(header) xml_text(xml_find_all(E$row[[which(headers == header)[1]]], "./a"), trim = TRUE)
    a.href <- function(header) xml_attr(xml_find_all(E$row[[which(headers == header)[1]]], "./a"), "href")
    img.title <- function(header) xml_attr(xml_find_all(E$row[[which(headers == header)[1]]], "./img"), "title")
    text2.num <- function(header) as.numeric(xml_text(E$row[[which(headers == header)[2]]])) # assumed in the 2nd header col
    
    ## Parse each schedule row
    sched.lst <- lapply(seq_along(grid), function(row){

        ## Set current row with the schedule to parse with the *text*() funcs defined above
        E$row <- grid[[row]] 
        sched <- list()
        
        sched$desc <- a.text('Exam session description')
        internals <- a.href('Exam session description') # has internal pars
        sched$esse3SchedID <- str_match(internals, "APP_ID=([^&]+)")[,2]

        sched$datetime <- .parse.time.eu(text('Classroom date time'))
        sched$dateEU <- .eudate(sched$datetime)  
    
        sched$enrol.status  <- img.title('Enrolled students')
        num <- text2.num('Enrolled students')
        sched$enrol.num <- if(is.na(num)) 0 else num 
        sched$enrol.green  <- sched$enrol.status == "Registrations closed"
        
        sched$graded.status <- img.title('Results entered')
        num <- text2.num('Results entered')
        sched$graded.num <- if(is.na(num)) 0 else num
        sched$graded.green  <- sched$graded.status == "Results input closed"
        
        sched$recorded.status <- img.title('Records generated')
        num <- text2.num('Records generated')
        sched$recorded.num <- if(is.na(num)) 0 else num
        sched$recorded.green  <- sched$recorded.status == "Recordings closed"

        ## Return row as list 
        sched
    })

    ## Return a unique data.frame
    Reduce(rbind, lapply(sched.lst, data.frame))
        
}

setSched <- function( # Set entry as global `CurSchedule' and set related ESSE3 sitting ID as global `CurSchedule.e3'
                     entry # Schedule entry number as from getSchedules() 
                     ) {
    
    G <- SIENA
    if(is.null(G$Schedules)) stop("Please, run getCourses(), setCourse() and getSchedules() first")
    G$CurSchedule <- entry
    scheduleid <- G$Schedules$esse3SchedID[entry]
    #G$e3CoursePars[[G$CurCourse]]["APP_ID"] <- scheduleid
    G$CurSchedule.e3 <- scheduleid # The field is set manually to avoid a reset every getCourses()
    G$Schedules[entry, "dateEU"]
}

getCurrent <- function(){ # Get current course name and schedule (EU date)

    G <- SIENA

    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")

    name <- G$Courses$Course[[G$CurCourse]]
    sched <- G$Schedules$dateEU[G$CurSchedule]
    #sched <- sub(".+=", "",  sched)

    c(name=name, schedule=sched)
     
}

getSched.details <- function( # Get details for the default exam schedule or a given schedule entry number or date
                         entrynum = NULL,  # if both entrynum and date are NULL, SIENA$CurSchedule is used 
                         date = NULL
                         ){

    G <- SIENA

    ## Consistency 
    if(is.null(G$CurCourse)) stop("Please, set course first")
    if(is.null(entrynum) && is.null(date) && is.null(G$CurSchedule))
        stop("Please, set a current schedule first or pass the schedule entry number or date")
    if(!is.null(entrynum) && !is.null(date)) 
        stop("Please, give me whether the schedule entry number or its date")    

    ## Get schedules
    scheds <- .getSchedules()

    ## Set entrynum 
    dates  <- .parse.date(scheds$datetime)
    if(is.null(entrynum)) entrynum <- G$CurSchedule
    if(!is.null(date)) entrynum <- which(dates == date)
    if(length(entrynum) == 0)
        stop("Unable to find a schedule on ", date, " for ", coursename)

    sched <- scheds[entrynum, ]

    ## Add course name after sitting desc
    cname <- getCurrent()['name']
    sched <-  append(sched, list(course = cname), 1)

    ## Pretty print
    sched <- t(list2DF(sched))
    colnames(sched) <- " "
    rownames(sched) <- tools:::toTitleCase(gsub("\\.", " ",  rownames(sched)))
    sched       

}

getSched.studs <- function(# Get ESSE3 data for current course and schedule
                         short = FALSE, # only fullname, student.email (unless email = FALSE) , result, accept-flag
                         personal = TRUE # If true add email, birth date, an tax codes, from student personal page
                         ){ 

    G <- SIENA
    
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")

    examDateEU <- G$Schedules[G$CurSchedule, 'dateEU']
    message(sprintf("Querying %s on %s", G$Courses[G$CurCourse, "Course"], examDateEU))

    ## Get Enrol page
    esseData <- "CalendarioEsami/ListaStudentiEsame.do"
    # esseData <- .query(esseData, .getin("CDS_ID"), .getin("AD_ID"), scheduleid, examDateEU)
    esseData <- .query(esseData, .getin("CDS_ID"), .getin("AD_ID"), .getin("APP_ID"))#, examDateEU)

    html <- read_html(esseData)

    ## Test enrolled students
    enrolled.pos  <- grep("Total number of enrolled students:", xml_find_all(html, "//th[@class='tplMaster']"))    
    enrolled <- xml_text(xml_find_all(html, "//td[@class='tplMaster']")[enrolled.pos], trim = TRUE)
    if(length(enrolled) == "0") stop("There are no student enrolled!")

    ## Get enrolled students grid
    studs.tab <- xml_find_all(html, "//table[@class='detail_table']")
    studs <- .htmlTab2Array(studs.tab)

    ## Obtain Accepts flags (traffic lights)
    fmap <- list(accepted = "Accettato", seen = "Visionato", notaccepted = "Non accettato", notseen = "Non visionato")
    flags <- paste("@title=", sQuote(fmap, q=FALSE), collapse = " or ")    
    flags <- xml_find_all(studs.tab, paste("./tr/td/img[", flags, "]"))
    flags <- factor(xml_attr(flags, "title"))
    levels(flags) <- fmap
    
    ## Select and improve translations
    fmap <- c(
        "Reg. Date"            , "enrol-day" ,      
        "Student ID"           , "stud-id"   ,      
        "Family name and Name" , "fullname"  ,      
        "Family name and Name" , "stud-notes"  ,      
        "Cod. AD"              , "course-id" ,      
        "Cod. CdS"             , "program-id",      
        "Year att."            , "enrol-year",      
        "CFU"                  , "credits"   ,      
        "Result"               , "result"    ,      
        "Result"               , "accept-flag",
        "Svolg. Esame"         , "exam-type"        
    )
    fmap <- setNames(as.data.frame(matrix(fmap, ncol=2, byrow =TRUE)), c('old', 'new'))

    orig.names  <- trimws(as.character(studs[1,]))
    studs <- studs[, orig.names %in% fmap$old]
    names(studs) <- fmap$new
    studs <- studs[-1, ]
    rownames(studs) <- NULL
    
    studs$'accept-flag'  <- if(length(flags)) flags else NA
        
    ## Find Student personal page links
    stud.nodes <- .htmlTab2Array(studs.tab, toText = FALSE)[-1]
    fullname.pos <- which("Family name and Name"  == orig.names)[1]
    fullnames.col <- lapply(stud.nodes, `[[`, fullname.pos)
    stulinks <- sapply(sapply(fullnames.col, xml_find_all, "./a"), xml_attr, "href")
    esse3studid <- sapply(regmatches(stulinks, regexec("(^.+\\?STU_ID=)([^&]+)", stulinks)), `[`, 3)
        
    ## Download email, birth, and tax code from personal student pages
    if(personal){
        message("Retrieving ESSE3 birthdates and emails...")
        studata <- lapply(stulinks, function(stupag){
            ##        stupag <- stulinks[1] #debug
            stupag <- .query.split(str_replace(stupag, "^auth/docente/", "")) 
            stupag <- read_html(paste(stupag, collapse = " "))    
            l <- xml_text(xml_find_all(stupag,  "//div[@id='esse3old']/table/tr[3]/td/table/tr[1]/td/table/tr/th"))
            d <- xml_text(xml_find_all(stupag,  "//div[@id='esse3old']/table/tr[3]/td/table/tr[1]/td/table/tr/td"), trim=TRUE)
            names(d) <- l
            message(d["e-mail:"])
            c(d["Data di Nascita:"], d["e-mail:"], d["Codice Fiscale:"])
        })

        studata <- Reduce(rbind, studata, NULL) # null avoids dim reduction 
        colnames(studata) <- c('student.birth', 'student.email', 'tax.code')
        studs <- cbind(studs, studata, esse3studid)
        message()
    }

    
    email  <- if(personal == FALSE) NULL else "student.email"
    if(short) {
        studs[, c("fullname", email, "result", "accept-flag")]
    } else studs
    
}

getPending <- function(){ # Get traffic lights data for due exams 

    G <- SIENA

    courses <- getCourses(prompt = FALSE)
    courses.entries  <- courses$Entry    
    oldcourse <- G$CurCourse
    
    for(curcourse in courses.entries){

        setCourse(curcourse)
        scheds <- .getSchedules()

        ## Remove rows with future dates
        scheds <- scheds[scheds$datetime <= Sys.time(), ]

        ## Remove rows with sittings not subject to recording 
        scheds <- scheds[scheds$recorded.status != "Verbalizzazioni non previste", ]
        
        scheds.df <- data.frame(date = .eudate(scheds$datetime), time = format(scheds$datetime, "%H:%M"),
                                graded.green = scheds$graded.green,
                                recorded.green = scheds$recorded.green
                                )

        ## Select only rows where grades or records are non-green
        non.green <- scheds.df$graded.green == FALSE | scheds.df$recorded.green == FALSE
        scheds.df <- scheds.df[non.green,]

        ## Pretty print
        message("\n", courses$Course[curcourse])
        if(nrow(scheds.df) > 0) {
            print(scheds.df, row.names = FALSE)
        } else message("No red flags found.")        
    }
}

getSummary <- function( # Enrolled students for each course and for first available sitting from current date or a user given date
                       fromDate = Sys.Date()){

    smry <- t(sapply(seq_along(unique(getCourses(prompt = FALSE))), function(cur){
        course <- setCourse(cur)
        dates <- as.Date(getSchedules(prompt = FALSE)$datetime) # dates are descending
        enrolcount <- paste("No sitting available from", fromDate)
        sitdate <- NA
        which.dates <- which(dates >= fromDate)
        if(length(which.dates)){
            sitdate <- setSched(max(which.dates))
            enrolcount <- getSched.details()['Enrol Num',]
        }
        c(paste(as.Date(sitdate, "%d/%m/%Y")), course, enrolcount)
    }))

    message("Asked date ", fromDate, "\n")
    message("Enrolled Students\n",
            paste(apply(smry, 1, paste, collapse=" "), collapse = "\n"))

}


#################################################################
##                           Grading                           ##
#################################################################


testmacs.postGrades <- function(# Find and post Testmacs grade CSVs
                       testmacs.dir, # Tesmacs output dir with course answers, results etc. 
                       gradescsv     # file name of the CSV file with grades 
                       ){
### You need "official-name.txt" in the same directoy as grades files with the official ESSE3 course name

    G <- SIENA
    
    ## Check we are logged and set course and session
    if(is.null(G$e3Handle)) stop("Please, login().")  
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")
    
    ## Get Testmacs results
    testmacs <- .testmacs.results(testmacs.dir, gradescsv)

    ## Upload grades
    x <- lapply(testmacs, function(course){
        ofname.txt <- file.path(course['dir'], "official-name.txt")
        if(!file.exists(ofname.txt)) stop(sprintf("A file was expected with the official ESSE3 course name:\n%s",
                                                        ofname.txt), call. = FALSE)
        indir.name <- toupper(readLines(ofname.txt))[1]
        cur.name <- toupper(G$Courses$Course[G$CurCourse])
        if(indir.name != cur.name)
            stop("Current course is set to ", G$Courses$Course[G$CurCourse], ",\nbut course dir refers to ", indir.name,
                    ".\nAdjust the former with setCourse() or the latter with testmacs.postGrades()")
        message("Reading grades for ", course['name'], " = ", cur.name)
        if(!file.exists(course['file'])) stop("Unable to find grade file\n", course['file'])
        message("Uploading grades to ", course['name'])
        postGrades(csv.full = course['file'])
    })
}

postGrades <- function(# Post grade from csv data.frame using matching email
                       csv.work = "grades.csv", # results file name in SIENA$workdir
                       csv.full = NULL         # results full path, alternative to csv.work. 
                       ){ 
### If not NULL, csv.full has priority over csv.work.
### grade.R sets a NA grade for missing exam-IDs, which means a failed exam (INSUFFICENTE), mapped as 0 by ESSE3 
### CSV format: "given-name","family-name","student-id","student-birth","seat-ex-file","exam-id","grade"
### The student.id column needs to be the ESSE3 student email


    G <- SIENA
    
    ## Check we are logged and set course and session
    if(is.null(G$e3Handle)) stop("Please, login().")  
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")


    ## Get grades
    if(is.null(csv.full)) {
        if(!dir.exists(G$workdir)) stop("Unable to find the global workdir\n", G$workdir)
        workdir <- path(G$workdir)
    }       
    csvfile <- if(!is.null(csv.full)) csv.full
               else  file.path(workdir, csv.work)
    if(!file.exists(csvfile)) stop("Unable to find grade file\n", csvfile)
    workdir <- dirname(csvfile)
    csv <- read.csv(csvfile)

    
    ## Inform and wait for slow connections
    examDateEU <- G$Schedules[G$CurSchedule, 'dateEU']
    message(sprintf("In 5 seconds, grading %s on %s", G$Courses[G$CurCourse, "Course"], examDateEU))
    Sys.sleep(5)

    ## Build url to post grades
    gradeurl <- "/auth/docente/CalendarioEsami/RegistrazioneEsitiEsameSubmit.do"
    gradeurl <- paste0(G$esse3url, gradeurl)
    
    ## Get ESSE3 student data (e.g. emails)
    essedata <- getSched.studs()

    ## Check bad given emails
#    miss.emails <- sum(csv$email == "NIL")
#    if(miss.emails > 0){
#        print(paste(miss.emails, "missing email(s) for", G$Courses[G$CurCourse, "Course"], ":" ))
#        print( csv[csv$email == "NIL", ][1:2], print.gap=5)
#        warning(miss.emails, " missing email(s) for ", G$Courses[G$CurCourse, "Course"])
# 
#    }

    ## In Testmacs we use 'student.id' for 'email' (not so in Moodle)
    ## Until this is sorted we need to be flexible
    mail.fld <- if("student.id" %in% names(csv)) "student.id" else "email"
    
    given.emails.bad <- csv[[mail.fld]][ ! tolower(csv[[mail.fld]]) %in% essedata$student.email ] # considered absent, to be manually graded
    if(length(given.emails.bad)) warning("Wrong emails or not enrolled: ", paste(given.emails.bad, collapse = ", "))
    
    ## Get grades when email available
    grades <- sapply(essedata$student.email, function(essemail){
        pos <- grep(essemail, csv[[mail.fld]], ignore.case = TRUE)
        if(length(pos)) csv[pos , "grade"]  else "NULL"
    })
       
    ## Non-numerical filters
    ## If an email from ESSE3 is not in the graded emails, then the student was absent (ASSENTE)
    absents <- grades == "NULL"

    ## grade.R sets a NA grade for missing exam-IDs  ### obsolete now, ID is a compulsory field! Remove 
    incompletes <- is.na(grades)

    ## Numerical filters
    grades[absents]  <- NA
    grades  <-  setNames(as.integer(round(grades)), names(grades)) # names help debug
    
    ## Scores below 18 imply a "give-up" exam (RITIRATO)
    giveups <- 10 < grades & grades < 18 
    
    ## Scores below 11 imply a failed exam (INSUFFICENTE), mapped as 0 by ESSE3
    fails  <- grades < 11

    ## Remap grades
    grades[absents]     <- "ASSENTE"   # note that now everything is a string
    grades[giveups]     <- "RITIRATO"
    grades[fails]       <- 0 # appears as "INSUFFICENTE"
    grades[incompletes] <- 0 # Exam rules imply a failed exam (INSUFFICENTE) for incompletes

    ## Grading Type
    message("Retrieving grading type from submission form")
    #sched <- G$Schedules[ G$CurSchedule, ]  
    #subpage <- .query("CalendarioEsami/RegistrazioneEsitiEsameForm.do", .getin("AA_ID"),
    #                  "MIN_AA_CAL_ID=0", paste(sched$adid, sched$cdsid, sched$scheduleid, sep="&"))

    subpage <- .query("CalendarioEsami/RegistrazioneEsitiEsameForm.do", .getin("AA_ID"),
                       .getin("CDS_ID"), .getin("AD_ID"), .getin("APP_ID"), "MIN_AA_CAL_ID=0")
                             

    ## Grading types (GRUPPO_VOTO)
    ## To see available grading types, check Teaching activities > Templates, under the ham menu 
    grdtypeID <- xml_find_all(read_html(subpage), "//input[@name='GRUPPO_VOTO_APP_ID']")
    grdtypeID <- xml_attr(grdtypeID, "value")

    ## Test ESSE3 APP ID variable
    sched <- G$Schedules[ G$CurSchedule, ]
    if(paste0("APP_ID=", sched$esse3SchedID) != .getin("APP_ID"))
        stop("In ",  getCurrent()['name'], " course,\non exam scheduled on ", getCurrent()['schedule'], 
            "\nthe ESSE3 schedule ID found in the global SienaR pars and in the current schedule variable differ.",
             "\n\nPlease, run getSchedules() and setSched(entry) again.")
    
    ## Prepare postdata
    ptamp <- function(...) paste(..., sep = '&')
    nstud <- nrow(essedata) 
    pflds <- paste0("nRows=", nstud)
    pflds <- ptamp(pflds, .getin("CDS_ID"), .getin("AD_ID"), .getin("APP_ID"), .getin("AA_ID"))   
    pflds <- ptamp(pflds, "APP_LOG_ID=&SORT_ORDER=ascending&SORT_CODE=2")
    pflds <- paste0(pflds, "&MIN_AA_CAL_ID=0&VIS_VOTI=1&GRUPPO_GIUD_COD=&GRUPPO_VOTO_APP_ID=", grdtypeID)    
    
    ## stu_id
    x <- paste0("stu_id", 1:nstud)
    x <- paste(x, essedata$esse3studid, sep="=", collapse= "&")
    postdata <- paste0("&INS_ESITI=1&", x, "&")

    ## old_esito means old_grade
    x <- paste0("old_esito", 1:nstud)
    x <- paste(x, sub("-", "NULL", essedata$grade), sep="=", collapse= "&")
    postdata <- paste0(postdata, x, "&")

    ## esito means grade
    x <- paste0("esito", 1:nstud)
    x <- paste(x, grades, sep="=", collapse= "&")
    postdata <- paste0(postdata, x)
  
    postdata <- paste0(pflds, postdata)
    
    handle_reset(G$e3Handle)  # avoid conflicts last fetch from getSched.studs()
    handle_setopt(G$e3Handle,  postfields=postdata, followlocation = FALSE, timeout = 15) # timeout = <seconds>    
    n <- 0
    while(n <- n +1) {
        
        resp <- curl_fetch_memory(gradeurl, G$e3Handle)
        checkLogin(resp) # After getting Esse3 data, expiration's unlikely, but this is cheap
        hdr <- parse_headers(resp$headers)
        if(hdr[1]!="HTTP/1.1 200 OK") {
            if(n==4) stop("Unable to post after ", n, " attempts. Last error is\n", hdr[1])
            message(hdr[1], "\nSleeping 5 secs before reposting")
            Sys.sleep(5)
        } else {
            message(sprintf("Posted grades for %s on %s\nPlease. check via web app.",
                            G$Courses[G$CurCourse, "Course"], examDateEU))
            if(length(given.emails.bad))
                message("but ", length(given.emails.bad), " missing email(s) will appear as absent student(s)")
            break
        }
    }
}


cmp_csvesse3 <- function( ## Compare item in grade tab with esse3 tab and look for student ID
                         item,     # i-th row on grade tab
                         gradetab, # grade tab as data.frame
                         essetab   # esse3 exam tab + student details (birth/email)
                         ){
### Return the vector c(studid, notfound, conflict)
### The first is NA if notfound or conflict
### A conflict denpends now on same fullname (will be email)

    notfound  <- FALSE
    conflict <- FALSE
    studid    <- NA
    fullname  <- paste(gradetab$family.name, gradetab$given.name)[item]

    ## Match fullname
    matches <- essetab[essetab$fullname %in% fullname,]

    ## Based on people found, set stud ID or notfound or conflict
    count <- nrow(matches)
    if(count  == 0) notfound <- TRUE 
    if(count  == 1) studid <- matches$`stud-id`
    if(count   > 1) conflict <- TRUE 
    
    list(studid=studid, notfound=notfound, conflict=conflict)
}

testmacs.add.credits <- function(# Find Testmacs grade CSVs and add ESSE3 course credit weights
                                 maxcredits,   # the maximum number of credits possible for the course 
                                 testmacs.dir, # Tesmacs output dir with course answers, results etc. 
                                 gradescsv     # basename of the CSV file with grades to normalise 
                       ){
### See add.credits for info on weighting 

    G <- SIENA
    
    ## Check we are logged and set course and session
    if(is.null(G$e3Handle)) stop("Please, login().")  
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")
    
    ## Get Testmacs results
    testmacs <- .testmacs.results(testmacs.dir, gradescsv)

    ## Add credit weights
    x <- lapply(testmacs, function(course){
        message("Reading grades for ", course['name'])
        if(!file.exists(course['file'])) stop("Unable to find grade file\n", course['file'])
        message("Adding credit weights to ", course['name'])
        add.credits(maxcredits, csv.full = course['file'])
    })
}

add.credits <- function( # Add ESSE3 course credit weights to a Testmacs results CSV
                        maxcredits,              # the maximum number of credits possible for the course 
                        csv.work = "grades.csv", # results file name in SIENA$workdir
                        csv.full = NULL,         # results full path, alternative to csv.work. 
                        save = TRUE              # Save weighted grades to wgrades.csv and  wgrades.txt 
                        ){
### Each grade is multiplied by M/C, where C are student's credits and M are the maximum possible credits 
### In the CSV the student.id column needs to be the ESSE3 student email
### If not NULL, csv.full has priority over csv.work.
    
    G <- SIENA

    ## Check we are logged and set course and session
    if(is.null(G$e3Handle)) stop("Please, login().")  
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")

    ## Check args
    if(missing(maxcredits))
        stop("You are missing the argument 'maxcredits': the maximum number of credits possible for the course.")

    ## Get standard grades
    if(is.null(csv.full)) {
        if(!dir.exists(G$workdir)) stop("Unable to find the global workdir\n", G$workdir)
        workdir <- path(G$workdir)
    }       
    csvfile <- if(!is.null(csv.full)) csv.full
               else  file.path(workdir, csv.work)
    if(!file.exists(csvfile)) stop("Unable to find\n", csvfile)
    workdir <- dirname(csvfile)
    grades <- read.csv(csvfile)
    
    ## Get ESSE3 data with credits 
    esse3data <- getSched.studs()
    e3file <- file.path(workdir, "studata.rds")
    saveRDS(esse3data, e3file)
    ## esse3data <- readRDS(e3file)
    message("ESSE3 data saved to ", e3file, "\n")

    ## Join by results.csv and ESS3 mail 
    given.emails <- tolower(grades$student.id)
    rownames(grades) <- given.emails
    esse3.emails <- tolower(esse3data$student.email)
    e3match <- esse3data[esse3.emails %in% given.emails, ] # only ESSE3 studs in results' CSV
    rownames(e3match) <- e3match$student.email
    given.emails.good <- esse3.emails[esse3.emails %in% given.emails]
    given.emails.bad <- given.emails[! given.emails %in% given.emails.good]
    if(length(given.emails.bad)) warning("Wrong emails or not enrolled: ", paste(given.emails.bad, collapse = ", "))

    ## Change grade name to standard-grade
    nms <- names(grades)
    nms[which("grade" == nms)] <- "sgrade"
    names(grades) <- nms

    ## Add credits and weighted grade
    grades$credit <- NA
    grades[given.emails.good, "credit"] <- e3match[given.emails.good, ]$credits
    weights <- maxcredits/as.numeric(grades$credit)
    grades$grade <- grades$sgrade * weights
    
    ## Save when this is the final output
    if(save){
        grfile.csv <- file.path(workdir, "wgrades.csv")
        grfile.txt <- file.path(workdir, "wgrades.txt")
        write.csv(grades, grfile.csv, row.names=FALSE)
        write.table(grades, grfile.txt, sep=" ", row.names=FALSE)
        print(grades)
        message("\nGrades saved to \n", grfile.csv, "\n", grfile.txt)
        invisible(grades) # invisibly to avoid adding message
    } else grades
}

.testmacs.results <- function( # Get Testmacs grade CSVs
                              testmacs.dir, # Tesmacs output dir with course answers, results etc. 
                              gradescsv     # file name to find for the CSV file with grades (upload or normalise)
                       ){
### The path to gradescsv is <testmacs.dir>/<course name>-result/<gradescsv>

    if(!dir.exists(testmacs.dir)) stop("Unable to find the Testmacs dir\n", testmacs.dir)
    testmacs.dir <- path(testmacs.dir)
    resglob <- file.path(testmacs.dir, "*-results")
    resdirs <- Sys.glob(resglob)
    if(!length(resdirs)) stop("Unable to find any result dir in\n", testmacs.dir)

    lapply(resdirs, function(resdir){
        c(dir = resdir,
          file = file.path(resdir, gradescsv),
          name = basename(sub("-results$", "",  resdirs)))
    }) 
}

##################################################################
##                           Sittings                           ##
##################################################################

addSitting <- function( # Add a new sittings for courseName. Returns the sitting ID to be used when adding the committee
                       courseName, # well spelled, but with whatever formatting 
                       examdate, examtime,   
                       bookStart = Sys.Date() +1 , bookEnd = examdate - 4,   
                       results,  # 'FWA': publish before, 'FWS': no rejection, 'WEB': no results just names. 
                       examtype, # S/O Written/Oral                                                          
                       examdesc, examnote ="No notes"
                       ){ 


    ## Test pars 
    ## courseName <- "  banking  Management "
    ## examdate  <- as.Date("2021/12/31"); examtime <- "08:00"; bookStart <- as.Date("2021/08/11"); bookEnd <- as.Date("2021/08/30")
    ## results   <- "FWA" # 'FWA': publish before, 'FWS': no rejection, 'WEB': no results just names.
    ## examtype  <- "S"   # S/O Written/Oral    
    ## examdesc  <- "Sitting VI"; examnote <- "No notes"


    G <- SIENA
    
    ## Format arguments
    eval(bookEnd)
    examdate  <- format(examdate,  "%d/%m/%Y")
    bookStart <- format(bookStart, "%d/%m/%Y")
    bookEnd   <- format(bookEnd,   "%d/%m/%Y")
    time <- strptime(examtime, "%H:%M")
    if(is.na(time)) stop("\nThe string ", examtime, " cannot be interpreted as an exam time.")
    hour <- format(time, "%H")
    mint <- format(time, "%M")

    setCourse.infer(courseName)
    message("Adding sitting for ", G$Courses$Course[G$CurCourse], ". Current course set accordingly")

    ## Course shortcuts
    p <- G$e3CoursePars[[G$CurCourse]]
    progrid   <- p[['CDS_ID']] # "10224"
    yearid    <- p[['AA_ID']]  # "2020"
    courseid  <- p[['AD_ID']]  # "22602"

    ## Add non-encoded post fields. Compare with cURL --data-row via curl_unescape 
    postfields <-c(`/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/cds_id` = progrid, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/aa_cal_id` = yearid, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/ad_id` = courseid, 
               INS_TURNO = "0", COUNT_TURNI = "1",
               GG_INI_ISCR = "20",  # GG_INI_ISCR is required but seems to do nothing 
               GG_FINE_ISCR = "1", MOD_SES = "2", 
               `/WS/DataSet[@LocalEntityName='APP_LOG_DATI_WEB']/Row/cds_id` = progrid, 
               `/WS/DataSet[@LocalEntityName='APP_LOG_DATI_WEB']/Row/ad_id` = courseid, 
               TIPO_PROVA = "PF", # Prova Finale
               `/WS/DataSet[@LocalEntityName='APP_LOG_DATI_WEB']/Row[@Num='1']/app_log_id` = "", 
               CHECK_MODIFICA = "1", NEW_APP = "1", 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/data_inizio_app` = examdate, 
               hh_esa = hour, mm_esa = mint,
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/tipo_gest_app_cod` = results, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/tipo_iscr_cod_prev` = examtype, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/data_inizio_iscr` = bookStart, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/data_fine_iscr` = bookEnd, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/des`  = examdesc, 
               `/WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/note` = examnote, 
               sbmDef = "Save")  
    ## Possibly add: Instructor reserved sitting 
    ## /WS/DataSet[@LocalEntityName='APP_CAL_ESA_WEB']/Row/riservato_flg=1

    resp <- .query.post("CalendarioEsami/InserisciAggiornaAppelloCalEsaSubmit.do", postfields)

    ## Getting sitting, which can be used for later adding a committee 
    if(!resp$is.redirect) stop("A redirect was expected to the new sitting URL.")
    sittingID <- regmatches(resp$first.redirect, regexec("APP_ID=([[:digit:]]+)", resp$first.redirect))[[1]][2]
    if(is.na(sittingID)) stop("I did not receive the sittingID from the server, after the submission")
    sittingID
}
## addSitting("banking management", examdate = as.Date("2021/09/02"), "11:00", results = "FWA", examtype = "S", examdesc = "Sit. I")

addCommittee <- function( # Add a new committee for given course exam date, and possibly a sitting ID
                         course,  # well spelled string, but with whatever formatting, or course entry number
                         examdate,
                         committee,  # e.g. "John Doe, John-Robert Mac-Neil, 123456" (see .parseMember() for member strings) 
                         sittingID = NULL # Auto-found with findSittingID or the value returned while using addSitting()
                         ){

### `committee` format is a comma-separated string: <member 1>, <member 2>, <member 3>
### See .parseMember() for individual member string format

    G <- SIENA
    
    ## Find and set current corse
    setCourse.infer(course)
    message("Setting committee for ", G$Courses$Course[G$CurCourse], ". Current course set accordingly")

    ## Beyond sittingID check, findSittingID() avoids a 500 error,
    ## In fact, server wants us to pay a visit to the Sittings page, before fiddling with committees
    sittingID  <- findSittingID(examdate, compareID = sittingID)

    ## Find and add each member
    members <- strsplit(committee, ",")[[1]]
    nil <- lapply(members, .addCommittee.memb, course, examdate, sittingID)

}
## addCommittee("banking management", examdate = as.Date("2021/09/02"),  committee ="AnToNio    FaSaNo, Jon Doe", 28)


.addCommittee.memb <- function(member, course, examdate, sittingID){ # Workhorse for addCommittee()

    G <- SIENA

    membsrch <- findMember(course, examdate,  member, sittingID)
    if(any(is.na(membsrch))) stop()

    ## Set vars
    person <- membsrch$fullstr
    checkboxCode <- membsrch$checkboxCode    
    examdate.str  <- format(examdate,  "%d/%m/%Y")

    ## Add to committee
    message("Adding and verifying the member")
    postfields <-c(
        APP_ID = sittingID, 
        APP_LOG_ID = "-1", CHECK_MODIFICA = "1", DATA_ESA = examdate.str, #doc_id_presidente = "28298",
        FILTRO_COMMISSIONI = "0", 
        checkbox = checkboxCode,
        btnAssocia =  "Associa")
    postfields <- c(postfields, .comnPostdata())
    
    url <-  'CalendarioEsami/AggiungiDocentiCalEsaSubmit.do'
    resp <- .query.post(url, postfields)

    ## Verifing
    comm <- findCommittee(course, examdate,  sittingID, print=FALSE)
    msg <- paste(person, "to", G$Courses$Course[G$CurCourse], "on", examdate)
    if(any(comm$Code ==  membsrch$employeeID)) {
        message(paste("Added:", msg))
    } else {
        stop(paste("Unable to add", msg), call. = FALSE)
    }

    invisible(resp)
}

findMember <- function( # Find an instructor which can be added to a committee on given date, and possibly a sitting ID
                       course,  # well spelled string, but with whatever formatting, or course entry number
                       examdate,
                       member,  # 'John Doe' or  'John-Robert Mac-Neil' or '123456' (see below for details) 
                       sittingID = NULL # Auto-found with findSittingID or the value returned while using addSitting()
                       ){
### Invisibly return the person checkbox code in the search results (needed by .addCommittee.memb) or NA with failure


    G <- SIENA
    
    ## Parse committee string
    member <- .parseMember(member)
    
    ## Find and set current course
    setCourse.infer(course)
    message("Searching for ", paste(member, collapse = " "), " to be added to the ", G$Courses$Course[G$CurCourse],
            " exam committee. Current course set accordingly")

    ## Beyond sittingID check, findSittingID() avoids a 500 error,
    ## In fact, server wants us to pay a visit to the Sittings page, before fiddling with committees
    sittingID  <- findSittingID(examdate, compareID = sittingID)

    examdate  <- format(examdate,  "%d/%m/%Y")

    ## Search for a Member
    postfields <-c(APP_ID = sittingID, DATA_ESA = examdate, APP_LOG_ID = "-1", FILTRO_COMMISSIONI = "0", btnRicDoc = "Ricerca")
    postfields <- c(postfields, .comnPostdata(), member)
    url <- 'CalendarioEsami/AggiungiDocentiCalEsa.do'
    resp <- .query.post(url, postfields)       
    
    ## Page for search results 
    html <- read_html(paste(resp$content, collapse = " "))
    search.tab <- xml_find_all(html, "//table[@class='detail_table']")
    trs <- xml_find_all(search.tab, "./tbody/tr")
    persons  <-  lapply(trs, xml_find_all, "./td/cell")[-1]
    persons <- lapply(persons, xml_text)

    ## Too many people 
    if(length(persons)>1) {
        message("To many people under this name. Try using the employee ID (Matricola)")
        print(data.frame("Possible members:" =sapply(persons, paste,  collapse = " "),
                     check.names = FALSE), row.names =  FALSE)
    }

    ## No one  
    if(length(persons) == 0) {
        message("Unable to find the instructor ",
             if(length(member)==1) paste("Employee ID (Matricola):", member)
             else paste(tools:::toTitleCase(names(member)), member, sep=': ', collapse=", "),
             "\nCheck in case s/he is alredy member.")
    } 

    invisible(
        if(length(persons) == 1) {
            ## OK
            p <- persons[[1]]
            fullstr <-  paste(p, collapse = " ")    
            message(paste("Found:", fullstr))
            ## Return checkboxCode in search tab,  for .addCommittee.memb()
            list(employeeID = p[1], name = p[2], surname = p[3],
              checkboxCode = xml_attr( lapply(trs, xml_find_all, "./td/input")[[2]], "value"),
              fullstr = fullstr) 

        } else NA)

}
## findMember("banking management", examdate = as.Date("2021/09/02"),  member ="AnToNio   FaSaNo")

findCommittee <- function( # Find an exam committee members on given date, and possibly a sitting ID
                       course,  # well spelled string, but with whatever formatting, or course entry number 
                       examdate,
                       sittingID = NULL, # Auto-found or returned while using addSitting()
                       print = TRUE # else just return invisibly
                       ){

    G <- SIENA
    
    ## Find and set current course
    setCourse.infer(course)

    ## Beyond sittingID check, findSittingID() avoids a 500 error,
    ## In fact, server wants us to pay a visit to the Sittings page, before fiddling with committees
    sittingID  <- findSittingID(examdate, compareID = sittingID)

    if(print)  message("Searching committee members for ", G$Courses$Course[G$CurCourse], " on ", examdate, 
                       ". Current course set accordingly")

    APP_ID <- paste0("APP_ID=", sittingID)
    committee <- "CalendarioEsami/ListaDocentiAppelloCalEsa.do"
    committee <- .query(committee, .getin("AA_ID"), .getin("CDS_ID"), .getin("AD_ID"), APP_ID)
    committee <- read_html(committee)
    comm.rows <- xml_find_all(committee, "//table[@class='detail_table']/tbody/tr")
    comm.rows <-  lapply(comm.rows, xml_find_all, "./td/cell")[-1]

    commdf <- as.data.frame( t(head(sapply(comm.rows, xml_text), -1)))[, -1]
    names(commdf) <- c('Code', 'Name', 'E-mail')
    if(print) print(commdf, row.names = FALSE )
    invisible(commdf)
    
}
## findCommittee("banking management", examdate = as.Date("2021/09/02"),  28)

.parseMember <- function( # Parse a member string. Used by findMember()
                        member) {

### member format is 'name surname' or 'empoloyeeID' (Italian matricola)  (case is irrelevant)
### If a person has more (sur)names, use a dash for spaces, e.g. Mac-Neil
### While you cannot use a surname only, you can use first letters, e.g. Rob Smith.

    
    ## In case it is called from findMember()
    if(grepl(',', member)) stop("You passed a comma-separated list of members, but you can find one person at time.")

    memstr  <- trimws(gsub(" +", " ", member))
    memb <- strsplit(memstr, " ")[[1]]

    ## Check lengths
    l <- length(memb)
    if(l>2) {
        cat(memstr, sep = '\n')
        stop("Some members have more than two names. Use dashes here, rather than spaces.")
    }
    
    ## Single string which are not numbers 
    not.nums <- suppressWarnings(is.na(as.numeric(memstr)))
    if(l==1 && not.nums) {
        cat(memstr, sep = '\n')
        stop("You cannot use a single string, e.g. the surname, for a member, unless it is their employee ID.")
    }

    if(l>1) names(memb)  <- c('nome', 'cognome')
    else
        memb <- c(mat_doc = memb, cognome='', nome='')    
    memb
    
}


findSittingID <- function(examdate, compareID = NULL){ # Given a sitting date for the current course, find its internal sitting ID.
### Stop a) if none or many dates are found; b) if the found ID != compareID, unless it is NULL. 

    G <- SIENA

    if(is.null(G$CurCourse)) stop("Please, run getCourses() and  setCourse() first")

    
    ## English/Italian date
    examdate  <- format(examdate,  "%d/%m/%Y")
      
    schedules <- "CalendarioEsami/ElencoAppelliCalEsa.do"
    schedules <- .query(schedules, .getin("AA_ID"), .getin("CDS_ID"), .getin("AD_ID"))    
    html <- read_html(schedules)
    sched.tab <- xml_find_all(html, "//table[@class='detail_table']")

    ## Identify the row with matching date
    dates  <- .htmlTab2Array(sched.tab)[-1,3]
    dates <- sapply(strsplit(dates, " "), `[`, 1)
    rowpos <- grep(examdate, dates) +1 # most likely 1 (for last date) plus th
    if(length(rowpos)==0) stop("No sitting found on ", examdate)
    if(length(rowpos)>1) stop("Too many sitting found on ", examdate)
    
    ## The first row in the cell has an anchor with the sitting ID
    tab <- .htmlTab2Array(sched.tab, toText = FALSE)
    acell <- tab[[rowpos]][[1]] # first cell in matching row
    schedhref <- xml_attr(xml_find_all(acell , "./a"), "href")
    foundID <- regmatches(schedhref, regexec("APP_ID=([[:digit:]]+)", schedhref))[[1]][2]

    if(!is.null(compareID) && compareID != foundID)
        stop("You asked for ", G$Courses$Course[G$CurCourse], ", with sitting ID", compareID, " on ", examdate,
             ",\nbut I can't found it.")
    foundID 
}
## findSittingID(as.Date("2021/09/02"))
## findSittingID(as.Date("2021/09/02"), compareID=33)


#################################################################
##                           Plugins                           ##
#################################################################

### Plugin scripts have an ID, to make sure local version match remote ones.
### If you modify myplugin, use .signPlugin("myplugin") and update myplugin() with the new ID string

moodle <- function(){ # load the Moodle plugin
    idstring <- "57673a680a26066b38370f066c2a5e373f182870712b61174d6210512b"
    mscript.git <- "https://raw.githubusercontent.com/AntonioFasano/SienaR/master/moodle.R"    
    .loadPlugin("moodle", idstring, mscript.git)
}

shibboleth <- function(){ # load the Shibboleth plugin
    idstring <- "2244356b1c2d352a2b5d54590456680e2d567b0a4367380b3776286011"
    mscript.git <- "https://raw.githubusercontent.com/AntonioFasano/SienaR/master/shibboleth.R"
    .loadPlugin("shibboleth", idstring, mscript.git)
}

.loadPlugin <- function( # Load the a plugin with a given ID-string and URL
                        plugname, # file basename
                        idstring, # To check a possible local version is the same as the remote one
                        url){ 

    G <- SIENA

    script.url <- url
    basename <- paste0(plugname, ".R")
    script.path  <- file.path(dirname(G$mainpath), basename)
    if(file.exists(script.path)) {
        tail <-  tail(readLines(script.path))
        verified <- any(grepl(paste("###", idstring), tail, fixed = TRUE))
        if(!verified) stop("The script\n", script.path, "\ndoes not seem original. ",
                           "If is a file of yours, move it in a safe place and try again.") 
    } else {
        message(tools:::toTitleCase(plugname), " plugin not found. Downloading from:\n", script.url)
        curl_download(script.url, script.path)
    }
    message("Sourcing ", tools:::toTitleCase(plugname))
    source(script.path)

}

.signPlugin <- function(plugname){ # Sing local plugin

    G <- SIENA

    script.url <- url
    basename <- paste0(plugname, ".R")
    script.path  <- file.path(dirname(G$mainpath), basename)
    script.txt <- readLines(script.path)

    script.txt <- head(script.txt, -1)
    idstring <- .idstring(script.txt)
    comm <- paste(c("### ", as.character(idstring)), collapse = "")
    new.script <- c(script.txt, comm)
    ans <- readline(paste0("Update ", basename, " ID string? (Y/any) "))
    if(ans == "Y") writeLines(new.script, script.path) 
    message("ID string in ", basename, " overwritten.\nManually update idstring in ",
            paste0(tools::file_path_sans_ext(basename), "() with:\n"),
            idstring)
}
## .signPlugin("shibboleth")


.idstring <- function( # This is not a real hash 
                  text,     
                  len = 29, # length of the idstring
                  pad = 13  # pad byte as a decimal 
                  ){
    

    if(length(text) > 1) text <- paste(text, collapse = "\n")
    
    raw <- charToRaw(text)
    ## If text length not a multiple of len fill with "pad"
    raw <- c(raw, rep(as.raw(pad), len - length(raw) %% len)) 

    ## Split in vectors of length len and xor them
    raw <- split(raw,  rep(1:(length(raw)/len), each = len))
    Reduce(xor, raw)    
}
## .idstring("Hello World")  == .idstring("Hello\nWorld") 
## .idstring("Hello\nWorld") == .idstring(c("Hello", "World"))


version <- function(changes = FALSE){

    G <- SIENA    
    changelog <- file.path(dirname(G$mainpath), "changelog.md")
    txt <- readLines(changelog)

    secs <- grep("^#", txt)
    if(length(secs) == 0) stop(changelog, " is corrupt.")
    
    firstsec <- secs[1]
    endsec <- if(length(secs) > 1) secs[2] -1 else length(txt)

    version <- sub("^# +", "", txt[firstsec])
    sectxt <- head(txt, endsec)

    ## Cut blank tail
    rle <- rle(grepl("^ +|(^$)", sectxt))
    n <- length(rle$values)    
    if(rle$values[n]){ # final blank found
        cut <- rle$lengths[n]
        sectxt <- head(sectxt, -cut)
    }

    ## Pretty print
    sectxt <- c(sectxt, "", paste("Read",  changelog))
    wrap <-strwrap(sectxt, options("width")$width - 10)
    if(changes) message(paste(wrap, collapse = "\n")) else message(version)
    
}


##################################################################
##                         Date Helpers                         ##
##################################################################

.parse.date <- function(datevec){ # Convert strings in 'datevec' with  .parse.date.eu() and non-string with as.Date    
    x <- lapply(datevec, function(x)
        if(is.character(x)) .parse.date.eu(x) else as.Date(x))
    Reduce(c, x)
}

.parse.date.eu <- function(datevec) { # Convert strings like dd-mm-yyyy, in a  vector, to Date class. Seps can be on of '/ - .'.
### Return as soon as at least a date is not NA using the test order: '/ - .'  

    as.Date(.parse.time.eu(datevec))
}

.parse.time.eu <- function(timevec) { # Time string vector, like "dd-mm-yyyy hh:mm", to posix class. Date seps one of '/ - .'.
### Return as soon as at least a date is not NA using the test order: '/ - .'  

    ## Add time for date only. An extra time string would be ingnored 
    timevec <- paste(timevec, "00:00")
    
    timep <- strptime(timevec, "%d/%m/%Y %H:%M")
    if(any(!is.na(timep))) return(timep)

    timep <- strptime(timevec, "%d-%m-%Y %H:%M")
    if(any(!is.na(timep))) return(timep)

    strptime(timevec, "%d.%m.%Y %H:%M")
}

.eudate <- function(datevec) { # Date vector to EU string vector with '/' sep.
    format(datevec, "%d/%m/%Y")    
}

#################################################################
##                         XML Helpers                         ##
#################################################################

.htmlTab2Array <- function( # Does what it say but doesn't work yet for table with tbody tags
                          htmltab, # A xml_nodeset representing a table
                          toText=TRUE 
                          ){
### Convert an HTML table with TD spans to a list
### Logic:
### Create a TRS: list of a htmltab's TRs via xml_find_all(...)
### Add to each TRS cell R attributes emulating HTML col/rowspan attributes
### Create SPANNEDTRS: an empty version of TRS filled with NA
### Loop TRS cells, and find the first free cell on the related SPANNEDTRS row
### Copy looped TRS cell to the free cell and span as from cell span attributes


    ## Create row list with colspan and rowspan attributed for cells 
    trs <- xml_find_all(htmltab, "./tr")
    trs <- lapply(trs, xml_find_all, "./td|./th")
    trs <- lapply(trs, function(row) {
        for(i in seq_along(row)){
            attr(row[[i]], "rowspan")  <- .span2int(row[[i]], "rowspan")
            attr(row[[i]], "colspan")  <- .span2int(row[[i]], "colspan")
        }
        row})

    ## Create an empty copy of trs
    nrows <- length(trs)
    ncols <- max(sapply(trs, length))
    spannedtrs <- rep(list(NA), ncols)
    spannedtrs <- rep(list(spannedtrs ), nrows)

    ## Loop trs and copy to free spannedtrs cells with spanning
    for(i in 1:nrows){
        row <- trs[[i]]
        for(j in seq_along(row)){

            td <- row[[j]]            
            rs <- attr(td, "rowspan")
            cs <- attr(td, "colspan")

            td.col <- .firstNA(spannedtrs[[i]]) # find first free cell on row
            
            for(rspan in 1:rs) 
                for(cspan in 1:cs)
                    spannedtrs[[i+rspan-1]] [[td.col+cspan-1]] <- td 
        }
    }

    if(toText)
        as.data.frame(t(sapply(spannedtrs, sapply, xml_text))) else spannedtrs
}

.span2int <- function( # Convert TD/TH rowspan or colspan attributes to an integer. Used by .htmlTab2Array
                     td,  # TD or TH node
                     type # "rowspan" or "colspan" 
                     ){ 
    span <- xml_attr(td, type)
    span <- ifelse(!nzchar(span) || span == "0" || grepl("%", span), 1, span)
    as.numeric(span)
}
    
.firstNA <- function( # Used by .htmlTab2Array
                    vec) which(sapply(vec, is.na))[1]


#################################################################
##                         File system                         ##
#################################################################

path <- function(..., ext = ""){
    
    win <- .Platform$file.sep == "\\"
 
    ## Join as Unix 
    p <- paste(..., sep = "/")
 
    ## Unixify
    if(win)  p <- gsub("\\\\", "/", p)
 
    ## Uniquify 
    p <- gsub("/+", "/", p)
 
    ## Add ext
    if(nzchar(ext)) p <- paste(p, ext, sep=".")
 
    ## No trail slash 
    p <- sub("/$", "", p)
 
    ## Back to win, if needed  
    if(win) gsub("/", "\\\\", p)  else p
 
}
