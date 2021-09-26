## Originally intended to upload Testmacs results to ESSE3. Now it can work with Moodle.

## Browse data:
##   login() >
##   getCourses() > setCourse(Code) > getSchedules() > setSched(Code) >
##   getEsse3data()

## Add sittings
## sitID <- addSitting(courseName, examdate, examtime, bookStart, bookEnd, results, examtype, examdesc, examnote)
## addCommittee(courseName, examdate, committee, sitID)
## findMember(course, examdate, member)

## Post grades from Testmacs or Moodle (see moodle.R)
## setSched(Code) > postGrades(read.csv(resp.csv))

## Testmacs note:
## grade.R sets a NA grade for missing exam-IDs, which means a failed exam (INSUFFICENTE), mapped as 0 by ESSE3 


## Globals
SIENA <- new.env()  #Init global environment
SIENA$esse3url  <- NULL
SIENA$e3Credents <-  NULL # ESSE3 credentials as user:pass
SIENA$e3Handle <- NULL
SIENA$e3CoursePars <- NULL # List of vectors of pars to query a course, ordered by course code
SIENA$Courses <- NULL
SIENA$Schedules <- NULL
SIENA$CurCourse <- NULL
SIENA$CurSchedule <- NULL
SIENA$mainpath <-  sys.calls()[[1]] [[2]]
SIENA$workdir  <- NULL

## Libs
library('curl')
library('xml2')
library('stringr')


login <- function(){ # login to ESSE3

    G <- SIENA
    .internetOK()

    ## Check endpoint
    if(is.null(G$esse3url)) stop("Hi there. You forgot to set the Esse3 endpoint variable. Read the docs, please.")
    G$esse3url <- sub("/$", "", G$esse3url)
    message("Log in to ", G$esse3url)

    ## Set global credentials 
    if(is.null(G$e3Credents)) G$e3Credents <- charToRaw(.cred.prompt()) 
    credlst  <- .cred.split(G$e3Credents)
    userfld <- "/WS/DataSet[@LocalEntityName='LDATA']/Row[@Num='1']/u"
    passfld <- "/WS/DataSet[@LocalEntityName='LDATA']/Row[@Num='1']/p"
    creds.post  <- paste0(curl_escape(userfld), "=", curl_escape(credlst$user), "&",
                          curl_escape(passfld), "=", curl_escape(credlst$pw))
    
    ## Set and post credentials to Esse3 login url
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


.query <- function(page, ...){ # Get the HTML content of an ESSE3 instructor page via

    G <- SIENA
    if(is.null(G$e3Handle)) stop("Please, login().")
    baseurl=paste0(G$esse3url, "/auth/docente/")
    url=paste0(baseurl, page, "?", paste(..., sep="&"))
    resp <- curl_fetch_memory(url, G$e3Handle)
    checkLogin(resp)
    rawToChar(resp$content)

}

.query.split <- function(page, ...){ # Like .query() but split outputs at new lines for better grepping
    content <- .query(page, ...)
    strsplit(content, "\n")[[1]]
}


.parseInputs  <- function(form) { # Parse  form input tags
        vals <- xml_attr(xml_find_all(form, "./input[@value]"), "value")
        nams <- xml_attr(xml_find_all(form, "./input[@name]"), "name")
        names(vals) <-  nams
        vals
}
    
.getin <- function(fld){ # Extract course elements stored by .parseInputs
    G <- SIENA
    val <- G$e3CoursePars[[G$CurCourse]][fld]
    paste0(fld, "=", val) 
}
 
getCourses <- function( # Get available courses, assign a sequential code, collect course internal web data
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
    names(courses) <- c("Code", "Course", "Program")
    courses$Program <- gsub(", D.M. 270/2004", "", courses$Program)
    courses$Code <- as.integer(as.factor(courses$Course))
    if(prompt) {
        print(courses , row.names=FALSE)
        message("\nUse setCourse(Code) to select one.\n")
    }
    
    ## Store courses
    rawnames <- unique(courses$Course)
    G$Courses <- data.frame(
        Course= tools::toTitleCase(tolower(gsub(" \\[[[:digit:]]+\\]", "", rawnames))),
        Code= seq_along(rawnames),
        ESSE3ID = sapply(
            regmatches(rawnames, gregexec("\\[([[:digit:]]+)\\]", rawnames)),
            `[`, 2) 
    )

    ## Extract course data from links
    courses.raw <-  .htmlTab2Array(course.tab, toText = FALSE)
    links <- unique(lapply(courses.raw, "[[", 3)[-1])
    forms <- lapply(links, xml_find_all, "./form") 
    G$e3CoursePars <- setNames(lapply(forms, .parseInputs), G$Courses$Code)
    invisible(G$Courses)
    
}

setCourse <- function( # Set current course
                      code # Course code as from getCourses
                      ) {
    G <- SIENA
    if(is.null(G$Courses)) stop("Please, run getCourses() first")
    G$CurCourse <- code
    G$Courses$Course[code]
}

getSchedules <- function( # Get exam schedules (sessions) and internal schedule ID
                         prompt = TRUE # Prompt to use setSched() 
                         ){ 
    ## Note the schedule ID is different for different courses 

    G <- SIENA
    if(is.null(G$CurCourse)) stop("Please, run getCourses(), setCourse() first")   
    schedules <- "CalendarioEsami/ElencoAppelliCalEsa.do"
    ##aaid <- paste0("AA_ID=", G$e3CoursePars[[G$CurCourse]]['AA_ID'])
    schedules <- .query.split(schedules, .getin("AA_ID"), .getin("CDS_ID"), .getin("AD_ID"))

    datefd <- "&DATA_ESA="    
    schedules <- grep(datefd, schedules, value=TRUE) 
          
    ## Store and show 
    G$Schedules <- str_match(schedules, "&DATA_ESA=([^\"]+)")[,2]
    if(prompt) {
        message(sprintf("Schedules for %s:", G$Courses[G$CurCourse, "Course"]))
        print(data.frame(Schedules=G$Schedules, ID=seq_along(G$Schedules)), row.names=FALSE)
        message("\nUse setSched(ID) to select one.\n")
    }
    
    ## Get schedule ID
    scheduleid <- str_match(schedules, "&(APP_ID=[^&]+)")[,2]
    cdsid <- str_match(schedules, "&(CDS_ID=[^&]+)")[,2]
    adid <- str_match(schedules, "&(AD_ID=[^&]+)")[,2] 
    G$Schedules <- data.frame(schedule=paste0("DATA_ESA=", G$Schedules),
                              scheduleid, cdsid, adid, stringsAsFactors=FALSE)

}

setSched <- function( # Set cur schedule and adjust scheduleid
                     code # Schedule ID as from getSchedules()
                     ) {
    G <- SIENA
    if(is.null(G$Schedules)) stop("Please, run getCourses(), setCourse() and getSchedules() first")
    G$CurSchedule <- code
    scheduleid <- sub("APP_ID=", "", G$Schedules$scheduleid[code])    
    G$e3CoursePars[[G$CurCourse]]["APP_ID"] <- scheduleid
    sub("DATA_ESA=", "", G$Schedules[code,1])
}


getCurrent <- function(){ # Get current course name and Italian sitting date

    G <- SIENA

    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")

    name <- G$Courses$Course[[G$CurCourse]]
    sched <- G$Schedules$schedule[G$CurSchedule]
    sched <- sub(".+=", "",  sched)

    c(name=name, schedule=sched)
     
}

getEsse3data <- function(# Get Esse3 data for current course and schedule
                         long=FALSE #If true add email and birth date, but it seems that I forgot to implement it
                         ){ 

    G <- SIENA
    
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")

    examDateIta=G$Schedules[G$CurSchedule, 'schedule']
    scheduleid <- G$Schedules[G$CurSchedule, 'scheduleid']
    message(sprintf("Querying %s on %s", G$Courses[G$CurCourse, "Course"],
                    sub("DATA_ESA=", "", examDateIta)))

    esseData <- "CalendarioEsami/ListaStudentiEsame.do"
    esseData <- .query.split(esseData, .getin("CDS_ID"), .getin("AD_ID"), scheduleid, examDateIta)

    html <- read_html(paste(esseData, collapse = " ")) # change to query rather than query.split, so no paste necessary 
    studs.tab <- xml_find_all(html, "//table[@class='detail_table']")
    studs <- .htmlTab2Array(studs.tab)

    ## Obtain Accepts flags (traffic lights)
    fmap <- list(accepted = "Accettato", seen = "Visionato", notseen = "Non visionato")
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
        
    ## Download email and birth from personal student pages
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
    rownames(studs) <- NULL
    studs

}

postGrades <- function(csvpath = NULL) { # Post grade from csv data.frame using matching email
### grade.R sets a NA grade for missing exam-IDs, which means a failed exam (INSUFFICENTE), mapped as 0 by ESSE3 
### CSV format: "given-name","family-name","student-id","student-birth","seat-ex-file","exam-id","grade"
### In TESTMACS "student-id" is the email.

    G <- SIENA

    ## read CSV file to post
    if(is.null(csvpath)) csvpath <- file.path(G$workdir, "grades.csv")
    if(!file.exists(csvpath)) stop("I can't file the response file\n", normalizePath(csvpath, mustWork = FALSE))   
    csv <- read.csv(csvpath, check.names=FALSE)
    
    ## Check we are logged and set course and session
    if(is.null(G$e3Handle)) stop("Please, login().")  
    if(is.null(G$CurCourse)) stop("Please, set course and schedule first")
    if(is.null(G$CurSchedule)) stop("Please, set course and schedule first")

    ## Inform and wait for slow connections
    examDateIta=G$Schedules[G$CurSchedule,1]
    message(sprintf("In 5 seconds grading %s on %s", G$Courses[G$CurCourse, "Course"],
                    sub("DATA_ESA=", "", examDateIta)))
    Sys.sleep(5)

    ## Build url to post grades
    gradeurl <- "/auth/docente/CalendarioEsami/RegistrazioneEsitiEsameSubmit.do"
    gradeurl <- paste0(G$esse3url, gradeurl)
    
    ## Get ESSE3 student data (e.g. emails)
    essedata <- getEsse3data(long=TRUE)

    ## Check student emails in test data
    miss.emails <- sum(csv$email == "NIL")
    if(miss.emails > 0){
        print(paste(miss.emails, "missing email(s) for", G$Courses[G$CurCourse, "Course"], ":" ))
        print( csv[csv$email == "NIL", ][1:2], print.gap=5)
        warning(miss.emails, " missing email(s) for ", G$Courses[G$CurCourse, "Course"])

    }
     
    ## Get grades when email available
    grades <- sapply(essedata$student.email, function(essemail){
        pos <- grep(essemail, csv$email)
        if(length(pos)) csv[pos , "grade"]  else "NULL"
    })
       
    ## Non-numerical filters
    ## If an email from ESSE3 is not in the graded emails, then the student was absent (ASSENTE)
    absents <- grades == "NULL"

    ## grade.R sets a NA grade for missing exam-IDs
    incompletes <- is.na(grades)

    ## Numerical filters
    grades[absents]  <- NA
    grades  <-  setNames(as.integer(grades), names(grades)) # names help debug
    
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
    sched <- G$Schedules[ G$CurSchedule, ]  
    subpage <- .query("CalendarioEsami/RegistrazioneEsitiEsameForm.do", .getin("AA_ID"),
                     "MIN_AA_CAL_ID=0", paste(sched$adid, sched$cdsid, sched$scheduleid, sep="&"))
    grdtypeID <- xml_find_all(read_html(subpage), "//input[@name='GRUPPO_VOTO_APP_ID']")
    grdtypeID <- xml_attr(grdtypeID, "value")
    ## Ancient Turkish??? From API docs: 
    ## Id del gruppo voto nel caso gli ordinamenti collegati ai cds dell'appello abbiano gruppi voto differenti
    ## gruppoVotoId integer($int64) example: 1
    ## (NB: il campo è relativo ad una gestione particolare utilizzata solo da alcuni atenei)

    ## Prepare postdata
    sched <- G$Schedules[ G$CurSchedule, ]
    nstud <- nrow(essedata) 
    pflds <- paste0("nRows=", nstud)
    pflds <- paste(pflds, sched$adid, sched$cdsid, sched$scheduleid, sep="&")
    pflds <- paste0(pflds, "&AA_ID=", G$e3CoursePars[[G$CurCourse]]['AA_ID'])
    pflds <- paste0(pflds, "&APP_LOG_ID=&SORT_ORDER=ascending&SORT_CODE=2")
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
    
    handle_reset(G$e3Handle)  # avoid conflicts last fetch from getEsse3data()
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
            message(sprintf("Posted grades for %s on %s\nPlease. check via web app.", G$Courses[G$CurCourse, "Course"],
                            sub("DATA_ESA=", "", examDateIta)))
            if(miss.emails > 0)
                message("but ", miss.emails, " missing email(s) will appear as absent student(s)")
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
                         course,  # well spelled string, but with whatever formatting, or course code integer 
                         examdate,
                         committee,  # e.g. "John Doe, John-Robert Mac-Neil, 123456" (see .parseMember() for member strings) 
                         sittingID = NULL # Returned by addSitting() or findSittingID(). If missing, the latter is used 
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
                       course,  # well spelled string, but with whatever formatting, or course code integer 
                       examdate,
                       member,  # 'John Doe' or  'John-Robert Mac-Neil' or '123456' (see below for details) 
                       sittingID = NULL # Auto-found or returned while using addSitting()
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
                       course,  # well spelled string, but with whatever formatting, or course code integer 
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

setCourse.infer <- function(course){ # Get course code from a badly formatted string or from the actual integer, and set it current 

    G <- SIENA
    
    if(is.null(G$Courses)) getCourses(prompt = FALSE)
    
    if(class(course) == "numeric")
        ccode <- course
    else {
        rxname <- paste0("^", toupper(gsub(" +", " ",  trimws(course))), "$")
        ccode  <- grep(rxname, toupper(G$Courses$Course))
        if(length(ccode)==0) stop("Unable to find course named: ", course)
    }
    G$CurCourse <- ccode
    
}

findSittingID  <- function(examdate, compareID){ # Given a sitting date for the current course, find its internal sitting ID.
### Stop a) if none or many dates are found; b) if the found ID != compareID, unless it is NULL. 

    G <- SIENA
    
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
                                        #sittingID <-
    foundID <- regmatches(schedhref, regexec("APP_ID=([[:digit:]]+)", schedhref))[[1]][2]


    if(!is.null(compareID) && compareID != foundID)
        stop("You asked for ", G$Courses$Course[G$CurCourse], ", sitting ", sittingID, " on ", examdate,
             ",\nbut I can't found it.")
    foundID 
}
## findSittingID(as.Date("2021/09/02"))
## findSittingID(as.Date("2021/09/02"), compareID=33)

.query.post <- function( # Equivalent of query for post verbs, but returning full response
                       page, postfields,
                       encode=TRUE, follow = FALSE, reset = TRUE){

    G <- SIENA
    
    if(is.null(G$e3Handle)) stop("Please, login().")
    baseurl <- paste0(G$esse3url, "/auth/docente/") 
    url <- paste0(baseurl, page)

    handle_reset(G$e3Handle)
    if(encode) postfields <-  .encpostfields(postfields)  # encode as cURL --data-raw string     
    options <- list(post = TRUE, postfields = postfields, postfieldsize = nchar(postfields), followlocation = FALSE)
    handle_setopt(G$e3Handle, .list = options)    
    resp <- curl_fetch_memory(url, G$e3Handle)

    ## Parse status code of first rederict. Curl gives the last 
    firstreq.hds <- parse_headers(resp$headers, multiple = TRUE)[[1]]
    fstatus <- firstreq.hds[1]
    firstreq.status <- regmatches(fstatus, regexec("^HTTP/.+ ([[:digit:]]+) ", fstatus))[[1]][2]

    ## In case ofa login problem (session expired), we get a 200 status code (no 50x nor 300x redirect to login page)
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

moodle <- function(){ # load the Moodle plugin
    idstring <- "K&x2kvmHdDGyQ62tZN$TmuoeGJgVjzTCqH7tl2HL3#Sdhc@njS&h3K@P"
    mscript.git <- "https://...."
    .loadPlugin("moodle", idstring, mscript.git)
}

shibboleth <- function(){ # load the Shibboleth plugin
    idstring <- "nC2L35TPRsbXlqUw9Ku$GV@wrzz34$Kk*eSw#Q8LlbafywtE*bT3QGn%"
    mscript.git <- "https://...."
    .loadPlugin("shibboleth", idstring, mscript.git)
}

.loadPlugin <- function(plugname, idstring, url){ # Load the a plugin with a given ID-string and URL

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

