## Download Moodle responses (or use local CSV), generate grades and upload to ESSE3, via email join.
## The Moodle file is expected to have a positive/negative value for correct answers or a dash.
## Credits weighted grades can be obtained via ESSE3 credits. 

## Typical workflows: 
## moodle() > login.moodle() >  getResponses(quizID) > scores() >  grader()
## grader.credits(maxPossible)  # weight with credits 
## anonymise(grader(save = FALSE))
## resps <- read.csv(file.path(SIENA$workdir, "grades.csv")) ; postGrades(resps)

if(!exists("SIENA")) stop("This script is a plugin for the main Siena script. Run the latter, before this.")
SIENA$moodleurl    <- NULL
SIENA$moodCredents <- NULL 
SIENA$moodHandle   <- NULL
SIENA$moodRestToken <- NULL
SIENA$goodans  <-  2  # correct answer
SIENA$badans   <- -1  # wrong answer. Set to 0 for no penalty
SIENA$skipans  <-  0  # skipped answer

login.moodle  <- function(onelog = FALSE){ # login to Moodle 

    G <- SIENA
    .internetOK()

    ## Check endpoint
    if(is.null(G$moodleurl)) stop("Hi there. You forgot to set the Moodle endpoint variable. Read the docs, please.")
    G$moodleurl <- sub("/$", "", G$moodleurl)
    message("Log in to ", G$moodleurl)

    ## Set global credentials 
    if(onelog) G$moodCredents <- G$e3Credents
    if(is.null(G$moodCredents)) G$moodCredents <- charToRaw(.cred.prompt())     
    credlst  <- .cred.split(G$moodCredents)
    creds <-  paste0("username", "=", curl_escape(credlst$user), "&",
                     "password", "=", curl_escape(credlst$pw))
    creds.post <- charToRaw(creds) # because other post fields below are raw 

    ## Set Moodle login url and get token 
    h <- new_handle()
    logurl <- paste0(G$moodleurl, "/login/index.php?lang=en")
    resp <- curl_fetch_memory(logurl, h)
    m <- regexec("logintoken\" +value=\"([^\"]+)", rawToChar(resp$content))
    logintoken <- regmatches(rawToChar(resp$content), m)[[1]][2]

    ## Add login token to credentials
    esctok.v <- curl_escape(c("logintoken", logintoken))
    esctok   <- paste0(esctok.v, collapse = "=")
    rawtok   <- charToRaw(paste0(esctok, "&"))  
    logpars <- c(rawtok, creds.post)
    
    ## Post Credentials with login token 
    handle_setopt(h, postfields=logpars)
    resp <- curl_fetch_memory(logurl, h)

    ## Test
    checkLogin.moodle(resp, first=TRUE)

    ## On success return handle 
    G$moodHandle <- h

    ## Obtain Moodle restful toke
    resturl <- paste0(G$moodleurl, "/login/token.php?service=moodle_mobile_app&", creds) 
    resp <- curl_fetch_memory(resturl)
    json <- rawToChar(resp$content)
    G$moodRestToken <- strsplit(json, "\"")[[1]][[4]]    

    ## Bash tests 
    ## echo -n "username=username&password=password:" ; read -s usrpw
    ## logurl=paste moodleurl/login/index.php
    ## parsed=$(curl -c cookies.txt $logurl | grep "logintoken")
    ## ltok=$(echo $parsed | grep logintoken | cut -d\" -f6)
    ## curl -b cookies.txt -c cookies.txt -d "logintoken=$ltok&$usrpw" $logurl
    ## curl -s -b cookies.txt $logurl |  grep "/login/logout.php" --colour
}

setCreds.moodle <- function(){ # Ask and set Moodle creds.     
    G <- SIENA
    G$moodCredents <- charToRaw(.cred.prompt()) 
}
    
checkLogin.moodle <-  function(resp, first=FALSE){
    testkey <- "/login/logout.php"
    succ <- grepl(testkey, rawToChar(resp$content), fixed = TRUE)
    if(succ) {
        if(first) message("You are now logged in to Moodle.")
    } else {
        if(first) {
            stop("Endpoint or credentials might be wrong. Check and run setCreds.moodle()")
        } else {
            stop("Connection expired. Use login()")
        }
    }
}

.makeOutdir <- function (){ # Create workdir if missing

    G <- SIENA

    if(is.null(G$workdir)) stop("Hi there. You forgot to set the workdir variable. Read the docs, please.")
    workdir <- path.expand(G$workdir)  
    dircreate <- if(!dir.exists(workdir)) {
                     dir.create(workdir)
                     TRUE } else FALSE
    if(!dir.exists(workdir)) stop("Unable to create ", G$workdir, "\n Perhaps you are not privileged to do this.")

    if(dircreate) message("Successfully created\n", normalizePath(workdir), "\n") else 
                  message("The workdir is:\n", normalizePath(workdir), "\n")
}

getResponses <- function( # Download response CSV in SIENA$workdir
                          report # Moodle response report URL, as a string, or quiz ID, as a string or number
                         ){

    G <- SIENA
    
    ## Validate func arg
    if(missing(report)) stop("This function wants the quiz report URL or ID.")
    isurl <- grepl("^http", report) 
    isID <- suppressWarnings(!is.na(as.numeric(report)))
    if(!isurl & !isID) stop(report, "\ndoes not seem a URL nor a report ID")

    ## Create output dir
    .makeOutdir()
    
    ## Extract quiz ID
    quizID <- if(isurl) {
                   m <- regexec("&id=([0-9]+)", report)
                   regmatches(report, m)[[1]][2]
              } else as.character(report)

    ## Make download url 
    repurl <- paste0(G$moodleurl, "/mod/quiz/report.php?download=csv&mode=overview&slotmarks=1")
    attempts <- "&attempts=enrolled_with" # that is: all users who have attempted the quiz  
    quizID.par <- paste0("&id=", quizID)
    repurl <-  paste0(repurl, attempts, quizID.par)
    outfile  <-  file.path(G$workdir, "responses.csv")
    .internetOK()

    message("Downloading response CSV to:\n", outfile, "\n")
    curl_download(repurl, outfile, handle = G$moodHandle)

    ## Empty file?
    if(!nzchar(readLines(outfile, warn = FALSE)[1]))
        stop("The response file was downloaded to\n ", outfile, "\n but it is empty.")
 
    ## If connection expired, you get status 200 and an HTML file with the errors 
    first <- readLines(outfile, n=1)
    error <- isTRUE(grepl("<!DOCTYPE html>", first)) # assuming Internet OK, if session is gone, we just get an HTML file
    if(error) {
        checkLogin.moodle(curl_fetch_memory(G$moodleurl, G$moodHandle))
        stop("A problem occurred while downloading response file", #  checkLogin should stop us before this
             "\nYou may check your report URL/ID or connection and try another attempt.")
    }
}

.readResp <- function( # Read CSV response file
                     csvpath = NULL # if NULL use responses.csv in in SIENA$workdir
                     ){

    G <- SIENA

    if(is.null(csvpath)) csvpath <-  file.path(G$workdir, "responses.csv")
    if(!file.exists(csvpath)) stop("I can't file the response file\n", normalizePath(csvpath, mustWork = FALSE))
    
    read.csv(csvpath, check.names=FALSE)
}

scores <- function( # Import and reweight Moodle responses based on goodans, badans, skipans weights in SIENA
                   csvpath = NULL, # if NULL use responses.csv in in SIENA$workdir
                   save = TRUE     # Save to space separated file 
                   ){

    G <- SIENA

    csvresp <- .readResp(csvpath)

    ## Moodle scores' grid in a char matrix 
    anames <- grep("Q. 1 ", names(csvresp))
    anames <- seq(anames, ncol(csvresp))
    scores.mod <- head(as.matrix( csvresp[, anames]), -1)

    ## Reweight score with our weights
    weighting <- function(x) ifelse(x=="-", G$skipans, ifelse(as.numeric(x)>0, G$goodans, G$badans))  
    ## weighting <- function(x) ifelse(x=="-", 0, ifelse(as.numeric(x)>0, 2, 0))  # no  penalties
    scores <- apply(scores.mod, 1:2, weighting)
    colnames(scores) <- seq_along(colnames(scores))

    ## Add Moodle emails for pretty print
    moodlemail <- head(csvresp$"Email address", -1)
    scores <- setNames(data.frame(moodlemail, scores),
                       c("moodlemail", colnames(scores)))

    ## Save when this is the final output
    if(save){
        scfile <- file.path(G$workdir, "mscores.csv")
        write.csv(scores, scfile, row.names=FALSE)
        print(scores)
        message("\nScores saved to ", scfile)
        invisible(scores) # invisibly to avoid adding message
    } else scores

}

grader <- function( # Import Moodle responses, reweight by goodans, badans, skipans weights in SIENA and grade
                   csvpath = NULL, # if NULL use responses.csv in in SIENA$workdir
                   save = TRUE     # Save to space separated file 
                   ){
### See scores() individual response scores 

    G <- SIENA

    ## Get score grid
    scores <- scores(csvpath, save = FALSE)
    scgrid <- scores[, -1]
    
    ## Compute each student's mark
    marks <- apply(scgrid, 1, sum)
    marks <- data.frame(email = scores$moodlemail, grade = marks)

    ## Compute good/bad ratio
    goodbad <- apply(scgrid, 1, function(sturesps) paste0(sum(sturesps == 2), "/", sum(sturesps == -1)))
    grades <- cbind(marks['email'], goodbad, marks['grade'])

    ## Save when this is the final output
    if(save){
        grfile <- file.path(G$workdir, "grades.csv")
        write.csv(grades, grfile, row.names=FALSE)
        print(grades)
        message("\nGrades saved to ", grfile)
        invisible(grades) # invisibly to avoid adding message
    } else grades
}

grader.credits <- function( # This is a version of grader() weighting grades by course credits
                           maxcredits,      # the maximum number of credits possible for the course 
                           csvpath = NULL, # if NULL use responses.csv in in SIENA$workdir
                           save = TRUE     # Save to space separated file 
                           ){
### Each grade is multiplied by M/C, where C are student's credits and M are the maxmum credits possible
        
    G <- SIENA
    
    if(is.null(G$CurCourse)) stop("Before using this function, set a current course in ESSE3\n",
                                     "To do this use:\n",
                                     "login(); getCourses(); setCourse(...code); getSchedules(); setSched(...num)\n")

    
    if(missing(maxcredits))
        stop("You are missing the argument 'maxcredits': the maximum number of credits possible for the course.")

    ## Get standard grades 
    sgrades <- grader(csvpath, save = FALSE)

    ## Get ESSE3 data with credits 
    esse3data <- getSched.studs()
    e3file  <- file.path(G$workdir, "studata.rds")
    saveRDS(esse3data, e3file)
    ## esse3data <- readRDS(e3file)
    message("ESSE3 data saved to ", e3file, "\n")

    ## Join by Moodle and ESS3 mail 
    moodlemail <- sgrades$email
    e3match <- esse3data[esse3data$student.email %in% moodlemail,] # only ESSE3 studs in Moodle
    rownames(e3match) <- e3match$student.email
    e3match <- e3match[ moodlemail,] # sort as in Moodle

    weights <- maxcredits/as.numeric(e3match$credits)
    grades <- data.frame(email = moodlemail, fullname=e3match$fullname,
                         goodbad = sgrades$goodbad, mark = sgrades$grade,
                         credits = as.numeric(e3match$credits), grade = sgrades$grade * weights)

    ## Save when this is the final output
    if(save){
        grfile <- file.path(G$workdir, "grades.csv")
        write.csv(grades, grfile, row.names=FALSE)
        print(grades)
        message("\nGrades saved to ", grfile)
        invisible(grades) # invisibly to avoid adding message
    } else grades

}


anonymise <- function( # use SHA-1 hash of student-ID
                      grades,         # output of grader() or grader.credits()
                      tax = FALSE,    # TRUE to replace ESSE3 tax code for student ID 
                      csvpath = NULL, # if NULL use responses.csv in in SIENA$workdir
                      save = TRUE     # Save to space separated file 
                      ){
        
    G <- SIENA

    ## Check and Load 'digest'
    if(! "digest" %in% rownames(installed.packages()))
        stop("This plugin requires the 'digest' pacakge. Use once:\n install.packages('digest')\n or an equivalent method.")
    library('digest')
    
    if(is.null(G$CurCourse)) stop("Before using this function, set a current course in ESSE3\n",
                                     "To do this use:\n",
                                     "login(); getCourses(); setCourse(...code); getSchedules(); setSched(...num)\n")
    
    ## Get ESSE3 data with credits 
    esse3data <- getSched.studs() 
    e3file  <- file.path(G$workdir, "studata.rds")
    saveRDS(esse3data, e3file)
    ## esse3data <- readRDS(e3file)
    message("ESSE3 data saved to ", e3file, "\n")

    ## Join by Moodle and ESS3 mail 
    moodlemail <- grades$email
    e3match <- esse3data[esse3data$student.email %in% moodlemail,] # only ESSE3 studs in Moodle
    rownames(e3match) <- e3match$student.email
    e3match <- e3match[ moodlemail,] # sort as in Moodle

    ## Debug 
    ##print(e3match[c('stud-id', 'tax.code')]) 
    
    ## Attach encrypted stud-id
    anonkey  <- if(tax) 'tax.code' else 'stud-id'
    grades <- cbind(e3match[anonkey], grades)
    grades[[anonkey]] <- sapply(grades[[anonkey]], digest, "sha1", serialize=FALSE)
    names(grades)[names(grades) == anonkey] <- paste0(anonkey, "-SHA1")
    
    ## Remove sensitive data
    rownames(grades) <- NULL
    grades$email <- NULL 
    if(!is.null(grades$fullname)) grades$fullname <-  NULL
    
    ## Save when this is the final output
    if(save){
        grfile <- file.path(G$workdir, "encgrades.csv")
        write.csv(grades, grfile, row.names=FALSE)
        print(grades)
        message("\nGrades saved to ", grfile)
        invisible(grades) # invisibly to avoid adding message
    } else grades
}

findMoodleID <- function(# Print Moodle courses with IDs, contains the current course as a substring or a given one 
                         searchfor = NULL # Use this as search string if not NULL  
                         ){
  ## The match is case insensitive

  G <- SIENA
  if(is.null(G$CurCourse) && is.null(searchfor)) stop("Please, set course first")

  courses.url <- paste0(G$moodleurl, "/webservice/rest/server.php?wsfunction=core_course_get_courses_by_field&wstoken=",
                        G$moodRestToken)

  courses.xml <- read_xml(courses.url)
  courses <- xml_find_all(courses.xml, "/RESPONSE/SINGLE/KEY[@name='courses']/MULTIPLE/SINGLE")
  courses.names <- xml_text(xml_find_all(courses, "KEY[@name='fullname']"))

  searchfor <- G$Courses$Course[[G$CurCourse]]
  found <- grep(toupper(searchfor), toupper(courses.names), fixed = TRUE)
  if(!length(found)) stop("Unable to find a course containing (ignoring case) the string:\n", searchfor)

  course.tab <- t(sapply(courses[found], function(course) {
    CourseID <- xml_text(xml_find_all(course, "KEY[@name='id']"))
    Fullname <- xml_text(xml_find_all(course, "KEY[@name='fullname']"))
    Contacts <- xml_text(xml_find_all(course, "KEY[@name='contacts']/MULTIPLE/SINGLE/KEY[@name='fullname']"))
    c(CourseID = CourseID, Fullname = Fullname, Contacts = Contacts)
  }))

  row.names(course.tab) <- course.tab[, 'CourseID']
  course.tab <- course.tab[, -1, drop = FALSE]
  print(course.tab , quote = FALSE)

}


findQuizID <- function(# Print all quizzes found for a given course ID, with related quiz ID
                         courseID
                         ){

  G <- SIENA
  course.url <- paste0(G$moodleurl, "/webservice/rest/server.php?wsfunction=core_course_get_contents&wstoken=",
                        G$moodRestToken,
                        "&courseid=", courseID)

  course.xml <- read_xml(course.url)
  modules <- xml_find_all(course.xml, "/RESPONSE/MULTIPLE/SINGLE/KEY[@name='modules']/MULTIPLE/SINGLE")
  found <- xml_text(xml_find_all(modules, "KEY[@name='modname']")) == "quiz"
  if(!length(found)) stop("Unable to find any quiz in course ", courseID)
  modules[found]

  module <- modules[found][[1]]

  quiz.tab <- t(sapply(modules[found], function(module){
    QuizID <- xml_text(xml_find_all(module, "KEY[@name='id']"))
    QuizName <- xml_text(xml_find_all(module, "KEY[@name='name']"))
    c(QuizID = QuizID, QuizName = QuizName)
  }))

  row.names(quiz.tab) <- quiz.tab[, 'QuizID']
  quiz.tab <- quiz.tab[, -1, drop = FALSE]
  print(quiz.tab , quote = FALSE)

  
}


### Please do not touch the strng below  
### K&x2kvmHdDGyQ62tZN$TmuoeGJgVjzTCqH7tl2HL3#Sdhc@njS&h3K@P
