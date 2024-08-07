Not to be confused with the fictional [company](https://www.starwars.com/databank/sienar-fleet-systems).

SienaR is a collections of R scripts to manage exams in the ESSE3 system, which is a Student Information System used in most (if not all) Italian academic institutions.  
While SienaR is written in R, no specific knowledge of R is required to use it. 

You can do essentially two things with SienaR. 

- Browse and set exam sittings. 
- Upload exam results.  

The second feature was intended as a plug-in for my [Testmacs](https://github.com/antoniofasano/testmacs) e-learning tool for R. It is also possible by means of a plugin based on [Moodle](https://moodle.org/), which can download MCQ responses, grade them and upload grades to ESSE3.   

_Warning_  I can only test SienaR with my university ESSE3 system. The login page of yours might be customised in a non-standard way not enlisted here.

- [How to Install](#how-to-install)
- [Usage](#usage)
  - [Login](#login)
  - [Browse Sittings](#browse-sittings)
  - [Passing Arguments](#passing-arguments)
  - [Add a New Sitting Date](#add-a-new-sitting-date)
  - [Add an Exam Committee](#add-an-exam-committee)
  - [Adding Grades to an Exam Sitting](#adding-grades-to-an-exam-sitting)
- [SienaR Plugins](#sienar-plugins)
  - [Information About Plugins](#information-about-plugins)
  - [Moodle Plugin](#moodle-plugin)
    - [Setup](#setup)
    - [Usage](#usage-1)
    - [Login to Moodle](#login-to-moodle)
    - [Getting the Moodle Response File](#getting-the-moodle-response-file)
    - [Alternative Ways to Find Moodle Quiz IDs](#alternative-ways-to-find-Moodle-quiz-IDs) 
    - [Grading](#grading)
    - [Posting Grades to ESSE3](#posting-grades-to-esse3)
  - [Shibboleth Plugin](#shibboleth-plugin)
    - [Endpoints with SSO redirection](#endpoints-with-sso-redirection)
    - [Shibboleth Setup and Use](#shibboleth-setup-and-use)
- [Specifying File and Directory Paths](#specifying-file-and-directory-paths)
- [Versions, Troubleshooting, and Known Issues](#versions-troubleshooting-and-known-issues)

# How to Install

__Step 1__ Install [R](https://www.r-project.org/).  If you did already,  just make sure the version is not several years old. 

__Step 2__ Head on to R console.  
In Linux, this is as simple as typing `R` in the terminal.   
In Windows, there are several fancy GUIs to make your  experience more likeable, including my 
[BloomR](https://github.com/antoniofasano/BloomR/) 
distro, targeting finance labs. If you use the vanilla R, you should be good to go with
 
    "C:\Program Files\R\R-4.X.X\bin\R.exe"

Replace `4.X.X` with your actual version. 

In macOS, I don't know, but if you have a Mac, you probably don't need this. 

 __Step 3__  In the R console, type:

    install.packages(c('stringr', 'curl', 'xml2'))

to install the related packages. 

 __Step 4__  Download the main [siena.r](https://raw.githubusercontent.com/AntonioFasano/SienaR/master/siena.R) script.
 Just somewhere you deem fit, but make sure the local name is `siena.r`.

 __Step 5__  Identify the local [Endpoint](https://en.wikipedia.org/wiki/Web_API#Endpoints) of ESSE3 and possibly Moodle. 
 
Because ESSE3 is a self-hosted web app (like perhaps your employer mail server), you have to figure out which is the home address (endpoint). For the [Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication) method, you just go to the ESSE3 login page. Its address is similar to:
 
     https://....yourdomain.edu/.../auth/Logon.do
 
 
You strip the trailing `auth/Logon.do` and the remaining part of the string is your endpoint. Of course, the ellipses (`...`) depend on your site structure. They further qualify the place in your domain where the app is hosted, and they could be simply missing.

If you do not use Basic Authentication, this script has a plugin to support [Shibboleth Single Sign-on](https://www.shibboleth.net/products/), a somewhat common server for academic institutions. See the [related section](#Shibboleth-Plugin) section for this.

If you plan to use the Moodle, see the [Moodle Plugin](#Moodle-Plugin) section  endpoint information.


# Usage

In your R console type:

    source("/path/to/siena.R")


`/path/to/siena.R` denotes the absolute path to the file you downloaded in Step 4 above.  
In Linux, you can type it as is.  In Windows, you have to adjust the slashes. If the actual path is `C:\siena\siena.R`, you can type:

    source("C:/siena/siena.R")

or alternatively 

    source("C:\\siena\\siena.R")


Now type:


    SIENA$esse3url <- "https://yourESSE3Endpoint.edu/"


Where `https://yourESSE3Endpoint.edu/` is the your ESSE3 web app local endpoint, discussed in Step 5 above. 



Note that the console where you type the commands (more formally, you _evaluate functions_) sports an auto-completion feature. For example, if you type `sou`  and then press the  TAB key, the string will be normally completed as the function `source`. If there are more completion alternatives possible, you will be presented with them. In the latter case, you can type some extra characters, to remove ambiguities, and hit TAB again to autocomplete. This feature applies also to path and function arguments. Try it. 




## Login

Before doing anything, you have to log in to the ESSE3 web app.

To login  just type:

     login()

You are then prompted to supply you credentials in the format `username:password`, e.g. `jondoe:mystrongpass`. 

If you mistype them, or you later want to  supply alternative credentials, type: 

    setCreds()

and you will be prompted to supply the new credentials. 

The credentials are now stored as internal variables. After some time of inactivity, you can be asked to login again. In this case, `login()`  will not prompt again for credentials, but will use those internally stored. 

If you are using my [BloomR](https://github.com/antoniofasano/BloomR/) distro, then credentials are asked at the bottom of your app window  and, when typing, the usual asterisks are displayed. In vanilla R, there is no such a convenience (while it is not so difficult to implement it yourself), so pay attention to prying eyes.

When you are done with your work, the proper way to **quit** is:

    q("no")

`"no"` avoids saving your session data (e.g. credentials), which is probably what you want.
 

_Note_: There is no difference in R between single (`'`) and double (`"`)  quotes. Use what is more convenient for you. 


## Browse Sittings

The rational to browse exam sitting schedules is to first set the current course, then the sitting schedule of interest, and eventually download data.  

To see  courses<sup id="a1">[1](#f1)</sup> associated with your credentials, use:

    getCourses()
	
You get something similar to:

    Entry                                             Course                                   Program
        1                       BANKING MANAGEMENT [2001563] SCIENZE ECONOMICHE E BANCARIE [EE004] (L)
        2                     FINANCIAL ENGINEERING [107420]            FINANCE - FINANZA [EG008] (LM)
        2                     FINANCIAL ENGINEERING [107420]                      FINANCE [EG005] (LM)
        3 FINANCIAL INVESTMENTS AND RISK MANAGEMENT [107421]            FINANCE - FINANZA [EG008] (LM)
        3 FINANCIAL INVESTMENTS AND RISK MANAGEMENT [107421]           ECONOMIA/ECONOMICS [EG007] (LM)
        3 FINANCIAL INVESTMENTS AND RISK MANAGEMENT [107421]                     ECONOMICS [D254] (LM)
     
    Use setCourse(Entry) to select one.

	
The number in the first column is used to select the course. It could be repeated if the course belongs to more programmes.  
As the output of `getCourses()` suggests, we can use it to select a course by means of its related entry. Before doing this, we want to get an overview of enrolled students for the next available sittings from a given date. To achieve this, we can use a line similar to this:

    getSummary(as.Date('2021-10-04'))

This will give the next available sitting for each course starting from 04 October 2021. Note how the passed date uses the special date notation: `as.Date("year/month/day")`.  The output will be similar to the following:

    Asked date 2021-10-04
     
    Enrolled Students
    2021-10-05 Banking Management 15
    2021-10-05 Financial Engineering 10
    2021-10-05 Financial Investments and Risk Management 13
	
This implies that for the asked date the next sitting is available for each course on 05 October 2021 with the shown enrolled students. If no date argument is given to `getSummary()`, then the current day is used. 


If we want to read or make modifications to a specific course, we need to make it current. As noted before, this is done using the course entry returned by `getCourses()`. Given the output of this function just shown above, if we want to make current, say, the the Financial Engineering course, we use:

    setCourse(2)

Now, to list the scheduled exam sitting  for this course, use:

    getSittings() 

The output will be similar to:

    Sittings for Banking Management:
      Sittings Entry
     05/10/2021     1
     17/09/2021     2
     02/09/2021     3
     29/06/2021     4
     
    Use setSit(entry) to select one.


Identify the sitting entry number which interests you, and, assuming it's the last-date entry, select it with: 
   
   
    setSit(1)
	
The function returns the related date, which, based on the example above, would be 05/10/2021, and  sets it  as the current sitting.	 
If, at any time, you forget  what are the current course and sitting schedule and want to check them, use:

    getCurrent()

The output is similar to:

                                           name      sitting 
    "Financial Investments and Risk Management"  "05/10/2021" 

At this point, if you want to print the details of the current exam sitting, issue:

    getSit.details()

The output is similar to:
                                      
    Desc            "Sitting VIII"        
    esse3SitID    "94"                  
    Datetime        "2021-10-05 09:00:00" 
    dateEU          "05/10/2021"          
    Enrol Status    "Registrations closed"
    Enrol Num       "12"                   
    Enrol Green     "TRUE"                
    Graded Status   "Results input closed"
    Graded Num      "12"                   
    Graded Green    "TRUE"                
    Recorded Status "Recordings closed"   
    Recorded Num    "12"                   
    Recorded Green  "TRUE"                


The output, which could be improved in the future for readability, is roughly similar to the related row in the sitting grid of the exam sittings page in the web app. "Recording" is the  digital signature of the  grades published online, that is ESSE3 legalese. Don't confuse the `esse3SitID` sitting ID, used internally by ESSE3, with the sitting's  entry number from `getSittings()`: the latter is just a convenience to set the current sitting.   
Note that you can use this function to query any sitting for the current course. To do so, use whether the related entry number (from `getSittings()`) or its date, e.g: 

    getSit.details(2)
    getSit.details( date = as.Date("2021/09/17") )
	
Yes, `as.Date("2021/09/17")` is the formal way to use a date in R language, that is: `as.Date("year/month/day")`. There are some advantages in using the so-called ISO dates, but it is annoying to type them so and that will be simplified in the future, opting for a human date syntax. 

Now, you might want to get some information about students enrolled to the current sitting, this is done with:

    getSit.studs()


The output is quite large, so is usually split, unless you have a very large screen, and is similar to: 


       enrol-day stud-id                  fullname stud-notes course-id program-id enrol-year credits result accept-flag 
    1 25/09/2021  ******        ******************               107421      EG008  2020/2021       9     27    accepted 
    2 24/09/2021  ******        ******************               107421      EG008  2020/2021       9     25     notseen 
	.        ...     ...                       ...                  ...        ...        ...     ...    ...         ... 
	  
      exam-type student.birth                       student.email         tax.code esse3studid
    1         D    26/03/1998 *********************************** ****************      ******
    2         D    20/08/1998 *********************************** ****************      ******
	.        ...          ...                                 ...              ...         ...  
	

What you get is, by and large, the grid found on the Enrolled Students List page in the web app, plus some extra columns that are found on each personal student web page, such as the student email (which works much better than the Student ID as identifier). The function prints also the traffic light icons, denoting the grading acceptance/rejection, in textual format.   
Nota that the `esse3studid` column is an internal student ID, used  by the web app to identify the personal page of the student; you normally want the  `stud-id` column instead, 

To get personal student data, this function chases each link to student pages, which could slow down a bit the output production. To avoid this, you can use:


    getSit.studs(personal = FALSE)

which  essentially gives  the same output as the related ESSE3 page, speeding up results. If what you need is a compact print, showing just the results with the accept flags next to the student name and email, use: 

    getSit.studs(short = TRUE)

Of course, if you use the arguments `short = TRUE, personal = FALSE`, you will not get the email could, as that would require accessing  the student personal pages. 


Finally, you can make a general check of the traffic lights status with:
 
 
    getPending()
 
This function scans the exam-sittings page of each course and looks for any non-green status under the "Results entered", and "Records generated" columns. Essentially, that means you did not yet publish grades or did not digitally sign them. 


    Banking Management
           date  time graded.green recorded.green
     05/10/2021 11:00         TRUE          FALSE
     
    Financial Engineering
    No non-green label found.
     
    Financial Investments and Risk Management
    No non-green label found.

In this case, for the Banking Management exam, grades were published but not digitally signed. 

 
<b id="f1">1</b> : That is the subject that you teach, not the programme where you teach it. [↩](#a1)



## Passing Arguments

In the next sections, R functions with arguments are used.

There are two ways to supply arguments to a function. One is to use both argument names and values.  
Consider the fictitious function:

    create_sitting(course_name = "Finance", course_day = "Monday")

Because we write the arguments' names, even if we mess with positions:

    create_sitting(course_day = "Monday", course_name = "Finance")

...there is no problem. 

But this is long to write. So, if you know and remember that the first argument is the course name, you can  just write the argument value:


    create_sitting("Finance", course_day = "Monday")

And similarly, if you can remember  in second position goes the `course_day`, in the line above, you can limit yourself to type `Monday`.  


Also, remember that some arguments are optional: you pass them only if you want to override their default value. 


## Add a New Sitting Date

The function to add a sitting  has several parameters, but you don't have to use all of them.

For your convenience, I put every argument on a line of its own. When you type in the R console, this is irrelevant. 


    addSitting(
       courseName = "Financial Engineering", 
       examdate   = as.Date("2021/10/30"), 
	   examtime   = "11:30", 
       bookStart  = as.Date("2021/10/25"), 
	   bookEnd    = as.Date("2021/10/15"),
       results    = "FWA"
       examtype   = "S", # S/O Written/Oral                                                          
       examdesc   = "Sitting number ten.", 
	   examnote   = "There are no notes"
    )


`courseName`, `examdate`, `examtime` are self-explanatory. Just note that `courseName` is not case-sensitive. 
`examdate` uses  the special date notation: `as.Date("year/month/day")`. I realise that it is boring to type, and I am going to simplify this in the next versions. 

`bookStart`	and `bookEnd` are the dates between which a student can book an exam. If you don't provide them, they default to the day after you create  the sitting  and four days before the exam day. 

`results` can be one of the following: `"FWA"`, which  means  a student has the possibility to reject (or accept) the grade after the results are published; `"FWS"`, which means no rejection possible;  `"WEB"`, which  creates a special sitting without results, just a list of enrolled students. These labels are those used internally by the official web app. I should  probably make them more friendly in the future versions.

`examtype` can be `"S"`, `"O"`, resp. for written or viva voce exam.  

`examdesc` and `examnote` are a description and a note. The first one is mandatory, the second is optional. 



## Add an Exam Committee

The exam committee can be added immediately after the creation of the sitting or at a later date. In the first case, it is suggested to retrieve the ESSE3 internal sitting ID, from the function `addSitting()`, as follows:


    sid <- addSitting("Financial Engineering", examdate = as.Date("2021/10/30"), examtime = "11:30", 
                     results = "FWA", examtype = "S", "Sitting number ten.")
	
    
_Note_:  `<-`  just means `=`. If you don't like this convention, use the latter symbol.  	


`sid` is just any variable name you choose to assign the ESSE3 sitting ID value. 
You can, then,  use this variable when giving a committee to the exam sitting, with:

    addCommittee(
       course = "Financial Engineering",
       examdate = as.Date("2021/10/30"),
       committee = "John Doe, John-Robert Mac-Neil, 123456",
	   sittingID = sid
    )

If you don't have the sitting ID from the sitting function, just don't pass this argument, and  `addCommittee()` will find it by itself. 

The argument `committee` requires some usage notes. It is a comma-separated list of member names. The way member names are formatted reflects the fields used in the official web app and format is "First Family", such as for John Smith. For cases with multiple first and family names, such as Melinda Ann French Gates, you join them with a dash, therefore "Melinda-Ann French-Gates". By using the dash, we can  say that Melinda and Ann are first names, while French and Gates are family names. Finally, you can use the employee ID (Matricola), which is recognised as consisting only of numbers.  


Because things can become pretty tricky, you might want to find the person with a dedicated function. The following one can find an instructor which can be added to a committee on a given date.


    findMember(
       course = "Financial Engineering",
       examdate = as.Date("2021/10/30"),
       member   = "John S"
	)

This will give a list with all people whose first name starts with "John", and family name with "S". Including the employee ID. The output is like follows:

            Possible members:
           123456 Smith John
    123457 Smith John Robert
        123458 Sullivan John
		
Note, by the way, that here we use the internal web app format, which starts with the family name. This will be changed in the future.  	
	
At a later time, when you are all done with setting committees, you might want to check what are the members for a given sitting committee. Type something along the lines of:

    findCommittee("Banking Management", examdate = as.Date("2021/10/05"))

and you'll get a table with the employee ID, the full name, and the e-mail of each member of the exam committee for the Banking Management sitting scheduled on 05/10/2021.

## Adding Grades to an Exam Sitting

This is handled by the function `postGrades()`. That was used to automatically route grades computed in the lab by my e-learning tool, [Testmacs](https://github.com/antoniofasano/testmacs),  to the Student Management System. Due to the pandemic, this feature is temporarily halted. However, it is possible to automatically retrieve, grade and post Moodle quiz responses.

	
# SienaR Plugins

## Information About Plugins

Plugins are scripts that add extra functionalities to the main SienaR script. To use a plugin, you execute a function named after the plugin, such as:

    plugin()

The first time, this  will download the related script in the same directory as the main Siena script. The second time it will use this local file for efficiency. 

What if, in the (unlikely?) case, there is already a file of yours named exactly like the plugin? SienaR reads a special string at the bottom of the plugin script before executing it, so it would identify the misnomer and stop, warning about this problem. 


## Moodle Plugin

The Moodle plugin allows to downaload the CSV files of Moodle quiz response files; give several type of gradings, and upload grades to ESSE3. 

It is the result of the pandemic, which ruled out my my [Testmacs](https://github.com/antoniofasano/testmacs) e-learnig, requiring a physical lab. 

### Setup

There is no particular setup, but, if you want to use the Moodle plugin to download quiz results, you need to identify its local endpoint. For Basic Access Authentication, head on to the login page, which is like:

     https://....yourdomain.edu/.../login/index.php

 
Strip the trailing `login/index.php` to obtain the endpoint. Note that webmasters might have the site to omit the `index.php` string.  


If you do not use Basic Authentication, but [Shibboleth SSO](https://www.shibboleth.net/products/), see the [related section](#Shibboleth-Plugin) for this.

### Usage

To use the plugin, you just type in the console:

    moodle()

You might want to look the general [plugin info section](#Information-About-Plugins), for more insights about this init function. 

Set the endpoint for Moodle, replacing the web address below as needed:


    SIENA$moodleurl <- "https://yourMoodleEndpoint.edu/"

Note that the  `<-` is the R way to assign a value to a variable, but, if you prefer `=`, you can use it. 

Because you will work with files,  set the absolute path of your working directory, possibly with the Linux `~`, as below:


    SIENA$workdir <- "work path"


where `"work path"` denotes the path of your working directory. In particular if you are a Windows user, you might want to read the [path rules](#Specifying-File-and-Directory-Paths)


If the working directory does not exist and the path is valid, it is created. Bear in mind that SienaR functions might **overwrite** files in the working directory without warning. 



### Login to Moodle

Unless you are using local files manually downloaded, before doing anything, you have to log in to the Moodle web app.

The process if similar to ESSE3 logon, so you type:

     login.moodle()

You are then prompted to supply you credentials in the format `username:password`, e.g. `jondoe:mystrongpass`. 

If you mistype them, or you later want to  supply alternative credentials, type: 

    setCreds.moodle()

and you will be prompted to supply the new credentials. 


Chances are that you use the exact same credentials for both Moodle and ESSE3. This being the case, replace the function calls above, with:


    login.moodle(onelog = TRUE)
	
and you will not be asked to re-enter your credentials again, because the credentials stored for ESSE3 are used, if any, otherwise you are still prompted for them.   
Programmatically speaking, when you use the `onelog = TRUE` argument, SienaR looks for any stored Moodle credentials, if they are missing, it copies to ESSE3 credentials over Moodle's. If the operation is successful, on next login, whatever the boolean value of `onelog`, ESSE3 credentials will be used, as they have already been copied. This is to say that you can write: 

    login.moodle(onelog = TRUE)
    login.moodle(onelog = FALSE)

and the second time copied ESSE3 credential will be used. Of course, you can use, `setCreds.moodle()` to change stored Moodle credentials. Indeed, the `onelog` argument would be clearer if written `copy_esse3_credentials`, however that would have been too long. 


Note that the onelog feature is not the same as the [Shibboleth Single Sign-on](https://www.shibboleth.net/products/) used by the  [plugin](#Shibboleth-Plugin) of the same name, since the latter  happens at the web app level. 



### Getting the Moodle Response File


There are several types of quiz questions in Moodle. When the quiz includes only multiple-choice questions, it is possible to download Moodle students' responses, grade them, and upload grades to ESSE3.   

To download the response file for a given quiz, you should provide the Moodle quiz ID. To this regard,  visit the quiz home page or the quiz report page, and you will see that their addresses are like:


    ENDPOINT/mod/quiz/view.php?id=1234
    ENDPOINT/mod/quiz/report.php?id=1234&mode=overview

where `ENDPOINT` denotes your Moodle endpoint and `1234` denotes the quiz ID. 

At this point, if you are not interested to automatic grading, for example if you want to just download and read student essays, then you can download the responses for the quiz ID 1234 with:

    md.getResps(1234)


As an alternative, you can copy-and-paste from the browser any address that contains the string `id=1234`, such as the above report page:

    md.getResps("ENDPOINT/mod/quiz/report.php?id=1234&mode=overview")
	
and the quiz ID will be automatically recognised and extracted.


The response  file is  downloaded in the working directory, which you have set before,  as `moodle-responses.html`, and you can read it with your browser. 


If the response file consists only of multiple-choice files and you want to automatically grade the quizzes, to download the quiz 1234, you replace `md.getResps()` with:

    md.getGrades(1234)

Again, the response file is  downloaded in the working directory, at this time,  as `moodle-grades.csv`, and you can view it in Excel. 

Bear in mind that this is a real Comma Separated File (CSV), and, for some cultures, Excel uses semicolons in place of commas. In these instances, you have to apply well-known workarounds (which go beyond scope here). 


### Alternative Ways to Find Moodle Quiz IDs 

As an alternative to browsing the Moodle to find the quiz ID, you can use the functions `findMoodleID` `findQuizID`. The first function prints a list of Moodle courses based on the name of the current course, e.g.:

    findMoodleID()

You get a listing similar to:


         Fullname                                       Contacts      
    1234 FINANCIAL INVESTMENTS AND RISK MANAGEMENT 2020 Antonio Fasano
    1235 FINANCIAL INVESTMENTS AND RISK MANAGEMENT 2021 Antonio Fasano
    ....

The research is case insensitive and approximate, that is, in case of slight differences between the name used by Moodle and the ESSE3 one, you still get results. Of course, there is no way to find a match if, say, the Moodle convention is to use an initialism, such FIRM, for the case above. In this case, you can specify yourself a search string, that is:

    findMoodleID("FIRM")
  

As you guessed, the rightmost column of the output is the course ID used by Moodle and, once you identify the reference course, you give it as an argument to: 

    findQuizID(1234)

So you get


         QuizName                                 
    1234 Financial Investments and Risk Management - Quiz 1
    1235 Financial Investments and Risk Management - Quiz 2
    ...


As noted the course search is approximate. If you want a straight search use:

    findMoodleID("FIRM", approx = FALSE)

Note the match is still case insensitive.



### Grading

Grading functions  are intended for multiple-choice questions previously downloaded. 


I am not a Moodle geek, by the way, I have seen that all my MCQ response files look like follows:


    Surname,"First name","Email address",State,"Started on",Completed,"Time taken",Grade/XX,"Q. 1 /X.XX","Q. 2 /XX", ....
    ...
    ...
	
	
The column `Grade/XX` is the grade given by Moodle, where `XX` is the maximum value possible.  
The column `"Q. 1 /X.XX"` is the grade for Question 1, `X.XX` being the maximum possible, and so on for the other question.  
Because I deal with multiple-choice questions, a positive value denotes a correct answer and a negative one denotes a wrong answer, while a dash is a question skipped. 
	
If the structure of your responses matches  this standard MCQ scheme, you can use these grading functions. If you have questions or answers  worth different grades, these functions do not apply. 

To get a score per question, type:

    md.scores()

You obtain a grid with the student emails and the score for each question. By default, if the answer is correct, the score is 2, if it is wrong, the score is -1, if it was skipped, it is zero. So the output is similar to: 

                    moodlemail  1  2 3  4  5  6  7 8  9 10 11 12 13 14 15 
    1 ************************  2 -1 2  2  2  2  2 2  2  2  0  2  2  2  2 
    2 ************************  2  2 2  2  2  2  2 2  0  2  2  2  2  2  0 
    3 ************************  2  2 2  2  2  2  2 2  2  2  0  2  0  0  2 
     
    Scores saved to *******************/mscores.csv


This can be adjusted by setting the variables:

    SIENA$goodans  <-  3  
    SIENA$badans   <-  0  
    SIENA$skipans  <-  0  

In the schema above, there is no penalty for wrong answers and a correct one counts as 3. 

The response file used by `md.scores()` is found in the working directory you have set, as `moodle-grades.csv`, and you have probably downloaded it automatically with `md.getGrades()`. If you want to use some other file, specify the absolute path with:

    md.scores(csvpath = "path to your file.csv")

The usual  [path rules](#Specifying-File-and-Directory-Paths) apply to `"path to your file.csv"`. 

Beyond printing the  score grid to the console, this is also saved in CSV format as `mscores.csv` in the working directory. 
If you do not want to print and save results, but simply store the results in a variable, for further analysis, use: 

    myscores <- md.scores(save = FALSE)

Note that there is currently no option to change the saves file path, but tif you really need this, you can do it yourself with: 

    file.rename("yourworkdir/mscores.csv", "new path")

Above, you pass to the renaming function, first, the old absolute path and, secondly, the new desired path. This method applies to all grading functions listed here. 

The function:

    md.grader() 
		
runs `md.scores()` again, without generating files, and sums all questions' scores to get the final student grades (we assume 'final'  for simplicity).  It also adds a good/bad column with the total correct and wrong question answered by each student. So the output is similar to:


                         email goodbad grade
    1 ************************    13/1    25
    2 ************************    13/0    26
    3 ************************    12/0    24
     
    Grades saved to *******************/grades.csv


Just like for `md.scores()`, the grades are  printed to the console and saved in CSV format as `grades.csv` in the working directory. 
Still similarly to `md.scores()`, you can pass the arguments `csvpath` and `save` to control the function input and output files.

Finally, `md.grader.credits()` applies a credit weighting scheme. Assume that students participating in a test might have the right to different academic credits, perhaps because they belong to different programs or have already gained some credits. Then we can give the students with fewer credits to achieve fewer questions to answer. Of course,  this would result in a lower quiz grade and we have to reweigh it up in proportion to credits.  
If the highest credits possible are 10, then a  grade 24 is actually 24 for a 10-credit student, but, for an 8-credit student (who  is tasked to answer only 4/5 of the given questions), the grade is reweighed by the factor 10/8, which yields 30. 

Another possibility is to give the lower-credits students the same number of questions, but still remap to a higher grade in  proportion to the credits ratio. In this case, with a fixed proportion, the remapped grade could  bypass the top mark, in Italy 30 cum laude, here 31, To avoid this The base weight factor used to remap is rescaled as the mark approaches the top. 

For example, assume a student with 6 credits, whereas the full credit level is 9, then base weight factor is 9/5 = 1.5. Thus, given the Italian pass mark, 18, the related unweighted pass-mark is 18/1.5 = 12. As the mark approaches the top mark 31, the weight is decremented by: `(1.5 - 1) / 19  = 0.026`, where 19 is the distance from pass mark to top mark (31 - 12). 
Therefore 13 is remapped as:   

    13 * (1.5 - 0.026 * (13 - 12)) = 19.16

and 30 cum laude (with proper decimals) remains as is:

    31 * (1.5 - 0.026 * (31 - 12)) = 31 


To make this possible, SienaR connects to ESSE3 and looks for student credits, while we should pass the maximum possible credits  to allow rescaling. In fact, it could happen that, for the current sitting, there is nobody to whom the top credits are attributed. For a mix of 10 and 8 credit students, without assuming the weight factor rescaling, we use:

    md.grader.credits(maxcredits = 10, rescale = FALSE)

which produces an output similar to the following:

    ESSE3 data saved to *******************/studata.rds
     
                         email goodbad mark credits grade
    1 ************************    13/1   25      10  25.0
    2 ************************    13/0   26      10  26.0
    3 ************************    12/0   24       8  30.0
     
    Grades saved to *******************/grades.csv


The last column, `grade`,  is now the weighted grade;  the `mark` column is the former unweighted grade. 


An important thing to note is that you have to set the current course and sitting schedule, by means of `setCourse()` and `setSit()`, otherwise `md.grader.credits()` does not know where to look for. If you already did this before, you might want to re-check current values with `getCurrent()`. 

`md.grader.credits()` supports the usual  `csvpath` and  `save` arguments. As regards the latter,  `md.grader.credits()` produces by default the same `grades.csv` as `md.grader()` overwriting the existing one. This is so because  `md.grader.credits()` output is a superset of `md.grader()`, where you have both the weighted and the unweighted grade, but you can rename the old score file, as shown above, to avoid it being overwritten. 

If you want to rescale the weight, set `rescale = TRUE` or simply don't pass the argument (it's the default):

    md.grader.credits(maxcredits = 10, rescale = TRUE)


You might also have noticed, from the output above, that there is another file created, named `studata.rds`. This file is in the R own data format and, if you know the R language,  you can use it to make some further processing. 


What if you want to publish the grades informally. I personally care about privacy, and this is where `md.anonymise()` comes to help. 
This function reparses the output of the grading functions, replacing sensitive student personal data with the SHA-1 hashes of  their student ID. So you type:

    grades <- md.grader()
    md.anonymise(grades)

and you get:

                                  stud-id-SHA1 goodbad grade
    1 7c4a8d09ca3762af61e59520943dc26494f8941b    13/1    25
    2 908f704ccaadfd86a74407d234c7bde30f2744fe    13/0    26
    3 118a43489e2f9ab66823eabdada672c906bb387f    12/0    24

`md.grader()` can also be `md.grader.credits()`. 

[comment1]: # ("123456", "123457", "123458")

You see the email address has been removed, while you the first column shows the SHA-1 of the student ID. A student knowing their student ID can obtain its SHA-1 hash from a myriad of [online services](https://duckduckgo.com/?q=hash+online), or a local utility, and identify their own grade, but not those of the others'. 


Because student ID hashes are not resistant to a rule based attack, you might want to use (yet this could be overkilling to many):


    md.anonymise(grades, tax = TRUE)


which yields:

                                 tax.code-SHA1 goodbad grade
    1 917e307ed8630d48e2d6f4081ca7e0c28e33631f    13/1    25
    2 7967801d253eb91bc3bff6dcf4c32e697cd215ec    13/0    26
    3 08360d11f5e0b1db74fa9f7bd12d14d02316b521    12/0    24


In this case, in place of the student ID, we have the ESSE3-stored national tax code. If you want to test the hashes above, note that the first table line encrypts,  respectively, the fictitious student ID "123456" and tax code "BBBCCC00A01H501G".

[comment2]: # ("BBBCCC00A01H501G", "BBBCCC00A41H501K", "BBBCCC00A01Z112R")

The encrypted output is saved as  `encgrades.csv` in the working directory, but you can add the usual `save` argument to prevent this.
Note that, if you want to use this function, you need to have the package `digest`. To install it,  run once:


    install.packages('digest')


### Posting Grades to ESSE3

Posting grades to ESSE3 is as simple as writing:

    postGrades()

As noted for other functions, to post to the proper scheduled sitting, you have to use  `setCourse()` and `setSit()`, and if you  did this a while ago, you'd  better check the current values with `getCurrent()`. 

By default, `postGrades()` uses the `grades.csv` found in the working directory you have set and produced by `md.grader()` or 
`md.grader.credits()` in the [Moodle Plugin](#moodle-plugin), or by [Testmacs](https://github.com/antoniofasano/testmacs).  

If you want to load an alternative CSV file still located in the working directory, use:

    postGrades(csv.work = "name grade CSV in workdir")

Note that Testmancs generates two grading files: `grades.txt` and `grades-ex.txt`, the first refers to multiple-choice question results, while the seconds adds also the computational exercises results. You normally want the second one. 

If the CSV file you want to load is not located in the working directory, use its full path with::

    postGrades(csv.full = "path to grade CSV")

The usual  [path rules](#Specifying-File-and-Directory-Paths) apply to `"path to grade CSV"`. 

Note that this function does not publish the results: for this, you have to click the designed  button on the web app, after verifying the grades were properly posted. Indeed, the function's objective is just to avoid the stress of manually selecting tons of drop down list to insert grades.  
  
Grade file generated by Testmacs does not take into account course credits, just like `md.grader.credits()` for Moodle Plugin, before uploading, you can use:

    add.credits(maxcredits = 10)

which will weigh grades by course credits, where `maxcredits` denotes the maximum possible credits attainable. For the rescaling procedures, see the description of `md.grader.credits()` in [Moodle Plugin](#moodle-plugin) section.

To identify the grade file, `add.credits()` uses the same approch as `postGrades()`, that is it defaults to `grades.csv` found in the working directory, but you can use an alternative name or a full path with:

    add.credits(maxcredits = 10, csv.work = "name grade CSV in workdir")
    add.credits(maxcredits = 10, csv.full = "path to grade CSV")

`add.credits()` generates two new grade files named `wgrades.csv` and `wgrades.txt` in the same directory of the input grade file 
The CSV  file is suitable for uploading to ESSE3, the other one is more convenient for human reading.


If you manage more courses under Testmacs, than you might want to upload all of them with a single command. In this case case you use Testmacs output directory, that  is the directory where the questions, results, and students' answers are generated for each course. The bulk verions of `postGrades()` is:

    testmacs.postGrades("Path to Testmacs output directory", "grade file name")
	
The first argument is the  path to the Testmacs output directory and the second is the grade file. 
`testmacs.postGrades()`, you need a file named `official-name.txt`, in the same directoy where the grade files to upload, whose content consist of the official ESSE3 course name. 



There is a bulk verision of `add.credits()`, that is:


    testmacs.add.credits(maxcredits = 10, "Path to Testmacs output directory", "grade file name")


If you need to add credit weighting,  you normally call the two bulk functions like this:

    testmacs.add.credits(10, "Path to Testmacs output directory", "grades-ex.csv")
    testmacs.postGrades(     "Path to Testmacs output directory", "wgrades.csv")

For each course, the first line takes the full grade CSVs (including exercise assessment) and generates their weighted versions as `wgrades.csv`, which are then uploaded to ESSE3.


## Shibboleth Plugin

This plugin helps in case your institution uses [Shibboleth Single Sign-on](https://www.shibboleth.net/products/) for authentication. 

SSO means that there is a single authentication page for all services provided by your organisation. When you follow the login link of a service,  you are redirected to this common page and, after filling the login form, you are redirected back to the original service page as a logged-in user. 

Indeed, while using the its protocol, the Shibboleth plugin does not take any advantage of the SSO, because it makes a distinct connection for each service. Why? you might ask. Because for a human it is boring to type the credentials multiple times, but for a robot there is little overhead. In this regard, while the protocol at server level is different from basic auth, SienaR uses locally a mechanism similar to `onelog` for basic auth. 


### Endpoints with SSO redirection

Because of redirection, in the login page, you don't see the endpoint any more (but the SSO page address). You should be creative and grab it before or after logging in. 

If you do this before, rather than clicking on the link, you can just right-click on it, and select the "Copy Link" entry from the context menu. The string you get should be the usual: 

     https://....yourdomain.edu/.../auth/Logon.do

where you strip `auth/Logon.do`. 

You can extract the endpoint from the ESSE3 home, before the login, expected to be like: 

     https://....yourdomain.edu/.../Home.do

You strip the trailing `Home.do` and the remaining part of the string is your endpoint. Note that there is no `auth/` substring in the second address.

Finally, you can  extract the endpoint from  the home page after login. This is like:


     https://....yourdomain.edu/.../auth/docente/AreaDocente.do


Strip `auth/docente/AreaDocente.do` to get the endpoint. 

You might check all the three methods to be sure they are consistent and you are not making errors. 


If you need the Moodle endpoint too, similar care should be used. The right-click is not available here. The home page before logging does not use a specific address path, but it is the very endpoint. So:  

     https://....yourdomain.edu/.../

is both the home page address and the needed endpoint. 


After the login, you have:

     https://....yourdomain.edu/.../my/

and you have to strip `my/`. Webmasters might hide the  `my/`, but you can add it yourself and visit the page, to be sure you are in the home page. 

You can also visit a course page like the following:


     https://....yourdomain.edu/.../course/view.php?id=1234


Here you strip `course/view.php?id=1234` (where `1234` is the Moodle course ID) to get the endpoint. 


### Shibboleth Setup and Use


Once you have caught the endpoints, the setup is automatic and to use the plugin, you just type in the console:

    shibboleth()

You might want to look the general [plugin info section](#Information-About-Plugins), for more insights about this init function. 

At this point, you have both ESSE3 and Moodle will use single sign-on. However, some organisations use SSO for some services and basic authentication for others. To use SSO only for 'esse3' or only 'moodle' use accordingly: 
or 'both'. The

    shib.auth('esse3')
    shib.auth('moodle')
	
If you want to later go back to SSO for both services or set basic authentication (no SS0) use accordingly:

    shib.auth('both')
    shib.auth('none')

`'both'` can even be omitted as it is default argument. 


If you want to know the type of authentication currently configured use:

    authtype()

Of course, when using SSO for both ESSE3 and Moodle, it is worthless `setCreds.moodle()`.

If you use SSO for ESSE3 and basic-auth for Moodle, you might find convenient to use `login.moodle(onelog = TRUE)`, assuming the credentials are the same.  



# Specifying File and Directory Paths

Consider the R line setting the working directory path: 

    SIENA$workdir <- "work path"


Here `work path` is an *absolute path*. Using relative paths is possible, but generally discouraged because they, by default, are set to the directory where the script was originally sourced, which could create confusion for some people. 

R paths use a Unix style format, so, if you are a Windows user and you want to specify the path:

    C:\Users\antonio\Desktop\sienafiles
	
	
you have to rewrite it with a Unix style, i.e. with slashes for backslashes:

    SIENA$workdir <- "C:/Users/antonio/Desktop/sienafiles"

For Windows, you  can have also the possibility to _escape_ the backslashes (doubling them):

    SIENA$workdir <- "C:\\Users\\antonio\\Desktop\\sienafiles"

Note that in Linux, the backslash is an ordinary character that you are free to use, so `\\` denotes a path with a  backslash in it. 


If you use a leading  `~/` in the path, SienaR  expands it to the home directory, like in the Linux shell. Because there is no established convention for the  home directory location in Windows, R, hence SienaR, adopts:  

    C:\Users\USERNAME\Documents

where `USERNAME` is the name  of the current Windows user. This is unless you hacked R defaults. 

There is also another option for Windows users to use backslashes, that is,  the raw character syntax. Here, a trivial example is worth more than words:

    r"(C:\Users\antonio\Desktop\sienafiles)"

As you can see, using the syntax `r"(...)"` allows typing a Windows path as is. This is particularly convenient if you have a long path that you are pasting  from the Windows file browser.

If you pair these options with the TAB auto-completion features mentioned above, you can type paths fast and, most of all, without error. 



# Versions, Troubleshooting, and Known Issues

Use the `version()` function to learn about the version you are using or check the `changelog.md`. If you want to know about the last commit, use:

    version(changes = TRUE)


Currently, you might see some warnings like:

    Warning:
    ... : closing unused connection X (stdin)

This is a noisy  garbage collection procedure, which does not affect the final code results, and removing warnings might hide possible serious ones. I am rewriting some of the code logic to manually close connections.

These scripts do not use the services' web APIs, which in general make the code more robust and durable. Indeed, the API documentation I was able to found was tiny. I wanted to get some job done in a reasonable time, and finding the docs outweighed the time to reverse engineering browser traffic. However, I will gradually refactor code to service APIs, if I found the docs. 

