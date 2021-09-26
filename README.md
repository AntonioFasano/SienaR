
Not to be confused with the fictional [company](https://www.starwars.com/databank/sienar-fleet-systems).

SienaR is a collections of R scripts to manage exams in the ESSE3 system, which is a Student Information System used in most (if not all) Italian academic institutions.  
While SienaR is written in R, no specific knowledge of R is required to use it. 

You can do essentially two things with SienaR. 

- Browse and set exam schedules. 
- Upload exam results.  

The second feature was intended as a plug-in for my [Testmacs](https://github.com/antoniofasano/testmacs) e-learnig tool for R. Unfortunately, because of the pandemic, I can't use Testmacs in these days, so this feature is temporarily halted. However, as the labs open again, the development will restart. It is, by the way, replaced by a plugin using [Moodle](https://moodle.org/), which can download MCQ responses, grade them and upload grades to ESSE3.   

# How to Install

__Step 1__ Install [R](https://www.r-project.org/).  If you did already,  just make sure the version is not several years old. 

__Step 2__ Head on to R console.  
In Linux, this is as simple as typing `R` in the terminal.   
In Windows, there are several fancy GUIs to make your  experience more likeable, including my 
[BloomR](https://github.com/antoniofasano/BloomR/) 
distro, targeting finance labs. If you use the vanilla R, you should be good to go with
 
    "C:\Program Files\R\R-3.X.X\bin\R.exe"

Replace `3.X.X` with your actual version. 

In macOS, I don't know, but if you have a Mac, you probably don't need this. 

 __Step 3__  In the R console, type:

    install.packages(c('stringr', 'curl', 'xml2'))

to install the related packages. 

 __Step 4__  Download the main [siena.r](https://raw.githubusercontent.com/AntonioFasano/SienaR/master/siena.R) script.
 Just somewhere you deem fit, but make sure the local name is `siena.r`.

 __Step 5__  Identify the local [Endpoint](https://en.wikipedia.org/wiki/Web_API#Endpoints) of ESSE3 and possibly Moodle. 
 
 Because ESSE3 is a self-hosted web app (like perhaps your employer mail server), you have to figure out which is the home address. 
 For the [Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication) method, you just go to the ESSE3 login page. Its address is similar to:
 
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

The rational to browse exam sitting schedules is to first set the current course, then the sitting date of interest, and eventually download data.  

To see  courses<sup id="a1">[1](#f1)</sup> associated with your credentials, use:

    getCourses()
	
You get something similar to:

     Code                                             Course                                   Program
        1                       BANKING MANAGEMENT [2001563] SCIENZE ECONOMICHE E BANCARIE [EE004] (L)
        2                     FINANCIAL ENGINEERING [107420]            FINANCE - FINANZA [EG008] (LM)
        2                     FINANCIAL ENGINEERING [107420]                      FINANCE [EG005] (LM)
        3 FINANCIAL INVESTMENTS AND RISK MANAGEMENT [107421]            FINANCE - FINANZA [EG008] (LM)
        3 FINANCIAL INVESTMENTS AND RISK MANAGEMENT [107421]           ECONOMIA/ECONOMICS [EG007] (LM)
        3 FINANCIAL INVESTMENTS AND RISK MANAGEMENT [107421]                     ECONOMICS [D254] (LM)
     
    Use setCourse(Code) to select one.

	
The number in the first column is used to select the course. It could be repeated if the course belongs to more programmes. As suggested, to make, say, Financial Engineering current, use:

    setCourse(2)

Now, to list the sitting dates for this course, use:

    getSchedules() 

The output will be similar to:

    Schedules for Banking Management:
      Schedules ID
     05/10/2021  1
     17/09/2021  2
     02/09/2021  3
     29/06/2021  4
     
    Use setSched(ID) to select one.


Identify  the sitting ID that interests you. Assuming it's the last date ID, issue: 
   
   
    setSched(1)
    getEsse3data()

The second function dumps  the table that you find in the Enrolled Students List page found in the official web app.  The original table lacks the student email, which works much better than the Student ID as identifier. For this reasons The function follows each link (in the table name column) to the personal student page  and from there it recovers the email. If available, the function dumps also the traffic light icons, denoting grade acceptance/rejection, in textual format. 


If at any time you want to know what are the current course and sitting schedule, use:

    getCurrent()

The output is similar to:

                                           name 
    "Financial Investments and Risk Management" 
                                       schedule 
                                   "17/09/2021" 



 
 
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

The exam committee can be added immediately after the creation of the sitting or at a later date. In the first case, it is suggested to retrieve the internal web app sitting ID, from the function `addSitting()`, as follows:


    sid <- addSitting("Financial Engineering", examdate = as.Date("2021/10/30"), examtime = "11:30", 
                     results = "FWA", examtype = "S", "Sitting number ten.")
	
    
_Note_:  `<-`  just means `=`. If you don't like this convention, use the latter symbol.  	


`sid` is a variable name you choose to assign the ID value. 
You can use this variable when giving a committee to the exam sitting, with:

    addCommittee(
       course = "Financial Engineering",
       examdate = as.Date("2021/10/30"),
       committee = "John Doe, John-Robert Mac-Neil, 123456",
	   sittingID = sid
    )

If you don't have the sitting ID from the sitting function, just don't pass this argument, and the function `addCommittee()` will find it by itself. 

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
	
	
## Adding Grades to an Exam Sitting

This is handled by the function `postGrades()`. That was used to automatically route grades computed in the lab by my e-learning tool, [Testmacs](https://github.com/antoniofasano/testmacs),  to the Student Management System. Due to the pandemic, this feature is temporarily halted. 

	
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

Unless you using local files manually downloaded, before doing anything, you have to log in to the Moodle web app.

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

and the second time copied ESSE3 credential will be used. Of course, you can use, `setCreds.moodle()` to change stored Moodle credentials. Indeed, the `onelog` argument would be clearer if written `copy__esse3_credentials`, however that would have been too long. 


Note that the onelog feature is not the same as the [Shibboleth Single Sign-on](https://www.shibboleth.net/products/) used by the  [plugin](#Shibboleth-Plugin) of the same name, since the latter  happens at the web app level. 



### Getting the Moodle Response File

To download the response file for a given quiz, you should provide the Moodle quiz ID.  To this regard,  visit the quiz home page or the quiz report page, and you will see that their addresses are like:


    ENDPOINT/mod/quiz/view.php?id=1234
    ENDPOINT/mod/quiz/report.php?id=1234&mode=overview

where `ENDPOINT` denotes your Moodle endpoint and `1234` denotes the quiz ID. 


You can then, download the responses for the quiz 1234 with:

    getResponses(1234)


As an alternative, you can copy-and-paste from the browser any address that contains the string `id=1234`, such as the above report page:


    getResponses("ENDPOINT/mod/quiz/report.php?id=1234&mode=overview")
	
	
and the quiz ID will be recognised.


The response file is  downloaded in the working directory, which you have set before,  as `responses.csv`, and you can view it in Excel. 

Bear in mind that this is a real Comma Separated File (CSV), and, for some cultures, Excel uses semicolons in place of commas. In these instances, you have to apply well-known workarounds (which go beyond scope here). 



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

    scores()

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

The response file used by `scores()` is found in the working directory you have set, as `responses.csv`, and you have probably downloaded it automatically with `getResponses()`. If you want to use some other file, specify the absolute path with:

    scores(csvpath = "path to your file.csv")

The usual  [path rules](#Specifying-File-and-Directory-Paths) apply to `"path to your file.csv"`. 

Beyond printing the  score grid to the console, this is also saved in CSV format as `mscores.csv` in the working directory. 
If you do not want to print and save results, but simply store the results in a variable, for further analysis, use: 

    myscores <- scores(save = FALSE)

Note that there is currently no option to change the saves file path, but tif you really need this, you can do it yourself with: 

    file.rename("yourworkdir/mscores.csv", "new path")

Above, you pass to the renaming function, first, the old absolute path and, secondly, the new desired path. This method applies to all grading functions listed here. 

The function:

    grades() 
		
runs `scores()` again, without generating files, and sums all questions' scores to get the final student grades (we assume 'final'  for simplicity).  It also adds a good/bad column with the total correct and wrong question answered by each student. So the output is similar to:


                         email goodbad grade
    1 ************************    13/1    25
    2 ************************    13/0    26
    3 ************************    12/0    24
     
    Grades saved to *******************/grades.csv


Just like for `scores()`, the grades are  printed to the console and saved in CSV format as `grades.csv` in the working directory. 
Still similarly to `scores()`, you can pass the arguments `csvpath` and `save` to control the function input and output files.

Finally, `grader.credits()` applies a credit weighting scheme. Assume that students participating in a test might have the right to different academic credits, perhaps because they belong to different programs or have already gained some credits. Then we can give the students with fewer credits to achieve fewer questions to answer. Of course,  this would result in a lower quiz grade and we have to reweigh it up in proportion to credits.  
If the highest credits possible are 10, then a  grade 24 is actually 24 for a 10-credit student, but, for an 8-credit student (who  is tasked to answer only 4/5 of the given questions), the grade is reweighted by the factor 10/8, which yields 30. 

To make this possible, SienaR connects to ESSE3 and looks for student credits, while we should pass the maximum possible credits  to allow rescaling. In fact, it could happen that, for the current sitting, there is nobody to whom the top credits are attributed. Given the example above, this would be:

    grader.credits(maxcredits = 10)

which produces an output similar to the following:

    ESSE3 data saved to *******************/studata.rds
     
                         email goodbad mark credits grade
    1 ************************    13/1   25      10  25.0
    2 ************************    13/0   26      10  26.0
    3 ************************    12/0   24       8  30.0
     
    Grades saved to *******************/grades.csv


The last column, `grade`,  is now the weighted grade;  the `mark` column is the former unweighted grade. 


An important thing to note is that you have to set the current course and sitting schedule, by means of `setCourse()` and `setSched()`, otherwise `grader.credits()` does not know where to look for. If you already did this before, you might want to re-check current values with `getCurrent()`. 

`grader.credits()` supports the usual  `csvpath` and  `save` arguments. As regards the latter,  `grader.credits()` produces by default the same `grades.csv` as `grader()` overwriting the existing one. This is so because  `grader.credits()` output is a superset of `grader()`, where you have both the weighted and the unweighted grade, but you can rename the old score file, as shown above, to avoid it being overwritten. 

You might also have noticed, from the output above, that there is another file created, named `studata.rds`. This file is in the R own data format and, if you know the R language,  you can use it to make some further processing. 


What if you want to publish the grades informally. I personally care about privacy, and this is where `anonymise()` comes to help. 
This function reparses the output of the grading functions, replacing sensitive student personal data with the SHA-1 hashes of  their student ID. So you type:

    grades <- grader()
    anonymise(grades)

and you get:

                                  stud-id-SHA1 goodbad grade
    1 7c4a8d09ca3762af61e59520943dc26494f8941b    13/1    25
    2 908f704ccaadfd86a74407d234c7bde30f2744fe    13/0    26
    3 118a43489e2f9ab66823eabdada672c906bb387f    12/0    24

`grader()` can also be `grader.credits()`. 

[comment1]: # ("123456", "123457", "123458")

You see the email address has been removed, while you the first column shows the SHA-1 of the student ID. A student knowing their student ID can obtain its SHA-1 hash from a myriad of [online services](https://duckduckgo.com/?q=hash+online), or a local utility, and identify their own grade, but not those of the others'. 


Because student-ID hashes are not resistant to a rule based attack, you might want to use (yet this could be overkilling to many):


    anonymise(grades, tax = TRUE)


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

As noted for other functions, to post to the proper scheduled sitting, you have to use  `setCourse()` and `setSched()`, and if you  did this a while ago, you'd  better check the current values with `getCurrent()`. 

By default, `postGrades()` uses the `grades.csv` found in the working directory you have set. If you want to load an alternative CSV file, use:

    postGrades(csvpath = "path to your grades.csv")

The usual  [path rules](#Specifying-File-and-Directory-Paths) apply to `"path to your grades.csv"`. 

Note that this function does not publish the results: for this, you have to click the designed  button on the web app, after verifying the grades were properly posted. Indeed, the function's objective is just to avoid the stress of manually selecting tons of drop down list to insert grades.  



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


Here you strip `course/view.php?id=1234` (where `1234` is the course ID) to get the endpoint. 


### Shibboleth Setup and Use


Once you have caught the endpoints, the setup is automatic and to use the plugin, you just type in the console:

    shibboleth()

You might want to look the general [plugin info section](#Information-About-Plugins), for more insights about this init function. 

At this point, you have to replace:

    login.shibboleth()
    login.moodle.shibboleth()

for the equivalent basic auth functions, that is, `login()`, `login.moodle()`. Also, if you get a message prompting to log in or reset your credentials, use these functions. 

Note that Shibboleth uses always ESSE3 credentials for both ESSE3 and Moodle login. As a result, `setCreds.moodle()`, which stores the prompts for and stores Moodle credentials, is totally worthless here, as Moodle credentials are never actually touched under Shibboleth. 



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


When using paths, it is suggested to use **absolute path**. If you use a relative path, it is relative to the _current_ working directory, and you are supposed to understand which is. If you are confident with R language, you might use the function `getwd()` and  `setwd("new-wd-path")` to print or change the current working directory. 


# Troubleshooting

You might see some warnings like:

    Warning:
    ... : closing unused connection X (stdin)

This is a noisy  garbage collection procedure, which does not affect the final code results, and removing warnings might hide possible serious ones. I am rewriting some of code logic to manually close connections.


These scripts do not use the services' web APIs, which in general make the code more robust and durable. Indeed, the API documentation I was able to found was tiny. I wanted to get some job done in a reasonable time, and finding the docs outweighed the time to reverse engineering browser traffic. However, I will gradually refactor code to service APIs. 






