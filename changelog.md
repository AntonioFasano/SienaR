# v0.2.0 2022-10-07
      
- `testmacs.add.credits()` and `testmacs.postGrades()` can add course credit weights and post grades of all Testmacs courses.
- `add.credits()` add course credit weights to a Testmacs grade file.
- `postGrades()` CSV argument can now be `csv.full` or `csv.work`. They stand for full path to file ore relative to the global workdir `SIENA$workdir`.
- Most Moodle functions have now the "md." suffix for easy autocompleate.
- Added `md.getResps()` to download generic Moodle response files, not intended for auto-grading, and `getResponses()` is now `md.getGrades()`. 
- Simplified Shibboleth, based on new internal functions `.login.basic()`, `.login.moodle.basic()`, `.login.shibboleth()`, `.login.moodle.shibboleth()`, 
- `authtype()` gives current authotisation type.
- `defaultAuth()` restores default authentication type, currently basic authentication  
- `findSittingID()` gives a better message in case the `compareID` is not matched. Also `compareID` defaults to NULL
- `.addCommittee.memb()` fixed error in code controlling the success on the operations.

# v0.1.3 2022-07-20

- Added internal functions `.urlHost()`, `.urlIsAbsolute()` to manage URLs components.
- Added internal function `.replayForm()` to submit a form on a webpage as is. 
- `.idstring()`, `.signPlugin()`  to associate a script with a unique ID and check if it matches the remote git version.
- Updated `login.shibboleth()` to work with recent Shibboleth version.

# v0.1.2 2022-04-24

- Added `findMoodleID()`, `findQuizID()`. The first function looks for Moodle course IDs based on the name of the currently set SieanR course or you can pass a specific course name used by Moodle; the second returns Moodle quiz IDs based on a Moodle course ID. 

- Added a global variable (`CurSchedule.e3`) to track the current internal ESSE3 sitting ID (`APP_ID`). The latter is blanked whenever ESSE3 is queried for the course list, which required the user to set again the current exam schedule.

# v0.1.2 2022-01-16

- Added `getSummary()` which shows enrolled students for each course and for first available sitting from current date or a user given date
- Readme gets a summary based on [gfm-toc](https://github.com/atheiman/gfm-toc)

# v0.1.1 2022-01-15

- `getSched.details()` includes course name


# v0.1.1 2022-01-14

- Fixed: `getSchedules()` invisibly returns values if `prompt=FALSE`. 
- Fixed:  In `grader.credits()` and `anonymise()` replaced `getSched.studs()` for `getEsse3data()`


# v0.1.0 2021-10-10 

- Fixed: ESSE3 traffic-lights colours in the results pages, indeed more than three.   
- `getEsse3data()` is now `getSched.studs()` and gains a `personal` argument, which is faster when FALSE by avoiding downloading student-personal-page data, and a `short`argument, to output just results and students' contacts.  
- Added `getSched.details()`, which returns details about the current exam schedule, or a specific schedule entry number or date you pass.
- Added `getPending()`, which returns any non-green status for the "Results entered", and "Records generated" columns for the recent sittings of all courses.
- Added `findCommittee()`, which returns the exam committee of a given scheduled sitting. Indeed, it was already there but not mentioned in the docs.
- Added a `version()` function, which also returns the changes from the change-log if `changes = TRUE`. 
- Added autocompletion and raw syntax for paths in the docs.

