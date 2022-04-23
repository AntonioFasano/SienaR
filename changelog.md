# v0.1.2 2022-04-24

- Added `findMoodleID()` `findQuizID()`. The first function looks for Moodle course IDs based on the name of the currently set SieanR course or you can pass a specific course name used by Moodle; the second returns Moodle quiz IDs based on a Moodle course ID. 

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

