
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

