# v0.1.0 2021-10-10 

- Fixed: ESSE3 traffic-lights colours in the results pages, indeed more than three.   
- `getEsse3data()` is now `getSched.studs()` and gains a `personal` argument, which is faster when FALSE by avoiding downloading student-personal-page data, and a `short`argument, to output just results and students' contacts.  
- Added `getSched.details()`, which returns details about the current exam schedule, or a specific schedule entry number or date you pass.
- Added `getPending()`, which returns any non-green status for the "Results entered", and "Records generated" columns for the recent sittings of all courses.
- Added `findCommittee()`, which returns the exam committee of a given scheduled sitting. Indeed, it was already there but not mentioned in the docs.
- Added a `version()` function, which also returns the changes from the change-log if `changes = TRUE`. 
- Added autocompletion and raw syntax for paths in the docs.

