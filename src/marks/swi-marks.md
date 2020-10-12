These are predicates providing a portability layer for the SWI-Prolog platform, by implementing some unique, or convenient, SICStus Prolog features.  

1. GUID (UUID version 4) generation  
- `guid(-Guid)` - unify *Guid* wit a new GUID  
- `guids(+N, -Guids)` - unify *Guids* with *N* new GUIDs  

2. `datime/6` record (see notes in source code file)  
- `datime(-Datime)` - Unifies *Datime* with the current date and time as a `datime/6` record  
- `datime(+When, -Datime)` - convert a UNIX timestamp *When*, to a `datime/6` record  
- `now(-Now)` - unify *Now* with the current datetime as a UNIX timestamp (integer)  

3. file folder handling  
- `current_directory(?CurrDir)` - retrieve/set the current working directory  
- `current_directory(-OldDir, +NewDir)` - unify *OldDir* with current working directory and change it to *NewDir*  
- `delete_directory(+Dir, +Options)` - delete directory *Dir* (see notes in source code file for *Options*)  
- `directory_exists(+Dir)` - succeed if directory *Dir* exist, fail otherwise  
- `file_exists(+FilePath)` - succeed if file *FilePath* exists, fail otherwise  
- `make_directory(+Path)` - create the specified directory (fail if it already exists)  

4. random number generator  
- `setrand(+Seed)` - seed the random number generator SICStus style (with integer *Seed*)  