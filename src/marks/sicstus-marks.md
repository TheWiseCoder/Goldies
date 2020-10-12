These are predicates providing a portability layer for the SICStus Prolog platform, by implementing some unique, or convenient, SWI-Prolog features.  

The available predicates are:  

1. flag operations (see notes in the source code file)  
- `flag(+Key, -Old, +New)`  
- `get_flag(+Key, -Value)`  
- `set_flag(+Key, +Value)`  

2. GUID (UUID version 4) generation  
- `guid(-Guid)` - unify *Guid* wit a new GUID  
- `guids(+N, -Guids)` - unify *Guids* with *N* new GUIDs  

3. random integers generation  
- `randseq(+K, +N, -Set)` - unify *Set* with a list of approximately *K* unique random integers in the range 1..*N*  
- `randset(+K, +N, -Set)` - unify *Set* with a sorted list of approximately *K* unique random integers in the range 1..*N*  

4. miscellaneous operations  
- `getenv(+Name, -Value)` - unify *Value* with the value of the environment variable *Name*  
- `pairs_keys_values(?Pairs, ?Keys, ?Values)` - unify a list of key-value pairs with separate lists of keys and values  
- `read_line_to_codes(+Stream, -Codes)` - unify Codes with the line codes read from Stream  

Additionally, you may peruse the documentation and code on the source code files.  
