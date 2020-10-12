These modules provide a simple, minimalistic approach to implementing persistence for Prolog data, by means of the Berkeley DB utility package, in both SICStus
and SWI-Prolog environments. The code has been fully tested with SICStus 4.6.0 and SWI-Prolog 8.2.1, under Windows 10 Enterpise and Ubuntu 20.04 operating systems. It might perform equally well in other releases, but this has not been tested.

SICStus and SWI-Prolog generate distinct, and incompatible, database structures. However, these database can be fully manipulated with packages like Linux's *db-util* (see manpages at https://manpages.debian.org/jessie/db-util/index.html). There are many freely-available tutorials on Berkeley DB, and thus there is no need to digress on this subject here.

Depending on your Prolog platform, the environment variable *SICSTUS_BDB_DIR* or *SWI_BDB_DIR* must be set(regardless of the host OS, always use Linux path syntax). This indicates the starting path for the database files. These files are organized in *datasets* containing *tagsets*. Assuming that the environment variable has been set to *c:/Users/my_user/bdb/*, this is how the database files would be laid out:

**SICStus**
`c:/Users/my_user/bdb/data_set_1/tag_set_a/admin.db`
`c:/Users/my_user/bdb/data_set_1/tag_set_a/index.db`
`c:/Users/my_user/bdb/data_set_1/tag_set_a/terms.db`
`. . .`
`c:/Users/my_user/bdb/data_set_N/tag_set_n/admin.db`
`c:/Users/my_user/bdb/data_set_N/tag_set_n/index.db`
`c:/Users/my_user/bdb/data_set_N/tag_set_n/terms.db`

**SWI-Prolog**
`c:/Users/my_user/bdb/data_set_1/tag_set_a.bdb`
`. . .`
`c:/Users/my_user/bdb/data_set_1/tag_set_n.bdb`

Using the available predicates is straigh forward:

- `bdb_base(+DataSet)` - make sure the given dataset's base path for Berkeley DB exists
- `bdb_erase(+DataSet)` - erase the complete dataset from storage
- `bdb_erase(+TagSet, +DataSet)` - erase the tagset within the given dataset from storage
- `bdb_retrieve(+TagSet, +DataSet, -Data)` - retrieve the given tagset from storage
- `bdb_store(+TagSet, +DataSet, +Data)` - persist the given tagset

Additionally, you may peruse the documentation and code on the source code files. 