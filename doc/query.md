Merge these
* chronometrist-total-time-one-day
* chronometrist-reason-list
* chronometrist-project-time-one-day
* chronometrist-intervals-on
* chronometrist-diary-projects-reasons-on

into either
* a single 'event query' function (which only returns events), OR
* a single query function, which returns whatever fields you specify from the events returned by whatever criteria you specify

# chronometrist-query
(&key RETURN CODE DATE TIME PROJECT COMMENT)

RETURN can be
* nil - entire events are returned
* a symbol - `code`, `date`, `time`, `project`, or `comment` - for each event, return only that object.
* a list of these symbols - for each event, return only these objects

CODE ...

DATE can be
* nil - query all dates
* Any date format we accept, including (YEAR MONTH DAY) and calendrical information returned by (decode-time)
* a pair containing one of the above formats - query between these dates. (nil . DATE) means from the beginning to DATE; conversely, (DATE . nil) means from DATE to the end.
* a list containing dates and/or pairs

TIME ...

PROJECT can be
* nil - for all projects
* A string - a single project
* A list of strings - these specific projects

COMMENT ...

(Sounds an awful lot like SQL, doesn't it? 😓)

## TODO
* and/or/not

## Arbitrary key values (custom storage format)
If we end up creating a custom s-exp based format which allows for arbitrary key values apart from the ones listed above (see [doc/tags.md](doc/tags.md)), we'd probably need to go the macro route.

How do we know how to compare a given key's value to the specified value? Should we have the user define them first?

Do we really need to define values and comparators? Or can `equal` do? (which was edgar-rft's suggestion) I'd like to do
* pattern matching on dates e.g. `(2019 _ _)` for all dates in 2019 - but that can be accomplished with `:date (2019 1 1) (2019 12 31)`. A more complex example - `(_ 1 _)` for events from January for all years. Or `(_ _ 1)` for the first date of any month.
* regexp matching for string values

## Data structure
We use a hash table with dates as keys and events as values. But - if we want to make this something other projects can use - other applications may not want to have dates as keys.

### Integers as keys
One possibility is to use integers as keys. Even in Chronometrist's use-case, storing dates as keys is fun and useful, but becomes redundant in the presence of a general query language for the values.

Additionally, you have overhead from acquiring dates from the plists to add to the keys, and you either have to live with the redundancy of having dates in both keys and values, or you have to perform more work to remove them from the values.

With dates kept only in values, keys can be obtained with minimal work and values can be exactly what was read in from the file.

## Nic Ferrier's [emacs-db](https://github.com/nicferrier/emacs-db) and [emacs-kv](https://github.com/nicferrier/emacs-kv)
I tried out -db. I wanted to store plist values, but it seems to support only alists. Also, it stores them as a the printed representation of a hash table...which is probably faster than `read`ing individual s-exps and assembling that into one, but probably not nice to edit by hand (which is one of our major requirements).
