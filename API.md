**WARNING - NOT STABLE YET**

(?) - function has dubious utility/is candidate for cleanup
DEPRECATED - function is deprecated and should be removed in the future

# Legend of currently-used time formats
1. timeclock-timestamp - "year/month/day hours:minutes:seconds"
2. timeclock-date - "year/month/day"
3. decode-time - (seconds minutes hours day month year dow dst utcoff)
4. timestamp-list - (year month day hours minutes seconds)
5. date-list - (year month day)
6. date-vector - [year month day]
7. time-vector - [hours minutes seconds]
8. encode-time - (sec-high sec-low microsec picosec)
9. seconds - seconds as an integer

# chronometrist-common
## Commands
### chronometrist-open-timeclock-file

## Predicates
### chronometrist-buffer-exists?
String -> List?
### chronometrist-buffer-visible?
Buffer | String -> Boolean
### chronometrist-time-interval-span-midnight? (DEPRECATED)
timestamp-list timestamp-list -> Boolean
### chronometrist-first-event-spans-midnight? (DEPRECATED)
timeclock-date "project" -> Boolean

## Time operations
### chronometrist-timestamp->list
timeclock-timestamp -> timestamp-list
### chronometrist-timestamp-list->seconds (?)
timestamp-list -> encode-time
### chronometrist-timestamp->seconds
timeclock-timestamp -> encode-time
### chronometrist-format-time
time-vector | time-list -> "h:m:s"
### chronometrist-date-op-internal
s m h DD MM YYYY operator count -> decode-time
### chronometrist-date-op
decode-time | date-list operator &optional count ->
### chronometrist-time->seconds
time-vector -> seconds

## Timelog data (file-based)
### chronometrist-get-end-time (DEPRECATED)
timeclock-date -> timeclock-timestamp
### chronometrist-common-create-timeclock-file
### chronometrist-common-file-empty-p
### chronometrist-common-clear-buffer

## Timelog data (hash table-based)
###  chronometrist-project-time-one-day
"project" &optional date-list -> time-vector

# chronometrist
## Predicates
### chronometrist-project-active?
String -> Boolean

## Time operations
### chronometrist-seconds-to-hms
seconds -> time-vector
### chronometrist-time-add
time-vector time-vector -> time-vector

## Navigation
### chronometrist-goto-last-project
-> nil

## Side-effects
### chronometrist-print-non-tabular

## Etc
### chronometrist-current-project
-> String
### chronometrist-entries
-> (String . [String String String String])
-> (project . ["index" "project" "(hh:)?(mm:)?ss" "indicator"])
### chronometrist-total-time-one-day
(seconds minutes hours day month year &optional zone) -> time-vector
### chronometrist-project-at-point
-> String

# chronometrist-report
## Time operations
### chronometrist-report-day-of-week->number
String -> Integer
### chronometrist-date-op
(seconds minutes hours day month year) operator count? -> decode-time

# chronometrist-events
## Hash table querying
### chronometrist-events-subset
date-list date-list -> #(hash-table (date-list [event-vector*]))
