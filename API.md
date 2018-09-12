**WARNING - NOT STABLE YET**

# timeclock-ui-lib
## Predicates
### timeclock-ui-buffer-exists?
String -> List?
### timeclock-ui-buffer-visible?
Buffer | String -> Boolean

## Time operations
### timeclock-ui-timestamp->list
String -> (Int ...)
### timeclock-ui-timestamp-list->seconds
(Int Int Int Int Int Int) -> (Int Int Int Int)
(year month day hours minutes seconds) -> (sec-high sec-low microsec picosec)

# timeclock-list
## Predicates
### timeclock-list-project-active?
String -> Boolean

## Time operations
### timeclock-list-seconds-to-hms
Integer -> [Integer Integer Integer]
seconds -> [hours minutes seconds]
### timeclock-list-time-add
[Int Int Int] [Int Int Int] -> [Int Int Int]
[hours minutes seconds] [hours minutes seconds] -> [hours minutes seconds]

## Navigation
### timeclock-list-goto-last-project
-> nil

## Side-effects
### timeclock-list-print-non-tabular

## Etc
### timeclock-list-current-project
-> String
### timeclock-list-entries
-> (String . [String String String String])
-> (project . ["index" "project" "(hh:)?(mm:)?ss" "indicator"])
### timeclock-list-total-time-one-day
(Int Int Int Int Int Int Int?) -> [Int Int Int]
(seconds minutes hours day month year &optional zone) -> [hours minutes seconds]
### timeclock-list-project-at-point
-> String

# timeclock-report
## Time operations
### timeclock-report-day-of-week->number
String -> Integer
### timeclock-report-increment-or-decrement-date
(Int Int Int Int Int Int) Symbol Int? -> (Int Int Int Int Int Int Int?)
(seconds minutes hours day month year) operator count? -> (seconds minutes hours day month year dow dst utcoff)
