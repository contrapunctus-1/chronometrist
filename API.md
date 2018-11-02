**WARNING - NOT STABLE YET**

# chronometrist-common
## Predicates
### chronometrist-buffer-exists?
String -> List?
### chronometrist-buffer-visible?
Buffer | String -> Boolean

## Time operations
### chronometrist-timestamp->list
String -> (Int ...)
### chronometrist-timestamp-list->seconds
(Int Int Int Int Int Int) -> (Int Int Int Int)
(year month day hours minutes seconds) -> (sec-high sec-low microsec picosec)

# chronometrist
## Predicates
### chronometrist-project-active?
String -> Boolean

## Time operations
### chronometrist-seconds-to-hms
Integer -> [Integer Integer Integer]
seconds -> [hours minutes seconds]
### chronometrist-time-add
[Int Int Int] [Int Int Int] -> [Int Int Int]
[hours minutes seconds] [hours minutes seconds] -> [hours minutes seconds]

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
(Int Int Int Int Int Int Int?) -> [Int Int Int]
(seconds minutes hours day month year &optional zone) -> [hours minutes seconds]
### chronometrist-project-at-point
-> String

# chronometrist-report
## Time operations
### chronometrist-report-day-of-week->number
String -> Integer
### chronometrist-date-op
(Int Int Int Int Int Int) Symbol Int? -> (Int Int Int Int Int Int Int?)
(seconds minutes hours day month year) operator count? -> (seconds minutes hours day month year dow dst utcoff)
