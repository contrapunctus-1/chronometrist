# Thoughts on a new file format

## Certain
* s-exp based - easier parsing
* support for tags
* start and end entries combined into a single event entry. Necessary for querying e.g. "get an entry where comment is X and project is Y"

## Uncertain
* Latest entry first? Easier human editing
* Support for concurrent activities? I like the discipline imposed by one-activity-at-a-time, but some may like the option.
  * will complicate the code, e.g. to check whether a project is active or not.
  * isn't this partially addressed by tags?
* Time format? A compromise between human and machine readable? (easiest to parse in Lisp?)
  `:start (2019 4 1 22 10 00)`
  or ISO format as a string? (easiest to parse in the shell?)
  `:start "2019-04-01T22:10:00+05:30"`

Event pair
```
(event
  :tags    (music playing guitar solo)
  :start   (2019 4 1 11 00 00)
  :end     (2019 4 1 12 00 00)
  :comment "Comment.")
```

A different approach -
```
(:tags       (music playing guitar solo)
 :start-date (2019 4 1)
 :start-time (11 0 0)
 :stop-date  (2019 4 1)
 :stop-time  (12 0 0)
 :comment    "Comment.")
```
