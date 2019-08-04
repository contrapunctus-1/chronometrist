Thoughts for a new file format

## Certain
* s-exp based - easier parsing
* support for tags

## Unertain
* Latest entry first? Easier human editing
* Support for concurrent activities? I like the discipline imposed by one-activity-at-a-time, but some may like the option.
* Event-pair based? (i.e. combine start and end entries into one, so each entry is a complete entity by itself.)
* Time format? A compromise between human and machine readable? (easiest to parse?)
  `:start (2019 4 1 22 10 00)`
  or ISO format as a string?
  `:start "2019-04-01T22:10:00+05:30"`

Event pair
```
(event
  :tags  (music playing guitar solo)
  :start (2019 4 1 11 00 00)
  :end   (2019 4 1 12 00 00)
  :comment "Comment.")
```
