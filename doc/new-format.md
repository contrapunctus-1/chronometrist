# Thoughts on a new file format
## Certain
* s-exp based - easier parsing
* Support for tags
* Start and end entries combined into a single event entry. Necessary for querying e.g. "get an entry where comment is X and project is Y"
* Time format - ISO 8601 (as a string, or as a symbol?)
* Support for strings in tag lists

## Uncertain
* Latest entry first? Easier human editing
* Support for concurrent activities? I like the discipline imposed by one-activity-at-a-time, but some may like the option.
  * Will complicate the code, e.g. to check whether a project is active or not.
  * Isn't this partially addressed by tags?
* Arbitrary plist keys?

Event pair
```
(event
  :tags    (music playing guitar solo)
  :start   (2019 4 1 11 00 00)
  :end     (2019 4 1 12 00 00)
  :comment "Comment.")
```

# Status
Implementation of the new format has begun.
* The 'event' bit is being left out for the time being, for simplicity. Currently, each s-exp is simply a plist.
* Tag lists should definitely support strings. Symbols may also be permitted - in some cases, splitting a multi-word project name into individual symbols is a good choice, and they're syntactically less noisy than strings for single word tags.
* The reason we moved from the timeclock format to s-expressions was to have better-structured data. But converting some of that data directly to tags feels unstructured, too. Consider this example -
  ```
  (:tags  (theatre
           rehearsal
           "The Rose and The Ring"
           music
           practice
           "Ms. Foo")
   :start (2019 8 24 16 43 16)
   :stop  (2019 8 24 16 50 34))
  ```
  Would it not be nicer to write it like this?
  ```
  (:tags       (theatre rehearsal music practice)
   :production "The Rose and The Ring"
   :individual "Ms. Foo"
   :start      (2019 8 24 16 43 16)
   :stop       (2019 8 24 16 50 34))
  ```
  Of course, :production and :individual may not necessarily make sense outside the context of these particular tags. Therefore, this will require supporting arbitary plist keys.
  * Which, in turn, will complicate the UI.
  * Also, presented to the user, the keys will become "fields"...which won't be able to have spaces in them, unless we want to have keywords with escaped spaces :\

## Midnight-spanning events
Not sure how to deal with these. Previously, we checked if the first event for a day had an "o" code. Some possibilities -
1. Split them at the file level
   * Advantage - operation is performed only once for each such event (no repeated work) + benefits of both simplified data-consuming code and reduced post-parsing load.
2. Split them at the hash-table-level (i.e. rewrite chronometrist-events-clean)
   * Advantage - simplifies data-consuming code.
   * Now that the keys are indices, doing it here ties it to the function doing the deserialization. Alternatively, you have to update all the indices.
3. Split them at the data-consumer level (e.g. before calculating time for one day or getting events for one day)
   * Advantage - should reduce repetitive post-parsing load.

They are an issue not only in calculating time spent in $TIME_RANGE, but also acquiring the events for a day.

If we deal with it by changing the file, what happens when the user changes their day-start-time? The split-up events are now split wrongly, and the second event may get split _again._
* Maybe we should add another function to check if, for two events A and B, the :stop of A is the same as the :start of B, and that all their other tags are identical. Then we can re-split them according to the new day-start-time.
* Add a :split marker to split events? It can denote that the next event was originally a part of this one.
* Re-check and update the file when the day-start-time changes?
