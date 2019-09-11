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
3. Split them at the data-consumer level (e.g. before calculating time for one day or getting events for one day)
   * Advantage - should reduce repetitive post-parsing load.

They are an issue not only in calculating time spent in $TIME_RANGE, but also acquiring the events for a day.

If we deal with it by changing the file, what happens when the user changes their day-start-time? The split-up events are now split wrongly, and the second event may get split _again._
* Maybe we should add another function to check if, for two events A and B, the :stop of A is the same as the :start of B, and that all their other tags are identical. Then we can re-split them according to the new day-start-time.
* Add a :split marker to split events? It can denote that the next event was originally a part of this one.
* Re-check and update the file when the day-start-time changes?
  * Possible with `add-variable-watcher` or `:custom-set` in Customize (thanks bpalmer)

# Arbitrary key values
A new idea - store them in a separate sub-plist. i.e. instead of this -

```
(:name    "Programming"
 :tags    ("Emacs Lisp")
 :project "Chronometrist"
 :feature "key-values"
 :start   "2019-09-11T13:42:16+0530"
 :stop    "2019-09-11T16:09:48+0530")
```
do this -
```
(:name       "Programming"
 :tags       ("Emacs Lisp")
 :user-plist (:project "Chronometrist"
              :feature "key-values")
 :start      "2019-09-11T13:42:16+0530"
 :stop       "2019-09-11T16:09:48+0530")
```
That makes sorting the plist into `(:name <user tags> :start :stop)` simpler. Conflicts between user-provided and Chronometrist-default keywords can be avoided without work.

## UI ideas
1. Pre-define keys for task names in a plist. Prompt for each key when clocking in/out for that particular task. (Adding new ones is extra work. Probably acceptable?)
2. \<bpalmer\> ask the user to type 'key=value', one per line, in a buffer. parse the buffer after the user submits with C-c C-c
   * smart behaviour/quick entry of common key-values - pre-insert the N most commonly used ones
   * twist - query user with completing-read in infinite loop - after each accepted input, add - when they quit, they land in the buffer (now containing all their accepted input), where they can `C-c C-c` to accept or `C-c C-k` to cancel. Something like this -
     ```
     (while t
       (insert (completing-read "Key (C-g to quit): " nil) "=")
       (insert (completing-read "Value (C-g to quit): " nil) "\n"))
     ```
     * Why not just use that to build an s-expression instead? Add : to keys (if they don't start with it) to make keywords, convert values to strings if they contain spaces and don't start with parens. `read` the buffer to get your plist.
       * C-g seems to cease all execution, including the function which called `kv-read` and to which `kv-accept` would return. I have a few options -
         1. ~~Try something with catch/throw~~
         2. Try a recursive edit
         3. Since the Elisp manual advises against #2, see what it recommends as alternatives ("e" and "m" commands in Rmail)
         4. Define your own key to quit from the completing-read loop. Yes, that causes issues with ido/helm/ivy setups. There will probably have to be special cases written for each of them :\
         5. Ditch the "loop completing-read" approach, define completion keys in the minor mode and do without confirm-on-new-key.

What does this make the UX flow?
1. `RET` - user clocks in/out
2. prompt for tags - `RET`/enter values and `RET`
3. buffer opens, prompt for key values - `C-c C-c`/edit key values and `C-c C-c`

That's `RET RET C-c C-c` to simply clock in/out, for someone who doesn't need the bells and whistles. Maybe we can make `C-u RET` do that for short.
* Also, add two variables - `chronometrist-ask-tags-p` and `chronometrist-ask-key-values-p`. Don't prompt for them if these are nil.

## Populating history data
When?
* The naive, but foolproof way - when the file changes -_-
* The first time we read the file. Then save it for the session, or persistently. Should work for 90% (?) of use cases (i.e. where changes happen near the end of the file).
* Re-read the file on an idle timer?
* Ignore history from file entirely. Only record history of interactive use. (File will provide commonly-used combinations, but not in the order the user uses them.)
  * Maybe we can start with this, and later add history-from-file as a fallback, after the interactive information is exhausted.
