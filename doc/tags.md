# Thoughts on tag-based task description
## Tagging styles
1. A task has a name and zero or more tags. The name describes the task at a high level, and the tags are used to refine the description. A comment can be used for additional details.
   Example -
   ```
   (event :name "Playing"
          :tags ("guitar" "solo")
          ...)
   (event :name "OSM"
          :tags ("survey")
          ...)
    ```
2. A task only has tags, and no name. It is identified in the interface by the tags alone.
3. A task only has tags. The first N tags (1 or 2?) form the task name.
   Examples
   (timeclock text format)
   ```
   OSM; survey
   Guitar; solo repertoire
   ```
   or (s-exp format)
   ```
   (event :tags ("guitar" "solo repertoire")
   ...)
   (event :tags ("OSM" "mobile mapping")
   ...)
   ```
## Backend
1. Timeclock-compatible CSV. May grow timeclock-project-list to large lengths, since timeclock will see every unique combination of tags as a new project.
2. A new s-exp based format (see [new-format.md](new-format.md))
3. Add an s-expression containing tags and other structured in the comment part of timeclock. Hacky, and the s-exp must be limited to one line (i.e. cannot be pretty printed), since the timeclock format is line-based.

## UI
1. Something to suggest tags which are commonly used with a given tag. Probably store it as a hash table or such, with tags as keys and `((tag . score) ...)` as values, sorted in descending order of scores.

UX flow
1. User sees list of their tasks (names or first tags, depending on tagging system design)
2. They select a task to start
3. When they stop with a universal arg, ask them if they want to add any tags. If we have tag-usage history, suggest adding those tags first.
   * maybe use ~~magit-popup~~ Transient to help compose an entry instead?
   * tag input must suggest earlier used tags, but must also support adding new ones.

Add new field - name of field - data -

~~magit-popup~~ Transient

# Tag UI
Should enable user to
1. create new tags
2. enter previously used tags quickly
3. avoid misspellings

## Solutions
* Use read-from-minibuffer - provides editable history, but no completion
  * maybe use levenshtein.el to achieve #3
* "Add previous tags? [y]es, [n]o, [e]dit"
* completing-read-multiple - completion for multiple candidates, history, can suggest the last-used set of tags via DEF/INITIAL-INPUT
* A command which calls completing-read in an infinite loop, letting the user enter one tag at a time, and provides the user with keys to accept or cancel (quitting the loop). But completing-read doesn't have a common keymap which is inherited by ido and other completing-read replacements. (or does it?)

Default tag behavior for toggle commands (i.e. when no tagging is specified) - add no tags, or use last-used tags? (if present)
