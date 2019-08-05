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

## UI
1. Something to suggest tags which are commonly used with a given tag. Probably store it as a hash table or such, with tags as keys and `((tag . score) ...)` as values, sorted in descending order of scores.
