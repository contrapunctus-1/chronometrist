# Chronometrist-Assist
This is highly experimental and not in a usable stage, and thus lives in the `assist` branch.

TODO -
1. Investigate [this](https://emacs.stackexchange.com/questions/14466/how-to-run-an-after-save-hook-only-when-the-buffer-has-been-saved-manually) as a solution

## basic outline
1. user defines `chronometrist-patterns-list` in the form
`'(("PROJECT" :PATH ... :MODE ... :REASON ... :HIDE ...) *)`
and enables suggestions/auto clocking
2. `chronometrist` adds `chronometrist-assist` to `first-change-hook`
3. User edits a file, triggering `first-change-hook`
4. If `chronometrist-assist` is 'suggest or 'auto, for each entry in the list, `chronometrist-assist` checks if the file path matches `:PATH` or if the current buffer major-mode matches `:MODE`. For the first entry that does, it takes the configured action (suggestion or clock-in). If there is no matching entry, it does nothing.

## Problems
How will this work when working with multiple windows? Multiple file types?
- paths seem to be a better choice than modes now, eh...

`first-change-hook` doesn't seem to understand when a _user_ is editing a file

Lack of the assist feature taught you some self-control - you didn't go about switching between projects so frequently, because there was some overhead of clocking in/out involved.
- The reduced jumping between tasks also means your activity 'history' is a lot less fragmented and easier for a human to read.

If you decline the prompt, you tend to get asked again very soon. Maybe remember the rule that matched and don't ask for it again for X minutes?

Asked for non-user-edited files
- if you use :PATH, you
- maybe minor mode?

`("Programming" :path "\.emacs\.d" :mode ("emacs-lisp" "markdown" "org"))` matches all sorts of program-generated buffers and screws up your init and quit /o\

Why on earth does the Chronometrist buffer itself get matched? It's not even associated with any file!

## Keywords
`:REASON`
- t - default, always ask
- string - use as initial contents when asking
- function - use result as contents
- nil - never ask, always leave blank

`:HIDE` - if t, don't show the project in the Chronometrist buffer (but do show it in Report and Statistics). Effectively a way of 'deleting' the project without removing its entries from the timelog file.

## Expressing and/or
Problem - sometimes you want a path AND a mode to match, and sometimes you want to OR them.
- :PATH and :MATCH could be AND'd, actually. To do an OR, a project could have more than one entries -

```el
'(("Project 2501" :path ("path 1" "path 2") :mode "mode 1" :reason "reason 1")
  ("Project 2051" :path "path 2" :mode "mode 2" :reason "reason 2"))
```

In this, the same project will get different reasons depending on whether (AND "path 1" "path 2" "mode 1") conditions apply OR (AND "path 2" "mode 2").
- That's not how `:HIDE` should work, though
  - maybe any occurrence of it can mean "hide this project"
  - or maybe only an occurrence in the first entry for the project
  - or maybe create a separate list for projects to be hidden

An alternative approach - support `:path (or "path 1" "path 2")` (but that doesn't make no sense for `:mode`...)
- maybe multiple arguments to :path should always be AND, and multiple arguments to :mode always OR?

My usecase - `'(("Programming" :path "programming"))`
I want to clock-in for all Markdown, Org, and Elisp files in this directory, but not for HTML.

Waaaaiiiitasec. These are regexps. They have AND and OR built in!! 🤦‍♀️

Urgh, I now realize I also need to filter assistance to only buffers associated with files...what if, instead of `:path` and `:mode`, we just ask the user for a function which is passed the buffer object?

## suggest/auto clock out
+ never
+ on save
  - User can just add to `after-save-hook` :\
+ idle timer
  - Not an ideal (ha) idea - what if the user is still working in Emacs, but on another project? Idle timer won't trigger, they'll stay clocked in to the old one...bad.
  - A timer, then? Remember the condition entry that triggered the clock-in, see if any files/buffers that match it have been modified since X seconds - if not, take action.

# Projects as trees or tags
I seem to be using clock-out reasons as a way to describe projects more specifically...which seems like a clumsy hack stemming from the lack of task-trees/tags

I would like to use an s-expression backend for trees
- Not really necessary - you can keep using the timeclock-file format, and just store the tree relationships in a separate file.
- Tags could be shoehorned into the timeclock-file format too, we'd just have to change the way we read it. Each 'project name' would become quoted CSV tags.

An example -
```
'(("OSM" "Surveying" "JOSM")
  ("Music" "Guitar" "Keyboard" "Singing" "Sequencing" "Composing"))
```
