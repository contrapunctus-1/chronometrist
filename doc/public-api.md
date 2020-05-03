# Public API thoughts
Possible "public API" candidates for semantic versioning/defining backward compatibility.

## File format
~~On the face of it, an ideal candidate. However, it is actually very unlikely to be changed _by us_ (since timeclock.el is the upstream), so semantic versioning based entirely on this might just leave us stuck on v1 forever. (Which might be a good thing, too.)~~

~~May be _extended_ in the distant future (i.e. support for another format).~~

We did change it, abandoning timeclock.el as a backend. I'm mulling over an SQL backend. Will warrant a major version bump, if it means deprecating the s-expression backend.

## Custom variables
Are definitely meant for the user.

Change in name or removal = major version bump.

## Commands (user-facing functions)
May change in behaviour often, but changes in name or removal can justify a major version bump.
