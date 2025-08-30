cache/: local, ignored; tables/models/final: tracked if added

Policy:
- outputs/cache/  → local only (ignored by Git). Large files, temporary caches.
- outputs/tables/, outputs/models/, outputs/final/ → commit selected small results you want to share.

How to add a result:
1) Write to one of the tracked folders, e.g. outputs/tables/.
2) git add the file, commit, and push.

