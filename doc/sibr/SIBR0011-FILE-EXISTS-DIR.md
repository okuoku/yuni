SIBR0011: file-exists? can be #f on directory
=============================================

Affected: `Racket`

On Racket `file-exists?` procedure return `#f` for directories, because 
it requires the path to be a regular file.

```scheme
(file-exists? ".") ;; => #f on Racket, otherwise #t
```

Other implementations will return `#t` for both files and directories.

Workaround
==========

Avoid `file-exists?` where possible (ref. TOCTTOU problem.)

`(yuni util files)` provides `file-regular?` and `file-directory?`
to detect directory availablity, with additional syscall cost.

