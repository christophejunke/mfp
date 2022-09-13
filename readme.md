# mfp

Fetch RSS from http://musicforprogramming.net/ and download music.

# main usage

    ;; download all missing entries in mfp:*path*
    (mfp:update)

# utilities

    ;; fetch vector of (entry index title link) objects using RSS
    (mfp:fetch)

    ;; download an entry as (mfp:path entry), which depends on mfp:*path*
    (let ((entry (random-elt (mfp:fetch))))
      (mfp:download entry))

# settings

- `*rss-url*` : path to MFP website (default: `https://musicforprogramming.net/rss.php`)
- `*path*`: local directory for music storage (default: "~/music/mfp/")
- `*default-worker-count*`: when no existing lparallel kernel exists, number of workers to supply when creating a temporary one (default: 4)
- `*max-parallel-downloads*`: Maximum number of parallel downloads, or `nil` to use all workers available in the (possibly temporary) lparallel kernel (default: `nil`)
- `*naming-function*`: if non-nil, a custom function for naming downloaded files (default: `nil`). The function is called with an index and a title.

# errors

A condition of type `request-failed` is signaled on errors.
