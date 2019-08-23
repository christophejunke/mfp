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
