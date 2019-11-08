@include "strings/prefix.awk"

# dirname returns all but the last element of pathname, typically the pathname's directory.
function dirname(pathname) {
    if (!sub(/\/[^\/]*\/?$/, "", pathname)) {
        return "."
    } else if (pathname != "") {
        return pathname
    } else {
        return "/"
    }
}

# basename returns the last element of pathname.
function basename(pathname, suffix) {
    sub(/\/$/, "", pathname)
    if (pathname == "") {
        return "/"
    }

    sub(/^.*\//, "", pathname)

    if (suffix != "" && has_suffix(pathname, suffix)) {
        pathname = substr(pathname, 1, length(pathname) - length(suffix))
    }

    return pathname
}

# is_abs returns true if the path is absolute.
function is_abs(pathname) {
    return length(pathname) > 0 && has_prefix(pathname, "/")
}
