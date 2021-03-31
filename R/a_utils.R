
read_only <- function(name) {
    function(value) {
        if (missing(value)) {
            private$name
        } else {
            stop("variable is read only", call. = FALSE)
        }

    }
}

