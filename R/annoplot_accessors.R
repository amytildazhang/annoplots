
#' @export
ap_plots <- function(x) {
    UseMethod("ap_plots")
}

#' @export
ap_plots.AnnoPlot <- function(x) {
    x$plots
}

#' @export
ap_highlight <- function(x, points = c()) {
    UseMethod("ap_highlight")
}

#' @export
ap_highlight.AnnoPlot <- function(x, points = c()) {
    x$annotate_plots(points)
    x$anno_plots
}


#' @export
ap_set <- function(x, valname, value) {
    UseMethod("ap_set")
}


#' @export
ap_set.AnnoPlot <- function(x, valname, value) {
    x[[valname]] <- value
}



#' @export
ap_get <- function(x, valname) {
    UseMethod("ap_get")
}


#' @export
ap_get.AnnoPlot <- function(x, valname) {
    x[[valname]]
}

