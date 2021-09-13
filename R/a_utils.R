#' @importFrom magrittr "%>%"
#' @export



read_only <- function(name) {
    function(value) {
        if (missing(value)) {
            private$name
        } else {
            stop("variable is read only", call. = FALSE)
        }

    }
}




.gg_facet_dim <- function(p, row = T){

    valname <- ifelse(row, 'ROW', 'COL')
    checkmate::assert_class(p, 'ggplot')
    ggplot2::ggplot_build(p) %>%
        magrittr::extract2('layout') %>%
        magrittr::extract2('layout') %>%
        magrittr::extract2(valname) %>%
        unique() %>%
        length()
}


.plotid <- function(plotno) {
    sprintf("plot%s", plotno)
}

############################################################
# Utility functions for Shiny modules ap_shiny and multiap_shiny

# TODO: take in plotlist object type and output dimensions based on that
.plot_width_ <- function(n_col, overflow = 1, res = 72, scale = 1.5) {
    round(n_col * res * scale + overflow*res)

}

.plot_height_ <- function(n_row, overflow = 0.5, res = 72, scale = 1.5) {
    round(n_row * res * scale + overflow * res)
}

.plot_width <- function(dplist, plotno, ...) {

    n_cols <- dplist$plot_dims[[plotno]]$width

    .plot_width_(n_cols, ...)

}


.plot_height <- function(dplist, plotno, ...) {
    dp_class <- class(dplist)

    if ("SidebarPlots" %in% dp_class) {
        n_rows <- dplist$plot_dims[[plotno]]$height/2
    } else {
        n_rows <- dplist$plot_dims[[plotno]]$height
    }

    .plot_height_(n_rows, ...)
}


.get_rval <- function(val) {
    tryCatch(val(), error = function(c) NULL)
}




.inner_mod_name <- function(i) {
    sprintf("plots%s", i)
}

