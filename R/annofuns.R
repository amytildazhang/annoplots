




af_from_calls <- function(calls, env = parent.frame()) {
    function(plots, pointdf) {
        purrr::map(plots, function(p)  {
            for (c in calls) {
                c$data <- pointdf
                p <- p + eval(c, env)
            }
            p
        })
    }
}

af_call <- function(geom, call, ...) {
    call[[1]] <- geom
    call$geom <- NULL
    ads <- list(...)
    for (nm in names(ads))  {
        call[[nm]] <- ads[[nm]]
    }
    call
}

#' Internal constructor for annotation functions
#'
#' @param geom
#' @param call
#' @param env
#'
#' @return
#' @export
#'
#' @examples
af <- function(call, env = parent.frame()) {

    function(plots, pointdf) {
        call$data <- pointdf
        purrr::map(plots, function(p)  {
            p + eval(call, env)
        })
    }
}

#' Title
#'
#' @param color
#' @param type
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
af_call_highlight <- function(color = NA, fill = NA, geomfun = ggplot2::geom_point, ...) {

    af_call(geomfun, match.call(), ...)


}


#' Title
#'
#' @param type
#' @param ... Arguments to pass on to ggrepel::geom_text/label_repel function.
#' @param label
#' @param bordered If TRUE, use ggrepel::geom_label_repel; otherwise use ggrepel::geom_text_repel.
#'
#' @return
#' @export
#'
#' @examples
af_call_label <- function(label, geomfun, ...) {
    lab <- rlang::enquo(label)

    call <- match.call()
    mc <- match.call(ggplot2::geom_text,
                     call('ggplot2::geom_text',
                          ggplot2::aes(label = !!lab)))
    call$label <- NULL

    call$mapping <- mc$mapping
    af_call(geomfun, call, ...)


}




#' Title
#'
#' @param xintercept
#' @param color
#' @param ... Arguments to pass on to ggplot2::geom_vline
#'
#' @return
#' @export
#'
#' @examples
af_call_vlines <- function(xintercept, ...) {
    call <- match.call()
    call$xintercept <- NULL


    xin <- rlang::enquo(xintercept)
    call$mapping <- match.call(
        ggplot2::geom_vline,
        call("ggplot2::geom_vline",
             ggplot2::aes(xintercept = !!xin))
    )$mapping

    af_call(ggplot2::geom_vline, call, ...)
}



af_highlight <- function(color = NA, fill = NA, geomfun = ggplot2::geom_point,
                         with_label = FALSE, label = NULL, lgeom = ggrepel::geom_text_repel,
                         largs = list(), ...) {
    dots <- list(...)
    call <- rlang::call2("af_call_highlight", color, fill, geomfun, !!!dots)

    if (!with_label) {
        af(eval(call))
    } else {
        lcall <- rlang::call2("af_call_label", rlang::enquo(label), lgeom, !!!largs)
        af_from_calls(list(eval(call), eval(lcall)))
    }
}



af_label <- function(label, geomfun = ggplot2::geom_text, ...) {
    dots <- list(...)
    call <- rlang::call2("af_call_label", rlang::enquo(label), geomfun, !!!dots)
    af(eval(call))
}


af_vlines <- function(xintercept,
                      with_label = FALSE, label = NULL, lgeom = ggrepel::geom_text_repel, largs = list(),
                      ...) {
    dots  <- list(...)
    call <- rlang::call2("af_call_vlines", rlang::enquo(xintercept), !!!dots)

    if (!with_label) {
        af(eval(call))
    } else {
        lcall <- rlang::call2("af_call_label", rlang::enquo(label), lgeom, !!!largs)
        af_from_calls(list(eval(call), eval(lcall)))
    }
}

# af_highlightlabel <- function(label, color, fill = NA, hgeom,
#                               lgeom, hargs = list(), largs = list()) {
#     lcall <- rlang::call2("af_call_label", rlang::enquo(label), lgeom, !!!largs)
#     h_call <- rlang::call2("af_call_highlight", color, fill, hgeom, !!!hargs)
#
#     af_from_calls(list(eval(h_call), eval(lcall)))
# }
#
#
