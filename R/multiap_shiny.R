.is_hover <- function(dp) {
  !is.null(dp$hover_cols)
}


#' Module functions for displaying multiple 'annoplot' objects in one UI block.
#'
#' These are called internally by the
#'
#'
#' @param id
#' @param input
#'
#' @param output
#' @param session
#' @param listdps
#' @param goreactive
#' @param points
#' @param hover
#' @param ...
#'
#' @export
#' @family shinymod
multiAP <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("multiplots"))
}

#'
#' @export
#' @family shinymod
multiAPServer <- function(id, listdps, goreactive, points, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      n_dp <- length(listdps)


      server_points <- do.call(shiny::reactiveValues, purrr::map(1:n_dp, function(i) {
        APServer(id = .inner_mod_name(i),
                 dplist = listdps[[i]], goreactive = goreactive, points = points,
                 hover = .is_hover(listdps[[i]]),
                 ...)

      }) %>% purrr::set_names(sprintf("points%s", 1:n_dp)))



      output$multiplots <- shiny::renderUI({
        do.call(shiny::tagList, lapply(1:n_dp, function(i) {
          shiny::fluidRow(
            shiny::h4(shiny::tags$b(names(listdps)[i])),
            APlotOutput(session$ns(.inner_mod_name(i)), hover = .is_hover(listdps[[i]]))
          )
        }))

      })



      sel_points <- shiny::reactive({
        message("selected points")
        vals <- lapply(shiny::reactiveValuesToList(server_points), function(val) {

          # catch the error thrown by val() when nothing has triggered the
          # reactive expression for that plotmodule
          tryCatch(val(), error = function(c) NULL)

        })
        vec <- do.call("rbind", vals)
        names(vec) <- NULL

        vec

      })

      sel_points
    }
  )


}


