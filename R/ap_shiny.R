
#' @describeIn ap_shiny
#' @export
#' @family shinymod
APlotOutput <- function(id, hover = F) {
    ns <- shiny::NS(id)

    # placeholder
    if (hover) {
        shiny::fluidPage(
            shiny::div(shiny::plotOutput(ns("legend"), height = "auto")),
            shiny::fluidRow(shiny::textOutput(ns("hover"))),
            shiny::fluidRow(shiny::uiOutput(ns("plots")))
        )

    } else {
        shiny::fluidPage(
            # shiny::div(shiny::plotOutput(ns("legend"), height = "auto")),
            shiny::fluidRow(
                shiny::uiOutput(ns("plots"))
            )
        )

    }
}





#' Functions for adding 'annoplot' objects to a Shiny app
#' @param id
#' @param hover
#'
#' @param input
#' @param output
#' @param session
#' @param dplist
#' @param goreactive
#' @param points
#' @param brush
#' @param scale
#' @param hover
#'
#' @export
#' @family shinymod
APServer <- function(id, dplist, goreactive, points, brush = T, scale = 1.5, hover = F) {
    shiny::moduleServer(
        id,
        function(input, output, session) {

            n_plots <- ifelse(is.null(dplist), 0, 1) # number of plots bundled into dplist
            message(n_plots)
            output$plots <- shiny::renderUI({
                if (n_plots > 0) {
                    plot_output_list <- lapply(1:n_plots, function(i) {

                        plotname <- session$ns(.plotid(i))
                        annotype <- dplist$annotype[i]
                        message(dplist$annotype[i])
                        if (annotype == "brush") {
                            po <- shiny::plotOutput(plotname, inline = T,
                                                    brush = shiny::brushOpts(id = session$ns("brush"),
                                                                             fill = "gray"),
                                                    hover = session$ns("hover"))

                        } else if (annotype == "click") {
                            po <- shiny::plotOutput(plotname, inline = T, click = session$ns("click"),
                                                    hover = session$ns("hover"))
                        } else if (annotype == "none") {
                            po <- shiny::plotOutput(plotname, inline = T, hover = session$ns("hover"))
                        }

                        po
                        # shiny::fillCol(po)
                    })

                } else {
                    plot_output_list <- list()
                }

                shiny::tagList(plot_output_list)

            })


            plots <- shiny::eventReactive(goreactive(), {
                dplist$annotate_plots(shiny::isolate(points$pt_idx))
                dplist$anno_plots
            })


            # render 'plots'
            lapply(1:n_plots, function(i) {
                local({
                    my_i <- i
                    plotname <- .plotid(my_i)
                    output[[plotname]] <- shiny::renderPlot({plots()[[my_i]]},
                                                            width = .plot_width(dplist, my_i, scale = scale),
                                                            height = .plot_height(dplist, my_i, scale = scale),
                                                            res = 72)

                })

            })
            # if (dplist$has_legend) {
            #     message("adding legend")
            #     # shiny::insertUI(sprintf("#%s", session$ns("plots")),
            #     #                 ui = shiny::plotOutput("legend"))
            #     output$legend <- shiny::renderPlot({
            #         cowplot::plot_grid(dplist$legend)
            #         }, height = 36*scale)
            # }



            # record and return dataframe

            brushed_points <- shiny::eventReactive(input$brush, {
                message("brushing")
                df <- shiny::brushedPoints(dplist$data, input$brush, allRows = F)
                message(df)
                dplist$match_points(df)
            })

            clicked_points <- shiny::eventReactive(input$click, {
                df <- shiny::nearPoints(dplist$data, input$click, addDist = T, allRows = T) %>%
                    dplyr::slice_min(dist_)

                dplist$match_points(df)
            })

            react_points <- shiny::reactive({
                c(.get_rval(brushed_points), .get_rval(clicked_points))

            })

            output$hover <- shiny::renderText({
                if (is.null(dplist$hover_cols)) {
                    NULL
                } else {
                    hover <- input$hover
                    near_df <- shiny::nearPoints(dplist$data, input$hover, addDist = T, allRows = F) %>%
                        dplyr::slice_min(dist_) %>%
                        dplist$filter_points() %>%
                        dplyr::select_at(dplist$hover_cols) %>%
                        dplyr::mutate_if(is.numeric, ~round(., digits = 3))


                    paste(sapply(colnames(near_df), function(cnm) {
                        sprintf("%s: %s", cnm, near_df[[cnm]][sample(1:nrow(near_df), size = 1)])
                    }))

                }
            })


            return(react_points)
        }
    )
}



