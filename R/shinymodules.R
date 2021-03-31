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


dyn_plot_ui <- function(id, hover = F) {
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
      shiny::div(shiny::plotOutput(ns("legend"), height = "auto")),
      shiny::fluidRow(
        shiny::uiOutput(ns("plots"))
      )
    )

  }
}


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
dyn_plot <- function(input, output, session,
                     dplist, gobutton, points, brush = T, scale = 1.5, hover = F) {


  n_plots <- ifelse(is.null(dplist), 0, dplist$n_plots) # number of plots bundled into dplist

  #dynamically produce plot UI based on number required
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

    do.call(shiny::tagList, plot_output_list)

    # shiny::tagSetChildren(shiny::fillRow(), list = tags)
  })

  plots <- shiny::eventReactive(gobutton(), {
    dplist$annotate_plots(shiny::isolate(points$pt_idx))
    dplist$anno_plots
  })


  lapply(1:n_plots, function(i) {
    plotname <- .plotid(i)
    output[[plotname]] <- shiny::renderPlot({plots()[[i]]},
                                            width = .plot_width(dplist, i, scale = scale),
                                            height = .plot_height(dplist, i, scale = scale),
                                            res = 72)

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
    df <- shiny::brushedPoints(dplist$data, input$brush, allRows = F)
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
    hover <- input$hover
    near_df <- shiny::nearPoints(dplist$data, input$hover, addDist = T, allRows = F) %>%
     dplyr::slice_min(dist_) %>%
      # dplist$filter_points() %>%
      dplyr::mutate_if(is.numeric, ~round(., digits = 3))


    paste(sapply(colnames(near_df), function(cnm) {
      sprintf("%s: %s", cnm, near_df[[cnm]][sample(1:nrow(near_df), size = 1)])
    }))

    # paste(df$id__, collapse = " ")
  })



  # browser()


  return(react_points)
}




dyn_groupplot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("multiplots"))
}


.inner_mod_name <- function(i) {
  sprintf("plots%s", i)
}



dyn_groupplot <- function(input, output, session,
                          listdps, gobutton, points, hover = F, ...) {
  n_dp <- length(listdps)


  server_points <- do.call(shiny::reactiveValues, purrr::map(1:n_dp, function(i) {
    shiny::callModule(dyn_plot,  .inner_mod_name(i),
                      dplist = listdps[[i]], gobutton, points, ...)

  }) %>% purrr::set_names(sprintf("points%s", 1:n_dp)))



  output$multiplots <- shiny::renderUI({
    do.call(shiny::tagList, lapply(1:n_dp, function(i) {
      shiny::fluidRow(
        shiny::h4(shiny::tags$b(names(listdps)[i])),
        dyn_plot_ui(session$ns(.inner_mod_name(i)), hover)
      )
    }))

  })



  sel_points <- shiny::reactive({

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


