


#' @export
AnnoPlot <- R6::R6Class(
    "AnnoPlot",

    private = list(
        .data = NULL,
        .add_idcol = function(df) {
            if (!("id__" %in% colnames(df))) {
                df$id__ <- 1:nrow(df)
            }
            df
        }
    ),
    public = list(
        app_df = NULL,
        plot = NULL,
        anno_plot = NULL,
        annotype = NULL,
        plot_dims = NULL,
        annotation_function = NULL,
        id_col = NULL,
        hover_cols = NULL,
        initialize = function(plot, plotdata, annofun, shinydata = NULL,
                              annotype = "brush",
                              plot_dims = c(), hover = c(), filterfun = NULL) {
            if (checkmate::test_class(plot, "gg")) {
                plot <- list(plot)
            }
            if (length(names(plot)) == 0) {
                names(plot) <- sprintf("Plot %s", 1:length(plot))
            }
            self$plot <- plot
            self$plot_dims <- purrr::map(plot_dims, ~list(height = .[1], width = .[2]))

            self$annotation_function <- annofun
            self$annotype <- annotype
            self$hover_cols = hover

            if (!is.null(filterfun)) {
                self$filter_points <- filterfun
            }
            # self$id_col = id_col
            private$.data <- private$.add_idcol(dplyr::ungroup(plotdata))
            if (is.null(shinydata)) {
                self$app_df <- private$.data
            } else {
                self$app_df <-  private$.add_idcol(dplyr::ungroup(shinydata))
            }

            invisible(self)
        },
        annotate_plot = function(points) {
            if (length(points) > 0) {
                self$anno_plot <- self$annotation_function(
                    self$plot, self$filter_points(points)
                )

            } else {
                self$anno_plot <- self$plot
            }
            invisible(self)
        },

        mutate = function(...) {
            mutations = rlang::quos(...)
            private$.data <- dplyr::mutate(private$.data, !!!mutations)
        },
        # given point IDs, select corresponding rows for plot
        filter_points = function(points) {
            colnms <- intersect(colnames(self$app_df), colnames(self$data))
            self$app_df %>%
                dplyr::filter(id__ %in% points) %>%
                dplyr::select_at(unique(colnms)) %>%
                dplyr::left_join(self$data) %>% unique()
        },

        match_points = function(subdf) {
            colnms <- c("id__", intersect(colnames(self$app_df), colnames(self$data)))

            dplyr::inner_join(subdf, dplyr::select_at(self$app_df, unique(colnms))) %>%
                dplyr::pull(id__) %>% as.vector() %>% unique()
        }




    ),

    active = list(
        data = pryr::unenclose(read_only(.data))
    )
)




# Creates AnnoPlot objects
#' @export
AP_fade <-  R6::R6Class(
    "AP_fade",
    inherit = AnnoPlot,
    private = list(
        .plot_skeleton = NULL,
        .plot_background = NULL,
        #' @param p ggplot object
        .adjust_colors = function(p, addwhite = 0.5, saturation = 0.7) {
            with_points <- p + self$geomfn()
            gdat <- ggplot2::ggplot_build(with_points)$data
            grid1 <- gdat[[length(gdat)]] #

            colors_defined <- intersect(names(grid1), c("colour", "fill"))

            if (length(colors_defined) > 0) {
                p <- with_points
                for (colsetting in colors_defined) {
                    if (is.null(grid1[[colsetting]])) next

                    # match colors to groups
                    colors <- cbind(grid1$group, grid1[[colsetting]]) %>% unique()

                    # simulate reducing alpha by adding white
                    new_colors <- shades::addmix(colors[,2], "white", addwhite) %>%
                        shades::saturation(shades::scalefac(saturation))

                    order <- as.numeric(colors[, 1])
                    if (colsetting == "colour") {
                        p <- p + ggplot2::scale_color_manual(values = new_colors[order])
                    } else {
                        p <- p + ggplot2::scale_fill_manual(new_colors)
                    }
                }
            } else {
                p <- p + self$geomfn(alpha = addwhite, color = "gray", fill = "gray")
            }


            p + theme(legend.position = 'none')
        },
        .annotation_function = function(plot, pointdf) {
            if (nrow(pointdf) == 0) {
                plot
            }

            purrr::map2(private$.plot_background, private$.plot_skeleton, function(bp, sp) {
                # overlay <- cowplot::draw_plot(
                # sp +
                #         self$geomfn(alpha = 0) +
                #         ggplot2::theme(
                #             panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                #             plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                #             panel.grid = ggplot2::element_blank() # get rid of gridlines
                #             # panel.grid.minor = ggplot2::element_blank(), # get rid of minor grid
                #             # legend.background = ggplot2::element_rect(fill = "transparent"), # get rid of legend bg
                #             # legend.box.background = ggplot2::element_rect(fill = "transparent") # get rid of legend panel bg
                #         ) +
                #         self$geomfn(data = pointdf) +
                # ggplot2::geom_rug(data = pointdf, length = ggplot2::unit(0.05, "npc"), sides = "bl")
                # )
                # cowplot::align_plot(bp, overlay, align="hv", axis="tblr")

                bp +
                    self$geomfn(data = pointdf, size = 2.5)


            })
        }

    ),
    public = list(
        geomfn = NULL,
        initialize = function(clearplot, geomfn, plotdata, shinydata = NULL,
                              annotype = "brush", ...) {

            super$initialize(clearplot, plotdata,
                             annofun = NULL, shinydata, annotype, ...)
            self$geomfn <- geomfn
            self$annotation_function <- private$.annotation_function
            self$generate_plot(self$plot)

            invisible(self)
        },
        generate_plot = function(clearplot, addwhite = 0.25, saturation = 0.7, ...) {
            private$.plot_skeleton <- clearplot
            self$plot <- purrr::map(clearplot, function(p) {
                p + self$geomfn(...)
            })

            private$.plot_background <- purrr::map(clearplot, function(p) {
                # cowplot::ggdraw(private$.adjust_colors(p, addwhite, saturation))
                p + self$geomfn(alpha = addwhite)
            })


            invisible(self)
        },

        annotation_function = NULL
    )
)






#' @export
annoplot <- function(plot, plotdata, annofun, appdata = NULL, annotype = "brush",
                     dims = c(), hover = c(), filterFUN = NULL) {

    AnnoPlot$new(plot, plotdata, annofun, appdata, annotype = annotype,
                 plot_dims = dims, hover = hover, filterfun = filterFUN)
}


#' @export
annoplot_fade <- function(plot, geom, plotdata, shinydata = NULL, annotype = "brush", ...) {
    AP_fade$new(plot, geom, plotdata, shinydata, annotype, ...)
}
