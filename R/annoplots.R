
#' @export
AnnoPlot <- R6::R6Class(
    "AnnoPlot",

    private = list(
        .data = NULL
    ),
    public = list(
        app_df = NULL,
        plots = NULL,
        anno_plots = NULL,
        annotype = NULL,
        plot_dims = NULL,
        n_plots = 1,
        annotation_function = NULL,
        id_col = NULL,
        hover_cols = NULL,
        initialize = function(plots, data, annofun,
                              annotype = "brush",
                              plot_dims = c(), hover = c(), filterfun = NULL) {
            if (checkmate::test_class(plots, "gg")) {
                plots <- list(plots)
            }
            if (length(names(plots)) == 0) {
                names(plots) <- sprintf("Plot %s", 1:length(plots))
            }
            self$plots <- plots
            self$n_plots <- length(self$plots)
            self$plot_dims <- purrr::map(plot_dims, ~list(height = .[1], width = .[2]))

            self$annotation_function <- annofun
            self$annotype <- rep(annotype, self$n_plots)
            self$hover_cols = hover

            if (!is.null(filterfun)) {
                self$filter_points <- filterfun
            }
            # self$id_col = id_col

            private$.data <- dplyr::ungroup(data)

            invisible(self)
        },
        annotate_plots = function(points) {
            if (length(points) > 0) {
                self$anno_plots <- self$annotation_function(
                    self$plots, self$filter_points(points)
                )

            } else {
                self$anno_plots <- self$plots
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


new_annoplot <- function(annofun,
                         baseplots, data, annotype, plot_dims,
                         hover, filterFun = NULL) {



}

#' @export
validate_annoplot <- function() {

}


# Creates AnnoPlot objects
#' @export
AP_bg <-  R6::R6Class(
    "AP_bg",
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


            p
        },
        .annotation_function = function(plots, pointdf) {
            if (nrow(pointdf) == 0) {
                plots
            }

            purrr::map2(private$.plot_background, private$.plot_skeleton, function(bp, sp) {
                overlay <- cowplot::draw_plot(
                    sp +
                        self$geomfn(alpha = 0) +
                        ggplot2::theme(
                            panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                            plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = ggplot2::element_blank(), # get rid of major grid
                            panel.grid.minor = ggplot2::element_blank(), # get rid of minor grid
                            legend.background = ggplot2::element_rect(fill = "transparent"), # get rid of legend bg
                            legend.box.background = ggplot2::element_rect(fill = "transparent") # get rid of legend panel bg
                        ) +
                        self$geomfn(data = pointdf) +
                        ggplot2::geom_rug(data = pointdf, length = ggplot2::unit(0.05, "npc"), sides = "bl")
                )

                bp + overlay



            })
        }

    ),
    public = list(
        geomfn = NULL,
        initialize = function(clearplots, geomfn, data,
                              annotype = "brush", plot_dims = c()) {

            super$initialize(clearplots, data,
                             annofun = NULL, annotype, plot_dims)
            self$geomfn <- geomfn
            self$annotation_function <- private$.annotation_function
            self$generate_plots(self$plots)

            invisible(self)
        },
        generate_plots = function(clearplots, addwhite = 0.5, saturation = 0.7, ...) {
            private$.plot_skeleton <- clearplots
            self$plots <- purrr::map(clearplots, function(p) {
                p + self$geomfn(...)
            })

            private$.plot_background <- purrr::map(clearplots, function(p) {
                cowplot::ggdraw(private$.adjust_colors(p, addwhite, saturation))
            })


            invisible(self)
        },

        annotation_function = NULL
    )
)






