## code to prepare `test objects` dataset goes here
radon <- rstanarm::radon %>% dplyr::mutate(id__ = 1:dplyr::n())

p_base <- ggplot2::ggplot(radon, ggplot2::aes(x = log_radon, y = log_uranium, color = county)) +
    ggplot2::theme(legend.position = 'none')
p_radon <- p_base + ggplot2::geom_point()

af_radon <- function(plots, pointdf) {
    purrr::map(plots, ~. + ggplot2::geom_point(data = pointdf, color = 'red'))
}

test_ap <- annoplot(list(p_radon), radon, af_radon, annotype = "brush")
test_apfade <- annoplot_fade(list(p_base), ggplot2::geom_point, radon)

# usethis::use_data(test_objects, overwrite = TRUE)
