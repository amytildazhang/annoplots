library(testthat)
library(annoplots)


# global test objects
radon <- rstanarm::radon %>% dplyr::mutate(id__ = 1:dplyr::n())

p_base <- ggplot2::ggplot(radon, ggplot2::aes(x = log_radon, y = log_uranium, color = county))
p_radon <- p_base + ggplot2::geom_point()

af_radon <- function(plots, pointdf) {
    purrr::map(plots, ~. + ggplot2::geom_point(data = pointdf, color = 'red'))
}




test_check("annoplots")
