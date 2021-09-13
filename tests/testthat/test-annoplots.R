
source("../../data-raw/test-objects.R")
# test throwing errors


test_that("can construct test annoplots", {
    expect_silent(annoplot(list(p_radon), radon, af_radon, annotype = "brush"))
    expect_silent(annoplot_fade(list(p_base), ggplot2::geom_point, radon))


    expect_s3_class(test_ap, "AP_fade")
    expect_s3_class(test_apfade, "AnnoPlot")

})
