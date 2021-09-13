source("../../data-raw/test-objects.R")

test_that("Can access base plots", {
  expect_silent(ap_plots(test_ap))
  expect_silent(ap_plots(test_apfade))
      ps <- ap_plots(test_ap)[[1]]
  expect_s3_class(ps, "gg")

})


test_that("'highlight' returns plots", {
    # expect_silent(ap_highlight(test_ap, 1:10)) # currently not silent becuase of join in filter
    ps <- ap_highlight(test_ap, 1:10)[[1]]
    expect_s3_class(ps, "gg")

})


test_that("Can access public values", {
    for (valname in c("app_df", "plots", "annotype", "annotation_function",
                        "nano_plots", "data", "filter_points", "hover_cols", "id_col",
                        "mutate", "n_plots", "plot_dims")) {
        expect_silent(ap_get(test_ap, valname))
    }

})

#
#
# test_that("Can set public values", {
#
#
#     vals <- list("app_df" = radon, "plots" = list(p_radon),
#                  "annotation_function" = )
#     for (valname in c("app_df", "plots", "annotype", "annotation_function",
#                       "nano_plots", "data", "filter_points", "hover_cols", "id_col",
#                       "mutate", "n_plots", "plot_dims")) {
#         expect_silent(ap_get(test_ap, valname))
#     }
#
# })
