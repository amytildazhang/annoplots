

SLICE_app <- R6::R6Class(
    "SLICE_app",
    private = list(
        .factors = NULL,
        .sideplots = NULL,
        .errors = NULL,
        .data = NULL,



        .split_W = function(sliceobjs, subsets, .subsetfun) {
            objs <- purrr::map(sliceobjs, ~.$split_W(subsets, .subsetfun)) %>%
                purrr::set_names(names(sliceobjs))

            purrr::map(subsets, function(set) {
                purrr::map(objs, ~.[[set]]) %>% purrr::set_names(names(objs))
            }) %>% purrr::set_names(subsets)
        }

    ),
    public = list(


        initialize = function(data,
                              factorplots = NULL,
                              sideplots = NULL,
                              errorplots = NULL
                              # dflist = NULL,
                              # # relationship,
                              # pars = colnames(data), continuous = c(),
                              # stratify = NULL, bf = T,
                              # subsets = NULL, .subsetfun = NULL,
                              # clusters = NULL) {
        ){
            # message(sprintf("stratify is %s", as.character(stratify)))

            private$.data <- dplyr::ungroup(data)

            purrr::walk(list(factorplots, sideplots, errorplots), function(pl) {
                purrr::walk(pl, function(ap) {
                    ap$app_df <- data
                })
            })
            self$factorplots <- factorplots
            self$sideplots <- sideplots
            self$errorplots <- errorplots
            #  message(continuous)
            #  private$.data <- sliceobjs[[1]]$data
            #  # TODO: modify use of relationship in tidy eval style
            #
            # self$sideplots <- SidebarPlots$new(sliceobjs, pars, continuous, stratify)$generate_plots()
            # self$errorplots <- APL_Errors$new(sliceobjs, mod_x, mod_y, stratify)$generate_plots()
            #
            #  if (length(subsets) > 0) {
            #      fct_objs <- private$.split_W(sliceobjs, subsets, .subsetfun)
            #      fct_plots <- purrr::map(subsets, function(set) {
            #          APL_Factors$new(fct_objs[[set]], mod_x, mod_y, stratify, bf)
            #          APL_Factors$define_relationship(relationship, clusters)$generate_plots()
            #      })
            #      # TODO: accept list
            #
            #  } else {
            #      aplf <- APL_Factors$new(sliceobjs, mod_x, mod_y, stratify, bf)
            #
            #      fct_plots <- list(aplf$define_relationship(relationship, clusters)$generate_plots()) %>%
            #          purrr::set_names("Weight Plots")
            #
            #  }
            #
            # self$factorplots <- fct_plots


            invisible(self)

        },

        factorplots = NULL,
        sideplots = NULL,
        errorplots = NULL
    ),
    active = list(
        data = pryr::unenclose(read_only(.data))#,
        # factorplots = pryr::unenclose(read_only(.factors)),
        # sideplots = pryr::unenclose(read_only(.sideplots)),
        # errorplots = pryr::unenclose(read_only(.errors))
    )
)


launch <- function(sso, ...) {
    # credit to https://github.com/stan-dev/shinystan/

    checkmate::assert_r6(sso, "SLICE_app")
    .sso_env$.SLICE_OBJECT <- sso

    on.exit(.sso_env$.SLICE_OBJECT <- NULL, add = TRUE)
    shiny::runApp(system.file("SLICE", package = "slice"), ...)
}

