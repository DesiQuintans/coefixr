
#' Build all of the missing terms in a model
#'
#' R does not return interactions of covariates that involve the reference
#' levels of factors. This function finds all of the interactions in a model,
#' crosses all of the levels together, and returns that information and more.
#'
#' @param modelobj (Object) A model object.
#' @param data (Dataframe) The data used to fit the model.
#'
#' @return A named List.
#'     - `original_terms` has the original terms given by R.
#'     - `missing_terms` are the terms that were not originally given, which involve the reference levels of factors in interactions.
#'     - `filled_terms` is the union of `original_terms` and `missing_terms`.
#'     - `with_singletons` also includes the reference terms outside interactions.
#'     - `reference_levels` is the reference level of each Factor used in the model.
#' @md
#'
build_missing_terms <- function(modelobj, data = NULL) {
    # 0. I need to get the levels of factor variables from the original dataset.
    # Normally, models include this in a $xlevels item, but coxme does not, and
    # I bet other model objects may not either.
    if (is.null(data)) {
        stopifnot(
            "`$call$data` is not included in the model object, so it should be included in the `data =` argument." =
                !is.null(modelobj$call$data)
        )
        data <- eval(modelobj$call$data)
    }

    all_levels <- Map(levels, eval(modelobj$call$data))
    all_independent_vars <- unique(unlist(strsplit(attr(modelobj$terms, "term.labels"), ":")))

    relevant_levels <- all_levels[names(all_levels) %in% all_independent_vars]


    # 1. Get the names of all variables involved in each interaction. Interactions
    # have ":" in the column name, and covariates used in the interaction are
    # flagged with `1`.
    all_vars <- as.data.frame(attr(stats::terms(modelobj), "factors"))
    all_intx <- subset(all_vars, select = grep(":", colnames(all_vars)))
    all_intx <- apply(all_intx, 2, function(y) { names(y)[which(y == 1)] }, simplify = FALSE)


    # 2. Form the model term names. For Factors, it's the variable name and factor
    # label mashed together. For everything else, it's the variable name by itself.
    custom_xlevels <-
        Map(
            function(varname) {
                if (is.null(relevant_levels[[varname]])) {
                    # The variable had no levels, so it's not a factor. Just use
                    # the variable name by itself.
                    return(varname)

                } else {
                    # If the variable is a factor, then paste the name and
                    # level together.
                    return(paste0(varname, relevant_levels[[varname]]))
                }
            },

            unique(c(unlist(all_intx), names(relevant_levels)))
        )

    reference_levels <-
        unlist(
            Map(
                function(varname) {
                    if (!is.null(relevant_levels[[varname]])) {
                        paste0(varname, relevant_levels[[varname]][[1]])
                    }
                },

                names(relevant_levels)
            )
        )



    # 3. Construct the final terms by crossing the variables used in each
    # interaction.
    constructed_terms <-
        Map(
            function(this_interaction) {
                covars_to_expand <-
                    custom_xlevels[which(names(custom_xlevels) %in% this_interaction)]

                apply(
                    expand.grid(covars_to_expand, stringsAsFactors = FALSE),
                    1,
                    function(...) {
                        paste0(..., collapse = ":")
                    }
                )
            },
            all_intx
        )


    # 4. Return the results.
    original_terms <- names(stats::coef(modelobj))

    missing_terms <- unlist(constructed_terms, use.names = FALSE)
    missing_terms <- missing_terms[!(missing_terms %in% original_terms)]

    filled_terms <- unique(c(original_terms, missing_terms))
    sorted_filled_terms <- filled_terms[order(grepl(":", filled_terms, fixed = TRUE))]

    with_singletons <- unique(c(unlist(custom_xlevels, use.names = FALSE), original_terms, missing_terms))
    sorted_singletons <- with_singletons[order(grepl(":", with_singletons, fixed = TRUE))]

    list(
        original_terms   = original_terms,
        missing_terms    = missing_terms,
        filled_terms     = sorted_filled_terms,
        with_singletons  = sorted_singletons,
        reference_levels = reference_levels,
        custom_xlevels   = custom_xlevels
    )
}
