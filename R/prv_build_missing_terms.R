
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
#' @keywords internal
#'
build_missing_terms <- function(modelobj, data) {
    # 0. I need to get the levels of factor variables from the original dataset.
    # Normally, models include this in a $xlevels item, but coxme does not, and
    # I bet other model objects may not either.
    stopifnot(
        "The dataframe used to fit the model should be included in the `data =` argument." =
            !is.null(data)
    )

    all_levels <- Map(levels, data)
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

    # Which of the non-interaction terms are Factors?
    factor_vars <-
        !sapply(relevant_levels, is.null)

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

            # TAIL: Doing colnames(all_vars) here gives me the
            colnames(all_vars)
            # unique(c(unlist(all_intx), names(relevant_levels)))
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

                # This makes expand.grid() sort the names in their supposed factor
                # level order.
                pairs_grid <- expand.grid(covars_to_expand, stringsAsFactors = TRUE)
                pairs_grid <- pairs_grid[do.call(order, as.list(pairs_grid)), ]

                result <-
                    apply(
                        pairs_grid,
                        1,
                        function(...) {
                            paste0(..., collapse = ":")
                        }
                    )

                unname(result)
            },
            all_intx
        )

    # Remove all interaction terms from the unexpanded from the xlevels, then
    # replace them with their expanded versions. This works because interactions
    # go on the end of the model results by default, and then I am producing the
    # expanded names in the order that they appear in the original model.
    custom_xlevels[names(custom_xlevels) %in% names(constructed_terms)] <- NULL
    final_terms <- unlist(append(custom_xlevels, constructed_terms), use.names = FALSE)


    # 4. Return the results.
    original_terms <- names(stats::coef(modelobj))

    with_singletons <- c(original_terms[!(original_terms %in% final_terms)], final_terms)

    filled_terms <- with_singletons[!(with_singletons %in% reference_levels)]

    missing_terms <- filled_terms[!(filled_terms %in% original_terms)]

    list(
        original_terms   = original_terms,
        missing_terms    = missing_terms,
        filled_terms     = filled_terms,
        with_singletons  = with_singletons,
        reference_levels = reference_levels
    )
}
