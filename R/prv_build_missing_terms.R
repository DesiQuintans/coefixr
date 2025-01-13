
# Build all of the missing terms in a model
#
# R does not return interactions of covariates that involve the reference
# levels of factors. This function finds all of the interactions in a model,
# crosses all of the levels together, and returns that information and more.
#
# @param modelobj (Object) A model object.
# @param data (Dataframe) The data used to fit the model.
#
# @return A named List.
#
# For a formula `outcome ~ numA + Fct1 * Fct2` with the below columns:
#     - numA - Numeric
#     - Fct1 - Factor with levels A, B
#     - Fct2 - Factor with levels X, Y
#
# Then the list items will contain:
#
#     - `original_terms` is what R would return:
#       - numA
#       - Fct1B
#       - Fct2Y
#       - Fct1B:Fct2Y
#
#     - `complete_terms` has every possible term, which is the original terms,
#        plus the names of every covariate, plus all levels of all covariates, plus
#        all levels in interactions.
#        - numA
#        - Fct1
#        - Fct1A
#        - Fct1B
#        - Fct2
#        - Fct2X
#        - Fct2Y
#        - Fct1:Fct2
#        - Fct1A:Fct2X
#        - Fct1A:Fct2Y
#        - Fct1B:Fct2X
#        - Fct1B:Fct2Y
#
#     - `toplevel_all` is the names of the covariates, similar to how they are
#       named in the formula.
#        - numA
#        - Fct1
#        - Fct2
#        - Fct1:Fct2
#
#     - `all_ref_levels` is `complete_terms` without the names of covariates.
#        - numA
#        - Fct1A
#        - Fct1B
#        - Fct2X
#        - Fct2Y
#        - Fct1A:Fct2X
#        - Fct1A:Fct2Y
#        - Fct1B:Fct2X
#        - Fct1B:Fct2Y
#
#     - `only_intx_ref_levels` only includes reference levels within interactions.
#        - numA
#        - Fct1B
#        - Fct2Y
#        - Fct1A:Fct2X
#        - Fct1A:Fct2Y
#        - Fct1B:Fct2X
#        - Fct1B:Fct2Y
#
#     - `missing_terms` only includes the terms missing from `original_terms`, which
#       are interactions that involve reference levels.
#        - Fct1A:Fct2X
#        - Fct1A:Fct2Y
#        - Fct1B:Fct2X
#
#     - `reference_levels` is a named vector of the reference level of each Factor used in the model.
#       - Fct1  A
#       - Fct2  X
#
# @md
# @keywords internal
#
build_missing_terms <- function(modelobj, data) {
    # 0. I need to get the levels of factor variables from the original dataset.
    # Normally, models include this in a $xlevels item, but coxme does not, and
    # I bet other model objects may not either.
    stopifnot(
        "The dataframe used to fit the model should be included in the `data =` argument." =
            !is.null(data)
    )

    original_terms <- names(stats::coef(modelobj))

    all_levels <- Map(levels, data)
    all_independent_vars <- unique(unlist(strsplit(attr(modelobj$terms, "term.labels"), ":")))

    relevant_levels <- all_levels[names(all_levels) %in% all_independent_vars]


    # 1. Get the names of all variables in the model. All top-level variables
    # appear in the column names. Interactions have ":" in the column name.
    all_vars <- as.data.frame(attr(stats::terms(modelobj), "factors"))
    all_toplevel  <- colnames(all_vars)

    # Covariates used in each interaction are flagged with `1`.
    all_intx <- subset(all_vars, select = grep(":", all_toplevel))
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

            colnames(all_vars)
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
    final_terms <- append(custom_xlevels, constructed_terms)

    reference_levels <-
        unlist(
            Map(
                function(fctlvl) {
                    fctlvl[!(fctlvl %in% original_terms)]
                    # if (!is.null(relevant_levels[[varname]])) {
                    #     paste0(varname, relevant_levels[[varname]][[1]])
                    # }
                },

                # All
                final_terms[!grepl(":", names(final_terms))]
            )
        )

    # This is a version that has the covariate's name itself inserted above the
    # covariates's levels (if any).
    final_terms_with_varnames <-
        Map(
            function(nm, vec) {
                unique(c(nm, vec))
            },
            names(final_terms),
            final_terms
        )

    final_terms <- unlist(final_terms, use.names = FALSE)
    final_terms_with_varnames <- unlist(final_terms_with_varnames, use.names = FALSE)


    # 4. Return the results.
    complete_terms <- c(original_terms[!(original_terms %in% final_terms_with_varnames)], final_terms_with_varnames)

    all_ref_levels <-
        complete_terms[complete_terms %in% c(original_terms, final_terms)]

    only_intx_ref_levels <-
        all_ref_levels[!(all_ref_levels %in% reference_levels)]

    missing_terms <-
        only_intx_ref_levels[!(only_intx_ref_levels %in% original_terms)]

    list(
        original_terms       = original_terms,
        complete_terms       = complete_terms,
        toplevel_all         = all_toplevel,
        all_ref_levels       = all_ref_levels,
        only_intx_ref_levels = only_intx_ref_levels,
        missing_terms        = missing_terms,
        reference_levels     = reference_levels
    )
}
