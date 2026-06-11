
# Adjust an interaction term's Standard Error (SE)
#
# The adjustment is done by 1) retrieving the variance-covariance table for the
# model and multiplying the off-diagonal cells (covariances) by 2, then
# 2) selecting the cells that are relevant to the current term, then
# 3) summing all those together and taking their square root.
#
# @param modelobj (Object) The model to adjust. If `NULL`, supply the result of `coef()` in `coefs` and `vcov()` in `vcovs`.
# @param interest (Character) A regular expression that matches the term(s) you want to include in the adjustment. Any non-matching term is not used. If `NULL` (default), then all terms are included.
# @param coefs (Numeric) A named numeric vector; the result of a `coef()` call. Supply only if `modelobj` is not supplied.
# @param vcovs (Matrix) A named numeric matrix; the result of a `vcov()` call. Supply only if `modelobj` is not supplied.
#
# @return A named Numeric vector, where the names are all of the terms from the
#     model, and the values are the adjusted SEs.
# @md
# @keywords internal
#
adjust_interaction_se <- function(modelobj = NULL, interest = NULL, coefs = NULL, vcovs = NULL) {
    if (is.null(modelobj)) {
        stopifnot(
            "If `modelobj` is missing, then both of `coefs` and `vcovs` must be supplied." =
                !is.null(coefs) & !is.null(vcovs)
        )
    }


    # 1. Get the names of the effects and the variance-covariance table.
    # On-diagonal cells are variances. Off-diagonal cells are covariances and
    # need to be multiplied by 2 for the SE calculation. The off-diagonal values
    # are also duplicated on either side of the diagonal, so I'll only keep the
    # lower triangle.

    if (is.null(modelobj)) {
        mdl_terms <- names(coefs)
    } else {
        mdl_terms <- names(stats::coef(modelobj))
    }

    if (is.null(modelobj)) {
        covm <- vcovs
        colnames(covm) <- mdl_terms
        rownames(covm) <- mdl_terms
    } else {
        covm <- stats::vcov(modelobj)
    }

    # TAIL: Why does multiplying BOTH the off-diagonals, rather than only
    # multiplying the lower one and NAing the upper, create NaNs later on?
    # covm[lower.tri(covm, diag = FALSE)] <- covm[lower.tri(covm, diag = FALSE)] * 2

    covm[upper.tri(covm, diag = FALSE)] <- NA
    covm[lower.tri(covm, diag = FALSE)] <- covm[lower.tri(covm, diag = FALSE)] * 2


    # 2. Get a list of terms where each term appears with its full name, as well
    # as exploded into its components (if it's an interaction).
    # For example, if the 3rd fixed effect is "SexMale:EducationPrimary", then the
    # 3rd list element will be a Character vector with 3 elements:
    # "SexMale:EducationPrimary", "SexMale", and "EducationPrimary".
    split_terms <-
        .mapply(
            FUN  = function(...) { unique(c(...)) },
            dots = list(
                all_terms = as.list(mdl_terms),
                split     = strsplit(mdl_terms, ":", fixed = TRUE)
            ),
            MoreArgs = NULL
        )

    names(split_terms) <- mdl_terms


    # 3. If a interest was specified, ignore all terms within interactions that
    # don't have the interest term.
    split_terms <-
        mapply(
            function(x) {
                if (is.null(interest) | length(x) == 1) {
                    return(x)
                } else {
                    return(x[grepl(interest, x)])
                }
            },

            split_terms
        )


    # 4. Find out which cells of the vcov table will be used.
    which_cells <-
        Map(
            function(these_terms) {
                stats::setNames(mdl_terms %in% these_terms, mdl_terms)
            },

            split_terms
        )

    # 5. Grab the relevant cells from the vcov table and calculate the SE.
    calculated_se <-
        vapply(
            which_cells,

            function(loc) {
                sqrt(sum(covm[loc, loc], na.rm = TRUE))  # Remember that the off-diagonals (covariances) are already multiplied by 2, so summing them is fine.
            },

            double(1)
        )


    calculated_se
}
