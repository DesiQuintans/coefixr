
#' Adjust an interaction term's Standard Error (SE)
#'
#' The adjustment is done by 1) retrieving the variance-covariance table for the
#' model and multiplying the off-diagonal cells (covariances) by 2, then
#' 2) selecting the cells that are relevant to the current term, then
#' 3) summing all those together and taking their square root.
#'
#' @param modelobj (Object) The model to adjust.
#'
#' @return A named Numeric vector, where the names are all of the terms from the
#'     model, and the values are the adjusted SEs.
#' @md
#' @keywords internal
#'
adjust_interaction_se <- function(modelobj) {
    # 1. Get the names of the effects and the variance-covariance table.
    # On-diagonal cells are variances. Off-diagonal cells are covariances and
    # need to be multiplied by 2 for the SE calculation. The off-diagonal values
    # are also duplicated on either side of the diagonal, so I'll only keep the
    # lower triangle.

    mdl_terms <- names(stats::coef(modelobj))

    covm  <- stats::vcov(modelobj)
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

    # 3. Find out which cells of the vcov table will be used.
    which_cells <-
        Map(
            function(these_terms) {
                stats::setNames(mdl_terms %in% these_terms, mdl_terms)
            },

            split_terms
        )

    # 4. Grab the relevant cells from the vcov table and calculate the SE.
    calculated_se <-
        vapply(
            which_cells,

            function(loc) {
                sqrt(sum(covm[loc, loc], na.rm = TRUE))
            },

            double(1)
        )

    # 5. Add names to it, and done!
    names(calculated_se) <- mdl_terms
    calculated_se
}
