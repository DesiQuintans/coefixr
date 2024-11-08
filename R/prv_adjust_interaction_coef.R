
# Adjust an interaction term's coefficients
#
# The adjustment is done by summing all of the coefficients that go into a term.
# For example, if a model has the term "SexMale", then it will simply get the
# coefficient it already has. However, the term "SexMale:EducationPrimary" would
# be adjusted as "SexMale:EducationPrimary + SexMale + EducationPrimary".
#
# @param modelobj (Object) A model object.
# @param data (Dataframe) The data used to fit the model.
#
# @return A Numeric vector, where the names are all of the terms from the
#     model, and the values are the adjusted coefficients.
# @md
# @keywords internal
#
adjust_interaction_coef <- function(modelobj, data) {
    # 1. Get the names of the effects and the estimates of each.
    mdl_terms <- names(stats::coef(modelobj))
    mdl_coefs <- stats::coef(modelobj)


    # 2. Get an exploded list of the terms in the model, including reference
    # levels.
    filled_terms <- build_missing_terms(modelobj, data = data)$filled_terms

    split_terms <-
        mapply(
            function(x, y) {
                unique(c(x, y))
            },

            filled_terms,
            strsplit(filled_terms, ":")
        )


    # 3. For each effect, replace its full name with its associated estimate. E.g.
    # the previous example may be replaced with "-0.3423", "1.345", "0.845". Result
    # is coerced to Character because split_terms is Character.
    replaced_with_coefs <-
        Map(
            function(this_term) {
                for (i in seq_along(this_term)) {
                    this_term[i] <-
                        tryCatch(mdl_coefs[this_term[i]], error = function(e) { NA_real_ })
                }

                return(this_term)
            },

            split_terms
        )


    # 4. Join all of the replaced coefficients with a + sign (e.g. "-0.3423 + 1.345 + 0.845")
    # and then turn that text into code and calculate it to produce a new estimate.
    calculated_coefs <-
        Map(
            function(coef_list) {
                coef_list <- suppressWarnings(as.numeric(coef_list))

                # If every coefficient is missing, then this is the interaction
                # of reference levels, and the coefficient should be set to 0.00
                # (since this is not exponentiated yet).
                if (all(is.na(coef_list))) {
                    return(0.00)
                } else {
                    return(sum(coef_list, na.rm = TRUE))
                }
            },

            replaced_with_coefs
        )

    # 5. Done!
    unlist(calculated_coefs)
}
