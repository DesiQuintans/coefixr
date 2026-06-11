
# Adjust an interaction term's confidence interval (CI)
#
# Simply uses the adjusted coefficients and SEs to recalculate the 95%
# confidence interval.
#
# @param modelobj (Object) A model object.
# @param data (Dataframe) The data used to fit the model.
# @param interest (Character) A regular expression that matches the term(s) you want to include in the adjustment. Any non-matching term is not used. If `NULL` (default), then all terms are included.
#
# @return A named List. `$lwr` and `$upr` contain named Numeric vectors that
# are the lower and upper 95% confidence interval, respectively. The names in
# each vector are the names of the terms from the model.
# @md
# @keywords internal
#
adjust_interaction_ci <- function(modelobj = NULL, data, interest = NULL, coefs = NULL, vcovs = NULL) {
    # 1. Get model's adjusted estimates and SEs.
    model_se   <- adjust_interaction_se(modelobj = modelobj, interest = interest, coefs = coefs)
    model_coef <- adjust_interaction_coef(modelobj = modelobj, data = data, interest = interest, coefs = coefs)

    model_terms <- build_missing_terms(modelobj = modelobj, data = data)
    all_terms   <- model_terms$complete_terms
    ref_levels  <- model_terms$reference_levels
    ref_levels  <- ref_levels[order(nchar(ref_levels), decreasing = TRUE)]


    # SEs are only calculable for terms in the original model, i.e. terms
    # involving reference levels won't have SEs.
    nonref_coefs <- model_coef[names(model_se)]

    # Terms not involving interest should be flagged too.
    if (is.null(interest)) {
        uninterested_coefs <- character(0)
    } else {
        uninterested_coefs <- all_terms[!grepl(interest, all_terms)]
        uninterested_coefs <- uninterested_coefs[order(nchar(uninterested_coefs), decreasing = TRUE)]
    }

    # 2. Work out lower and upper CIs.
    lwr_cis <- nonref_coefs - 1.96 * model_se
    upr_cis <- nonref_coefs + 1.96 * model_se

    # 3. The interaction terms that involved the reference levels of factors are
    # missing CIs, and need to inherit them from the other non-reference term.
    missing_ci <-
        stats::setNames(nm = names(model_coef)[!(names(model_coef) %in% names(model_se))])

    # Remove reference levels from terms.
    for (i in seq_along(ref_levels)) {
        missing_ci <- gsub(ref_levels[i], "", missing_ci, fixed = TRUE)
    }

    # Remove non-interest variables from terms.
    for (i in seq_along(uninterested_coefs)) {
        missing_ci <- gsub(uninterested_coefs[i], "", missing_ci, fixed = TRUE)
    }

    missing_ci <- gsub("^:", "", missing_ci)      # ":ageOlder"         -> "ageOlder"
    missing_ci <- gsub(":$", "", missing_ci)      # "sexMale:"          -> "sexMale"
    missing_ci <- gsub(":{1,}", ":", missing_ci)  # "sexMale::ageOlder" -> "sexMale:ageOlder"

    missing_lwr <-
        Map(
            function(this_term) {
                unname(tryCatch(lwr_cis[this_term], error = function(e) { NA_real_ }))
            },

            as.list(missing_ci)
        )

    missing_upr <-
        Map(
            function(this_term) {
                unname(tryCatch(upr_cis[this_term], error = function(e) { NA_real_ }))
            },

            as.list(missing_ci)
        )


    # 4. Terms still missing CIs are reference terms, and should be set to 0.
    missing_lwr[is.na(missing_lwr)] <- 0.00
    missing_upr[is.na(missing_upr)] <- 0.00


    # 5. Return all of the CIs.
    all_lwr <- append(lwr_cis, missing_lwr)
    all_upr <- append(upr_cis, missing_upr)

    list(
        lwr = unlist(all_lwr[names(model_coef)]),
        upr = unlist(all_upr[names(model_coef)])
    )
}
