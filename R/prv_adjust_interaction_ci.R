
#' Adjust an interaction term's confidence interval (CI)
#'
#' Simply uses the adjusted coefficients and SEs to recalculate the 95%
#' confidence interval.
#'
#' @param modelobj (Object) A model object.
#' @param data (Dataframe) The data used to fit the model.
#'
#' @return A named List. `$lwr` and `$upr` contain named Numeric vectors that
#' are the lower and upper 95% confidence interval, respectively. The names in
#' each vector are the names of the terms from the model.
#' @md
#
adjust_interaction_ci <- function(modelobj, data) {
    # 1. Get model's adjusted estimates and SEs.
    model_se   <- adjust_interaction_se(modelobj)
    model_coef <- adjust_interaction_coef(modelobj, data = data)
    ref_levels <- build_missing_terms(modelobj, data = data)$reference_levels

    # SEs are only calculabled for terms in the original model, i.e. terms
    # involving reference levels won't have SEs.
    nonref_coefs <- model_coef[names(model_se)]

    # 2. Work out lower and upper CIs.
    lwr_cis <- nonref_coefs - 1.96 * model_se
    upr_cis <- nonref_coefs + 1.96 * model_se

    # 3. The interaction terms that involved the reference levels of factors are
    # missing CIs, and need to inherit them from the other non-reference term.
    missing_ci <-
        stats::setNames(nm = names(model_coef)[!(names(model_coef) %in% names(model_se))])

    for (i in seq_along(ref_levels)) {
        missing_ci <- gsub(ref_levels[i], "", missing_ci, fixed = TRUE)
    }

    missing_ci <- gsub("^:", "", missing_ci)
    missing_ci <- gsub(":$", "", missing_ci)
    missing_ci <- gsub(":{1,}", ":", missing_ci)

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

    # 4. Return all of the CIs.

    all_lwr <- append(lwr_cis, missing_lwr)
    all_upr <- append(upr_cis, missing_upr)

    list(
        lwr = unlist(all_lwr[names(model_coef)]),
        upr = unlist(all_upr[names(model_coef)])
    )
}
