
#' Return an adjusted `summary()` table for an interaction model
#'
#' Adjusts the coefficients and CIs of any interaction terms in the model.
#'
#' @param modelobj (Object) The model to adjust.
#' @param data (Dataframe) The data used to fit the model. If `NULL` (default), tries to
#'     detect what was used in `modelobj`.
#' @param exponentiate (Logical) If `TRUE`, exponentiates the coefficient and confidence interval.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # cancer_modified is a built-in dataset provided with the `coefixr` package.
#' my_model <- lm(status ~ age + sex * ph.ecog + sex * wt.loss, data = cancer_modified)
#'
#' summary(my_model)
#' adjust_interaction_model(my_model, cancer_modified, exponentiate = TRUE)
#'
adjust_interaction_model <- function(modelobj, data = NULL, exponentiate = FALSE) {
    # 0. Set up the sources of information.

    model_smry <- NULL

    if (inherits(modelobj, "coxme")) {
        requireNamespace("coxme", quietly = TRUE)
    }

    model_smry <- summary(modelobj)$coefficients

    if (is.null(model_smry)) {
        stop("No appropriate `summary()` method for the `modelobj` could be found.\n",
             "Do you have the right modelling package installed and loaded?\n",
             "The class of your model is '", paste(class(modelobj), sep = " "), "'.")
    }

    model_coef <- adjust_interaction_coef(modelobj, data = data)
    model_ci   <- adjust_interaction_ci(modelobj, data = data)


    # 1. Get specific bits of info into dataframes for merging.
    final_covars <- data.frame(covar = names(model_coef))

    # HACK: Tries to get the P value column. It is named differently depending
    # on the model object, e.g. "p", "Pr(>|z|)", "P(>|z|)", "Pr(>F)", etc.
    final_p <- model_smry[, grepl("^(p)", dimnames(model_smry)[[2]], ignore.case = TRUE)]
    final_p <- named_to_df(final_p, "p.value")

    final_coef <- named_to_df(model_coef, "coef")

    final_ci_lwr <- named_to_df(model_ci$lwr, "ci.95lwr")
    final_ci_upr <- named_to_df(model_ci$upr, "ci.95upr")


    # 2. Merge into one dataframe.
    result <- final_covars
    result <- merge(result, final_p,      by = "covar", all = TRUE)
    result <- merge(result, final_ci_lwr, by = "covar", all = TRUE)
    result <- merge(result, final_coef,   by = "covar", all = TRUE)
    result <- merge(result, final_ci_upr, by = "covar", all = TRUE)


    # 3. Add exponentiated values if asked for.

    if (exponentiate == TRUE) {
        result$ci.95lwr <- exp(result$ci.95lwr)
        result$coef     <- exp(result$coef)
        result$ci.95upr <- exp(result$ci.95upr)

        colnames(result)[colnames(result) == "ci.95lwr"] <- "exp_ci.95lwr"
        colnames(result)[colnames(result) == "coef"]     <- "exp_coef"
        colnames(result)[colnames(result) == "ci.95upr"] <- "exp_ci.95upr"
    }

    result
}
