
#' Calculate adjusted coefficients and CIs for models with interactions
#'
#' Adjusts the coefficients and CIs of any interaction terms in the model.
#'
#' @param modelobj (Object) A model object.
#' @param data (Dataframe) The data used to fit the model.
#' @param exponentiate (Logical) If `TRUE`, exponentiates the coefficient and confidence interval.
#' @param add.global.p (Logical) If `TRUE`, adds a global p-value for each covariate.
#' @param digits.n (Logical) Number of digits to round coefficients and confidence intervals to.
#' @param digits.p (Logical) Number of digits to round p-values to. Also handles very small ("<0.001") and large (">0.999") p-values.
#'
#' @return A data frame with these columns:
#'     \describe{
#'       \item{**covar**}{The covariate.}
#'       \item{**ref**}{`TRUE` marks the reference levels of covariates.}
#'       \item{**ref.intx**}{`TRUE` marks interactions that involve the reference level of a covariate.}
#'       \item{**global.p**}{The global p-value of the covariate. Column is omitted if `add.global.p = FALSE`.}
#'       \item{**p.value**}{The p-value of the covariate.}
#'       \item{**log_ci.95lwr** or **exp_ci.95lwr**}{Lower 95% confidence interval of the coefficient. Exponentiated if `exponentiate = TRUE`.}
#'       \item{**log_coef** or **exp_coef**}{The coefficient. Exponentiated if `exponentiate = TRUE`.}
#'       \item{**log_ci.95upr** or **exp_ci.95upr**}{Upper 95% confidence interval of the coefficient. Exponentiated if `exponentiate = TRUE`.}
#'     }
#'
#' @export
#'
#' @examples
#' # cancer_modified is a dataset provided with the `coefixr` package.
#' my_model <- lm(status ~ inst + age + sex * ph.ecog + sex * wt.loss, data = cancer_modified)
#'
#' # Unexponentiated coefficients.
#' adjust_interaction_model(my_model, cancer_modified)
#'
#' # To get unrounded numbers, set digits.n and digits.p to Inf.
#' adjust_interaction_model(my_model, cancer_modified, digits.n = Inf, digits.p = Inf)
#'
#' # Note that column names change when `exponentiate = TRUE`.
#' adjust_interaction_model(my_model, cancer_modified, exponentiate = TRUE)
#'
#' # Global p-values are provided by `car::Anova()`, just like `gtsummary::add_global_p()`.
#' adjust_interaction_model(my_model, cancer_modified, add.global.p = TRUE)
#'
adjust_interaction_model <- function(modelobj, data,
                                     exponentiate = FALSE,
                                     add.global.p = FALSE,
                                     digits.n     = 2,
                                     digits.p     = 3) {
    # 1. Get the names of terms, which will become the rows of the output table.
    built_terms <- build_missing_terms(modelobj = modelobj, data = data)

    final_covars <-
        data.frame(
            covar    = built_terms$complete_terms,
            ref      = built_terms$complete_terms %in% built_terms$reference_levels,
            ref.intx = built_terms$complete_terms %in% built_terms$missing_terms
        )


    # 2. Get the global p-value, if it was asked for.
    if (add.global.p == TRUE) {
        anova_res <- suppressWarnings(car::Anova(modelobj))

        final_global_p <- data.frame(
            covar    = rownames(anova_res),
            global.p = round_p(anova_res[, 4], digits = digits.p)
        )
    } else {
        final_global_p <- data.frame(covar = character(0))
    }


    # 3. Get the p-values for each covariate. This assumes that an appropriate
    # summary() method is available for the model type.
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

    # HACK: Using grepl() to try to get the P value column. It is named
    # differently depending on the model object, e.g. "p", "Pr(>|z|)",
    # "P(>|z|)", "Pr(>F)", etc.
    model_p <- model_smry[, grepl("^(p)", dimnames(model_smry)[[2]], ignore.case = TRUE)]

    final_p <-
        named_to_df(
            round_p(model_p, digits = digits.p),
            "p.value"
        )


    # 4. Get confidence intervals.
    model_ci    <- adjust_interaction_ci(modelobj, data = data)

    if (exponentiate == TRUE) {
        final_ci_lwr <- named_to_df(round_n(exp(model_ci$lwr), digits = digits.n), "exp_ci.95lwr")
        final_ci_upr <- named_to_df(round_n(exp(model_ci$upr), digits = digits.n), "exp_ci.95upr")
    } else {
        final_ci_lwr <- named_to_df(round_n(model_ci$lwr, digits = digits.n), "log_ci.95lwr")
        final_ci_upr <- named_to_df(round_n(model_ci$upr, digits = digits.n), "log_ci.95upr")
    }


    # 5. Get coefficients.
    model_coef  <- adjust_interaction_coef(modelobj, data = data)

    if (exponentiate == TRUE) {
        final_coef <- named_to_df(round_n(exp(model_coef), digits = digits.n), "exp_coef")
    } else {
        final_coef <- named_to_df(round_n(model_coef, digits = digits.n), "log_coef")
    }


    # 6. Assemble the final dataframe.
    result <- final_covars
    result <- merge(result, final_global_p, by = "covar", all.x = TRUE)
    result <- merge(result, final_p,        by = "covar", all.x = TRUE)
    result <- merge(result, final_ci_lwr,   by = "covar", all.x = TRUE)
    result <- merge(result, final_coef,     by = "covar", all.x = TRUE)
    result <- merge(result, final_ci_upr,   by = "covar", all.x = TRUE)

    # Sort the rows of the result by ordering the rownames of the result
    # in the same order as the built covariates list.
    rownames(result) <- result$covar

    if (add.global.p == FALSE) {
        # Don't include the top-level variable names if global p-values
        # are not requested. Those are only included because the global p-value
        # is displayed alongside them.
        result <- result[built_terms$all_ref_levels, ]
    } else {
        result <- result[final_covars$covar, ]
    }

    rownames(result) <- NULL


    # 7. Done!
    result
}
