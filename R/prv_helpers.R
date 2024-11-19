
# Convert a named vector into a two-column data frame
#
# @param vec   (Vector) A named vector.
# @param value (Character) The name of the column that will hold the values of `vec`.
# @param name  (Character) The name of the column that will hold the names of `vec`.
#
# @return A data frame.
# @md
# @keywords internal
#
named_to_df <- function(vec, value = "value", name = "covar") {
    if(is.null(names(vec))) {
        stop("The vector does not have any names.")
    }

    result <-
        data.frame(
            .namecol = names(vec),
            .valcol  = unname(vec)
        )

    names(result) <- c(name, value)

    return(result)
}



# Round a number to a specific number of digits
#
# @param num (Numeric) A vector of numbers.
# @param digits (Numeric) Number of digits.
#
# @return A Character vector.
# @md
# @keywords internal
round_n <- function(num, digits = Inf) {
    if (is.infinite(digits)) {
        return(num)
    }

    fmt <- paste0("%.", digits, "f")

    result <-
        suppressWarnings(
            sprintf(fmt, num)
        )

    names(result) <- names(num)

    result
}



# Round a p-value to a specific number of digits
#
# Very small or very large p-values are also handled. The output of this
# function is the same as `gtsummary::style_pvalue()`.
#
# @param pval (Numeric) A vector of p-values.
# @param digits (Numeric) Number of digits.
#
# @return A Character vector.
# @md
# @keywords internal
round_p <- function(pval, digits = Inf) {
    if (is.infinite(digits)) {
        return(pval)
    }

    small_p <-
        as.numeric(
            paste0("0.", paste(rep("0", digits - 1), collapse = ""), "1")
        )

    big_p <-
        as.numeric(
            paste0("0.", paste(rep("9", digits), collapse = ""))
        )

    # Borrowed the logic of `gtsummary::style_pvalue`, but in base R.
    result <- character(length(pval))

    for (i in seq_along(pval)) {
        if (is.na(pval[i])) {
            result[i] <- NA_character_
        } else if (pval[i] > 1 + 1e-15) {
            result[i] <- NA_character_
        } else if (pval[i] < 0 - 1e-15) {
            result[i] <- NA_character_
        } else if (pval[i] > big_p) {
            result[i] <- paste0(">", big_p)
        } else if (pval[i] >= small_p) {
            result[i] <- round_n(pval[i], digits = digits)
        } else if (pval[i] < small_p) {
            result[i] <- paste0("<", small_p)
        }
    }

    names(result) <- names(pval)

    result
}
