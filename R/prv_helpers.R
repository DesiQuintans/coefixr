
#' Convert a named vector into a two-column data frame
#'
#' @param vec   (Vector) A named vector.
#' @param value (Character) The name of the column that will hold the values of `vec`.
#' @param name  (Character) The name of the column that will hold the names of `vec`.
#'
#' @return A data frame.
#' @md
#' @keywords internal
#'
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
