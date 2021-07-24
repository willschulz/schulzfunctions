#' View Corner of a Matrix
#'
#' This function does for a matrix what head() does for a data frame.
#' @param x Matrix to view
#' @param min Minimum row and column indices to view.  Defaults to c(0,0)
#' @param max Maximum row and column indices to view.  Defaults to c(10,10)
#' @keywords classification
#' @export
#' @examples
#' plotTermCoefs()

corner <- function(x,
                   min=c(0,0),
                   max=c(10,10)
                   ) {
  return(x[c(min[1]:max[1]),c(min[2]:max[2])])
}
