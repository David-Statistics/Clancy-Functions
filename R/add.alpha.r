#' Add transparency to colors
#'
#' @param col Base color(s)
#' @param alpha Transparency of the colors (between 0 and 1 - 1 is opaque)
#' @export
#'
add.alpha <- function(col = "black", alpha=1){
  if(missing(col)) {
    stop("Please provide a vector of colours.")
  }
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
