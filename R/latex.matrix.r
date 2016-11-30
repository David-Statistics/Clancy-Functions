#' Used to quickly output LaTeX code for a matrix in R.
#'
#' @param mat $A$, a matrix in R
#' @param in_mm Boolean for whether or not include an entering and exiting of
#'    math mode
#'
#' @return Latex code for the matrix $A$
#'
#' @export

latex.matrix = function(mat, in_mm = TRUE) {
  n = nrow(mat)
  m = ncol(mat)
  body = lapply(1:n, FUN = function(i) {
    if(i != n) {
      paste(paste(mat[i,], collapse = " & "), "\\\\\n")
    } else {
      paste(mat[i,], collapse = " & ")
    }
  })
  body = do.call(paste, body)
  if (in_mm) {
    cat("\\left[\\begin{array}{",rep("c", m),"}\n",
        body,
        "\n\\end{array}\\right]", sep = "")
  } else {
    cat("\\[\\left[\\begin{array}{",rep("c", m),"}\n",
        body,
        "\n\\end{array}\\right]\\]", sep = "")
  }
}
