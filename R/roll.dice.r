#' Results of rolling dice
#'
#' @param n.iter The number of iterations in the monte carlo simulation (integer > 0)
#' @param n.dice The number of dice that should be rolled on each trial (integer > 0)
#' @param sides The number of sides on each die (integer > 0)
#' @param sum.dice Should the result be the sum of the dice on each iteration (defaults to \code{FALSE})
#' @param parallel Should the simulation take advantage of multiple cores (defaults to \code{TRUE} -
#'   highly recommended if \code{n.iter > 1e5})
#' @return If \code{sum.dice == TRUE} then the return is a vector of \code{n.iter} integer entries
#'   else the return is a \code{n.iter x n.dice} integer matrix
#' @export
#'
roll.dice = function(n.iter = 1e5, n.dice = 1, sides = 6,
                     sum.dice = FALSE, parallel = TRUE) {
  stopifnot(n.iter %% 1 == 0 & n.iter > 0)
  stopifnot(n.dice %% 1 == 0 & n.dice > 0)
  stopifnot(sides %% 1 == 0 & sides > 0)
  if(!parallel) {
    results = sapply(seq_len(n.iter), FUN = function(i) {sample(seq_len(sides), size = n.dice, rep = TRUE)})
  } else {
    results = do.call(cbind, mclapply(seq_len(n.iter), FUN = function(i) {sample(seq_len(sides), size = n.dice, rep = TRUE)}))
  }
  if(sum.dice & n.dice > 1) {return(colSums(results))}
  return(matrix(results, ncol = n.dice))
}
