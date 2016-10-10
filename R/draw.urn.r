#' Results from drawing different color balls from an urn
#'
#' @param n.iter The number of iterations in the monte carlo simulation (integer > 0)
#' @param n.colors The number of different colored balls (integer > 0)
#' @param n.balls The count of each color balls (vector of integers with same length as \code{n.colors})
#' @param n.drawn The number of balls drawn from the urn in each iteration
#' @param replacement Are the balls placed back in the urn after they're drawn? (defaults to \code{FALSE})
#' @param collapse Should the return be the draws or the count of each color? (defaults to \code{FALSE})
#' @param parallel Should the simulation take advantage of multiple cores (defaults to \code{TRUE} -
#'   highly recommended if \code{n.iter > 1e5})
#'
#' @return If \code{collapse == TRUE} the return is a \code{n.iter x n.colors} integer matrix
#'   where each row is the count of the colors of balls drawn. Otherwise, the return is a
#'   \code{n.iter x n.drawn} integer matrix where each row is the order that colors were drawn in.
#' @export
#'
draw.urn = function(n.iter = 1e5, n.colors = 2, n.balls = c(10,10),
                    n.drawn = 5, replacement = FALSE,
                    collapse = FALSE, parallel = TRUE) {
  stopifnot(n.iter %% 1 == 0 & n.iter > 0)
  stopifnot(n.colors %% 1 == 0 & n.colors > 0)
  stopifnot(all(n.balls %% 1 == 0) & all(n.balls > 0))
  stopifnot(length(n.balls) == n.colors)
  stopifnot(n.drawn %% 1 == 0 & n.drawn > 0)
  urn = unlist(lapply(seq_len(n.colors), FUN = function(i) rep(i, n.balls[i])))
  total.balls = sum(n.balls)
  if(!parallel) {
    results = lapply(seq_len(n.iter), FUN = function(i) {
      indices = sample(seq_len(total.balls), size = n.drawn, replace = replacement)
      urn[indices]
    })
  } else {
    results = mclapply(seq_len(n.iter), FUN = function(i) {
      indices = sample(seq_len(total.balls), size = n.drawn, replace = replacement)
      urn[indices]
    })
  }
  if(collapse) {
    return(t(sapply(results, FUN = function(x) {
      vapply(seq_len(n.colors), function(i) sum(x == i),1)
    })))
  }
  return(do.call(rbind, results))
}
