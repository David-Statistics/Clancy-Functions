#' Interarrival times and arrival times of a poisson process
#'
#' @param probability The probability of success in the poisson process
#' @param events The number of events in the process (or \code{NULL} if defining
#'   the cut off point by the time)
#' @param time.running The amount of time for the process to run (or \code{NULL}
#'   if defining the cut off by the number of events)
#' @return An numeric matrix with 2 columns. The first column contains the interarrival
#'   times and the second contains the arrival times.
#' @export
#'
bernoulli.process = function(probability = .5, events = NULL, time.running = NULL) {
  stopifnot(!all(c(is.null(events), is.null(time.running))))
  stopifnot(probability >= 0 & probability <= 1)
  if(is.null(time.running)) {
    times = rgeom(events, probability) + 1
    results = mclapply(seq_len(events), FUN = function(i) c(times[i], sum(times[1:i])))
    return(do.call(rbind,results))
  } else {
    times = rgeom(ceiling(time.running*probability), probability)+1
    results = mclapply(seq_len(length(times)), FUN = function(i) c(times[i], sum(times[1:i])))
    while(results[[length(results)]][2] < time.running) {
      times = rgeom(max(c(10,ceiling(time.running*probability*.25))), probability) + 1
      results_sub = mclapply(seq_len(length(times)), FUN = function(i) {
        c(times[i], results[[length(results)]][2]+sum(times[1:i]))
      })
      results = append(results, results_sub)
    }
    results = do.call(rbind,results)
    return(results[results[,2] < time.running,])
  }
}
