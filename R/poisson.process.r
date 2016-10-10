#' Interarrival times and arrival times of a poisson process
#'
#' @param rate The rate/intensity of the arrivals in the poisson process
#' @param events The number of events in the process (or \code{NULL} if defining
#'   the cut off point by the time)
#' @param time.running The amount of time for the process to run (or \code{NULL}
#'   if defining the cut off by the number of events)
#' @return An numeric matrix with 2 columns. The first column contains the interarrival
#'   times and the second contains the arrival times.
#' @export
#'
poisson.process = function(rate = 1, events = NULL, time.running = NULL) {
  stopifnot(!all(c(is.null(events), is.null(time.running))))
  stopifnot(rate > 0)
  if(is.null(time.running)) {
    times = rexp(events, rate)
    results = mclapply(seq_len(events), FUN = function(i) c(times[i], sum(times[1:i])))
    return(do.call(rbind,results))
  } else {
    times = rexp(ceiling(time.running*rate), rate)
    results = mclapply(seq_len(length(times)), FUN = function(i) c(times[i], sum(times[1:i])))
    while(results[[length(results)]][2] < time.running) {
      times = rexp(max(c(10,ceiling(time.running*rate*.1))), rate)
      results_sub = mclapply(seq_len(length(times)), FUN = function(i) {
        c(times[i], results[[length(results)]][2]+sum(times[1:i]))
        })
      results = append(results, results_sub)
    }
    results = do.call(rbind,results)
    return(results[results[,2] < time.running,])
  }
}
