#' Results of dealing a hand of cards
#'
#' @param n.iter The number of iterations in the monte carlo simulation (integer > 0)
#' @param hand.size The number of cards in each hand (shuffling would be \code{hand.size = 52})
#' @param parallel Should the simulation take advantage of multiple cores (defaults to \code{TRUE} -
#'   highly recommended if \code{n.iter > 1e5})
#' @return A list with \code{n.iter} elements. Each element is a data frame of
#'   size \code{hand.size x 3}.
#' @export
#'
deal.cards = function(n.iter = 1e5, hand.size = 5, parallel = TRUE) {
  stopifnot(n.iter %% 1 == 0 & n.iter > 0)
  stopifnot(hand.size %% 1 == 0 & hand.size > 0)
  suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
  cards <- c("Ace", "Deuce", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
  values <- c(0, 2:9, rep(10, 4))
  totalNumOfDecks <- 1
  deck <- expand.grid(cards=cards, suits=suits)
  deck$value <- values
  if(!parallel) {
    results = lapply(seq_len(n.iter), FUN = function(i) {
      deck[sample(1:52, size = hand.size, replace = FALSE),]
    })
  } else {
    results = mclapply(seq_len(n.iter), FUN = function(i) {
      deck[sample(1:52, size = hand.size, replace = FALSE),]
    })
  }
  return(results)
}

