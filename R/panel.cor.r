#' Used to replace duplicate scatterplots in a matrix
#'
#' @param x variable for x-axis
#' @param y variable for y-axis
#' @param digits rounding cut off
#' @param cex.cor type of point
#' @param include.p Include p-value for correlation value?
#' @param ... extra plotting parameters
#' @export
#' @examples data("iris")
#'   pairs(iris, upper.panel = panel.cor)
#'
panel.cor <- function(x, y, digits = 2, cex.cor, include.p = FALSE, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)

  if(include.p){
    # p-value calculation
    p <- cor.test(x, y)$p.value
    txt2 <- format(c(p, 0.123456789), digits = digits)[1]
    txt2 <- paste("p= ", txt2, sep = "")
    if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
    text(0.5, 0.4, txt2)
  }
}
