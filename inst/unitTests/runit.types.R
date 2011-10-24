
test.window.at.portfolio <- function()
{
  rets <- data.frame(matrix(rnorm(80), nrow=4))
  colnames(rets) <- c('A','B','C','D')
  p <- create(TawnyPortfolio, rets, 75)

  p1 <- window.at(p,1)
  checkTrue(nrow(p1$returns) == 75)
  checkTrue(p1$returns[1,] == p$returns[1,])

  p2 <- window.at(p,2)
  checkTrue(nrow(p2$returns) == 75)
  checkTrue(p2$returns[1,] == p$returns[2,])

  p3 <- window.at(p,3)
  checkTrue(nrow(p3$returns) == 75)
  checkTrue(p3$returns[1,] == p$returns[3,])

  p4 <- window.at(p,4)
  checkTrue(nrow(p4$returns) == 75)
  checkTrue(p4$returns[1,] == p$returns[4,])

  p5 <- window.at(p,5)
  checkTrue(nrow(p5$returns) == 75)
  checkTrue(p5$returns[1,] == p$returns[5,])
}

