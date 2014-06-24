all.costs <- function(meanexp, sdexp, 
                      exp, opt, overcost, undercost, numm) {
  
  probs <- c()
  allcosts<- matrix(nrow=2*opt,ncol=2*opt)
  oppcosts<- matrix(nrow=2*opt,ncol=2*opt)
  expcosts<- matrix(nrow=2*opt,ncol=2*opt)
  
  for (i in 0:(2*opt)){
    probs[i] <- pnorm(i+.5, meanexp, sdexp)-pnorm(i-.5, meanexp, sdexp)
  }
  
  for (i in 0:(2*opt)){
    for (j in 0:(i)) {
      allcosts[i,j] <- overcost*(i-j)
    }
    for (j in (i):(2*opt)) {
      allcosts[i,j] <- undercost*(j-i)
    }
  }
  
  for (i in 0:(2*opt)){
    for (j in 0:(i)) {
      expcosts[i,j] <- overcost*(i-j)
      oppcosts[i,j] <- 0.0
    }
    for (j in (i):(2*opt)) {
      oppcosts[i,j] <- undercost*(j-i)
      expcosts[i,j] <- 0.0
    }
  }
  
  costs <- c()
  oppcosts.sum <- c()
  expcosts.sum <- c()
  
  for (i in 0:(2*opt)){
    costs[i] = sum(allcosts[i,]*probs)
  }
  
  for (i in 0:(2*opt)){
    oppcosts.sum[i] = sum(oppcosts[i,]*probs)
  }
  
  for (i in 0:(2*opt)){
    expcosts.sum[i] = sum(expcosts[i,]*probs)
  }
  
  yearlycosts <- costs*365/exp
  yearlyoppcosts <- oppcosts.sum*365/exp
  yearlyexpcosts <- expcosts.sum*365/exp
  costsfromopt <- yearlycosts-yearlycosts[round(opt)]
  
  ycosts <- sprintf("$ %6.2f", yearlycosts)
  yocosts <- sprintf("$ %6.2f", yearlyoppcosts)
  yecosts <- sprintf("$ %6.2f", yearlyexpcosts)
  cfocosts <- sprintf("$ %6.2f", costsfromopt)
  
  costtable <- data.frame(Gallons.ordered=c(1:(2*opt)),
                          Difference.from.optimal=cfocosts,
                          Total.costs = ycosts,
                          Expired.milk.costs = yecosts,
                          Opportunity.costs = yocosts)
  
  costtable2 <- costtable[(1:(opt/(numm/2)))*numm,]
  
  return(costtable2)
  
}