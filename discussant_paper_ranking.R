discussant_sim <- function(n,c,r) { 
  # Here, n is the number of students, c is the number of choices, and r is the rank
        # i.e. setting r=1 will give the number of students who DO NOT get their 1st choice
        # r=5 will give the number of students who DO NOT get their 5th choice or better
  X <- t(replicate(n,sample(1:c, c)))
  X <- rbind(rep(0,c), X)
  taken <- rep(0, c)
  for (col in 1:r) {
    taken <- unique(c(taken,X[,col]))
    X <- X[duplicated(X[,col]),]
    if (col != r) {
      for (choice in taken) {
        X <- rbind(rep(choice, c), X)
      }
    }
  }
  
  return(length(X)/c)
}

discussant_sim(8,9,3)

results <- replicate(10^5, discussant_sim(8,9,3))

mean(results >= 1) # % of the time at least one person doesn't
                   # get their rth choice

n_range <- 2:20
results2 <- integer(length(n_range))
for (n in n_range) {
  results2[n-1] <- mean(replicate(10^4, discussant_sim(n,n+1,3)) >= 1)
}

plot(n_range, results2)

r_range <- 2:9
resultsr <- list()
for (r in r_range) {
  res <- replicate(10^4, discussant_sim(8,9,r))
  resultsr[[r-1]] <- res 
}

hist(resultsr[[3-1]])
plot(r_range, resultsr)

library('ggplot2')
x <- resultsr[[3-1]]
ggplot(data.frame(x),aes(seq_along(x),x))+geom_bar(stat="identity")
dat <- as.data.frame(table(x))
barplot(dat$Freq / length(x), names.arg = c('0','1','2','3'), 
        main='Distribution of the number of students who don\'t get top 3 choice',
        xlab='Number of Non-winners (i.e. losers)')
mtext('Assuming uniformly distributed preferences')
