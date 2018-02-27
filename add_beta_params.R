# Clear the console, and the environment.  Take this out when development is
# complete.
cat("\014")
rm(list=ls())

estBetaParams <- function(mu, var) {
  
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# Read the files.
vote_totals <- read.csv('vote_totals.csv')
aggregation <- read.csv('aggregation.csv')

# Create the variance column.
vote_totals$variance <- 1.
abbreviations <- unique(vote_totals$abbreviation)
for (abbreviation in abbreviations) {
  
  vote_totals[abbreviation == vote_totals$abbreviation,]$variance <-
    rep(aggregation[abbreviation == aggregation$abbreviation,]$variance, 5)
}

# Create the beta parameter estimates.
estimates <- apply(vote_totals, MARGIN = 1, FUN = function(x) {
  
  estBetaParams(as.numeric(x['fraction_democratic']), as.numeric(x['variance']))
})

# Add the columns to the vote totals.
vote_totals$alpha <- sapply(estimates, FUN = function(x) { x$alpha })
vote_totals$beta <- sapply(estimates, FUN = function(x) { x$beta })

# Rearrange the rows.
my_order <- c('year', 'abbreviation', 'state', 'democratic', 'republican', 'total', 'fraction_democratic',
              'fraction_republican', 'variance', 'alpha', 'beta', 'electoral_votes')
vote_totals <- vote_totals[,my_order]

# Write out a new CSV file.
write.csv(vote_totals, 'new.csv', row.names = F)
