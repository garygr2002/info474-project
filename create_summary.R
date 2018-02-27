# Clear the console, and the environment.  Take this out when development is
# complete.
cat("\014")
rm(list=ls())

estBetaParams <- function(mu, var) {
  
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# Build up the aggregation dataset.
vote_totals <- read.csv('vote_totals.csv')
vote_totals <- vote_totals[vote_totals$abbreviation != 'US',]
aggregation <- aggregate(fraction_democratic ~ state, data = vote_totals, mean)
temp <- aggregate(fraction_republican ~ state, data = vote_totals, mean)
aggregation$fraction_republican <- temp$fraction_republican
aggregation$partisanship <- sqrt(abs(aggregation$fraction_democratic - 0.5))
temp <- aggregate(fraction_democratic ~ state, data = vote_totals, var)
aggregation$variance <- temp$fraction_democratic
temp <- aggregate(total ~ state, data = vote_totals, mean)
aggregation$population <- round(temp$total, 0)

# Do some plots.
plot(variance ~ population, data = aggregation)
population_model <- lm(variance ~ population, data = aggregation)
summary(population_model)
abline(population_model, col = 'dark green')
plot(variance ~ partisanship, data = aggregation)
partisanship_model <- lm(variance ~ partisanship, data = aggregation)
summary(partisanship_model)
abline(population_model, col = 'dark green')

# Create the beta parameter estimates.
estimates <- apply(aggregation, MARGIN = 1, FUN = function(x) {
  
  estBetaParams(as.numeric(x['fraction_democratic']), as.numeric(x['variance']))
  })

# Add the columns to the aggregation.
aggregation$alpha <- sapply(estimates, FUN = function(x) { x$alpha })
aggregation$beta <- sapply(estimates, FUN = function(x) { x$beta })

# Rank rows by variance.  Write out the CSV.
aggregation <- aggregation[order(aggregation$variance),]
write.csv(aggregation, file = 'aggregation.csv', row.names = F)
t.test(aggregation$variance)
