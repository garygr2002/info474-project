# Clear the console, and the environment.  Take this out when development is
# complete.
cat("\014")
rm(list=ls())

library(LearnBayes)
library(MASS)

# See here:
# https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance

do_it <- function(data_frame) {
  
  states <- unique(data_frame$state)
  Illinois <- which('Illinois' == states)
  Massachusetts <- which('Massachusetts' == states)
  Michigan <- which('Michigan' == states)
  NewJersey <- which('New Jersey' == states)
  NewYork <- which('New York' == states)
  Pennsylvania <- which('Pennsylvania' == states)
  Washington <- which('Washington' == states)
  Arizona <- which('Arizona' == states)
  Georgia <- which('Georgia' == states)
  Louisiana <- which('Louisiana' == states)
  Missouri <- which('Missouri' == states)
  SouthCarolina <- which('South Carolina' == states)
  Texas <- which('Texas' == states)
  Utah <- which('Utah' == states)
  Florida <- which('Florida' == states)
  Iowa <- which('Iowa' == states)
  Nevada <- which('Nevada' == states)
  Ohio <- which('Ohio' == states)
  
  electoral_votes <- data_frame[data_frame$year == 2016,]$electoral_votes
  electoral_votes_2000 <- electoral_votes
  electoral_votes_2000[Illinois] <- electoral_votes[Illinois] + 1
  electoral_votes_2000[Massachusetts] <- electoral_votes[Massachusetts] + 1
  electoral_votes_2000[Michigan] <- electoral_votes[Michigan] + 1
  electoral_votes_2000[NewJersey] <- electoral_votes[NewJersey] + 1
  electoral_votes_2000[NewYork] <- electoral_votes[NewYork] + 2
  electoral_votes_2000[Pennsylvania] <- electoral_votes[Pennsylvania] + 1
  electoral_votes_2000[Washington] <- electoral_votes[Washington] - 1
  electoral_votes_2000[Arizona] <- electoral_votes[Arizona] - 1
  electoral_votes_2000[Georgia] <- electoral_votes[Georgia] - 1
  electoral_votes_2000[Louisiana] <- electoral_votes[Louisiana] + 1
  electoral_votes_2000[Missouri] <- electoral_votes[Missouri] + 1
  electoral_votes_2000[SouthCarolina] <- electoral_votes[SouthCarolina] - 1
  electoral_votes_2000[Texas] <- electoral_votes[Texas] - 4
  electoral_votes_2000[Utah] <- electoral_votes[Utah] - 1
  electoral_votes_2000[Florida] <- electoral_votes[Florida] - 2
  electoral_votes_2000[Iowa] <- electoral_votes[Iowa] + 1
  electoral_votes_2000[Nevada] <- electoral_votes[Nevada] - 1
  electoral_votes_2000[Ohio] <- electoral_votes[Ohio] + 2
  
  big_evs <- c(rep(electoral_votes_2000, 3), rep(electoral_votes, 2))
  data_frame$electoral_votes <- big_evs
}

do_it2 <- function(data_frame) {
  
  data_frame$total <- data_frame$democratic + data_frame$republican
  data_frame$fraction_democratic <- data_frame$democratic / data_frame$total
  data_frame$fraction_republican <- data_frame$republican / data_frame$total
  
  my_order <- c('year', 'abbreviation', 'state', 'democratic', 'republican', 'total', 'fraction_democratic',
                'fraction_republican', 'electoral_votes')
  data_frame <- data_frame[,my_order]
}

build_us_row <- function(df, year) {
  
  new_row <- data.frame(year, 'US', 'United States', 0, 0, 0, 0., 0., 0)
  colnames(new_row) <- colnames(df)
  new_row$democratic <- sum(data_frame[year == df$year,]$democratic)
  new_row$republican <- sum(data_frame[year == df$year,]$republican)
  new_row$total <- new_row$democratic + new_row$republican
  new_row$fraction_democratic <- new_row$democratic / new_row$total
  new_row$fraction_republican <- new_row$republican / new_row$total
  new_row$electoral_votes <- 538
  rbind(df, new_row)
}

# Read the data frame.
data_frame <- read.csv('vote_totals.csv')
data_frame <- data_frame[data_frame$abbreviation != 'US',]

# Verify vote totals.
year <- 2000
50999897 == sum(data_frame[data_frame$year == year,]$democratic)
50456002 == sum(data_frame[data_frame$year == year,]$republican)
year <- 2004
59028444 == sum(data_frame[data_frame$year == year,]$democratic)
62040610 == sum(data_frame[data_frame$year == year,]$republican)
year <- 2008
69498516 == sum(data_frame[data_frame$year == year,]$democratic)
59948323 == sum(data_frame[data_frame$year == year,]$republican)
year <- 2012
65915795 == sum(data_frame[data_frame$year == year,]$democratic)
60933504 == sum(data_frame[data_frame$year == year,]$republican)
year <- 2016
65853516 == sum(data_frame[data_frame$year == year,]$democratic)
62984825 == sum(data_frame[data_frame$year == year,]$republican)

# Verify E.V. totals.
electoral_votes <- 538
electoral_votes == sum(data_frame[data_frame$year == 2000,]$electoral_votes)
electoral_votes == sum(data_frame[data_frame$year == 2004,]$electoral_votes)
electoral_votes == sum(data_frame[data_frame$year == 2008,]$electoral_votes)
electoral_votes == sum(data_frame[data_frame$year == 2012,]$electoral_votes)
electoral_votes == sum(data_frame[data_frame$year == 2016,]$electoral_votes)

# data_frame$percent <- data_frame$democratic /
#  (data_frame$democratic + data_frame$republican)

# data_frame$x <- data_frame$percent - mean(data_frame$percent)
# t.test(data_frame$x)
# fitdistr(data_frame$x, "normal")

# data_frame$electoral_votes <- 0
# write.csv(data_frame, "new.csv", row.names = F)
