# Clear the console, and the environment.  Take this out when development is
# complete.
cat("\014")
rm(list=ls())

state_ids <- read.csv("state_ids.csv", colClasses = c('character', 'character'))
vote_totals <- read.csv('vote_totals_old.csv')
vote_totals <- merge(vote_totals, state_ids, by = 'state')

vote_totals <- vote_totals[c('year',
                             'id',
                             'abbreviation',
                             'state',
                             'democratic',
                             'republican',
                             'total',
                             'fraction_democratic',
                             'fraction_republican',
                             'variance',
                             'alpha',
                             'beta',
                             'electoral_votes')]

vote_totals <- vote_totals[order(vote_totals$year, vote_totals$id, vote_totals$abbreviation,
                                 vote_totals$state),]
write.csv(vote_totals, file = 'vote_totals.csv', row.names = F)
