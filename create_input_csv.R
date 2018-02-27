# Clear the console, and the environment.  Take this out when development is
# complete.
cat("\014")
rm(list=ls())

# Augment abbreviations and names with the District of Columbia.
my_abbs <- c(state.abb, 'DC')
my_names <- c(state.name, 'D.C.')
state_count <- length(my_names)

# Get the ordering of augmented abbeviations, and order both the
# augmented abbreviations and augmented names thusly.
my_order <- order(my_abbs)
my_abbs <- my_abbs[my_order]
my_names <- my_names[my_order]

# Create the years.
years <- as.factor(seq(2000, 2016, 4))
year_count <- length(years)

# Replace years for the number of states.
years_rep <- rep(years, state_count)
years_rep <- years_rep[order(years_rep)]

# Create a data frame
my_frame <- data.frame(year = years_rep,
                       abbreviation = rep(my_abbs, year_count),
                       state = rep(my_names, year_count))

# Add vote total fields.
my_frame$democratic <- 0
my_frame$republican <- 0

# Write the CSV file.
# write.csv(my_frame, file = 'vote_totals.csv', row.names = F)
