# lifetime risk of infection is 28% per year
# frequency of subtypes give in subtype_counts.csv
# from 1960 to 1996, calculate the probability that a 
# person born in that year had primary influenza infection with 
# H1N1, H2N2, or H3N2


# try loading packages
tryCatch({
  library(tidyverse)},
  error = function(err){
    message(paste(err, 'Attempting to install tidyverse'))
    install.packages('tidyverse')
  }
)
tryCatch({
  library(glue)},
  error = function(err){
    message(paste(err, 'Attempting to install glue'))
    install.packages('glue')
  }
)
tryCatch({
  library(ggplot2)},
  error = function(err){
    message(paste(err, 'Attempting to install ggplot2'))
    install.packages('ggplot2')
    library(glue)
  }
)


# try loading the data
# user may need to edit path
try(flu_df <- read_csv("BSD-QBio/tutorials/defensive_programming/data/subtype_counts.csv"))
# look at the first bit of data
head(flu_df)

# calculate total cases by year and sort by year
flu_df <- flu_df %>%
  mutate(total_cases = H1N1 + H2N2 + H3N2) %>%
  arrange(Year)
# compute proportions
flu_df <- flu_df %>%
  mutate(
    H1N1_prop = H1N1 / total_cases,
    H2N2_prop = H2N2 / total_cases,
    H3N2_prop = H3N2 / total_cases
  )
# protect against division by zero
if(any(flu_df$total_cases == 0)) {
  warning('At least one year has zero total cases. Check the input data carefully. Converting proportions for these years to 0.')
  flu_df <- flu_df %>%
    filter(total_cases == 0) %>%
    mutate(
      H1N1_prop = 0, 
      H2N2_prop = 0,
      H3N2_prop = 0
    )
}


# define a function to compute probabilities of when first infection occurs given birth year
# e.g., x years after birth, first infection occurs if 
# (1) infection did NOT occur in the first x-1 years and
# (2) infection did occur in the xth year
# Letting p_infection = 0.28 be the probability of infection, 
# (1) has probability (1-p_infection)^(x-1) and (2) has probability p_infection
# This is equivalent to a geometric distribution with p = p_infection

FirstInfectionTime <- function(p_infection, yob, maxyear){
  # we have to choose how to truncate the geometric distribution since it must end at maxyear
  # here we choose to condition on infection occurring and normalize the truncated geometric distribution
  
  # here, we can run a few checks on the inputs... 
  # first, p_infection > 0
  if (p_infection <= 0){stop(glue('Current value of p_infection is {p_infection}. p_infection must be > 0.'))}
  # then, yob and maxyear > 0
  if (yob <= 0 | maxyear <=0){stop(glue('yob = {yob} and maxyear = {maxyear}. Both years must be > 0.'))}
  # finally, maxyear >= yob
  if (maxyear < yob){stop(glue('yob = {yob} and maxyear = {maxyear}. maxyear >= yob.'))}
  
  # initialize a vector of years since birth, ranging from 0 to maxyear - yob
  years_since_birth <- 0:(maxyear - yob)
  # calculate the non-normalized infection probabilities using dgeom
  infection_probs <- dgeom(x=years_since_birth, prob=p_infection)
  # normalize (see comment above)
  infection_probs <- infection_probs / sum(infection_probs)
  return(infection_probs)
}

# then, probability of primary infection for each influenza strain is the 
# dot product of infection_probs and the proportions of that strain from yob to maxyear
# so, let's write a little function which computes that for all three strains
PrimaryInfectionProb <- function(p_infection, yob, maxyear, flu_data){
  # calculate the infection probabilities by year
  first_infection_probs <- FirstInfectionTime(p_infection, yob, maxyear)
  
  # define the strains we want
  strains <- c('H1N1', 'H2N2', 'H3N2')
  
  # then, compute the primary infection probabilities
  primary_H1N1 <- sum(first_infection_probs * flu_data$H1N1_prop[flu_data$Year >= yob]) 
  primary_H2N2 <- sum(first_infection_probs * flu_data$H2N2_prop[flu_data$Year >= yob]) 
  primary_H3N2 <- sum(first_infection_probs * flu_data$H3N2_prop[flu_data$Year >= yob])
  
  # collect results in a vector
  primary_infection_probs <- c(primary_H1N1, primary_H2N2, primary_H3N2)
  
  # check to make sure the primary infection probabilities sum to 1
  if(!all.equal(sum(primary_infection_probs), 1)){
    warning('Primary infection probabilities do not sum to one.')
  }
return(primary_infection_probs)
}

# now, define a function which loops over the relevant birth years building a df 
# with the birth year and primary infection probs
BuildPrimaryInfectionDf <- function(p_infection, first_year, last_year, flu_data){
  # get a sequence of years
  years <- first_year:last_year
  # make an empty df
  df <- data.frame()
  # loop through years
  for (yob in years){
    primary_probs <- PrimaryInfectionProb(p_infection,
                                          yob, 
                                          max(flu_data$Year), 
                                          flu_data)
    # build df with rbind
    df <- rbind(df, primary_probs)
  }
  # add birth year to data frame with add_column
  df <- add_column(df, 'Year' = years, .before=1)
  # reset column names
  colnames(df) <- c('Year', 'H1N1', 'H2N2', 'H3N2')
  return(df)
}
# build the primary infection data frame with the above function
primary_infection_df <- BuildPrimaryInfectionDf(p_infection = 0.28,
                                                first_year = 1960,
                                                last_year = 1996,
                                                flu_data = flu_df)


# save following suggested naming convention
write_csv(primary_infection_df, 'xenopus_laevis_primary_exposure_by_year.csv')

# a bit of processing with dplyr to get the df in the right format for plotting
plotting_data <- primary_infection_df %>%
  pivot_longer(
    cols = c('H1N1', 'H2N2', 'H3N2'),
    names_to = 'Strain',
    values_to = 'Probability'
  )

# plot results
stacked_bar_plot <- plotting_data %>%
  ggplot(aes(x=Year, y=Probability, fill=Strain)) +
  geom_col(position = 'fill') + 
  theme_bw() + 
  labs(
    title = 'Primary Infection Probability by Strain and Birth Year',
    x = 'Birth Year', 
    y = 'Probability of Primary Infection',
    fill = 'Strain'
  ) 

# save the plot following naming convention
ggsave('xenopus_laevis_primary_exposure_plt.pdf', stacked_bar_plot, height=5, width=6.5)
