# 
# Simulation
# 

# set the working directory and put 'files' folder into it along with this script and 'sim_plots' folder
setwd()

source('files/MacGregor_2.R', echo=F)

# simulate 10 attempts of 9-dot problem solution with lookahead = 4
sim1 <- sim_macgregor(dots_number = 9, attempts = 100, lookahead = 4, k = 1)

# how  many successful solutions?
sum(sim1$success)


# 12 dots, lookahead = 3
sim2 <- sim_macgregor(dots_number = 12, attempts = 10, lookahead = 3, k = 1)
sum(sim2$success)

