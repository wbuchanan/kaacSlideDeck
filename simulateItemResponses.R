# Move to the directory where the slide deck is stored
setwd("~/Desktop/ConferenceStuff/Presentations/kaacSlideDeck/")

# Set random number seed
set.seed(8675309)

# Load library for pipe operators
library(magrittr)

# Define vector of alpha parameters
alphas <- runif(20, -2.5, 4)

# Vector of difficulties for 20 items drawn randomly 
betas <- runif(20, -0.5, 4.5) 

# Vector of pseudoguessing parameters
cparams <- runif(20, 0, 0.2)

# Vector of upper asymptote parameters
dparams <- runif(20, 0.875, 1.0)

# Vector of thetas for a few hundred test takers
thetas <- rnorm(200, 0, 1) %>% as.list()

# Generate an answer key
keyedResponses <- letters[rbinom(20, 5, .5)]

# Defines the response set for each of the items
responseSet <- c("a", "b", "c", "d", "e")


itemData <- psych::sim.irt(nvar = 20, n = 200, low = -4, high = 2, a = alphas,
						   d = betas, c = cparams, z = dparams, mu = -0.75,
						   sd = 1.75, mod = "logistic")

responses <- itemData[[1]] %>% dplyr::as_data_frame()

# Creates a list of item names and recodes the item responses with the selected 
# responses
itemNames <- plyr::llply(as.list(c(1:20)), .fun = function(x) {
	newItemName <- paste0("item", x)
	itemName <- paste0("V", x)
	for (i in c(1:200)) {
		responses[i, itemName] <<- ifelse(responses[i, itemName] == 1, keyedResponses[x], 
							responseSet[!(responseSet %in% keyedResponses[1])][runif(1, 1, 4)])
	}
	return(newItemName)
})

# Generate ID variable for the data
responses$id <- c(1:200)

# Creates a female indicator
responses$sex <- ifelse(rbinom(200, 1, 0.51) == 1, "Female", "Male")

# fixes the variable name for the ID variable
names(responses) <- c(unlist(itemNames), "id", "sex")

# Write the item responses to a csv file
write.csv(responses, file = "itemResponses.csv", row.names = FALSE)

keyedResponses
