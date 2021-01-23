library(dplyr)

#################################################################
# We want to check the hypothesis of a dice in 3
# to be a weighted at 6 by 1/2 probability
#################################################################

# Unknown dice trows
coin_a = c(6,1,3,6,4,5,6,1,2,6)
# Calculate the 6
coin_a_binomial = case_when(coin_a==6 ~ 1, TRUE ~ 0)

# Calculate the likelihood of having 6
p_binomial_weighted = dbinom(sum(coin_a_binomial), size=10, prob=1/6)
p_binomial_fair = dbinom(sum(coin_a_binomial), size=10, prob=1/2)

# Bayes factor based on the likelihood probabilities
bayes_factor = p_binomial_fair/p_binomial_weighted
print(bayes_factor)

# We have tree dices, which one is the weighted one?
# prior odds
p_weighted = 1/3
p_fair = 2/3

prior_odds = p_weighted/p_fair
print(prior_odds)

# posterior odds
posterior_odds = prior_odds * bayes_factor
print(posterior_odds)

# Now there are two weighted dices
p_weighted = 2/3
p_fair = 1/3

prior_odds = p_weighted/p_fair
print(prior_odds)

# posterior odds
posterior_odds = prior_odds * bayes_factor
print(posterior_odds)

#################################################################
# We can see here that we can get a better evidence 
# by generating more rolls
#################################################################

# Fair dice
coin_fair = sample(1:6, 100, replace = TRUE)
coin_fair_binomial = case_when(coin_fair==6 ~ 1, TRUE ~ 0)

# Weighted dice
coin_weighted_binomial = rbinom(100,1,1/2)

# Calculate the likelihood of having 6
p_binomial_weighted = dbinom(sum(coin_weighted_binomial), size=100, prob=1/6)
p_binomial_fair = dbinom(sum(coin_fair_binomial), size=100, prob=1/2)

# Bayes factor based on the likelihood probabilities
bayes_factor = p_binomial_fair/p_binomial_weighted
print(bayes_factor)

# prior odds
p_weighted = 1/3
p_fair = 2/3

prior_odds = p_weighted/p_fair
print(prior_odds)

# posterior odds
posterior_odds = prior_odds * bayes_factor
print(posterior_odds)
