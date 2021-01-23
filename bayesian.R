
##############################################
#
#       Beta Distribution in R
#
##############################################

# Create a sequence of x values (uniform distribution)
x1 = seq(0, 1, by=0.01)
hist(x1)

# Evaluate the beta density (not cumulative distribution)
# dbeta(xvalues, alpha, beta)
y1 = dbeta(x1, 1, 1)
plot(x1, y1, type="l", col="blue")

y1 = dbeta(x1, 2, 1)
plot(x1, y1, type="l", col="blue")

# quadratic function
y1 = dbeta(x1, 2, 2)
plot(x1, y1, type="l", col="blue")

# Mean = alpha / (alpha + beta)
# Maximum = (alpha - 1) / (alpha + beta - 2)
# Variance = alpha*beta/((alpha + beta)ˆ2* (alpha + beta + 1))

# Cumulative Distribution
y2 = pbeta(x1, 10, 20)

pbeta(0.2, 2, 2)
plot(x1, y2, type="l")

# Inverse Cumulative Distribution (y values that have )
x2 = seq(0.005, 0.99, by=0.01)
y3 = qbeta(x2, 300, 39700)
plot(x2, y3, type="l")

# Question: A child is going door to door selling candy bars, So far she has visited 30 houses
# and sold 10 bars. She will visit 40 more houses today. What is the 95% confidence interval 
# for how much candy bars she will sell the rest of the day.
qbeta(c(0.025, 0.975), 10, 20)

quartiles_prob = qbeta(c(0.025, 0.975), 10, 20)
quartiles_prob * 40

# Plot Bayesian PDF function


plot_posterior <- function(prior_a, prior_b, successes, failures){
  # x axis 0 to 1
  theta = seq(0.005, 0.995, length = 500)
  
  # likelihood and posterior parameters
  likelihood_a = successes
  likelihood_b = failures
  posterior_a = prior_a + successes
  posterior_b = prior_b + failures
  
  # calculate the distribution densities for each value 0 to 1
  prior = dbeta(theta, prior_a, prior_b)
  likelihood = dbeta(theta, likelihood_a, likelihood_b)
  posterior = dbeta(theta, posterior_a, posterior_b)
  
  # confidence interval 95% of confidence
  confidence_interval_upper = qbeta( 0.975, posterior_a, posterior_b)
  confidence_interval_lower = qbeta(0.025, posterior_a, posterior_b)
  
  m = max(c(prior, likelihood, posterior))
  
  plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
       main = paste("beta(", prior_a, ",", prior_b, ") prior, B(", likelihood_a, ",", likelihood_b, ") data,",
                    "beta(", posterior_a, ",", posterior_b, ") posterior",
                    "\n95% of confidence between ",format(confidence_interval_lower, 2, 2), "and",
                    format(confidence_interval_upper, 2, 2)), ylim = c(0, m), col = "red")
  lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
  lines(theta, prior, lty = 3, lwd = 3, col = "green")
  legend(x=0.8,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
         lwd = c(3, 3, 3), col = c("green", "blue", "red"))
  
}

# Suppose you're playing air hockey with some friends and flip a coin to see who starts with the puck.
# after 12 times, you realize that the friend who brings the coin almost always seems to go first: 
# 9 out of 12 times. Some of your other friends start to get suspicious. Define prior probability distributions for the following
# beliefs:

# * One person who weakly believes that the friend is cheating and the true rate of coming up heads is closer to 70 percent
# prior Beta(7,3)

plot_posterior(7, 3, 9, 3)

# * One person who very strongly trusts that the coin is fair and provided a 50 percent chance of coming up heads.
# Beta(10,10)

plot_posterior(100, 100, 9, 3)

# * One person who strongly believes that the coin is biased and to come up heads 70 percent of the time.
# Beta(21,9)

plot_posterior(21, 9, 9, 3)

# To test the coin, you flip it 20 more times and get 9 heads and 11 tails
# Using the priors you calculated in the previous question, what are the updated 
# posterior beliefs in the true rate of flipping a heads in terms of the 95 percent confidence interval.

# New Likelihood 
# Beta(18,14)

# * One person who weakly believes that the friend is cheating and the true rate of coming up heads is closer to 70 percent
# prior Beta(7,3)

plot_posterior(7, 3, 18,14)
qbeta(c(0.025, 0.975), 25, 17)

# * One person who very strongly trusts that the coin is fair and provided a 50 percent chance of coming up heads.
# Beta(10,10)

plot_posterior(100, 100, 18,14)
qbeta(c(0.025, 0.975), 118, 114)

# * One person who strongly believes that the coin is biased and to come up heads 70 percent of the time.
# Beta(21,9)

plot_posterior(21, 9, 18,14)
qbeta(c(0.025, 0.975), 39, 23)

