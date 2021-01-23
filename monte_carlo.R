
n_trials = 10000
prior_alpha = 3
prior_beta = 7

a_sample = rbeta(n_trials, 36 + prior_alpha, 114 + prior_beta)
b_sample = rbeta(n_trials, 50 + prior_alpha, 100 + prior_beta)

a_hist = hist(a_sample)
b_hist = hist(b_sample)

dev.off()
plot( a_hist, col=rgb(0,0,1,1/4))  # first histogram
plot( b_hist, col=rgb(1,0,0,1/4), add=T)  # second


# how many times the sample b is higher than sample a?
sum(b_sample>a_sample)/n_trials

# by how much?
hist(b_sample/a_sample)

plot(ecdf(b_sample/a_sample))


# Questions
# Suppose a director of marketing with many years of experience tells you he believes 
# very strongly that the variant without images B won't perform any differently than the original variant. 
# How could you account for this in our model?
# Implement this change and see how your final conclusion change as well.

n_trials = 10000
b_new_prior_alpha = 360
b_new_prior_beta = 1140

a_sample = rbeta(n_trials, 36 + prior_alpha, 114 + prior_beta)
b_sample = rbeta(n_trials, 50 + b_new_prior_alpha, 100 + b_new_prior_beta)

a_hist = hist(a_sample)
b_hist = hist(b_sample)

dev.off()
plot( a_hist, col=rgb(0,0,1,1/4))  # first histogram
plot( b_hist, col=rgb(1,0,0,1/4), add=T)  # second

# how many times the sample b is higher than sample a?
sum(b_sample>a_sample)/n_trials

# by how much?
hist(b_sample/a_sample)

plot(ecdf(b_sample/a_sample))


# Question: The lead designer sees your results and insists that there's no way that variant B should perform better wth no images
# She feels that you should assume the conversion rate for varriant B is closer to 20 percent than 30 percent.
# Implement a solution for this and again review the results of your analysis.

n_trials = 10000
a_new_prior_alpha = 30
b_new_prior_beta = 70

b_new_prior_alpha = 20
b_new_prior_beta = 80

a_sample = rbeta(n_trials, 36 + a_new_prior_alpha, 114 + b_new_prior_beta)
b_sample = rbeta(n_trials, 50 + b_new_prior_alpha, 100 + b_new_prior_beta)

a_hist = hist(a_sample)
b_hist = hist(b_sample)

dev.off()
plot( b_hist, col=rgb(1,0,0,1/4))  # second
plot( a_hist, col=rgb(0,0,1,1/4), add=T)  # first histogram

# how many times the sample b is higher than sample a?
sum(b_sample>a_sample)/n_trials

# by how much?
hist(b_sample/a_sample)

plot(ecdf(b_sample/a_sample))

