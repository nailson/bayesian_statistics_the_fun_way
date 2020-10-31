#install.packages("ggplot2")
#install.packages("reshape")
library(reshape)
library(ggplot2)
library(dplyr)

# PART I - Chapter 5 
# Beta Distribution

# Fair coin A, not so fair coin B and C 
coin_tosses = list(ifelse(runif(100) > 0.5, 1, 0),
                   ifelse(runif(100) > 0.3, 1, 0),
                   ifelse(runif(100) > 0.7, 1, 0)
                   )

# Create a Dataframe with all coin tosses
df = as.data.frame(coin_tosses, col.names =  c("coin_A","coin_B","coin_C"))
df["coin_toss"] = c(1:nrow(df))

# melt the tosses into a column
df_tosses = melt(df, id=c("coin_toss"))

# Now let's calculate the tosses by coin
grouped_tosses = df_tosses %>% group_by(variable, value) %>% count()
grouped_tosses

pbeta(1/2, 50, 50)

# First, what's the probability that the Coin is Fair with 5% of tolerance
for (coin in unique(grouped_tosses$variable)){
   n_heads = grouped_tosses[grouped_tosses$variable==coin & grouped_tosses$value==1, "n"][[1]]
   n_tails = grouped_tosses[grouped_tosses$variable==coin & grouped_tosses$value==0, "n"][[1]]
   print(integrate(function(x) dbeta(x, n_heads, n_tails),0.45, 0.55))
}
  

