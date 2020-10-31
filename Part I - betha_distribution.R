#install.packages("ggplot2")
#install.packages("reshape")
library(reshape)
library(ggplot2)
library(dplyr)

# PART I - Chapter 5 
# Betha Distribution

# Fair coin A 
data_coinA = ifelse(runif(1000) > 0.5, 1, 0)

# Not so fair coin B
data_coinB = ifelse(runif(1000) > 0.3, 1, 0)

# Not so fair coin C
data_coinC = ifelse(runif(1000) > 0.7, 1, 0)

df = as.data.frame(list(data_coinA, data_coinB, data_coinC), col.names =  c("coin_A","coin_B","coin_C"))
df["coin_toss"] = c(1:nrow(df))

df_tosses = melt(df, id=c("coin_toss"))

df_tosses %>% group_by(variable, value) %>% count()


dbeta(1/2, 454, 546)

integrate(function(x) dbeta(x, 454, 546),0.45, 0.55)
