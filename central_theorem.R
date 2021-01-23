

# plot hist of geometric n random values vs the average of n geometric random 
for(n in seq(20, 200, by=20)){
  array_a = c()
  lambda = 1/50
  par(mfrow=c(1,2))
  
  hist(rexp(n, lambda))
  
  for(i in 1:n){
    a = rexp(n, lambda)
    array_a[i] = sum(a)/n
  }
  hist(array_a)
}

# plot hist of uniform n random values vs the average of n geometric random 
for(n in seq(5, 200, by=5)){
  array_a = c()
  par(mfrow=c(1,2))
  n_classes = 10
  hist(sample(1:10, n, replace = TRUE))
  
  for(i in 1:n){
    a = sample(1:10, n, replace = TRUE)
    array_a[i] = sum(a)/n
  }
  hist(array_a)
}