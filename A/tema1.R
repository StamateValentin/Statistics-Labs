a1 = function(lambda, p, k, l) {
  po = dpois(0:l, lambda)
  geo = dgeom(0:l, p)
  
  par(mfrow = c(3, 1) )
  # plot(k:l, po[k:l], type = "l", main = "Poisson")
  # plot(k:l, geo[k:l], type = "l", main = "Geometric")
  
  hist(po[k:l])
  hist(geo[k:l])
  
}

a1(5, 0.25, 10, 100)

##################

a2a = function(x) {
  y = vector()
  
  y[1] = median(x)
  y[2] = mean(x)
  y[3] = sd(x)
  y[4] = as.vector(quantile(x))[1 + 1] # Q1
  y[5] = as.vector(quantile(x))[1 + 2] # Q2
  y[6] = as.vector(quantile(x))[1 + 3] # Q3
  
  return (y)
}


#################

a2b = function(x) {
  n = length(x)
  medie = sum(x) / n
  s = sd(x) # devSt
  
  left = medie - s * 2
  right = medie + s * 2
  
  y = vector()
  l = 0
  for(i in 1:n) {
    if(x[i] - left > 0 || x[i] - right > 0) {
      l = l + 1
      y[l] = x[i]
    }
  }
  
  return (y)
}

###################

a2c = function(y) {
  x = a2b(y) # vectorul curatat
  
  interval = seq(40, 100, 5)
  interval[1] = 40 + 0.00000000000001

  hist(x, breaks = interval,freq = T ,right = F)
  
}


tema1 = function() {
  a1(5, 0.25, 10, 100)
  
  
  x = scan("input.txt")
 
  a2a(x)
  a2b(x)
  a2c(x)

}

tema1()
