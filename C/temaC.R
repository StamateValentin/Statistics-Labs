c1a = function(x, a) {
  found = FALSE
  while (!found) {
    i = sample(1:n, 1)
    if (x[i] == a) {
      return (i)
    }
  }
}

###

c1aModif = function(x, a) {
  nr = 1
  while (TRUE) {
    i = sample(1:n, 1)
    if (x[i] == a) {
      return (nr)
    }
    nr = nr + 1
  }
}

c1b = function(n, x, k, a) {
  sum = 0
  for(i in 1:n) {
    sum = sum + c1aModif(x, a)
  }
  medie = sum / n
  cat("Difference between real case and worst case", n / k - medie, "\n")
  cat("Mean:", medie)
}

x = 1:200
k = 10
a = 400 # an element is not found in x
rp = sample(1:200, 10) # vector of random positions
for (i in 1:k) {
  x[rp] = a
}

n = 100
c1b(n, x, k, a)

###############

c2 = function(x) {
  n = length(x)
  m = ceiling(n^(3 / 4))
  r = vector(length = m)
  ind = sample(1:n, m, replace = T) # random positions
  
  for (i in 1:m) {
    r[i] = x[ind[i]]
  }
  
  r = sort(r)
  
  d = r[ floor((n^(3 / 4)) / 2 - sqrt(n)) %% (m + 1)] # %% for negative numbers
  u = r[ (floor((n^(3 / 4)) / 2 + sqrt(n)) + 1) %% (m + 1)]
  
  c = vector()
  cLen = 0
  
  ld = 0
  lu = 0
  
  for(i in 1:n) {
    if( x[i] <= u && x[i] >= d ) {
      cLen = cLen + 1
      c[cLen] = x[i]
    }
    if (x[i] < d) {
      ld = ld + 1
    }
    
    if ( x[i] > u ) {
      lu = lu + 1
    }
  }
  
  if (ld > n / 2 || lu > n / 2 || cLen > 4 * ceiling(n ^ (3 / 4)) ) {
    print("No median found")
  } else {
    c = sort(c)
    print( c[floor(n / 2) - ld + 1 %% (cLen + 1)])
  }
  
}

x = 1:100
c2(x)
median(c(1, 2, 3, 4, 5, 100))

###############

c3a = function(f, g, h) {
  n = length(f)
  p = sample(1:(3*n), 1)
  
  s1 = 0
  for(i in 1:n) {
    s1 = s1 + f[i] * p^(i - 1)
  }
  
  s2 = 0
  for(i in 1:n) {
    s2 = s2 + g[i] * p^(i - 1)
  }
  
  s3 = 0
  for(i in 1:length(h)) {
    s3 = s3 + h[i] * p^(i - 1)
  }
  
  if(s1 * s2 == s3) {
    return (TRUE)
  }
  return (FALSE)
}

c3b = function() {
  pGresi = 2 / 3
  e = 0.0001
  
  p = 1
  n = 1
  while (TRUE) {
    p = p * pGresi
    if (p < e) {
      return (n);
    }
    n = n + 1
  }
  
}

c3b()
#
###############

c4 = function(n) {
  aux = n - 1
  k = 0
  while( (aux %% 2) == 0) {
    aux = aux / 2
    k = k + 1
  }
  m = aux
  
  a = sample(2:(n - 2), 1)
  b = (a^m) %% m
  
  for(i in 1:k) {
    if(b != 1 && b != (n - 1) ) {
      return (FALSE)
    }
    if (b == (n - 1)) {
      return (TRUE)
    }
    b = (b^2) %% n
  }
  
  if(b != 1) {
    return (FALSE)
  }
  return (TRUE)
}

n = 13
c4(n)









