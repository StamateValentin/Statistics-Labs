b1 = function(n, r ,h) {
  real = pi * r^2 * h / 3
  count = 0
  for(i in 1:n) {
    x = runif(1, -r, r)
    y = runif(1, -r, r)
    z = runif(1, 0, h)
    
    if(x^2 + y^2 <= (r^2 * z^2)/(h^2)) {
      count = count+1;
    } 
  }
  
  val = ((4 * (r ^ 2)) * h *count) / n
  valRelativa = abs( (real - val) / val)
  cat("Aria Aproximata: ", val)
  cat("\nDiferenta : ", real - val)
  cat("\nEroarea Relativa: ", valRelativa)
}
r = 2
h = 3

x1 = b1(10000, r, h)
x2 = b1(20000, r, h)
x3 = b1(50000, r, h)

################

# calculand sistemul:
# 0 <= y <= x <= 3, deci
# 0 <= x si 0 <= y

b2 = function(n, a, b, c, d) {
  count = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    if(y >= 0 & 2 * y <= x & x + y <= 3) {
      count = count + 1
    }
  }
  return (  abs( (b - a) * (d - c) ) * count / n)
}

b2(15000, 0, 3, 0, 3)

################
# am pus aici functiile

fa = function(x) {
  return ( (2 * sqrt(x)) / (x + 1) )
}
fb = function(x) {
  return ( (1 + 2 * x) * exp(-x) )
}

b3a = function(n, a, b) {
    sum = 0
    for(i in 1:n) {
      x = runif(1, a, b)
      sum = sum + fa(x)
    }
    aria = ( (b - a) * sum / n )
    cat("Aria aproximata intre", a,"si",b, ":", aria, "\n")
    cat("Valoarea exacta intre 0 si 1:", 4 - pi)
}

b3b = function(n, lam) {
  sum = 0
  for(i in 1:n) {
    u = rexp(1, lam)
    sum = sum + fb(u) / (lam * exp(-lam * u))
  }
  aria = sum / n
  cat("Aria aproximata intre 0 si Inf:", aria, "\n")
  cat("Valoarea exacta intre 0 si Inf:", 3)
}

b3a(10000, 0, 1)
b3b(10000, 1)


################


b5 = function() {
  days = 1
  infectedPC = 4
  healtyPc = 21
  
  while(infectedPC > 0) {
    # try to repair
  
    infectedRepair = 3
    if(infectedPC < 3) {
      infectedRepair = infectedPC
    }
  
    for(i in 1:infectedRepair) {
      x = runif(1, 0, 1)
      if(x <= 0.65) {
        infectedPC = infectedPC - 1
        healtyPc = healtyPc + 1
      }
    }
    
    if(infectedPC == 0) {
      return (days)
    }
    
    #run infection
    for(i in 1:infectedPC) { # for every infected pc, another one is infected woth 0.65 pr

      if (healtyPc >= 1) {
        for (j in 1:healtyPc) { # pick an infected pc and attempt to infect all
          x = runif(1, 0, 1)
          if (x <= 0.01) { # i decrease the infecton probability
            infectedPC = infectedPC + 1
            healtyPc = healtyPc - 1
          }
        }
      }
    }
    
    days = days + 1
  }
  return (days)
}

runB5 = function(n) {
  sum = 0
  for(i in 1:n) {
    sum = sum + b5()
  }
  return (sum/n)
}

runB5(10000)

################

#       0.3     
#   0.4      0
# 0.5    1     0.3
#   0.7     0.4
#       0.5

tx = function(x) {
  return (x + 1)
}
ty = function(y) {
  return ( 99 - y )
}

printMatrix = function(a) {
  for(y in 1:99) {
    for(x in 1:197){
      cat(a[y,x])
    }
    cat("\n")
  }
}
# i don't need the function but i keep it as a reference
infect = function(a, x, y) {
  
  if (x - 2 >= y && a[ty(y),tx(x - 2)] != 1) { # West
    u = runif(1, 0, 1)
    if (u <= 0.5) {
      a[ty(y),tx(x - 2)] = 1
    }
  }
  
  if (x + 2 <= 196 - y && a[ty(y),tx(x + 2)] != 1) { # East
    u = runif(1, 0, 1)
    if (u <= 0.3) {
      a[ty(y),tx(x + 2)] = 1
    }
  }
  
  if (y + 2 <= 98 && a[ty(y + 2), tx(x)] != 1) { # North
    u = runif(1, 0, 1)
    if (u <= 0.3) {
      a[ty(y + 2),tx(x)] = 1
    }
  }
  
  if (y - 2 >= 0 && a[ty(y - 2), tx(x)] != 1) { # South
    u = runif(1, 0, 1)
    if (u <= 0.5) {
      a[ty(y - 2),tx(x)] = 1
    }
  }
  
  if ( x - 1 >= y + 1 && !(a[ty(y + 1), tx(x - 1)] == 2 || a[ty(y + 1), tx(x - 1)] == 1) ){ # North-West
    u = runif(1, 0, 1)
    if (u <= 0.4) {
      a[ty(y + 1),tx(x - 1)] = 1
    }
  }
  
  if (y - 1 >= 0 && a[ ty(y - 1), tx(x-1) ] != 1) { # South-West
    u = runif(1, 0, 1)
    if (u <= 0.7) {
      a[ty(y - 1),tx(x - 1)] = 1
    }
  }
  
  
  if (y - 1 >= 0 && a[ty(y - 1), tx(x + 1)] != 1) { # South-East
    u = runif(1, 0, 1)
    if (u <= 0.4) {
      a[ty(y - 1),tx(x + 1)] = 1
    }
  }
  
  # North-East neighbour can't burn
  
  return (a)
}

completePartial = function(a) {
  
  for(i in 0:98) {
    for(j in seq(0, 196, 2)) {
      x = j
      y = i
      if(a[ty(y), tx(x)] == 1) {
        a[ty(y), tx(x)] = 2 ## i do this to differentiate just'burned trees than the one that already burn
      }
    }
  }
  return (a)
}

b6a = function(ds) {
  hours = 1
  n = 98
  m = 196
  a = matrix(data = 0, nrow = n + 10, ncol = m + 10)
 
  x = 100
  y = 50
  a[ty(y),tx(x)] = 2
  
  nrTrees = (n + 1) * (n + 2) / 2 - 1 # the number of trees
  nrTreesInit = (n + 1) * (n + 2) / 2 - 1
  
  b = T
  
  while(nrTrees > 0) {
    
    a = completePartial(a) # here i change the mark of the trees from 1 to 2
    
    for(i in 0:98) {
      for(j in seq(0, 196, 2)) {
        x = j
        y = i
        
        #for b problems
        if (hours == ds) {
          if(nrTrees/nrTreesInit < 0.25) {
            b = F
          }
        }
        
        if (a[ty(y), tx(x)] == 2) {
        
          {
            if (x - 2 >= y && a[ty(y),tx(x - 2)] == 0) { # West
              u = runif(1, 0, 1)
              if (u <= 0.5) {
                a[ty(y),tx(x - 2)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            if (x + 2 <= 196 - y && a[ty(y),tx(x + 2)] == 0) { # East
              u = runif(1, 0, 1)
              if (u <= 0.3) {
                a[ty(y),tx(x + 2)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            if (y + 2 <= 98 && a[ty(y + 2), tx(x)] == 0) { # North
              u = runif(1, 0, 1)
              if (u <= 0.3) {
                a[ty(y + 2),tx(x)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            if (y - 2 >= 0 && a[ty(y - 2), tx(x)] == 0) { # South
              u = runif(1, 0, 1)
              if (u <= 0.5) {
                a[ty(y - 2),tx(x)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            if ( x - 1 >= y + 1 && a[ty(y + 1), tx(x - 1)] == 1) { # North-West
              u = runif(1, 0, 1)
              if (u <= 0.4) {
                a[ty(y + 1),tx(x - 1)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            if (y - 1 >= 0 && a[ ty(y - 1), tx(x-1) ] == 0) { # South-West
              u = runif(1, 0, 1)
              if (u <= 0.7) {
                a[ty(y - 1),tx(x - 1)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            
            if (y - 1 >= 0 && a[ty(y - 1), tx(x + 1)] == 0) { # South-East
              u = runif(1, 0, 1)
              if (u <= 0.4) {
                a[ty(y - 1),tx(x + 1)] = 1
                nrTrees = nrTrees - 1
              }
            }
            
            # North-East neighbour can't ignite directly
          }
          
        }
        
      }
    }
    hours = hours + 1
  }
  
  if(ds == 0)
    return (hours)
  
  return (b)
}

runb6a = function(n) {
  sum = 0
  for(i in 1:n) {
    sum = sum + b6a(0)
  }
  return (sum/n)
}

runb6a(10000) # eventually a smaller number because it will take some time

### b
 
# run the program n times then i calculate the probability fav/pos

run6b = function(n) {
  s = 0
  for(i in 1:n) {
    if(b6a(1) == 1) {
      s = s + 1
    }
  }
  return (s/n)
}

run6b(10)


