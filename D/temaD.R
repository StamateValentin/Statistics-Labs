
zConfidenceInterval = function(n, media, dispersia, proc) {
  alfa = 1 - proc
  sigma = sqrt(dispersia)
  cz = qnorm(1 - alfa/2, 0, 1) # critical z  ; z*
  # capetele intervalelor
  a = media - cz * sigma / sqrt(n)
  b = media + cz * sigma / sqrt(n)
  interval = c(a, b)
  cat("Intervalul de incredere este: ", interval);
}

t_conf_interval=function(n,sample_mean,alfa,s){
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  interval = c(a, b)
  print("Interval de incredere")
  return(interval)
}

test_proportion= function(n, p0, succese, alfa, ipoteza){
  #as. la standa = 'left', 
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  p_prim = succese/n;
  z_score = (p_prim - p0)/(sqrt(p0*(1 - p0)/n));
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}

############### D1

zConfidenceInterval(20, 4.5, (0.5)^2, 0.99)

############### D2

t_conf_interval(255, 40, 0.99, sqrt(10))

############### D3

# p0 = 0.171
# p = 24/125 = 0.192
# p > po => right
test_proportion(125, 0.171, 24, 0.01, "right")
test_proportion(125, 0.171, 24, 0.05, "right")
# da, pot trage concluzia ca rata saraciei este mai mare decat cea la nivel national in ambele cazuri

############### D4

# p0 = 0.58
# p = 50 / 90 = 0.555
# p < po => left
test_proportion(90, 0.58, 50, 0.01, "left")
test_proportion(90, 0.58, 50, 0.05, "left")
# da, pot trage concluzia ca populatia in Detroit este mai mica decat la nivel national in ambele cazuri



