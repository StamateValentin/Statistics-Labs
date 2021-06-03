test_Z= function(n, population_mean, sample_mean,dispersion, alfa, ipoteza){
  #as. la standa = 'left', 
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  sigma=sqrt(dispersion)
  z_score = (sample_mean - population_mean)/(sigma/sqrt(n));
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}

test_Z_Mod= function(n, z_score, alfa, ipoteza){
  
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}

test_z_diferenta_medii= function(n1,n2,m0,sample_mean1,sample_mean2,sigma1,sigma2, alfa, ipoteza){
  #as. la standa = 'left', 
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  combined_sigma=sqrt(sigma1^2/n1+sigma2^2/n2)
  z_score = ((sample_mean1 - sample_mean2)-m0)/sqrt((sigma1^2)/n1+(sigma2^2)/n2);
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
  }
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");
}

test_F= function(n1,n2,s1,s2, alfa, ipoteza){
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  F_score = s1^2/s2^2
  if(ipoteza=='right'){
    critical_F = qf(1 - alfa,n1-1,n2-1);
    if(F_score > critical_F){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", F_score," Valoari critica: ", critical_F, "\n");
  }
  if(ipoteza=='sim'){
    critical_Fs = qf(alfa/2,n1-1,n2-1);
    critical_Fd = qf(1-alfa/2,n1-1,n2-1);
    if((F_score<critical_Fs )||(F_score>critical_Fd)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", F_score," Valori critice: ", critical_Fs, critical_Fd,  "\n");
  }
}

#Test F cu citire din fisier cu doua coloane
test_F_fisier= function(filename, alfa, ipoteza){
  #as. la dreapta = 'right' 
  #simetrica = 'sim'
  x1=read.table(filename, header = T)[['A']]
  x2=read.table(filename, header = T)[['B']]
  n1=length(x1)
  n2=length(x2)
  s1=sd(x1)
  s2=sd(x2)
  F_score = s1^2/s2^2
  if(ipoteza=='right'){
    critical_F = qf(1 - alfa,n1-1,n2-1);
    if(F_score > critical_F){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", F_score," Valoari critica: ", critical_F, "\n");
  }
  if(ipoteza=='sim'){
    critical_Fs = qf(alfa/2,n1-1,n2-1);
    critical_Fd = qf(1-alfa/2,n1-1,n2-1);
    if((F_score<critical_Fs )||(F_score>critical_Fd)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", F_score," Valori critice: ", critical_Fs, critical_Fd,  "\n");
  }
}

###############

#### E1
x = c(10.51, 10.22, 9.87, 9.93, 10.34, 10.48, 10.17)
n = length(x)
disp = var(x)
mean(x) # = 10.21714
# 10.3 > 10.21 -> dreapta
test_Z(disp, n, mean(x), 10.3, 0.01, 'right')
# nu pot zice ca nivelul este mai mic decat maximul posibil


#### E2

# in ipoteza -> right
test_Z_Mod(65, 1.85, 0.01, 'right')
# nu pot trage concluzia ca aceste scor este semnificativ

test_Z_Mod(65, 1.85, 0.05, 'right')
# pot trage concluzia ca aceste scor este semnificativ

#### E3
# sim, pentru ca se cere daca media difera
test_z_diferenta_medii(21, 22, 0, 76.56, 72.23, 2.95, 3.12, 0.01, 'sim')
# da, pot trage concluzia referitor la media cantitatii de cafea

test_z_diferenta_medii(21, 22, 0, 76.56, 72.23, 2.95, 3.12, 0.05, 'sim')
# da, pot trage concluzia referitor la media cantitatii de cafea

#### E4

# 20.5 - 21.6 < 0 -> 'left'
test_z_diferenta_medii(87, 91, 0, 20.5, 21.6, 1.15, 0.92, 0.01, 'left')
# pot trage concluzia ca prima medie este mai mica decat cea de-a doua cu un nivel de semnificatie de 1%

test_z_diferenta_medii(87, 91, 0, 20.5, 21.6, 1.15, 0.92, 0.05, 'left')
# pot trage concluzia ca prima medie este mai mica decat cea de-a doua cu un nivel de semnificatie de 5%

#### E5
# left dar inversez parametri ca sa devina right
test_F(197, 235, 2.11, 1.83, 0.01, 'right')
# nu am suficiente dovezi

