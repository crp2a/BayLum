##___ EXP+LIN growth  
##________________________________________________________________

AgeMultiBF_EXPLIN=list(
  cauchy="model {
    D~dnorm(A*ddot,omega)
    sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
    pD<-pow(sD,-2)
    
    # Likelihood:
    for(bf in 1:BinPerSample){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
        xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        De[(index[bf]+j),1]~dt(D,pD,1)
        #
        xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
        N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
        xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]+xd[(index[bf]+j)]
        
        for(k in 2:K[bf]){
          xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
          N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
          xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]+xd[(index[bf]+j)]
          De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
        }
        
      }
    } 
    omega<-1/(A^2*Sigma)
    u~dunif(0,1)
    A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }",
  
  gaussian="model {
    D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
  xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dnorm(D,pD)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]+xd[(index[bf]+j)]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]+xd[(index[bf]+j)]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }", 
  
  lognormal="model {
  # prior on De and sDe
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  tau2<-log(1+(sD^2/D^2))
  mu_D<-log(D)-0.5*tau2
  ptau<-1/tau2
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
  xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  log_De[(index[bf]+j),1]~dnorm(mu_D,ptau)
  De[(index[bf]+j),1]<-exp(log_De[(index[bf]+j),1])
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]+xd[(index[bf]+j)]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]+xd[(index[bf]+j)]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }"
)

save(AgeMultiBF_EXPLIN,file=c("BUGFiles/AgeMultiBF_EXPLIN"))

##-----------------------------------------------------------------------------------------------------------------------------------


##___ EXP+Zero Origin growth  
##________________________________________________________________

AgeMultiBF_EXPZO=list(
  cauchy="model {
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dt(D,pD,1)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }",
  
  gaussian="model {
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dnorm(D,pD)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))

  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }", 
  
  lognormal="model {
  # prior on De and sDe
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  tau2<-log(1+(sD^2/D^2))
  mu_D<-log(D)-0.5*tau2
  ptau<-1/tau2
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  log_De[(index[bf]+j),1]~dnorm(mu_D,ptau)
  De[(index[bf]+j),1]<-exp(log_De[(index[bf]+j),1])
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }"
)

save(AgeMultiBF_EXPZO,file=c("BUGFiles/AgeMultiBF_EXPZO"))
##-----------------------------------------------------------------------------------------------------------------------------------


##___ EXP growth  
##________________________________________________________________

AgeMultiBF_EXP=list(
  cauchy="model {
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dt(D,pD,1)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }",
  
  gaussian="model {
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dnorm(D,pD)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }", 
  
  lognormal="model {
  # prior on De and sDe
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  tau2<-log(1+(sD^2/D^2))
  mu_D<-log(D)-0.5*tau2
  ptau<-1/tau2
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  log_De[(index[bf]+j),1]~dnorm(mu_D,ptau)
  De[(index[bf]+j),1]<-exp(log_De[(index[bf]+j),1])
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }"
)

save(AgeMultiBF_EXP,file=c("BUGFiles/AgeMultiBF_EXP"))

##-----------------------------------------------------------------------------------------------------------------------------------

##___ EXP+LIN+ Zero Origin growth  
##________________________________________________________________

AgeMultiBF_EXPLINZO=list(
  cauchy="model {
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dt(D,pD,1)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }",
  
  gaussian="model {
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  De[(index[bf]+j),1]~dnorm(D,pD)
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]
  
  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }", 
  
  lognormal="model {
  # prior on De and sDe
  D~dnorm(A*ddot,omega)
  sD~dt(0,pow(0.16*D,-2),1)T(0,)#dgamma(0.01,0.01)
  pD<-pow(sD,-2)
  
  tau2<-log(1+(sD^2/D^2))
  mu_D<-log(D)-0.5*tau2
  ptau<-1/tau2
  
  # Likelihood:
  for(bf in 1:BinPerSample){ 
  for(j in 1:J[bf]){
  # prior on growth function
  xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
  xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
  xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
  sigmaf[(index[bf]+j)]~dexp(20)
  
  log_De[(index[bf]+j),1]~dnorm(mu_D,ptau)
  De[(index[bf]+j),1]<-exp(log_De[(index[bf]+j),1])
  #
  xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
  N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
  xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]

  for(k in 2:K[bf]){
  xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
  N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
  xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]
  De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
  }
  
  }
  } 
  omega<-1/(A^2*Sigma)
  u~dunif(0,1)
  A<-exp(u*log(xbound[2]/xbound[1])+log(xbound[1])) 
  }"
)

save(AgeMultiBF_EXPLINZO,file=c("BUGFiles/AgeMultiBF_EXPLINZO"))

