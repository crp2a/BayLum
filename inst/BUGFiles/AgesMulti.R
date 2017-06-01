##___ EXP+LIN growth  
##________________________________________________________________

AgesMultiBF_EXPLIN=list(
  cauchy="model {

  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
      # prior on growth function
      xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
      xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
      xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
      xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
      sigmaf[(index[bf]+j)]~dexp(20)
      
      De[(index[bf]+j),1]~dt(D[i],pD[i],1)
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
  }

  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }

  }",
  gaussian="model {

  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
      # prior on growth function
      xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
      xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
      xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
      xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
      sigmaf[(index[bf]+j)]~dexp(20)
      
      De[(index[bf]+j),1]~dnorm(D[i],pD[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  lognormal="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
      tau2[i1]<-log(1+(sD[i1]^2/D[i1]^2))
    pD[i1]<-pow(sD[i1],-2)
      ptau[i1]<-1/tau2[i1]
    mu[i1]<-A[i1]*ddot[i1]
      mu_D[i1]<-log(D[i1])-0.5*tau2[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)

  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
      # prior on growth function
      xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
      xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
      xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
      xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
      sigmaf[(index[bf]+j)]~dexp(20)
      
      log_De[(index[bf]+j),1]~dnorm(mu_D[i],ptau[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }"
)

save(AgesMultiBF_EXPLIN,file=c("BUGFiles/AgesMultiBF_EXPLIN"))

##________________________________________________________________
##___ EXP+Zero Origin growth   
##________________________________________________________________

AgesMultiBF_EXPZO=list(
  cauchy="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
      # prior on growth function
      xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
      xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
      sigmaf[(index[bf]+j)]~dexp(20)
      
      De[(index[bf]+j),1]~dt(D[i],pD[i],1)
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  gaussian="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        De[(index[bf]+j),1]~dnorm(D[i],pD[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  lognormal="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    tau2[i1]<-log(1+(sD[i1]^2/D[i1]^2))
    pD[i1]<-pow(sD[i1],-2)
    ptau[i1]<-1/tau2[i1]
    mu[i1]<-A[i1]*ddot[i1]
    mu_D[i1]<-log(D[i1])-0.5*tau2[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
      # prior on growth function
      xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
      xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
      sigmaf[(index[bf]+j)]~dexp(20)
      
      log_De[(index[bf]+j),1]~dnorm(mu_D[i],ptau[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }"
)

save(AgesMultiBF_EXPZO,file=c("BUGFiles/AgesMultiBF_EXPZO"))

##________________________________________________________________
##___ EXP growth 
##________________________________________________________________

AgesMultiBF_EXP=list(
  cauchy="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        De[(index[bf]+j),1]~dt(D[i],pD[i],1)
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  gaussian="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        De[(index[bf]+j),1]~dnorm(D[i],pD[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  lognormal="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    tau2[i1]<-log(1+(sD[i1]^2/D[i1]^2))
    pD[i1]<-pow(sD[i1],-2)
    ptau[i1]<-1/tau2[i1]
    mu[i1]<-A[i1]*ddot[i1]
    mu_D[i1]<-log(D[i1])-0.5*tau2[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        log_De[(index[bf]+j),1]~dnorm(mu_D[i],ptau[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }"
)

save(AgesMultiBF_EXP,file=c("BUGFiles/AgesMultiBF_EXP"))

##________________________________________________________________
##___ XP+LIN+ Zero Origin growth  
##________________________________________________________________

AgesMultiBF_EXPLINZO=list(
  cauchy="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        De[(index[bf]+j),1]~dt(D[i],pD[i],1)
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  gaussian="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    pD[i1]<-pow(sD[i1],-2)
    mu[i1]<-A[i1]*ddot[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        De[(index[bf]+j),1]~dnorm(D[i],pD[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }",
  lognormal="model {
  
  D~dmnorm(mu,omega)
  for(i1 in 1:I){
    sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
    tau2[i1]<-log(1+(sD[i1]^2/D[i1]^2))
    pD[i1]<-pow(sD[i1],-2)
    ptau[i1]<-1/tau2[i1]
    mu[i1]<-A[i1]*ddot[i1]
    mu_D[i1]<-log(D[i1])-0.5*tau2[i1]
    for(i2 in 1:I){
      Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
    }
  }
  omega<-inverse(Sigma)
  
  # Likelihood:
  for(i in 1:I){
    for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){ 
      for(j in 1:J[bf]){
        # prior on growth function
        xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
        xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
        xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
        sigmaf[(index[bf]+j)]~dexp(20)
        
        log_De[(index[bf]+j),1]~dnorm(mu_D[i],ptau[i])
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
  }
  
  for(i0 in 1:I){
    u[i0]~dunif(0,1)
    A[i0]<-exp(u[i0]*log(xbound[2*i0]/xbound[2*i0-1])+log(xbound[2*i0-1])) 
  }
  
  }"
)

save(AgesMultiBF_EXPLINZO,file=c("BUGFiles/AgesMultiBF_EXPLINZO"))

