gm<-function(f,lvl,rep,int){
  
  #Funciones utlizadas
  #Funcion productoria
  prod<-function(a){
    b<-1
    for(i in 1:length(a)){
      b<-b*a[i]}
    return(b)
  }
  
  #Funcion Prouctoria parcial 
  pp<-function(a,c){
    b<-1
    for(i in 1:c){
      b<-b*a[i]}
    return(b)
  }
  
  #Funcion duplica filas
  df<-function(A,d){
    B<-vector("numeric",ncol(A))
    for(i in 1:nrow(A)){
      H<-matrix(rep(A[i,],d),nrow=d,byrow=TRUE)
      B<-rbind(B,H)
    }
    B<-B[-1,]
    return(B)
  }
  
  #Funcion duplica matrices
  dm<-function(A,n){
    C<-A
    while(nrow(A)<n){
      A<-rbind(A,C)}
    return(A)
  }
  
  #Funcion particion de un vector
  sf<-function(a,b){
    c<-1
    d<-0
    lista<-list(rep(0,length(b)))
    for(i in 1:length(b)){
      d<-d+b[i]
      lista[[i]]<-a[c:d]
      c<-d+1	
    }
    return(lista)
  }
  
  #Funcion de interacciones maximo 6 niveles
  pc<-function(A,a){
    f<-length(a)
    n<-nrow(A)
    val<-c(0)
    c<-1
    for(i in 1:n){
      lista<-sf(A[i,],a)
      ##
      if(f>=1){
        for(j in 1:length(lista[[1]])){
          val[c]<-lista[[1]][j]
          if(f>=2){
            for(k in 1:length(lista[[2]])){
              val[c]<-lista[[1]][j]*lista[[2]][k]
              if(f>=3){
                for(l in 1:length(lista[[3]])){
                  val[c]<-lista[[1]][j]*lista[[2]][k]*lista[[3]][l]
                  if(f>=4){
                    for(m in 1:length(lista[[4]])){
                      val[c]<-lista[[1]][j]*lista[[2]][k]*lista [[3]][l]*lista[[4]][m]
                      if(f>=5){
                        for(n in 1:length(lista[[5]])){
                          val[c]<-lista[[1]][j]*lista[[2]][k]*lista [[3]][l]*lista[[4]][m]*lista[[5]][n]
                          if(f>=6){
                            for(o in 1:length(lista[[6]])){
                              val[c]<-lista[[1]][j]*lista[[2]] [k]*lista[[3]][l]*lista[[4]][m]*lista[[5]][n]*lista[[6]][o]
                              if(f==6){c=c+1}
                            }
                          }
                          if(f==5){c=c+1}
                        }
                      }
                      if(f==4){c=c+1}
                    }
                  }
                  if(f==3){c=c+1}
                }
              }
              if(f==2){c=c+1}
            }
          }
          if(f==1){c=c+1}
        }
      }
    }
    M<-matrix(val,nrow=n,byrow=TRUE)
    return(M)
  }
  #
  
  if(missing(int)==TRUE){
    int=FALSE}
  if(missing(rep)==TRUE){
    rep=1}
  if(missing(lvl)==TRUE){
    lvl=rep(2,f)}
  
  n<-prod(lvl)
  M<-rep(1,n)
  
  for(i in 1:f){
    I<-diag(lvl[i])  
    s<-pp(c(1,lvl),i)
    A<-df(I,s)
    A<-dm(A,n)
    M<-cbind(M,A)	
  }
  if(f!=1&&int==TRUE){
    M<-cbind(M,pc(M[,-1],lvl))
    M<-df(M,rep)
  }
  else{
    M<-df(M,rep)
  }
  return(M)
}
