ph<-function(y,X,lambda,A,D){
  library(MASS)
  xtx<-t(X)%*%X
  xty<-t(X)%*%y
  yty<-t(y)%*%y
  
  newxtx<-function(xtx,A){
    h<-nrow(A)
    zeros<-matrix(rep(0,h*h),nrow=h)
    M<-cbind(rbind(xtx,A),rbind(t(A),zeros))
    return(M)
  }
  newlambda<-function(lambda,A){
    h<-nrow(A)
    zeros<-matrix(rep(0,h*h),nrow=h)
    M<-cbind(lambda,zeros)
    return(M)
  }
  newx<-function(X,D){
    M<-X%*%ginv(D)
    return(M)
  }
  ##################### Restriccion #######################
  if(missing(A)==FALSE){
    lambda<-newlambda(lambda,A)
    xtx<-newxtx(xtx,A)
    xty<-c(xty,rep(0,nrow(A)))
    c<-rep(0,nrow(A))
  }
  ################### Reparametrizacion ###################
  if(missing(D)==FALSE){
    X<-newx(X,D)
    xtx<-t(X)%*%X
    xty<-t(X)%*%y
  }
  beta<-ginv(xtx)%*%xty
  scres<-yty-t(beta)%*%xty
  
  r<-qr(X)$rank
  n<-length(y)
  m<-qr(lambda)$rank
  d<-rep(0,m)
  
  sch0<-t(lambda%*%beta-d)%*%ginv(lambda%*%ginv(xtx)%*%t(lambda))%*%(lambda%*%beta-d)
  Fc<-(sch0/m)/(scres/(n-r))
  pvalue<-1-pf(Fc,m,n-r)
  
  result<-matrix(c(round(scres,3),round(Fc,3),round(pvalue,5)),nrow=1)
  colnames(result)<-c("SS","Fc","P-value")
  return(result)
}
