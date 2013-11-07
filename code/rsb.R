"rsb" <-function(x,y,iter=50, penalty=c("direct", "hlasso", "pelasso"),
                 test.response=NULL,test.x,lambda=1,k=1,boost=0,
                 coef=TRUE,bag=TRUE,bag.frac=.5,FUN=mean,...){
  tmp<-function(i){
    a1<-sample(which(y==1),1)
    a2<-sample(which(y==-1),1)
    ind<-c(a1,a2)
    return(c(sample(setdiff(1:n,ind),n-val-2,TRUE),ind))
  }
  if(missing(penalty))
    penalty="direct"
  if(penalty=="direct"){
    penfun<-function(lam,u){
      lam
    }
  }
  if(penalty=="hlasso"){
    penfun<-function(lam,u){
      if(u==0){
        return(1)
      }
      top<-.5*log( (1-u+lam)/(u+lam))
      bot<-.5*log( (1-u)/u )
      if(bot==0)
        return(0)
      top/bot
    }
  }
  if(penalty=="elasso")
     penalty="pelasso"
  if(penalty=="pelasso"){
    penfun<-function(lam,u){
      if(u==0){
        return(1)
      }
      val<-as.numeric(1-2*u<lam)
      top<-.5*log( (1-u)/(u+lam))*(1-val)
      bot<-.5*log( (1-u)/u)
      if(bot==0)
        return(0)
      top/bot
    }
  }
  n=dim(x)[1]
  w=rep(1,n)/n
  alpha=vector(length=iter)
  anu=vector(length=iter)
  alpha0=vector(length=iter)
  cv.lam=vector(length=iter)
  fits=rep(0,n)
  fit=list()
  y<-c(-1,1)[as.numeric(as.factor(y))]
  dat<-data.frame(y=as.factor(y),x)
  
  oobm.mat<-matrix(0,nrow=n,ncol=2)
  oobm.err<-rep(0,iter)
  train.err<-rep(0,iter)
  if(!is.null(test.response)){
    test.response=c(-1,1)[as.numeric(as.factor(test.response))]
    test.err<-rep(0,iter)
    test.n<-dim(test.x)[1]
    fits.test<-rep(0,test.n)
  }
  nsamp=ceiling(bag.frac*n)

  ##Kick it off
  depth=bag-1
  ay1<-which(y==-1)
  ay2<-which(y==1)
  my1<-length(ay1)/n
  my2<-length(ay2)/n

  mx1<-max(2,floor(my1*nsamp))
  mx2<-max(2,floor(my2*nsamp))

  a<-c(sample(ay1,mx1,FALSE),sample(ay2,mx2,FALSE))
  for (m in 1:iter){
    control=rpart.control(minsplit=1)
    fit = rpart(y~., data=dat[a,],weights=w[a], method="class",control=control)
    ans<-predict(fit,newdata=dat,type="prob")
    f=c(-1,1)[as.numeric(apply(ans,1,which.max))]
    if(sum(is.na(f))>0)
      f[is.na(f)]<-1
    B=as.numeric(y!=f)
    errm=sum(w*B)
    if(is.na(errm)){
      cat("w=",w,"\n f=",f,"\n y=",y,"\n")
    }
    if( (1-errm)==1 | errm==1 ){
      errm=(1-errm)*0.0001+errm*.9999
    }
    alpha0[m]=.5*log( (1-errm)/errm)
    if(!coef)
      alpha0[m]=1
    if(length(lambda)>1){
      if(k==5){
        sz1<-list()
        for(i in 1:(k-1))
          sz1[[i]]<-floor(n/k)
        sz1[[k]]<-ceiling(n/k)
        indtr<-NULL
        vals<-rep(0,length(lambda))
        ind<-NULL
        for(i in 1:k){
          indtr<-sample(setdiff(1:n,ind),sz1[[i]],FALSE)
          ind<-c(ind,indtr)
          indte<-setdiff(1:n,indtr)
          umi=(errm-sum(w[indte]*B[indte]))/(1-sum(w[indte]))
          atmp=0
          if(umi>0)
            atmp<-.5*log( (1-umi)/umi )
          f1<-function(j){
            ans<-penfun(j,umi)*atmp*f[indte]+fits[indte]
            j1<-ans==0
            if(sum(j1)>0)              
              ans[j1]<-1
            sum(sign(ans)!=y[indte])/length(y)
          }
          vals<-vals+sapply(lambda,f1)/k
        }
      }
      if(k==1){
        umi=(errm-w*B)/(1-w)
        atmp=0
        if(sum(abs(umi))>0)
          atmp<-.5*log( (1-umi)/umi )
        f1<-function(j){
          ans<-sapply(umi,function(i)penfun(j,i))*atmp*f+fits
          j1<-ans==0
          if(sum(j1)>0)
            ans[j1]<-1
          ##sum(sign(ans)!=y)/length(y)
          sum(0.5*exp(-ans*y))/length(y)
        }
        f2<-function(j){
          ans<-sapply(umi,function(i)penfun(j,i))*f+fits
          j1<-ans==0
          if(sum(j1)>0)
            ans[j1]<-1
          sum(sign(ans)!=y)/length(y)
        }
        if(coef){
          vals<-sapply(lambda,f1)/n
        }else{
          vals<-sapply(lambda,f2)/n
        }
      }
      lam=FUN(lambda[vals==min(vals)])
    }else{
      lam=lambda
    }
    cv.lam[m]=lam
    anu[m]=penfun(lam,errm)
    if(anu[m]>0){
      if(length(lambda)>1)
	if(sum(vals==min(vals))==length(lambda)){
	  anu[m]=0.1
        }
    }
    alp=anu[m]*alpha0[m]
    alpha[m]=alp
    
    w=w*exp(alp*B)
    w=w/sum(w)
    if(bag)
      alpha[m]=( (1-anu[m])/iter+alp)
    fits<-fits+alpha[m]*f
    train.err[m]=sum(sign(fits)!=y)/n

    indx<- setdiff(1:n,a)
    btmp<-as.numeric(as.factor(sign(f)[indx]))
    if(length(btmp)==1){
      oobm.mat[indx,btmp]<-oobm.mat[indx,btmp]+1
    }else{
      oobm.mat[indx,][btmp==1,1]<- oobm.mat[indx,][btmp==1,1]+1
      oobm.mat[indx,][btmp==2,2]<- oobm.mat[indx,][btmp==2,2]+1
      denom<-apply(oobm.mat,1,sum)
      vals<-denom>0
      if(sum(vals)==1){
	ytr<-c(-1,1)[which.max(oobm.mat[vals,])]
      }else{
        ytr<-c(-1,1)[apply(oobm.mat[vals,],1,which.max)]
      }
      oobm.err[m]<-sum(ytr!=y[vals])/length(vals)
    }
    a<-c(sample(ay1,mx1,FALSE),sample(ay2,mx2,FALSE))

    if(is.null(test.response))
      next
    ans<-predict(fit,newdata=test.x,type="prob")
    f1=c(-1,1)[as.numeric(apply(ans,1,which.max))]
    if(sum(is.na(f1))>0)
      f1[is.na(f1)]<-1
    fits.test<-fits.test +(alpha[m])*f1
    test.err[m]<- sum(sign(fits.test)!=test.response)/test.n
  }
  a1=(fits==0)
  if(sum(a1)>0)
    fits[a1]<-sample(c(-1,1),sum(a1),TRUE,c(.5,.5))
  errs<-cbind(train.err,oobm.err)
  ans<-list()
  ans[[1]]=fits
  if(!is.null(test.response)){
    errs<-cbind(errs,test.err)
    ans[[2]]=fits.test
  }
  pen=cbind(alpha0,anu)
  obj=list(alpha=alpha,pen=pen,cv.lam=cv.lam,F=ans,errs=errs,lw=w,names=names(x),iter=iter)
  obj
}

