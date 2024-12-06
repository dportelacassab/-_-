# Funciones ####

# QQ plot function #####

qqplot.normal <- function(objeto,k,alfa,d,nombres){
  y <- objeto$residuals + fitted(objeto)
  if(length(nombres)<length(y)){
    nombres <- seq(1,length(y),by=1)
  }
  y <- objeto$residuals + fitted(objeto)
  X <- model.matrix(objeto)
  n <- nrow(X)
  p <- ncol(X)
  phi <- sum(objeto$residuals*objeto$residuals)/(length(y)-length(coef(objeto)))
  XtX <- vcov(objeto)/phi
  h <- matrix(0,length(y),1)
  for(i in 1:length(y)){
    h[i] <- t(X[i,])%*%XtX%*%X[i,]
  }
  r <- (y-fitted(objeto))/sqrt((1-h)*phi)
  r <- r*sqrt((length(y)-ncol(X)-1)/(length(y)-ncol(X)-r*r))
  alfa1 <- ceiling(k*alfa)
  alfa2 <- ceiling(k*(1-alfa))
  epsilon <- matrix(0,n,k)
  e <- matrix(0,n,k)
  e1 <- numeric(n)
  e2 <- numeric(n)
  
  for(i in 1:k){
    resp <- fitted(objeto) + rnorm(n,mean=0,sd=1)*sqrt(phi) 
    fits <- glm(resp ~ X, family=gaussian())
    phis <- sum(fits$residuals*fits$residuals)/(length(y)-length(coef(fits)))
    rs <- (resp-fitted(fits))/sqrt((1-h)*phis)
    rs <- rs*sqrt((length(y)-ncol(X)-1)/(length(y)-ncol(X)-rs*rs))
    e[,i] <- sort(rs)
  }
  med <- apply(e,1,mean)
  
  for(i in 1:n){
    e0 <- sort(e[i,])
    e1[i] <- e0[alfa1]
    e2[i] <- e0[alfa2]
  }
  faixa <- range(r,e1,e2)
  par(pty="s")
  qqnorm(e1,axes=F,xlab="",type="l",ylab="",main="",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(e2,axes=F,xlab="",type="l",ylab="",main="",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(med,axes=F,xlab="",type="l",ylab="",main="",ylim=faixa,lty=3)
  par(new=T)
  dd <- qqnorm(r,xlab="Percentiles de la N(0,1)", ylab="Residuos",main="QQ Plot", ylim=faixa, cex=0.3, lwd=3)
  identify(dd$x,r,n=d, labels=nombres)
}

### Leverage Function #####################
Leverage.normal <- function(objeto,d,nombres){
  y <- objeto$residuals + fitted(objeto)
  if(length(nombres)<length(y)){
    nombres <- seq(1,length(y),by=1)
  }
  H <- matrix(0,length(y),1)
  X <- model.matrix(objeto)
  sigma2 <- sum(objeto$residuals^2)/(length(y)-ncol(X))
  XtX <- vcov(objeto)/sigma2
  for(i in 1:length(y)){
    H[i] <- t(X[i,])%*%XtX%*%X[i,]
  }
  maxy <- max(max(H),2*mean(H))
  plot(H, main="Puntos de alto Leverage", xlab="Índice", ylim=c(0,maxy), ylab="h", cex=0.3, lwd=3)
  abline(2*mean(H),0,lty=3)
  identify(H, n=d,labels=nombres)
  H
}

# Residual function ##################

Residuos.normal <- function(objeto,d,nombres){
  y <- objeto$residuals + fitted(objeto)
  h <- matrix(0,length(y),1)
  X <- model.matrix(objeto)
  sigma <- sum(objeto$residuals^2)/(length(y)-ncol(X))
  XtX <- vcov(objeto)/sigma
  for(i in 1:length(y)){
    h[i] <- t(X[i,])%*%XtX%*%X[i,]
  }
  r <- (y-fitted(objeto))/sqrt((1-h)*sigma)
  r <- r*sqrt((length(y)-ncol(X)-1)/(length(y)-ncol(X)-r*r))
  maxy <- max(max(r),3)
  miny <- min(min(r),-3)
  if(length(nombres)<length(y)){
    nombres <- seq(1,length(y),by=1)
  }
  plot(fitted(objeto), r, main="Observaciones extremas en la respuesta", xlab="Media estimada", ylab="Residuo estandarizado", cex=0.3, lwd=3, ylim=c(miny,maxy))
  abline(2,0,lty=3)
  abline(0,0,lty=3)
  abline(-2,0,lty=3)
  identify(x=fitted(objeto), y=r, n=d, labels=nombres)
  r
}

# Influence function ##############
Influence.normal <- function(objeto,a,b,d,nombres){
  y <- objeto$residuals + fitted(objeto)
  X <- model.matrix(objeto)
  if(length(nombres)<length(y)){
    nombres <- seq(1,length(y),by=1)}
  delta <- lm.influence(objeto)$coef
  DC <- diag(delta%*%solve(vcov(objeto))%*%t(delta))/ncol(X)
  maxy <- max(max(DC),3*mean(DC))
  plot(DC, main="Observaciones influyentes", xlab="Índice", ylim=c(0,maxy), ylab="Distancia de Cook", cex=0.3, lwd=3)
  abline(3*mean(DC),0,lty=3)
  identify(DC, n=d, labels=nombres)
  p <- ncol(X)
  labels <- labels(coef(objeto))
  respuesta <- names(objeto$model[1])
  X11()
  par(mfrow=c(a,b))
  for(i in 1:p){
    a <- matrix(0,1,p)
    a[i] <- 1
    delta <- lm.influence(objeto)$coef[,i]
    DCi <- diag(delta%*%solve(a%*%vcov(objeto)%*%t(a))%*%t(delta))
    maxy <- max(max(DCi),3*mean(DCi))
    plot(DCi, main=labels[i], xlab="Índice", ylim=c(0,maxy), ylab="Distancia de Cook", cex=0.3, lwd=3)
    abline(3*mean(DCi),0,lty=3)
    identify(DCi, n=d, labels=nombres)
  }
  DC
}
