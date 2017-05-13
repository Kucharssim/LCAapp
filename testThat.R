### Test updateTheta ###

testUpdateTheta <- function(d, posterior, theta){
  t1 <- updateTheta(d, posterior, theta)
  t2 <- updateTheta2(d, posterior, theta)
  
  test <- sapply(1:length(t1), function(x){
    mean(t1[[x]] == t2[[x]])
  }) 
  
  if(mean(test) == 1){
    return("Method Passed")
  } else{
    return("Did not pass")
  }
}

testTimeTheta <- function(d, posterior, theta){
  len <- seq(30, nrow(d), 10)
  time1 <- sapply(len, function(cases){
    t.start <- Sys.time()
    updateTheta(d[1:cases,], posterior[1:cases,], theta)
    return(Sys.time()-t.start)
  })
  
  time2 <- sapply(len, function(cases){
    t.start <- Sys.time()
    updateTheta2(d[1:cases,], posterior[1:cases,], theta)
    return(Sys.time()-t.start)
  })
  
  plot(0, type="n",
       ylim=c(0, max(c(time1, time2))),
       xlim=c(10, nrow(d)),
       ylab="time (sec)", xlab="number of cases")
  lines(len, time1, type="l", col="blue", lwd=2)
  lines(len, time2, type="l", col="green", lwd=2)
}

testTimeThetaLCA <- function(d, k){
  len <- seq(30, nrow(d), 10)
  theta <- randomTheta(d, k)
  pi <- rdirichlet(1, rep(1, k))
  
  time1 <- sapply(len, function(cases){
    t.start <- Sys.time()
    emLCA(d[1:cases,], 3, theta, pi, method=1)
    return(Sys.time()-t.start)
  })
  
  
  time2 <- sapply(len, function(cases){
    t.start <- Sys.time()
    emLCA(d[1:cases,], 3, theta, pi, method=2)
    return(Sys.time()-t.start)
  })
  
  plot(0, type="n",
       ylim=c(0, max(c(time1, time2))),
       xlim=c(10, nrow(d)),
       ylab="time (sec)", xlab="number of cases")
  lines(len, time1, type="l", col="blue", lwd=2)
  lines(len, time2, type="l", col="green", lwd=2)
}


### asignprob

testAssignProb <- function(d, k, theta, pi, scale, tol=15){
  p1 <- t(apply(d, 1, assignProb, k=k, theta=theta, pi=pi, scale=scale))
  p2 <- assignProb2(d, k, theta, pi, scale)
  dif <- as.vector(p1-p2)

  v <- var(dif)
  m <- mean(dif)
  print(m)
  #print(p1-p2)
  print(v)
  if(round(v, tol)==0 & round(m, tol)==0){
    print("Passed")
  } else{
    print("Fail")
  }
  
  invisible(dif)
}

testTimeProb <- function(d, k, theta, pi, scale){
  len <- seq(30, nrow(d), 10)
  theta <- randomTheta(d, k)
  pi <- rdirichlet(1, rep(1, k))
  
  time1 <- sapply(len, function(cases){
    t.start <- Sys.time()
    m <- emLCA(d[1:cases,], 3, theta, pi, method=2, method2=1)
    return(c((Sys.time()-t.start), m$n.iter))
  })
  
  
  time2 <- sapply(len, function(cases){
    t.start <- Sys.time()
    m <- emLCA(d[1:cases,], 3, theta, pi, method=2, method2=2)
    return(c((Sys.time()-t.start), m$n.iter))
  })
  
  plot(0, type="n",
       ylim=c(0, max(c(time1[1,], time2[1,]))),
       xlim=c(10, nrow(d)),
       ylab="time (sec)", xlab="number of cases")
  lines(len, time1[1,], type="l", col="blue", lwd=2)
  lines(len, time2[1,], type="l", col="green", lwd=2)
  
  return(list(time1, time2))
}