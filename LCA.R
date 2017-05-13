emLCA <- function(d, k, start.theta=randomTheta(d, k), 
                  start.pi=rdirichlet(1, rep(1, k)), tol=1e-3,
                  method=1, method2 = 1){
  
  theta <- start.theta
  pi  <- start.pi
  
  likelihoods <- t(apply(d, 1, assignProb, k=k, theta=start.theta, pi=start.pi, scale=FALSE))
  
  llik.old <- compLik(likelihoods)
  llik.new <- llik.old + 2*tol
  n.iter <- 0
  # loop until convergence
  while (abs(llik.new-llik.old)>tol){
    n.iter <- n.iter + 1 
    llik.old <- llik.new
    
    #expectation
    if(method2==1){
      likelihoods <- apply(d, 1, assignProb, k=k, theta=theta, pi=pi, scale=FALSE)
    } else{
      likelihoods <- assignProb2(d, k, theta, pi, FALSE)
    }
    
    if(k>1 & method2==1){
      likelihoods <- t(likelihoods)
    }
    posterior <- sweep(likelihoods, 1, rowSums(likelihoods), "/")
    
    #maximisation
    pi <- colMeans(posterior)
    if(method==1){
      theta <- updateTheta(d, posterior, theta)
    } else {
      theta <- updateTheta2(d, posterior, theta)
    }
    #log-likelihood
    llik.new <- compLik(likelihoods)
    print(llik.new)
  }
  
  #posterior <- apply(d, 1, assignProb, k=k, theta=theta, pi=pi)
  invisible(list(llik=llik.new,
              n.iter=n.iter,
              theta=theta,
              pi=pi,
              posterior=posterior,
              starting.values=list(theta=start.theta,
                                   pi=start.pi)))
}

compLik <- function(likelihoods){
  sum(log(rowSums(likelihoods)))
}
  
updateTheta <- function(d, posterior, theta){
  d <- as.data.frame(lapply(d, factor))
  posterior <- matrix(posterior, nrow=nrow(d))

  # update theta over items
  new.theta <- lapply(1:ncol(d), function(item){
    i.theta <- theta[[item]]
    i.theta[,] <- 0

    for(row in 1:nrow(d)){
      tab.d <- table(d[row,item])
      i.theta <- i.theta + posterior[row, ] %*% t(tab.d)
    }
    
    return(sweep(i.theta, 1, rowSums(i.theta), "/"))
  })
  
  return(new.theta)
}

updateTheta2 <- function(d, posterior, theta){
  new.theta <- lapply(1:ncol(d), function(item){
    tab.d <- model.matrix(~factor(d[,item])-1)

    i.theta <- t(posterior) %*% tab.d
    return(sweep(i.theta, 1, rowSums(i.theta), "/"))
  })
  
  return(new.theta)
}

assignProb <- function(d, k, theta, pi=NA, scale=TRUE){
  # compute the likelihood for class membership
  d <- as.vector(t(d))
  
  p <- sapply(1:k, function(class){
    # select the prob of observed value
    # for each item on the current class 

    lik <- sapply(1:length(d), function(item){
      theta[[item]][class, d[item]]
    })
    
    lik <- prod(lik)
    if(!is.na(pi[1])){ # if prior supplied, compute posterior
      lik <- lik * pi[class]
    }

    return(lik)
  })
  
  if(scale){
    # scale it
    return(p / sum(p))
  }
  else{
    return(p)
  }
}

assignProb2 <- function(d, k, theta, pi=NA, scale=TRUE){

  likelihood <- matrix(1, ncol=k, nrow=nrow(d))
  for(item in 1:ncol(d)){
    tab.d <- model.matrix(~factor(d[,item])-1)

    likelihood <- likelihood * (tab.d %*% t(theta[[item]]))
  }
  p <- sweep(likelihood, 2, pi, "*")
  #p <- likelihood
  if(scale){
    sweep(p, 1, rowSums(p), "/")
  } else{
    p
  }
}

randomTheta <- function(d, k){
  theta <- lapply(1:ncol(d), function(item){
    levels <- length(unique(d[, item]))

    return(rdirichlet(k, rep(1, levels)))
  })
}
