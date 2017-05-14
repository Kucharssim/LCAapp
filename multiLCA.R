#### Run estimation in parallel ####
funLCA <- c("emLCA", "compLik", "assignProb",
            "randomTheta", "updateTheta", "tab.d", "k")

library(parallel)
cores <- detectCores() - 1
cl <- makeCluster(cores)

multiLCA <- function(d, models, rep.n, tol=1e-5){
  clusterExport(cl=cl, varlist = funLCA)
  fits <- lapply(models, function(k){
    print(k)
    print(funLCA)
    parLapply(cl, 1:rep.n, function(x) {
      emLCA(d, k, tol=tol, output.all = FALSE)
    })
  })
  names(fits) <- models
  return(fits)
}

foo <- multiLCA(tab.d, 1:5, 10, tol=1e-1)
stopCluster(cl)
