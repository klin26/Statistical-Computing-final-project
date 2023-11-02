library(mvtnorm)
library(fields)
# initial
initial <- function(data,k){
  # set.seed(111024509)
  n <- dim(data)[2]
  m <- dim(data)[1]
  x <- list()
  mu <- list()
  sigma <- list()
  rho <- list()
  cov <- list()
  cor <- list()
  # data, mu, sigma
  for(i in 1:n){
    x[[i]] <- data[,i]
    mu[[i]] <- rnorm(k,mean=mean(x[[i]]),sd=sd(x[[i]])/3)
    sigma[[i]] <- abs(sd(x[[i]])+runif(k,min=(-sd(x[[i]])/5),max=sd(x[[i]]))/5)
  }
  # rho
  for(j in 1:((n^2-n)/2)){
    rho[[j]] <- rep(cor(data)[which(upper.tri(cor(data)))][[j]],k)
  }
  # tau
  tau <- rep(1/k,k-1)
  # covariance matrix
  for(i in 1:k){
    cov[[i]] <- matrix(NA,ncol=n,nrow=n)
    sigma_j <- c()
    rho_k <- c()
    for(j in 1:n){
      # cov[[i]] <- sigma[[j]][i]^2
      sigma_j <- as.matrix(cbind(sigma_j,sigma[[j]][i]))
    }
    for(k in 1:((n^2-n)/2)){
      rho_k <- cbind(rho_k,rho[[k]][i])
    }
    cov[[i]] <- t(sigma_j) %*% sigma_j
    cov_offdiag <- cov[[i]][upper.tri(cov[[i]])]*rho_k
    cov[[i]][upper.tri(cov[[i]])] <- cov_offdiag
    cov[[i]][lower.tri(cov[[i]])] <- t(cov[[i]])[lower.tri(cov[[i]])]
    cor[[i]] <- cov2cor(cov[[i]])
  }
  return(list(x=x,tau=tau,mu=mu,sigma=sigma,rho=rho,cov=cov,cor=cor))
}

# E-step
E <- function(data,par,k){
  x <- as.matrix(do.call(cbind, par$x))
  tau <- c(par$tau,1-sum(par$tau))
  mu <- as.matrix(do.call(cbind, par$mu))
  # sigma <- as.matrix(do.call(cbind, par$sigma))
  cov <- par$cov
  n <- dim(data)[2]
  m <- dim(data)[1]
  gamma <- matrix(NA,m,k)
  for(i in 1:m){
    for(j in 1:k){
      gamma[i,j] <- (tau[j]*dmvnorm(x=x[i,],mean=mu[j,],sigma=cov[[j]]))
    }
  }
  summ <- apply(gamma,1,sum)
  gamma <- gamma/summ
  return(gamma)
}

# M-step
M <- function(data,par,k){
  n <- dim(data)[2]
  m <- dim(data)[1]
  x <- par$x
  gamma_m <- E(data,par,k)
  gamma_sum <- apply(gamma_m, 2, sum)
  # tau
  tau <- t(as.matrix(apply(gamma_m, 2, mean)))[1:(k-1)]
  mu <- list()
  cov <- list()
  cor <- list()
  # mu
  for(i in 1:n){
    mu[[i]] <- (t(gamma_m)%*%x[[i]])/gamma_sum
  }
  # covariance matrix
  for(j in 1:k){
    # x - mu
    center <- 
      t(as.matrix(do.call(cbind, par$x)))-matrix(as.matrix(do.call(cbind, mu))[j,],n,m)
    se <- NULL
    for(z in 1:n){
      gamma_sq <- sqrt(gamma_m[,j])
      ifelse(gamma_sq=="NaN",0,gamma_sq)
      se_z <- c(t(gamma_sq)*center[z,])
      se <- cbind(se,se_z)
    }
    cov[[j]] <- t(as.matrix(se))%*%as.matrix(se)/gamma_sum[j]
    colnames(cov[[j]]) <- NULL
    rownames(cov[[j]]) <- NULL
    cor[[j]] <- cov2cor(cov[[j]])
    colnames(cor[[j]]) <- NULL
    rownames(cor[[j]]) <- NULL
  }
  for(i in 1:n){
    mu[[i]] <- c(mu[[i]])
  }
  M.list <- list(x=x,tau=tau,mu=mu,cov=cov,cor=cor)
  return(M.list)
}

# EM
EM <- function(data,k){
  n <- dim(data)[2]
  m <- dim(data)[1]
  par <- initial(data,k)
  
  # convergence tolarence
  tol <- 10^-7
  err <- tol
  count <- 1
  
  # par save
  cov_diag_save <- list()
  cor_upper_save <- list()
  tau_save <- as.data.frame(par$tau)
  colnames(tau_save) <- c("0")
  mu_save <- as.data.frame(do.call(c, par$mu))
  colnames(mu_save) <- c("0")
  for(j in 1:k){
    cov_diag_save[[j]] <- sqrt(diag(par$cov[[j]]))
    cor_upper_save[[j]] <- par$cor[[j]][lower.tri(par$cor[[j]])]
  }
  sigma_save <- as.data.frame(do.call(c, cov_diag_save))
  colnames(sigma_save) <- c("0")
  cor_save <- as.data.frame(do.call(c, cor_upper_save))
  colnames(cor_save) <- c("0")
  
  while(err>=tol&count<=500){
    # print(count)
    # par old
    tau_old <- par$tau
    mu_old <- as.matrix(do.call(cbind, par$mu))
    cov_diag_old <- list()
    cor_upper_old <- list()
    for(j in 1:k){
      cov_diag_old[[j]] <- sqrt(diag(par$cov[[j]]))
      cor_upper_old[[j]] <- par$cor[[j]][lower.tri(par$cor[[j]])]
    }
    sigma_old <- as.matrix(do.call(cbind, cov_diag_old))
    cor_old <- as.matrix(do.call(cbind, cor_upper_old))
    
    # par new
    par_new <- M(data,par,k)
    
    # if(min(par_new$tau)==0 | length(na.omit(do.call(c, par_new$mu)))!=(k*n)){ 
    #   break
    # }
    
    tau_new <- par_new$tau
    mu_new <- as.matrix(do.call(cbind, par_new$mu))
    cov_diag_new <- list()
    cor_upper_new <- list()
    for(j in 1:k){
      cov_diag_new[[j]] <- sqrt(diag(par_new$cov[[j]]))
      cor_upper_new[[j]] <- par_new$cor[[j]][lower.tri(par_new$cor[[j]])]
    }
    sigma_new <- as.matrix(do.call(cbind, cov_diag_new))
    cor_new <- as.matrix(do.call(cbind, cor_upper_new))
    
    # par save
    tau_save_new <- as.data.frame(par$tau)
    colnames(tau_save_new) <- c(paste(count))
    mu_save_new <- as.data.frame(do.call(c, par$mu))
    colnames(mu_save_new) <- c(paste(count))
    sigma_save_new <- as.data.frame(do.call(c, cov_diag_new))
    colnames(sigma_save_new) <- c(paste(count))
    cor_save_new <- as.data.frame(do.call(c, cor_upper_new))
    colnames(cor_save_new) <- c(paste(count))
    tau_save <- cbind(tau_save,tau_save_new)
    mu_save <- cbind(mu_save,mu_save_new)
    sigma_save <- cbind(sigma_save,sigma_save_new)
    cor_save <- cbind(cor_save,cor_save_new)
    
    # error
    err_tau <- max(abs(tau_new-tau_old))
    err_mu <- max(abs(mu_new-mu_old))
    err_sigma <- max(abs(sigma_new-sigma_old))
    err_cor <- max(abs(cor_new-cor_old))
    err <- max(err_tau,err_mu,err_sigma,err_cor)
    count <- count+1
    par <- par_new
  }
  par_table <- t(rbind(tau_save,mu_save,sigma_save,cor_save))
  colnames(par_table) <- c(paste("tau",seq(1,k-1)),paste("mu",rep(seq(1,k),each=n),rep(seq(1,n),k)),
                           paste("sigma",rep(seq(1,k),each=n),rep(seq(1,n),k)),
                           paste("rho",rep(seq(1,k),each=((n^2-n)/2)),rep(combn(1:n,2)[1,],k),rep(combn(1:n,2)[2,],k)))
  index_clustering <- apply(E(data,par,k),1,which.max)
  
  # if(min(par_new$tau)==0 | length(na.omit(do.call(c, par_new$mu)))!=(k*n)){ 
  #   print("Some components have reduced with such initial, please try again or choose a smaller k.")
  #   break
  # }else{
  #   return(list(k=k,iter=(count-1),par=par,par_table=par_table,index_clustering=index_clustering))
  # }
  
  return(list(k=k,iter=(count-1),par=par,par_table=par_table,index_clustering=index_clustering))
}

# plot
plotEM <- function(data,par){
  n <- dim(data)[2]
  m <- dim(data)[1]
  x <- par$par$x
  k <- par$k
  tau <- c(par$par$tau,1-sum(par$par$tau))
  mu <- par$par$mu
  sigma <- par$par$cov
  
  # pdf function
  f_1dim <- function(x,s=i) {
    f <- 0
    for (a in 1:k) {
      f <- f+tau[a]*dnorm(x,mean=mu[[s]][a],sd=sqrt(sigma[[a]][s,s]))
    }
    return(f)
  }
  f_1dim <- Vectorize(f_1dim)
  f_2dim <- function(x,s=i,t=j){
    f <- 0
    for (b in 1:k) {
      f <- f+tau[b]*dmvnorm(x,mean=c(mu[[s]][b],mu[[t]][b]),sigma=sigma[[b]][c(s,t),c(s,t)])
    }
    return(f)
  }
  
  # plot
  par(mfrow=c(n,n), mar=c(3,3,1,1), mgp=c(1.6,0.6,0))
  for (i in 1:n) {
    for (j in 1:n) {
      if(i==j){
        hist(data[,i],xlab = paste0('x',i),
             main='',
             probability = T,breaks = 20)
        curve(f_1dim,from = min(data[,i]),to=max(data[,i]),add = T,col="darkred",lwd=2)
      }
      else{
        grid1 <- seq(min(data[,i]),max(data[,i]),length=100)
        grid2 <- seq(min(data[,j]),max(data[,j]),length=100)
        uv = expand.grid(u=grid1, v=grid2)
        z1 <- apply(uv, 1, f_2dim)
        quilt.plot(uv$u, uv$v, z1,
                   xlim = c(min(data[,i]),max(data[,i])),
                   ylim = c(min(data[,j]),max(data[,j])),
                   col = rev(heat.colors(1000)),add.legend = F,
                   xlab=paste0('x',i),ylab=paste0('x',j))
        points(data[,c(i,j)], pch = 20, col = par$index_clustering)
        contour(grid1, grid2, matrix(z1, length(grid1), length(grid2)),
                levels = round(quantile(z1, c(seq(0.01,0.99,0.05))), 5),
                xlab = "x", ylab = "y", add = T, col = "darkgrey")
      }
    }
  }
}

# ts.plot
ts.plotEM <- function(data,par){
  k <- par$k
  n <- dim(data)[2]
  ts.plot(par$par_table,col=1:(k*(2*n+1+(n^2+n)/2)-1));title(paste("convergence k=",k))
}

# BIC
BICplotEM <- function(data,k_min,k_max){
  n <- dim(data)[2]
  m <- dim(data)[1]
  randomseed <- sample(1:10000,1)
  BIC <- c()
  AIC <- c()
  for(c in k_min:k_max){
    set.seed(randomseed)
    par <- EM(data,c)
    x <- as.matrix(do.call(cbind, par$par$x))
    k <- par$k
    tau <- c(par$par$tau,1-sum(par$par$tau))
    mu <- as.matrix(do.call(cbind, par$par$mu))
    cov <- par$par$cov
    logl <- c()
    for(i in 1:m){
      l <- 0
      for(j in 1:k){
        l <- l+(tau[j]*
                  dmvnorm(x=x[i,],mean=mu[j,],
                          sigma=cov[[j]]))
      }
      logl[i] <- log(l)
    }
    logls <- sum(logl)
    BIC[(c-k_min+1)] <- (k*(2*n+1+(n^2+n)/2)-1)*log(m)-2*logls
    # AIC[(c-k_min+1)] <- (k*(2*n+1+(n^2+n)/2)-1)*2-2*logls
  }
  k <- seq(k_min,k_max,1)
  plot(k,BIC,pch=19,col="darkred",xlab="k",
       ylab="BIC",type="l",main="BIC comparing",lwd=2)
  points(k,BIC,pch=19,col="darkred",lwd=2)
  # points(k,AIC,pch=19,col="orange",lty=2)
  # legend("bottomright",c("BIC","AIC"),pch=c(19,19),lty=c(1,1),col=c("lightblue","orange"),bty="n")
  #return(BIC)
}

