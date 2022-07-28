drawpar <- function(D_e=2,
                    D_p=3,
                    D_s=3,
                    p_a=0.4,
                    R0=2.5,
                    p_p=0.5, ## proportion of presymptomatic transmission for symptomatic individuals
                    k=0.1,
                    preport=1) {
  
  c(
    D_e=D_e,
    D_p=D_p,
    D_s=D_s,
    p_a=p_a,
    R0=R0,
    p_p=p_p,
    k=k,
    preport=preport
  )
}

##' @param x parameter (from `drawpar()`)
convertpar <- function(x) {
  with(as.list(x), {
    ## (1-p_p) * R0 = beta * D_s
    ##
    ##
    
    beta <- R0 * (1 - p_p)/D_s
    Rs <- D_s * beta
    
    Rp <- p_p/(1-p_p) * Rs
    
    beta_p <- Rp/D_p
    
    list(
      sigma=-log(1-1/D_e), 
      gamma_a1=-log(1-2/D_s),
      gamma_a2=-log(1-2/D_s),
      gamma_p1=-log(1-2/D_p), 
      gamma_p2=-log(1-2/D_p), 
      gamma_s1=-log(1-2/D_s),
      gamma_s2=-log(1-2/D_s),
      p_a=p_a, 
      beta=beta,
      beta_p=beta_p,
      k=k,
      preport=preport
    )
  })
}

initialize <- function(param,
                       N,
                       I,
                       R) {
  with(as.list(param), {
    .state <- c("S", "E", "Ip1", "Ip2", "Ia1", "Ia2", "Is1", "Is2", "R")
    
    distrb <- c(rmultinom(1, N, c(1-I-R, I, R)))
    
    prob <- c(D_e, D_p/2, D_p/2, D_s/2, D_s/2, 0, 0)
    
    prob <- prob/sum(prob)
    
    state <- rmultinom(1, distrb[2], prob)
    
    final <- c(distrb[1], state, distrb[3])
    
    sample(unname(unlist(mapply(function(x, y) rep(x, y), .state, final))))
  })
}

simulate <- function(param=drawpar(),
                     vacc,
                     vprop=0.9,
                     veff=0.9,
                     return,
                     returnfrac=1/7,
                     N=8000,
                     I=0.01,
                     R=0,
                     datestart=as.Date("2021-08-25"),
                     datesemester=as.Date("2021-09-01"),
                     dateinternal=as.Date("2021-09-01"),
                     dateend=as.Date("2021-12-07"),
                     thetavec,
                     asymp_testing=TRUE,
                     testfreq_vacc=7,
                     testfreq_unvacc=3,
                     return_testing=TRUE,
                     ntest=6000,
                     sensitivity=0.95,
                     specificity=1,
                     qperiod=14,
                     iperiod=10,
                     qcontact=TRUE,
                     trace=TRUE,
                     traceeff=0.85,
                     tracedelay=1,
                     tracememory=2,
                     returnq=1) {
  func.arg <- list(
    returnfrac=returnfrac,
    datestart=datestart,
    datesemester=datesemester,
    dateinternal=dateinternal,
    dateend=dateend,
    thetavec=thetavec,
    asymp_testing=asymp_testing,
    testfreq_vacc=testfreq_vacc,
    testfreq_unvacc=testfreq_unvacc,
    return_testing=return_testing,
    ntest=ntest,
    sensitivity=sensitivity,
    specificity=specificity,
    qperiod=qperiod,
    iperiod=iperiod,
    qcontact=qcontact,
    trace=trace,
    traceeff=traceeff,
    tracedelay=tracedelay,
    tracememory=tracememory,
    returnq=returnq
  )
  
  call <- match.call()
  call.orig <- call
  
  cparam <- convertpar(param)
  
  ## all susceptible
  state <- initialize(N=N, I=I, R=R, param)
  
  if (missing(return)) {
    return <- rep(FALSE, length(state))
  }
  
  if (missing(vacc)) {
    vacc <- rbinom(length(state), 1, vprop)
  }
  
  func.arg2 <- c(func.arg, cparam)
  
  func.arg2$state <- state
  func.arg2$return <- return
  func.arg2$vacc <- vacc
  func.arg2$veff <- veff
  
  simres <- do.call("simulate_internal", func.arg2)
  
  out <- list(
    call=call,
    res=simres
  )
  
  out
}
