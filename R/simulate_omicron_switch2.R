drawpar <- function(D_e=2,
                    D_p=3,
                    D_s=3,
                    p_a=0.4,
                    R0=2,
                    p_p=0.5, ## proportion of presymptomatic transmission for symptomatic individuals
                    k=0.1,
                    preport=1,
                    delta=0.6/140) {
  
  c(
    D_e=D_e,
    D_p=D_p,
    D_s=D_s,
    p_a=p_a,
    R0=R0,
    p_p=p_p,
    k=k,
    preport=preport,
    delta=delta
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
      preport=preport,
      delta=delta
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

simulate_omicron_switch2 <- function(param=drawpar(),
                     vprop=0.99,
                     boostprop=0.65,
                     veff_twodose=0.1,
                     veff_threedose=0.7,
                     veff_twodose_inf=0.1 * 0.2/0.9,
                     veff_threedose_inf=0.7 * 0.2/0.9,
                     vaccine_delay=7,
                     boostdaily=10,
                     return,
                     returnfrac=1/7,
                     N=4000,
                     I=0.01,
                     R=0,
                     datestart=as.Date("2022-01-03"),
                     datesemester=as.Date("2022-01-10"),
                     dateinternal=as.Date("2022-01-10"),
                     dateend=as.Date("2022-04-25"),
                     dateswitch=as.Date("2022-03-01"),
                     thetavec,
                     asymp_testing=TRUE,
                     testfreq_vacc=3,
                     testfreq_unvacc=3,
                     testfreq_new=7,
                     Rnewratio=1.5,
                     return_testing=TRUE,
                     ntest=6000,
                     sensitivity=0.95,
                     specificity=1,
                     qperiod=14,
                     iperiod=5,
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
    dateswitch=dateswitch,
    thetavec=thetavec,
    asymp_testing=asymp_testing,
    testfreq_vacc=testfreq_vacc,
    testfreq_unvacc=testfreq_unvacc,
    testfreq_new=testfreq_new,
    Rnewratio=Rnewratio,
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
    return <- rep(FALSE, N)
  }
  
  vacc <- rbinom(length(state), 1, vprop)
  boost0 <- rbinom(sum(vacc), 1, boostprop)
  boost <- rep(0, length(state))
  boost[which(vacc==1)[boost0==1]] <- 1
  
  veff <- rep(0, length(state))
  veff[vacc==1] <- veff_twodose
  veff[boost==1] <- veff_threedose
  
  veff_inf <- rep(0, length(state))
  veff_inf[vacc==1] <- veff_twodose_inf
  veff_inf[boost==1] <- veff_threedose_inf
  
  func.arg2 <- c(func.arg, cparam)
  
  func.arg2$state <- state
  func.arg2$return <- return
  func.arg2$vacc <- vacc
  func.arg2$veff_twodose <- veff_twodose
  func.arg2$veff_threedose <- veff_threedose
  func.arg2$veff_twodose_inf <- veff_twodose_inf
  func.arg2$veff_threedose_inf <- veff_threedose_inf
  func.arg2$vaccine_delay <- vaccine_delay
  func.arg2$boost <- boost
  func.arg2$boostdaily <- boostdaily
  func.arg2$veff <- veff
  func.arg2$veff_inf <- veff_inf
  
  simres <- do.call("simulate_internal_omicron_switch2", func.arg2)
  
  out <- list(
    call=call,
    res=simres
  )
  
  out
}
