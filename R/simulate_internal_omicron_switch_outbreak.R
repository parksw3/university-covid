##' @param state (list) initial state of individuals (X_t^{(i)} in the Appendix)
##' @param return (list) initial return state of individuals (X_t^{(i)} in the Appendix)
##' @param sigma transition rate from the exposed stage
##' @param p_a probability of asymptomatic infection
##' @param gamma_a transition rate from the asymptomatic infection stage to the recovered stage
##' @param gamma_p transition rate from the presymptomatic infection stage to the symptomatic infection stage
##' @param gamma_s transition rate from the symptomatic infection stage to the recovered stage
##' @param beta_a transmission rate during the asymptomatic infection stage
##' @param beta_p transmission rate during the presymptomatic infection stage
##' @param beta_s transmission rate during the symptomatic infection stage
##' @param k negative binomial over-dispersion parameter
##' @param epsilon within- and between-group mixing rates (n x n matrix representing contact from ith row -> jth column)
##' @param theta infection from outside
##' @param tmax final time
##' @param internalstart internal test starting time
##' @param teststart mass test starting time
##' @param ntest number of internal tests that can be processed per day
##' @param sensitivity sensitivity
##' @param specificity specificity
##' @param qperiod quarantine period
##' @param qcontact (TRUE) infected individuals can "contact" quarantined individuals; (FALSE) infected individuals cannot "contact" quarantined individuals; analogous to densitiy and frequency dependence
##' @param preport probability of self-reporting symptoms and getting tested (and isolate until the test result comes back)
##' @param trace (logical) contact trace?
##' @param traceeff tracing efficacy
##' @param tracedelay days
##' @param tracememory how far can we go back
simulate_internal_omicron_switch_outbreak <- function(state,
                                      vacc,
                                      boost,
                                      veff_twodose,
                                      veff_threedose,
                                      veff_twodose_inf,
                                      veff_threedose_inf,
                                      vaccine_delay,
                                      veff,
                                      veff_inf,
                                      boostdaily,
                                      return,
                                      returnfrac=1/7,
                                      sigma,
                                      p_a, gamma_a1, gamma_a2,
                                      gamma_p1, gamma_p2, gamma_s1, gamma_s2,
                                      beta_p, beta,
                                      delta,
                                      k, preport,
                                      thetavec,
                                      datestart=as.Date("2022-01-03"),
                                      datesemester=as.Date("2022-01-10"),
                                      dateinternal=as.Date("2022-01-10"),
                                      dateend=as.Date("2022-04-25"),
                                      dateoutbreak=as.Date("2022-02-12"),
                                      dateswitch=as.Date("2022-02-08"),
                                      outbreaksize,
                                      asymp_testing,
                                      testfreq_vacc,
                                      testfreq_unvacc,
                                      testfreq_new,
                                      return_testing,
                                      ntest,
                                      sensitivity,
                                      specificity,
                                      qperiod,
                                      iperiod,
                                      qcontact,
                                      trace,
                                      traceeff,
                                      tracedelay,
                                      tracememory,
                                      returnq) {
  ## tmp sets up a temporary state before going into Ia1 and Ip1 (see transfun)
  ## makes some computation slightly easier
  .state <- c("S", "E", "Ip1", "Ip2", "tmp", "Ia1", "Ia2", "R", "Is1", "Is2", "R")
  .transition <- c(0, sigma, gamma_p1, gamma_p2, 0, gamma_a1, gamma_a2, 0, gamma_s1, gamma_s2, 0)
  .transmission <- c(0, 0, beta_p, beta_p, 0, beta, beta, 0, beta, beta, 0)
  .positive <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  .preport <- c(0, 0, 0, 0, 0, 0, 0, 0, preport, preport, 0)
  
  returndays <- ceiling(1/returnfrac)
  
  returnlist <- data.frame(
    returnday=sample(1:returndays, length(which(!return)), replace=TRUE),
    number=which(!return) ## basically everyone
  )
  
  datevec <- seq(from=datestart,to=dateend,by=1)
  tmax <- length(datevec)
  internalstart <- match(dateinternal, datevec)
  semesterstart <- match(datesemester, datevec)
  
  statelist <- vector('list', tmax)
  statelist[[1]] <- state
  
  tstate <- rep(FALSE, length(state))
  
  qstate <- rep(FALSE, length(state))
  
  istate <- rep(FALSE, length(state))
  
  ## quarantine start
  qstart <- rep(0, length(state))
  
  ## quarantine end
  qend <- rep(0, length(state))
  
  ## isolation start
  istart <- rep(0, length(state))
  
  ## isolation end
  iend <- rep(0, length(state))
  
  itype <- rep("none", length(state))
  
  qduration <- rep(0, length(state))
  
  iduration <- rep(0, length(state))
  
  ## symptom start
  sstart <- rep(0, length(state))
  
  ## symptom end
  send <- rep(0, length(state))
  
  ## last time testing positive
  tpositive <- rep(-1000, length(state))
  
  ## last time testing negative
  tnegative <- rep(-1000, length(state))
  
  infected_time <- rep(NA, length(state))
  infected_by <- vector('list', length(state))
  infected_to <- lapply(1:length(state), function(x) data.frame())
  q_to <- lapply(1:length(state), function(x) data.frame())
  
  incidencevec <- samplevec <- testvec <- positivevec <- rep(0, tmax)
  
  Nvec <- Svec <- Evec <- Ia1vec <- Ia2vec <- Ip1vec <- Ip2vec <- Is1vec <- Is2vec <- Rvec <- qvec <- tvec <- ivec <- rep(NA, tmax)
  
  boostvec <- effSvec <- rep(NA, tmax)
  
  testqueue_rapid <- data.frame()
  testqueue <- data.frame()
  
  boostqueue <- NULL
  
  if (!asymp_testing) {
    testorder_vacc <- testorder_unvacc <- data.frame()
  } else {
    testorder_vacc <- data.frame(
      number=which(vacc==1),
      mod=sample(1:testfreq_vacc, size=sum(vacc==1), replace=TRUE)
    )
    
    testorder_unvacc <- data.frame(
      number=which(vacc==0),
      mod=sample(1:testfreq_unvacc, size=sum(vacc==0), replace=TRUE)
    )
    
    testorder_new <- data.frame(
      number=1:length(state),
      mod=sample(1:testfreq_new, size=length(state), replace=TRUE)
    )
  }
  
  ## simulate
  for (t in 1:tmax) {
    today <- datevec[t]
    today_wday <- lubridate::wday(today)
    
    ## waning
    veff <- veff * exp(-delta)
    veff_inf <- veff_inf * exp(-delta)
    
    ## giving out boosters
    need_to_be_boosted <- which(boost==0 & vacc==1)
    if (length(need_to_be_boosted) > 0) {
      if (length(need_to_be_boosted) >= boostdaily) {
        which_boost <- sample(need_to_be_boosted, boostdaily)
      } else {
        which_boost <- need_to_be_boosted
      }
      
      boost[which_boost] <- 1
      
      boostqueue <- c(boostqueue, which_boost)
    }
    
    if (today - datestart >= vaccine_delay & length(boostqueue) > 0) {
      nboost <- min(boostdaily, length(boostqueue))
      
      which_boost2 <- boostqueue[1:nboost]
      boostqueue <- boostqueue[-c(1:nboost)]
      
      veff[which_boost2] <- veff_threedose
      veff_inf[which_boost2] <- veff_threedose_inf
    }
    
    ## infection
    if (t >= 2) {
      ## infection stage transition
      statelist[[t]] <- transfun(statelist[[t-1]], p_a=p_a, .transition=.transition,
                                 .state=.state)
      
      sstart[statelist[[t-1]]=="Ip2" & statelist[[t]]=="Is1"] <- t
      send[statelist[[t-1]]=="Is2" & statelist[[t]]=="R"] <- t
      
      ## release 3 days after symptoms resolve and at least 10 days after symptom onset
      iend[sstart==t] <- pmax(iend[sstart==t], t+10)
      iend[send==t] <- pmax(t + 1, sstart[send==t] + 10)
      
      ## big outbreak
      
      if (today==dateoutbreak) {
        whichinfect <- sample(which(statelist[[t]] == "S"), outbreaksize)
        
        statelist[[t]][whichinfect] <- "E"
        infected_time[whichinfect] <- t
        
        incidencevec[t] <- incidencevec[t] + outbreaksize
      }
      
      ## infection
      infect <- infectfun(statelist[[t-1]],
                          veff_inf,
                          k=k,
                          .transmission=.transmission,
                          .state=.state)
      
      whichinfect <- which(infect * as.numeric(!qstate) * as.numeric(!istate) * as.numeric(return) != 0)
      
      for (w in whichinfect) {
        ninfect <- infect[w]
        
        if (ninfect > 0) {
          if (qcontact) {
            contactset <- (1:length(statelist[[t]]))
          } else {
            contactset <- (1:length(statelist[[t]]))[!qstate & !istate & return]
          }
          
          ww_group <- sample(contactset, ninfect, replace=TRUE)
          
          ## only infect susceptibles and non-quarantined individuals
          ww_group <- ww_group[statelist[[t]][ww_group] == "S" & !qstate[ww_group] & !istate[ww_group] & return[ww_group]]
          
          ## vaccination...
          ww_group <- ww_group[rbinom(length(ww_group), size=1, prob=(1-veff[ww_group]))==1]
          
          if (length(ww_group) > 0) {
            statelist[[t]][ww_group] <- "E"
            infected_by[ww_group] <- lapply(infected_by[ww_group], function(x) c(x, w))
            infected_time[ww_group] <- t
            
            infected_to[[w]] <- do.call("rbind", list(
              infected_to[[w]],
              data.frame(
                number=ww_group,
                time=t
              )
            ))
            
            incidencevec[t] <- incidencevec[t] + length(ww_group)
          }
        }
      }
      
      ## external infection
      external_infect <- rpois(1, lambda=thetavec[t])
      
      if (external_infect > 0) {
        if (qcontact) {
          contactset <- (1:length(statelist[[t]]))
        } else {
          contactset <- (1:length(statelist[[t]]))[!qstate & !istate & return]
        }
        
        ww_group <- sample(contactset, external_infect, replace=TRUE)
        
        ## only infect susceptibles and non-quarantined individuals
        ww_group <- ww_group[statelist[[t]][ww_group] == "S" & !qstate[ww_group] & !istate[ww_group] & return[ww_group]]
        
        ## vaccination...
        ww_group <- ww_group[rbinom(length(ww_group), size=1, prob=(1-veff[ww_group]))==1]
        
        if (length(ww_group) > 0) {
          statelist[[t]][ww_group] <- "E"
          infected_time[ww_group] <- t
          
          incidencevec[t] <- incidencevec[t] + length(ww_group)
        }
      }
    }
    
    ## performing internal tests
    ## internal testing before sampling to allow for 24 hour delay
    if (nrow(testqueue) > 0) {
      testwhich <- testqueue[1:min(nrow(testqueue), ntest),]
    } else {
      testwhich <- data.frame()
    }
    
    if (nrow(testwhich) > 0) {
      testres <- testfun(testwhich$state, sensitivity, specificity, .positive, .state)
      
      wpos <- testwhich$number[which(testres==1)]
      wneg <- testwhich$number[which(testres==0)]
      
      positivevec[t] <- positivevec[t] + length(wpos)
      
      ## if positive: isolate for 10 days
      ## beginning from their test date if they are asymptomatic
      ## otherwise until 3 days after the resolution of symptoms today
      istart[wpos[!istate[wpos]]] <- t
      iend[wpos[!istate[wpos] & !(testwhich$state[match(wpos, testwhich$number)] %in% c("Is1", "Is2"))]] <- t + iperiod
      itype[testwhich$state[match(wpos, testwhich$number)] %in% c("Is1", "Is2")] <- "symptomatic"
      itype[!(testwhich$state[match(wpos, testwhich$number)] %in% c("Is1", "Is2"))] <- "asymptomatic"
      istate[wpos] <- TRUE
      qstate[wpos] <- FALSE
      tpositive[wpos] <- t
      
      ## if negative while in isolation: release
      istate[wneg[istate[wneg]]] <- FALSE
      iend[wneg[istate[wneg]]] <- t
      itype[wneg[istate[wneg]]] <- "none"
      qstate[wneg[qend[wneg] > t]] <- TRUE ## go back to quarantine ...
      
      tnegative[wneg] <- t
      
      ## release contacts
      qrelease <- do.call("rbind", q_to[wneg])
      if (length(qrelease) > 0) {
        qrelease_g <- qrelease[qstate[qrelease$number],]
        
        if (nrow(qrelease_g) > 0) {
          qstate[qrelease_g$number] <- FALSE
          qend[qrelease_g$number] <- t
        }
      }
      
      ## no longer waiting for tests
      tstate[testwhich$number] <- FALSE
      
      testvec[t] <- testvec[t] + nrow(testwhich)
      
      testqueue <- testqueue[-c(1:min(nrow(testqueue), ntest)),]
    }
    
    ## sampling
    testlist_asymp <- list()
    testlist_symp <- list()
    
    if (today < dateswitch) {
      testorder_tmp <- rbind(
        testorder_vacc[testorder_vacc$mod==(t%%testfreq_vacc+1),],
        testorder_unvacc[testorder_unvacc$mod==(t%%testfreq_unvacc+1),]
      )
    } else {
      testorder_tmp <- testorder_new[testorder_new$mod==(t%%testfreq_new+1),]
    }
    
    tmp_asymp <- data.frame()
    tmp_symp <- data.frame()
    
    ww_report <- reportfun(statelist[[t]], .preport, .state)==1
    
    ## either self-reporting of non-quarantined and non-isolated individuals
    ## or reporting of quarantined individuals who just developed symptoms
    ## or if quarantined individuals already had symptoms...
    ww_vtest <- (ww_report & !tstate & !istate & return) | (sstart==t & qstate) | (statelist[[t]] %in% c("Is1", "Is2") & qstate)
    
    if (length(which(ww_vtest)) > 0) {
      tmp_symp <- data.frame(
        number=1:length(statelist[[t]]),
        state=statelist[[t]],
        time=t
      )[ww_vtest,]
    }
    
    ww_utest_g <- testorder_tmp$number
    ww_utest_g <- ww_utest_g[tpositive[ww_utest_g] + 90 <= t & return[ww_utest_g]]
    
    ## sampling when they return
    wreturn <- returnlist$number[which(returnlist$returnday==t)]
    
    if (return_testing) {
      ww_utest_g2 <- c(ww_utest_g, wreturn)
    } else {
      ww_utest_g2 <- c(ww_utest_g)
    }
    
    return[wreturn] <- TRUE
    
    ## quarantine on return?
    qww <- wreturn[rbinom(length(wreturn), 1, returnq)==1]
    
    qstate[qww] <- TRUE
    qstart[qww] <- t
    qend[qww] <- t + qperiod
    
    ww_utest_tmp <- data.frame(
      number=1:length(statelist[[t]]),
      state=statelist[[t]],
      time=t
    )[ww_utest_g2,]
    
    ww_utest_tmp2 <- ww_utest_tmp[!tstate[ww_utest_tmp$number] & !istate[ww_utest_tmp$number],]
    
    ww_utest_tmp_symp <- ww_utest_tmp2[ww_utest_tmp2$state %in% c("Is1", "Is2"),]
    ww_utest_tmp_asymp <- ww_utest_tmp2[!(ww_utest_tmp2$state %in% c("Is1", "Is2")),]
    
    tmp_symp <- rbind(tmp_symp, ww_utest_tmp_symp)
    tmp_symp <- tmp_symp[!duplicated(tmp_symp),]
    
    if (nrow(tmp_symp) > 0) {
      tmp_symp$type <- "symptomatic"
    }
    
    tmp_asymp <- ww_utest_tmp_asymp
    
    if (nrow(tmp_asymp) > 0) {
      tmp_asymp$type <- "asymptomatic"
    }
    
    testlist_symp <- tmp_symp
    testlist_asymp <- tmp_asymp
    
    tstate[c(testlist_symp$number, testlist_asymp$number)] <- TRUE
    
    ## isolation of symptomatic upon testing
    isymptom_which <- testlist_symp$number
    istate[isymptom_which] <- TRUE
    istart[isymptom_which] <- t
    itype[isymptom_which] <- "symptomatic"
    qstate[isymptom_which] <- FALSE
    
    samplevec[t] <- samplevec[t] + nrow(testlist_symp) + nrow(testlist_asymp)  
    
    testqueue_asymp_add <- testlist_asymp
    testqueue_symp_add <- testlist_symp
    
    if (nrow(testqueue_symp_add) > 0) {
      ## random order
      testqueue_symp_add <- testqueue_symp_add[sample(1:nrow(testqueue_symp_add)),]
    }
    
    if (nrow(testqueue_asymp_add) > 0) {
      testqueue_asymp_add <- testqueue_asymp_add[sample(1:nrow(testqueue_asymp_add)),]
      testqueue <- rbind(testqueue, testqueue_asymp_add)
    }
    
    testqueue_rapid <- rbind(testqueue_rapid, testqueue_symp_add)
    
    ## performing rapid tests for symptomatic individuals
    testwhich <- testqueue_rapid
    
    if (nrow(testwhich) > 0) {
      testres <- testfun(testwhich$state, sensitivity, specificity, .positive, .state)
      
      wpos <- testwhich$number[which(testres==1)]
      wneg <- testwhich$number[which(testres==0)]
      
      positivevec[t] <- positivevec[t] + length(wpos)
      
      ## if positive: isolate for 10 days
      ## beginning from their test date if they are asymptomatic
      ## otherwise until 3 days after the resolution of symptoms today
      istart[wpos[!istate[wpos]]] <- t
      iend[wpos[!istate[wpos] & !(testwhich$state[match(wpos, testwhich$number)] %in% c("Is1", "Is2"))]] <- t + iperiod
      itype[testwhich$state[match(wpos, testwhich$number)] %in% c("Is1", "Is2")] <- "symptomatic"
      itype[!(testwhich$state[match(wpos, testwhich$number)] %in% c("Is1", "Is2"))] <- "asymptomatic"
      istate[wpos] <- TRUE
      qstate[wpos] <- FALSE
      tpositive[wpos] <- t
      
      ## if negative while in isolation: release
      istate[wneg[istate[wneg]]] <- FALSE
      iend[wneg[istate[wneg]]] <- t
      itype[wneg[istate[wneg]]] <- "none"
      qstate[wneg[qend[wneg] > t]] <- TRUE ## go back to quarantine ...
      
      tnegative[wneg] <- t
      
      ## release contacts
      qrelease <- do.call("rbind", q_to[wneg])
      if (length(qrelease) > 0) {
        qrelease_g <- qrelease[qstate[qrelease$number],]
        
        if (nrow(qrelease_g) > 0) {
          qstate[qrelease_g$number] <- FALSE
          qend[qrelease_g$number] <- t
        }
      }
      
      ## no longer waiting for tests
      tstate[testwhich$number] <- FALSE
      
      testvec[t] <- testvec[t] + nrow(testwhich)
    }
    
    testqueue_rapid <- data.frame()
    
    ## tracing
    if (trace) {
      which_trace <- which(istart > 0 & istart==(t-tracedelay))
      
      if (length(which_trace) > 0) {
        q_to[which_trace] <- lapply(which_trace, function(x) {
          if (tnegative[x] > tpositive[x]) {
            return(data.frame())
          }
          
          y <- infected_to[[x]]
          
          if (itype[x] == "symptomatic") {
            y <- y[(y$time>=sstart[x]-tracememory) & (t < y$time+qperiod),]  
          } else {
            y <- y[y$time>=tpositive[x]-tracememory & t < y$time+qperiod,]
          }
          
          y
        })
        
        contactee2 <- do.call("rbind", q_to[which_trace])
        contactee2 <- contactee2[rbinom(nrow(contactee2), 1, traceeff)==1,]
        
        contactee_g <- contactee2[!qstate[contactee2$number] & !istate[contactee2$number],]
        
        ## quarantine close contacts
        qstate[contactee_g$number] <- TRUE
        qstart[contactee_g$number] <- t
        qend[contactee_g$number] <- contactee_g$time + qperiod
      }
    }
    
    ## release of isolated and quarantined
    which_qrelease <- qend==t & qstate
    
    if (sum(which_qrelease) > 0) {
      qstate[which_qrelease] <- FALSE
    }
    
    which_irelease <- iend==t & istate
    
    if (sum(which_irelease) > 0) {
      which_symptom <- statelist[[t]] %in% c("Is1", "Is2")
      
      istate[which_irelease & !which_symptom] <- FALSE
      itype[which_irelease & !which_symptom] <- "none"
      iend[which_irelease & which_symptom] <- iend[which_irelease & which_symptom] + 3
      
      qstate[which(which_irelease)[qend[which_irelease] > t]] <- TRUE
    }
    
    qduration <- qduration + qstate
    iduration <- iduration + istate
    
    Nvec[t] <- length(statelist[[t]])
    Svec[t] <- sum(statelist[[t]]=="S")
    Evec[t] <- sum(statelist[[t]]=="E")
    Ip1vec[t] <- sum(statelist[[t]]=="Ip1")
    Ip2vec[t] <- sum(statelist[[t]]=="Ip2")
    Ia1vec[t] <- sum(statelist[[t]]=="Ia1")
    Ia2vec[t] <- sum(statelist[[t]]=="Ia2")
    Is1vec[t] <- sum(statelist[[t]]=="Is1")
    Is2vec[t] <- sum(statelist[[t]]=="Is2")
    Rvec[t] <- sum(statelist[[t]]=="R")
    qvec[t] <- sum(qstate)
    ivec[t] <- sum(istate)
    tvec[t] <- sum(tstate)
    boostvec[t] <- sum(boost)
    effSvec[t] <- sum((statelist[[t]]=="S") * (1-veff))
  }
  
  list(
    incidencevec=incidencevec,
    samplevec=samplevec,
    testvec=testvec,
    positivevec=positivevec,
    qduration=qduration,
    iduration=iduration,
    Svec=Svec,
    Evec=Evec,
    Ip1vec=Ip1vec,
    Ip2vec=Ip2vec,
    Ia1vec=Ia1vec,
    Ia2vec=Ia2vec,
    Is1vec=Is1vec,
    Is2vec=Is2vec,
    Rvec=Rvec,
    qvec=qvec,
    ivec=ivec,
    tvec=tvec,
    veff=veff,
    veff_inf=veff_inf,
    datevec=datevec,
    boostvec=boostvec,
    effSvec=effSvec
  )
}

##' @param x current state
transfun <- function(x,
                     p_a,
                     .transition,
                     .state) {
  prob <- 1-exp(-.transition[match(x, .state)])
  
  trans <- rbinom(length(x), rep(1, length(x)), prob)
  
  x2 <- x
  x2[trans==1] <- .state[match(x[trans==1], .state)+1]
  
  ww <- which(x2=="tmp")
  ntmp <- length(ww)
  
  whichasymp <- rbinom(ntmp, rep(1, ntmp), p_a)
  
  x2[ww[whichasymp==1]] <- "Ia1"
  x2[ww[whichasymp==0]] <- "Is1"
  
  x2
}

##' @param x current state
infectfun <- function(x,
                      veff,
                      k,
                      .transmission,
                      .state) {
  trate <- .transmission[match(x, .state)] * (1-veff)
  
  infect <- rnbinom(length(x), size=k, mu=trate)
  
  infect
}

##' @param x current state
testfun <- function(x,
                    sensitivity=0.9,
                    specificity=0.9,
                    .positive,
                    .state) {
  trueres <- .positive[match(x, .state)]
  
  prob <- c(1-specificity, sensitivity)[as.numeric(trueres)+1]
  
  rbinom(length(prob), 1, prob)
}

reportfun <- function(x,
                      .preport,
                      .state) {
  pp <- .preport[match(x, .state)]
  
  rbinom(length(x), 1, pp)
}
