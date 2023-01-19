library(dplyr)
library(lubridate)
source("../R/simulate_internal.R")
source("../R/simulate.R")

county <- read.csv("../data/us-counties-12-30-2021.csv")

mercer <- county %>%
  filter(county=="Mercer", state=="New Jersey") %>%
  arrange(date) %>%
  mutate(
    cases=c(0, diff(cases)),
    cases=ifelse(cases < 0, 0, cases)
  )

mercer_fall2020 <- mercer %>%
  filter(date >= "2020-08-30", date<= "2021-01-08")

mercer_fall2020$cases[128] <- (mercer_fall2020$cases[127]+mercer_fall2020$cases[129])/2

nsim <- 100

simulation_fall_2020 <- vector('list')

R0vec <- c(0.25, 0.5, 1, 2)
scalevec <- c(5e-6, 7.5e-6, 10e-6, 12.5e-6, 15e-6)

paramvec <- expand.grid(R0vec, scalevec)

for (j in 1:nrow(paramvec)) {
  tmplist <- vector('list')
  
  set.seed(101)
  for (i in 1:nsim) {
    ## 1000 grad and 
    ## 2000 staff
    ## and a small number of undergrads
    ## assuming 3000 for simplicity
    ## trick to change testing... 
    ## assume 1/3 are vaccinated with no efficacy but assume vaccinated are tested 
    ## at different rates
    print(paste0(j, ", ", i))
    ss <- simulate(param=drawpar(preport=1, R0=paramvec[j, 1]),
                   N=3000,
                   I=0,
                   return=rep(TRUE, 3000),
                   returnfrac=1,
                   thetavec=paramvec[j,2] * mercer_fall2020$cases * 3000,
                   datestart=as.Date("2020-08-23"),
                   datesemester=as.Date("2020-08-31"),
                   dateinternal=as.Date("2020-08-31"),
                   dateend=as.Date("2021-01-01"),
                   testfreq_unvacc = 7,
                   testfreq_vacc = 3,
                   ntest = 8000,
                   vprop=0.33, veff=0,
                   asymp_testing=TRUE,
                   return_testing=FALSE,
                   trace=FALSE,
                   returnq=0)
    
    ss2 <- ss$res
    
    ss2$R0 <- paramvec[j,1]
    ss2$scalevec <- paramvec[j,2]
    
    tmplist[[i]] <- ss2
  }
  
  simulation_fall_2020[[j]] <- tmplist
}

save("simulation_fall_2020", file="simulation_fall_2020.rda")
