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

mercer_spring2020 <- mercer %>%
  filter(date >= "2021-01-23", date<= "2021-05-21")

nsim <- 100

simulation_spring_2020 <- vector('list')

R0vec <- c(0.25, 0.5, 1, 2)
scalevec <- c(1e-6, 2e-6, 3e-6, 4e-6, 5e-6)

paramvec <- expand.grid(R0vec, scalevec)

for (j in 1:nrow(paramvec)) {
  tmplist <- vector('list')
  
  set.seed(101)
  for (i in 1:nsim) {
    ## 3000 undergrad
    ## 2000 grad
    ## 3000 staff
    ## assuming 8000 for simplicity
    ## trick to change testing... 
    ## assume 5/8 are vaccinated with no efficacy but assume vaccinated are tested 
    ## at different rates
    print(paste0(j, ", ", i))
    ss <- simulate(param=drawpar(preport=1, R0=paramvec[j,1]),
                   N=8000,
                   I=0.01,
                   return=c(rep(FALSE, 3000), rep(FALSE, 1000), rep(TRUE, 1000), rep(TRUE, 3000)),
                   returnfrac=1/14,
                   thetavec=paramvec[j,2] * mercer_spring2020$cases * 8000,
                   datestart=as.Date("2021-01-16"),
                   datesemester=as.Date("2021-02-01"),
                   dateinternal=as.Date("2021-02-01"),
                   dateend=as.Date("2021-05-14"),
                   testfreq_unvacc = 7,
                   testfreq_vacc = 3,
                   ntest = 8000,
                   vacc=c(rep(TRUE, 5000), rep(FALSE, 3000)),
                   vprop=5/8, veff=0,
                   asymp_testing=TRUE,
                   return_testing=TRUE,
                   trace=FALSE,
                   returnq=1)
    
    ss2 <- ss$res
    
    ss2$R0 <- paramvec[j,1]
    ss2$scalevec <- paramvec[j,2]
    
    tmplist[[i]] <- ss2
  }
  
  simulation_spring_2020[[j]] <- tmplist
}

save("simulation_spring_2020", file="simulation_spring_2020.rda")
