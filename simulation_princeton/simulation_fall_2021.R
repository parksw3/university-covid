library(dplyr)
library(lubridate)
source("../R/simulate_internal_omicron.R")
source("../R/simulate_omicron.R")

county <- read.csv("../data/us-counties-01-18-2022.csv")

mercer <- county %>%
  filter(county=="Mercer", state=="New Jersey") %>%
  arrange(date) %>%
  mutate(
    cases=c(0, diff(cases)),
    cases=ifelse(cases < 0, 0, cases)
  )

mercer_fall2021 <- mercer %>%
  filter(date >= "2021-08-21", date<= "2022-01-14")

nsim <- 100

simulation_fall_2021 <- vector('list')

R0vec <- c(0.5, 1, 2, 4, 8)
scalevec <- c(0.005, 0.01, 0.015, 0.02, 0.025) * 10

paramvec <- expand.grid(R0vec, scalevec)

for (j in 1:nrow(paramvec)) {
  tmplist <- vector('list')
  
  set.seed(101)
  for (i in 1:nsim) {
    ## 5000 undergrad
    ## 2000 grad
    ## 6000 staff and faculty
    ## turn off omicron parameters
    print(paste0(j, ", ", i))
    ss <- simulate_omicron(param=drawpar(preport=1, R0=paramvec[j, 1]),
                   N=13000,
                   I=0.005,
                   vprop=0.98,
                   boostprop=0,
                   veff_twodose=0.9,
                   veff_threedose=0,
                   veff_twodose_inf=0.2,
                   veff_threedose_inf=0,
                   vaccine_delay=14,
                   boostdaily=0,
                   return=c(rep(FALSE, 5000), rep(TRUE, 8000)),
                   returnfrac=1/16,
                   thetavec=paramvec[j,2] * mercer_fall2021$cases,
                   datestart=as.Date("2021-08-14"),
                   datesemester=as.Date("2021-09-01"),
                   dateinternal=as.Date("2021-09-01"),
                   dateend=as.Date("2022-01-07"),
                   testfreq_unvacc = 3,
                   testfreq_vacc = 7,
                   ntest = 8000,
                   asymp_testing=TRUE,
                   return_testing=TRUE,
                   trace=FALSE,
                   returnq=0)
    
    ss2 <- ss$res
    
    ss2$R0 <- paramvec[j,1]
    ss2$scalevec <- paramvec[j,2]
    
    tmplist[[i]] <- ss2
  }
  
  simulation_fall_2021[[j]] <- tmplist
}

save("simulation_fall_2021", file="simulation_fall_2021.rda")
