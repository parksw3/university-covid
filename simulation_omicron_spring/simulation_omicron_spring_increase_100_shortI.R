library(dplyr)
source("../R/simulate_internal_omicron_switch2.R")
source("../R/simulate_omicron_switch2.R")

county <- read.csv("../data/us-counties-04-01-2022.csv")

mercer <- county %>%
  filter(county=="Mercer", state=="New Jersey") %>%
  arrange(date) %>%
  mutate(
    cases=c(0, diff(cases)),
    cases=ifelse(cases < 0, 0, cases)
  )

mercer_spring2021 <- mercer %>%
  filter(date >= as.Date("2022-01-08"), date <= as.Date("2022-03-25"))

nsim <- 100

simulation_omicron_spring_increase_100_shortI <- vector('list')

theta <- 1e-5 * mercer_spring2021$cases * 5000

R0vec <- c(2, 4, 6)

paramvec <- expand.grid(R0vec)

for (j in 1:nrow(paramvec)) {
  tmplist <- vector('list')
  
  set.seed(101)
  for (i in 1:nsim) {
    ## 5000 undergrad
    print(paste0(j, ", ", i))
    ss <- simulate_omicron_switch2(param=drawpar(preport=1, R0=paramvec[j, 1], D_e=1.5, D_p=2.5, D_s=2.5),
                           N=5000,
                           I=0.14,
                           R=100/5000,
                           return=c(rep(TRUE, 700), rep(FALSE, 4300)),
                           returnfrac=1/28,
                           datestart=as.Date("2022-01-01"),
                           datesemester=as.Date("2022-01-01"),
                           dateinternal=as.Date("2022-01-01"),
                           dateswitch=as.Date("2022-02-08"),
                           dateend=as.Date("2022-03-18"),
                           thetavec=theta,
                           testfreq_unvacc = 3,
                           testfreq_vacc = 3,
                           testfreq_new=7,
                           Rnewratio=2,
                           vprop=0.99,
                           boostprop=0.6,
                           boostdaily=70,
                           veff_twodose=0.1,
                           veff_threedose=0.7,
                           veff_twodose_inf = 0.1 * 0.2/0.9,
                           veff_threedose_inf = 0.7 * 0.2/0.9,
                           vaccine_delay=7,
                           iperiod=5,
                           ntest = 8000,
                           asymp_testing=TRUE,
                           return_testing=FALSE,
                           trace=FALSE,
                           returnq=0)
    
    ss2 <- ss$res
    
    ss2$R0 <- paramvec[j,1]
    
    tmplist[[i]] <- ss2
  }
  
  simulation_omicron_spring_increase_100_shortI[[j]] <- tmplist
}

save("simulation_omicron_spring_increase_100_shortI", file="simulation_omicron_spring_increase_100_shortI.rda")
