library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(lubridate)

data_asymp <- read_xlsx("../data/princeton-weekly-positive-03-22-2022.xlsx", sheet=1) %>%
  mutate(
    type="asymp"
  )

data_symp <- read_xlsx("../data/princeton-weekly-positive-03-22-2022.xlsx", sheet=2) %>%
  mutate(
    type="symp"
  )

data <- bind_rows(
  data_asymp, data_symp
)

spring2021 <- data %>%
  filter(`Week Ending` > as.Date("2021-12-31"),
         `Week Ending` <= as.Date("2022-03-18")) %>%
  select(`Week Ending`, `Undergrad Positive Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Positive Tests`
  ) %>%
  mutate(
    date=as.Date(date)
  )  %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  arrange(date) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=unique(date))
  ) %>%
  gather(key, value, -date, -year, -month, -day) %>%
  mutate(
    date2=as.Date(paste0(year, "-", month, "-", day))
  ) %>%
  group_by(date, year, month, day, key, date2) %>%
  summarize(
    value=sum(value)
  )

load("../simulation_omicron_spring/simulation_omicron_spring_null.rda")

simall_spring_2021_null <- lapply(simulation_omicron_spring_null, function(x){
  lapply(x, function(y) {
    data.frame(
      date=y$datevec,
      time=1:length(y$incidencevec),
      incidence=y$incidencevec,
      cases=y$positivevec,
      R0=y$R0,
      effSvec=y$effSvec/5000
    )
  }) %>%
    bind_rows(.id="sim")
})  %>%
  bind_rows()

simall_spring_2021_null2 <- simall_spring_2021_null %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2021$date2)))
  ) %>%
  group_by(sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) 

simall_spring_2021_null_Reff <- simall_spring_2021_null2 %>%
  merge(
    rename(select(ungroup(spring2021), date2, value), date=date2)
  ) %>%
  mutate(
    campuscases=value-cases,
    campuscases=ifelse(campuscases < 0, 0, campuscases)
  ) %>%
  group_by(
    sim
  ) %>%
  arrange(
    sim, date
  ) %>%
  mutate(
    Reff=c(tail(campuscases, -1)/head(cases, -1), NA)
  )

ggplot(simall_spring_2021_null2) +
  geom_line(aes(date, cases, group=sim), alpha=0.1, col="red") +
  geom_point(data=spring2021, aes(date2, value))
