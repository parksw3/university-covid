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

load("../simulation_omicron_spring/simulation_omicron_spring_increase_50_shortI.rda")
load("../simulation_omicron_spring/simulation_omicron_spring_increase_100_shortI.rda")

simall_spring_2021_increase_50 <- lapply(simulation_omicron_spring_increase_50_shortI, function(x){
  lapply(x, function(y) {
    data.frame(
      date=y$datevec,
      time=1:length(y$incidencevec),
      incidence=y$incidencevec,
      cases=y$positivevec,
      R0=y$R0
    )
  }) %>%
    bind_rows(.id="sim")
})  %>%
  bind_rows() 

simall_spring_2021_increase_100 <- lapply(simulation_omicron_spring_increase_100_shortI, function(x){
  lapply(x, function(y) {
    data.frame(
      date=y$datevec,
      time=1:length(y$incidencevec),
      incidence=y$incidencevec,
      cases=y$positivevec,
      R0=y$R0
    )
  }) %>%
    bind_rows(.id="sim")
})  %>%
  bind_rows() 

simall_spring_all <- bind_rows(
  simall_spring_2021_increase_50 %>% mutate(type="Weekly~testing~\n~50~'%'~increase~'in'~reproduction~number"),
  simall_spring_2021_increase_100 %>% mutate(type="Weekly~testing~\n~100~'%'~increase~'in'~reproduction~number")
) %>%
  filter(R0 != 8) %>%
  mutate(
    type=factor(type,
                levels=c(
                  "No~switching",
                  "Weekly~testing",
                  "Weekly~testing~\n~20~'%'~increase~'in'~reproduction~number",
                  "Weekly~testing~\n~50~'%'~increase~'in'~reproduction~number",
                  "Weekly~testing~\n~100~'%'~increase~'in'~reproduction~number"
                ),
                labels=c(
                  "No~switching",
                  "Weekly~testing",
                  "atop('Weekly testing','20% increase in reproduction number')",
                  "atop('Weekly testing','50% increase in reproduction number')",
                  "atop('Weekly testing','100% increase in reproduction number')"
                )),
    R0=factor(R0, 
              levels=c(2, 4, 6),
              labels=paste0("R[contact]==", c(2, 4, 6)))
  )

simall_spring_all_summ <- simall_spring_all %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2021$date2)))
  ) %>%
  group_by(R0, type, sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) 

simall_spring_all_summ2 <- simall_spring_all_summ %>%
  group_by(R0, type, date) %>%
  summarize(
    median=median(cases),
    lwr=quantile(cases, 0.05),
    upr=quantile(cases, 0.95)
  )

g1 <- ggplot(simall_spring_all_summ2) +
  geom_vline(xintercept=as.Date("2022-02-11"), lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.2, fill="red") +
  geom_line(aes(date, median), col="red") +
  geom_point(data=spring2021, aes(date2, value)) +
  scale_x_date(breaks=unique(simall_spring_all_summ2$date),
               labels=c("Jan 7, 2022", "Jan 14, 2022", "Jan 21, 2022", "Jan 28, 2022", "Feb 4, 2022",
                        "Feb 11, 2022", "Feb 18, 2022", "Feb 25, 2022", "March 4, 2022", "March 11, 2022", "March 18, 2022")) +
  scale_y_log10("Number of positive cases") +
  facet_grid(R0~type, labeller = label_parsed) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1)
  )

ggsave("figure_princeton_simulation_omicron_spring_shortI.pdf", g1, width=12, height=8)
