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

fall2021 <- data %>%
  filter(`Week Ending` >= as.Date("2021-08-20"),
         `Week Ending` <= as.Date("2021-12-31")) %>%
  select(`Week Ending`, `Undergrad Positive Tests`, `Grad Student Positive Tests`,
         `Faculty Staff Positive Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Positive Tests`,
    Grad=`Grad Student Positive Tests`,
    `Faculty and Staff`=`Faculty Staff Positive Tests`
  ) %>%
  mutate(
    date=as.Date(date),
    weekly=Undergrad+Grad+`Faculty and Staff`
  ) %>%
  group_by(date) %>%
  summarize(
    weekly=sum(weekly)
  )

load("../simulation_princeton/simulation_fall_2021_nowaning.rda")

simall_fall_2021 <- lapply(simulation_fall_2021_nowaning, function(x){
  lapply(x, function(y) {
    data.frame(
      date=y$datevec,
      time=1:length(y$incidencevec),
      incidence=y$incidencevec,
      cases=y$positivevec,
      R0=y$R0,
      scale=y$scalevec
    )
  }) %>%
    bind_rows(.id="sim")
})  %>%
  bind_rows()

simall_fall_2021_gof <- simall_fall_2021 %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2021$date)))
  ) %>%
  group_by(R0, scale, sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  arrange(R0, scale, sim, date) %>%
  merge(fall2021) %>%
  filter(date <= "2021-11-26") %>%
  group_by(R0, scale, sim) %>%
  summarize(
    gof=sum((log(weekly+1)-log(cases+1))^2)
  )

simall_fall_2021_gof2 <- simall_fall_2021_gof %>%
  group_by(R0, scale) %>%
  summarize(
    gof_lwr=quantile(gof, 0.025),
    gof_upr=quantile(gof, 0.975),
    gof=median(gof)
  )

simall_fall_2021_best <- simall_fall_2021 %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2021$date)))
  ) %>%
  group_by(R0, scale, sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  group_by(R0, scale, date) %>%
  summarize(
    median=median(cases),
    lwr=quantile(cases, 0.05),
    upr=quantile(cases, 0.95)
  ) %>%
  merge(fall2021) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=unique(date)),
    scale=paste0("theta==", scale),
    scale=factor(scale, levels=c("theta==5e-06",
                                 "theta==7.5e-06",
                                 "theta==1e-05",
                                 "theta==1.25e-05",
                                 "theta==1.5e-05")),
    R0=paste0("R[contact]==",R0)
  )

g1 <- ggplot(simall_fall_2021_best) +
  geom_ribbon(aes(date, ymin=lwr+1, ymax=upr+1, group=1), alpha=0.2, fill="red") +
  geom_line(aes(date, median+1, group=1), col="red", lwd=1) +
  geom_point(aes(date, weekly)) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases")+
  scale_fill_viridis_d("Basic\nreproduction\nnumber") +
  scale_color_viridis_d("Basic\nreproduction\nnumber") +
  scale_shape_discrete("Basic\nreproduction\nnumber") +
  facet_grid(R0~scale, labeller = label_parsed) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = "right",
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

ggsave("figure_princeton_simulation_nowaning.pdf", g1, width=16, height=12)
