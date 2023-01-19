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

fall2020 <- data %>%
  filter(`Week Ending` <= as.Date("2021-01-01")) %>%
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

spring2020 <- data %>%
  filter(`Week Ending` <= as.Date("2021-05-14"),
         `Week Ending` >= as.Date("2021-01-22"))  %>%
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

load("../simulation_princeton/simulation_fall_2020.rda")
load("../simulation_princeton/simulation_spring_2020.rda")
load("../simulation_princeton/simulation_fall_2021.rda")

simall_fall_2020 <- lapply(simulation_fall_2020, function(x){
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

simall_fall_2020_gof <- simall_fall_2020 %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2020$date)))
  ) %>%
  group_by(R0, scale, sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  arrange(R0, scale, sim, date) %>%
  merge(fall2020) %>%
  group_by(R0, scale, sim) %>%
  summarize(
    gof=sum((log(weekly+1)-log(cases+1))^2)
  )

simall_fall_2020_gof2 <- simall_fall_2020_gof %>%
  group_by(R0, scale) %>%
  summarize(
    gof=median(gof)
  )

g1 <- ggplot(simall_fall_2020_gof2) +
  geom_tile(aes(R0, scale, fill=log(gof))) +
  geom_point(aes(x=0.25, y=0.0000075), col="white", size=5, shape=1, stroke=2) +
  scale_x_log10(expression(R[contact]), expand=c(0, 0), breaks=c(0.25, 0.5, 1, 2)) +
  scale_y_continuous(expression(theta), expand=c(0, 0),
                     breaks=c(5e-6, 7.5e-6, 10e-6, 12.5e-6, 15e-6)) +
  scale_fill_viridis_c("log SSQ ") +
  ggtitle("A. Fall 2020-2021")

simall_fall_2020_gof2 %>%
  ungroup %>%
  filter(gof==min(gof))

simall_fall_2020_best <- simall_fall_2020 %>%
  filter(R0==0.25, scale==0.0000075) %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2020$date)))
  ) %>%
  group_by(sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  group_by(date) %>%
  summarize(
    median=median(cases),
    lwr=quantile(cases, 0.05),
    upr=quantile(cases, 0.95)
  ) %>%
  merge(fall2020) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date)
  )

simall_fall_2020_gof %>%
  filter(R0==0.25, scale==0.0000075) %>%
  filter(gof==min(gof)) ## sim 90

simall_fall_2020_best2 <- simall_fall_2020 %>%
  filter(R0==0.25, scale==0.0000075, sim==90)  %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2020$date)))
  ) %>%
  group_by(sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  merge(fall2020) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date)
  )

g2 <- ggplot(simall_fall_2020_best) +
  geom_ribbon(aes(date, ymin=lwr+1, ymax=upr+1, group=1), alpha=0.2, fill="red") +
  geom_line(aes(date, median+1, group=1), col="red", lwd=1) +
  geom_line(data=simall_fall_2020_best2, aes(date, cases+1, group=1), col="black", lwd=1, lty=2) +
  geom_point(aes(date, weekly+1)) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases", expand=c(0, 0))+
  scale_fill_viridis_d("Basic\nreproduction\nnumber") +
  scale_color_viridis_d("Basic\nreproduction\nnumber") +
  scale_shape_discrete("Basic\nreproduction\nnumber") +
  ggtitle("B") +
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

cor.test(log(simall_fall_2020_best2$cases+1), log(simall_fall_2020_best$weekly+1))

g3 <- ggplot(simall_fall_2020_best) +
  geom_point(aes(median+1, weekly+1)) +
  geom_smooth(aes(median+1, weekly+1), method="lm", col="red", fill="red", alpha=0.2) +
  geom_abline(aes(intercept=0, slope=1), lty=2, col=1, lwd=1) +
  scale_x_log10("Predicted")  +
  scale_y_log10("Observed") +
  ggtitle("C") +
  theme(
    panel.grid = element_blank()
  )

simall_spring_2020 <- lapply(simulation_spring_2020, function(x) {
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

simall_spring_2020_gof <- simall_spring_2020 %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2020$date)))
  ) %>%
  group_by(R0, scale, sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  arrange(R0, scale, sim, date) %>%
  merge(spring2020) %>%
  group_by(R0, scale, sim) %>%
  summarize(
    gof=sum((log(weekly+1)-log(cases+1))^2)
  )

simall_spring_2020_gof2 <- simall_spring_2020_gof %>%
  group_by(R0, scale) %>%
  summarize(
    gof=median(gof)
  )

g4 <- ggplot(simall_spring_2020_gof2) +
  geom_tile(aes(R0, scale, fill=log(gof))) +
  geom_point(aes(x=0.25, y=0.000003), col="white", size=5, shape=1, stroke=2) +
  scale_x_log10(expression(R[contact]), expand=c(0, 0),
                     breaks=c(0.25, 0.5, 1, 2)) +
  scale_y_continuous(expression(theta), expand=c(0, 0)) +
  scale_fill_viridis_c("log SSQ ") +
  ggtitle("D. Spring 2020-2021")

simall_spring_2020_gof2 %>%
  ungroup %>%
  filter(gof==min(gof))

simall_spring_2020_best <- simall_spring_2020 %>%
  filter(R0==0.25, scale==0.000003) %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2020$date)))
  ) %>%
  group_by(sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  group_by(date) %>%
  summarize(
    median=median(cases),
    lwr=quantile(cases, 0.05),
    upr=quantile(cases, 0.95)
  ) %>%
  merge(spring2020) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date)
  )

simall_spring_2020_gof %>%
  filter(R0==0.25, scale==0.000003) %>%
  filter(gof==min(gof)) ## sim 20

simall_spring_2020_best2 <- simall_spring_2020 %>%
  filter(R0==0.25, scale==0.000003, sim==20)  %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2020$date)))
  ) %>%
  group_by(sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  merge(spring2020) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date)
  )

g5 <- ggplot(simall_spring_2020_best) +
  geom_ribbon(aes(date, ymin=(lwr+1), ymax=(upr+1), group=1), alpha=0.2, fill="red") +
  geom_line(aes(date, (median+1), group=1), col="red", lwd=1) +
  geom_line(data=simall_spring_2020_best2, aes(date, cases+1, group=1), col="black", lwd=1, lty=2) +
  geom_point(aes(date, weekly)) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases", expand=c(0, 0))+
  scale_fill_viridis_d("Basic\nreproduction\nnumber") +
  scale_color_viridis_d("Basic\nreproduction\nnumber") +
  scale_shape_discrete("Basic\nreproduction\nnumber") +
  ggtitle("E") +
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

cor.test(log(simall_spring_2020_best$median+1), log(simall_spring_2020_best$weekly+1))

g6 <- ggplot(simall_spring_2020_best) +
  geom_point(aes(median+1, weekly+1)) +
  geom_smooth(aes(median+1, weekly+1), method="lm", col="red", fill="red", alpha=0.2) +
  geom_abline(aes(intercept=0, slope=1), lty=2, col=1, lwd=1) +
  scale_x_log10("Predicted")  +
  scale_y_log10("Observed") +
  ggtitle("F") +
  theme(
    panel.grid = element_blank()
  )

simall_fall_2021 <- lapply(simulation_fall_2021, function(x){
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
  bind_rows() %>%
  filter(
    date<=as.Date("2021-12-31")
  )

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

g7 <- ggplot(simall_fall_2021_gof2) +
  geom_tile(aes(R0, scale, fill=log(gof))) +
  geom_point(aes(x=0.25, y=0.00001), col="white", size=5, shape=1, stroke=2) +
  scale_x_log10(expression(R[contact]), expand=c(0, 0), 
                breaks=c(0.25, 0.5, 1, 2, 4, 8, 16)) +
  scale_y_continuous(expression(theta), expand=c(0, 0),
                     breaks=c(5, 7.5, 10, 12.5, 15)*1e-6) +
  scale_fill_viridis_c("log SSQ ") +
  ggtitle("G. Fall 2021-2022")

simall_fall_2021_gof2 %>%
  ungroup %>%
  filter(gof==min(gof))

simall_fall_2021_best <- simall_fall_2021 %>%
  filter(R0 %in% c(0.25, 0.5, 1, 2, 4, 8), scale==0.00001) %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2021$date)))
  ) %>%
  group_by(R0, sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  group_by(R0, date) %>%
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
    R0=factor(R0)
  )

simall_fall_2021_gof %>%
  filter(R0==0.25, scale==0.00001) %>%
  filter(gof==min(gof)) ## sim 70

simall_fall_2021_best2 <- simall_fall_2021 %>%
  filter(R0==0.25, scale==0.00001, sim==70)  %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2021$date)))
  ) %>%
  group_by(sim, group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  merge(fall2021) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date)
  )

g8 <- ggplot(simall_fall_2021_best) +
  geom_ribbon(aes(date, ymin=lwr+1, ymax=upr+1, fill=R0, group=R0), alpha=0.2) +
  geom_line(aes(date, median+1, col=R0, group=R0), lwd=1) +
  geom_line(data=simall_fall_2021_best2, aes(date, cases+1, group=1), col="black", lwd=1, lty=2) +
  geom_point(aes(date, weekly+1)) +
  geom_vline(xintercept="Nov 26, 2021", lty=2) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases", limit=c(1, NA), expand=c(0, 0))+
  scale_fill_viridis_d(expression(R[contact]), option="A", end=0.9) +
  scale_color_viridis_d(expression(R[contact]), option="A", end=0.9) +
  ggtitle("H") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), col=guide_legend(nrow=2, byrow=TRUE)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red"),
    legend.position=c(0.25, 0.85),
    legend.direction = "horizontal",
    legend.background = element_rect(fill=NA)
  )

g9 <- ggplot(simall_fall_2021_best) +
  geom_point(aes(median+1, weekly+1, col=R0, shape=R0)) +
  geom_smooth(aes(median+1, weekly+1, col=R0, fill=R0), method="lm", alpha=0.2) +
  geom_abline(aes(intercept=0, slope=1), lty=2, col=1, lwd=1) +
  scale_fill_viridis_d("Basic\nreproduction\nnumber", option="A", end=0.9) +
  scale_color_viridis_d("Basic\nreproduction\nnumber", option="A", end=0.9) +
  scale_x_log10("Predicted")  +
  scale_y_log10("Observed") +
  ggtitle("I") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gfinal <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, nrow=3, widths=c(1, 2, 1), draw=FALSE)

ggsave("figure_princeton_simulation.pdf", gfinal, width=12, height=8)

simall_fall_2020_all <- simall_fall_2020 %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2020$date)))
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
  merge(fall2020) %>%
  group_by(R0, scale) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date),
    scale=paste0("theta==", scale),
    R0=paste0("R[contact]==",R0)
  )

g10 <- ggplot(simall_fall_2020_all) +
  geom_ribbon(aes(date, ymin=lwr+1, ymax=upr+1, group=1), alpha=0.2, fill="red") +
  geom_line(aes(date, median+1, group=1), col="red", lwd=1) +
  geom_point(aes(date, weekly+1)) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases", expand=c(0, 0))+
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

ggsave("figure_princeton_simulation_fall_2020_all.pdf", g10, width=16, height=6)

simall_spring_2020_all <- simall_spring_2020 %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2020$date)))
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
  merge(spring2020) %>%
  group_by(R0, scale) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date),
    scale=paste0("theta==", scale),
    R0=paste0("R[contact]==",R0)
  )

g11 <- ggplot(simall_spring_2020_all) +
  geom_ribbon(aes(date, ymin=lwr+1, ymax=upr+1, group=1), alpha=0.2, fill="red") +
  geom_line(aes(date, median+1, group=1), col="red", lwd=1) +
  geom_point(aes(date, weekly+1)) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases", expand=c(0, 0))+
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

ggsave("figure_princeton_simulation_spring_2020_all.pdf", g11, width=16, height=6)

simall_fall_2021_all <- simall_fall_2021 %>%
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
  group_by(R0, scale) %>%
  mutate(
    year=substr(date, 1, 4),
    month=as.numeric(gsub("-.*", "", gsub("20..-", "", date))), 
    day=as.numeric(gsub(".*-", "", gsub("20..-", "", date)))
  ) %>%
  mutate(
    date=paste0(month.abb[month], " ", day, ", ", year),
    date=factor(date, levels=date),
    scale=paste0("theta==", scale),
    R0=paste0("R[contact]==",R0)
  ) %>%
  ungroup %>%
  mutate(
    R0=factor(R0, levels=paste0("R[contact]==", c(0.5, 1, 2, 4, 8, 16)))
  )

g12 <- ggplot(simall_fall_2021_all) +
  geom_ribbon(aes(date, ymin=lwr+1, ymax=upr+1, group=1), alpha=0.2, fill="red") +
  geom_line(aes(date, median+1, group=1), col="red", lwd=1) +
  geom_point(aes(date, weekly)) +
  scale_x_discrete("Date") +
  scale_y_log10("Number of positive cases", expand=c(0, 0))+
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

ggsave("figure_princeton_simulation_fall_2021_all.pdf", g12, width=16, height=12)
