library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(gridExtra)
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

county <- read.csv("../data/us-counties-03-22-2022.csv")

philly <- county %>%
  filter(county=="Philadelphia", state=="Pennsylvania") %>%
  arrange(date) %>%
  mutate(
    cases=c(0, diff(cases)),
    cases=ifelse(cases < 0, 0, cases)
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

spring2020 <- data %>%
  filter(`Week Ending` <= as.Date("2021-05-14"),
         `Week Ending` >= as.Date("2021-01-22")) %>%
  select(`Week Ending`, `Undergrad Positive Tests`, `Grad Student Positive Tests`,
         `Faculty Staff Positive Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Positive Tests`,
    Grad=`Grad Student Positive Tests`,
    `Faculty and Staff`=`Faculty Staff Positive Tests`
  )  %>%
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
  )  %>%
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

spring2021 <- data %>%
  filter(`Week Ending` > as.Date("2021-12-31"),
         `Week Ending` <= as.Date("2022-03-18")) %>%
  select(`Week Ending`, `Undergrad Positive Tests`, `Grad Student Positive Tests`,
         `Faculty Staff Positive Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Positive Tests`,
    Grad=`Grad Student Positive Tests`,
    `Faculty and Staff`=`Faculty Staff Positive Tests`
  )  %>%
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

philly_fall2020 <- philly %>%
  filter(date >= "2020-08-24", date<= "2021-01-01") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2020$date2)))
  ) %>%
  group_by(group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  mutate(
    date=fall2020$date[match(as.Date(date), as.Date(fall2020$date2))]
  )

philly_spring2020 <- philly %>%
  filter(date >= "2021-01-16", date<= "2021-05-14") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2020$date2)))
  ) %>%
  group_by(group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  mutate(
    date=spring2020$date[match(as.Date(date), as.Date(spring2020$date2))]
  )

philly_fall2021 <- philly %>%
  filter(date >= "2021-08-14", date <= "2021-12-31") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2021$date2)))
  ) %>%
  group_by(group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  mutate(
    date=fall2021$date[match(as.Date(date), as.Date(fall2021$date2))]
  )

philly_spring2021 <- philly %>%
  filter(date > "2021-12-31", date <= "2022-03-18") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2021$date2)))
  ) %>%
  group_by(group) %>%
  summarize(
    cases=sum(cases),
    date=max(date)
  ) %>%
  mutate(
    date=spring2021$date[match(as.Date(date), as.Date(spring2021$date2))]
  )

g1 <- ggplot(fall2020) +
  geom_bar(aes(date, value, fill=key), stat="identity") +
  geom_line(data=philly_fall2020, aes(date, cases/400, group=1), col="red") +
  geom_point(data=philly_fall2020, aes(date, cases/400), col="red") +
 scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases", expand=c(0, 0), limits=c(0, 50),
                     sec.axis = sec_axis(trans=~.*400, "New York City cases")) +
  scale_fill_viridis_d() +
  ggtitle("A. Fall 2020-2021") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = c(0.1, 0.8),
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

g2 <- ggplot(spring2020) +
  geom_bar(aes(as.factor(date), value, fill=key), stat="identity") +
  geom_line(data=philly_spring2020, aes(date, cases/400, group=1), col="red") +
  geom_point(data=philly_spring2020, aes(date, cases/400), col="red") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases", expand=c(0, 0), limits=c(0, 60),
                     sec.axis = sec_axis(trans=~.*400, "New York City cases")) +
  scale_fill_viridis_d() +
  ggtitle("B. Spring 2020-2021") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

g3 <- ggplot(fall2021) +
  geom_bar(aes(as.factor(date), value, fill=key), stat="identity") +
  geom_line(data=philly_fall2021, aes(date, cases/400, group=1), col="red") +
  geom_point(data=philly_fall2021, aes(date, cases/400), col="red") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases", expand=c(0, 0), limits=c(0, 500),
                     sec.axis = sec_axis(trans=~.*400, "New York City cases")) +
  scale_fill_viridis_d() +
  ggtitle("C. Fall 2021-2022") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

g4 <- ggplot(spring2021) +
  geom_bar(aes(as.factor(date), value, fill=key), stat="identity") +
  geom_line(data=philly_spring2021, aes(date, cases/400, group=1), col="red") +
  geom_point(data=philly_spring2021, aes(date, cases/400), col="red") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases", expand=c(0, 0), limits=c(0, 500),
                     sec.axis = sec_axis(trans=~.*400, "New York City cases")) +
  scale_fill_viridis_d() +
  ggtitle("D. Spring 2021-2022") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

fall2020a <- fall2020 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(philly_fall2020)

spring2020a <- spring2020 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(philly_spring2020)

fall2021a <- fall2021 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(philly_fall2021)

spring2021a <- spring2021 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(philly_spring2021)

cor.test(log(fall2020a$value+1), log(fall2020a$cases+1))

g5 <- ggplot(fall2020a) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

cor.test(log(spring2020a$value+1), log(spring2020a$cases+1))

g6 <- ggplot(spring2020a) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

cor.test(log(fall2021a$value+1), log(fall2021a$cases+1))

g7 <- ggplot(fall2021a) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

cor.test(log(spring2021a$value+1), log(spring2021a$cases+1))

g8 <- ggplot(spring2021a) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

gcomb <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, nrow=4, byrow=FALSE,
                    widths=c(2, 1), draw=FALSE)

ggsave("figure_princeton_philly_new.pdf", gcomb, width=12*1.2, height=10*1.2)

fall2020b <- fall2020 %>%
  merge(philly_fall2020)

spring2020b <- spring2020 %>%
  merge(philly_spring2020)

fall2021b <- fall2021 %>%
  merge(philly_fall2021)

spring2021b <- spring2021 %>%
  merge(philly_spring2021)

g9 <- ggplot(fall2020b) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("A. Fall 2020-2021") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

g10 <- ggplot(spring2020b) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("B. Spring 2020-2021") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

g11 <- ggplot(fall2021b) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("C. Fall 2021-2022") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

g12 <- ggplot(spring2021b) +
  geom_point(aes(cases+1, value+1)) +
  geom_smooth(aes(cases+1, value+1), method="lm", col="black") +
  scale_x_log10("New York City cases") +
  scale_y_log10("Princeton cases") +
  ggtitle("D. Spring 2021-2022") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

gfinal3 <- ggarrange(g9, g10, g11, g12, nrow=4, draw=FALSE)

ggsave("figure_princeton_correlation_philly_new2.pdf", gfinal3, width=12, height=10)
