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

mercer <- county %>%
  filter(county=="Mercer", state=="New Jersey") %>%
  arrange(date) %>%
  mutate(
    cases=c(0, diff(cases)),
    cases=ifelse(cases < 0, 0, cases)
  )

fall2020 <- data %>%
  filter(`Week Ending` <= as.Date("2021-01-01")) %>%
  select(`Week Ending`, `Undergrad Tests`, `Grad Student Tests`,
         `Faculty Staff Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Tests`,
    Grad=`Grad Student Tests`,
    `Faculty and Staff`=`Faculty Staff Tests`
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
  select(`Week Ending`, `Undergrad Tests`, `Grad Student Tests`,
         `Faculty Staff Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Tests`,
    Grad=`Grad Student Tests`,
    `Faculty and Staff`=`Faculty Staff Tests`
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
  select(`Week Ending`, `Undergrad Tests`, `Grad Student Tests`,
         `Faculty Staff Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Tests`,
    Grad=`Grad Student Tests`,
    `Faculty and Staff`=`Faculty Staff Tests`
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
    value=sum(value, na.rm=TRUE)
  )

spring2021 <- data %>%
  filter(`Week Ending` > as.Date("2021-12-31"),
         `Week Ending` <= as.Date("2022-03-18")) %>%
  select(`Week Ending`, `Undergrad Tests`, `Grad Student Tests`,
         `Faculty Staff Tests`) %>%
  rename(
    date=`Week Ending`,
    Undergrad=`Undergrad Tests`,
    Grad=`Grad Student Tests`,
    `Faculty and Staff`=`Faculty Staff Tests`
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
    value=sum(value, na.rm=TRUE)
  )

g1 <- ggplot(fall2020) +
  geom_bar(aes(date, value, fill=key), stat="identity", position = position_dodge2(preserve = "single")) +
  scale_x_discrete("Date") +
  scale_y_continuous("Total number of tests", expand=c(0, 0), limits=c(0, 4000)) +
  scale_fill_viridis_d() +
  ggtitle("A. Fall 2020-2021") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = c(0.35, 0.8),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

g2 <- ggplot(spring2020) +
  geom_bar(aes(date, value, fill=key), stat="identity", position = position_dodge2(preserve = "single")) +
  scale_x_discrete("Date") +
  scale_y_continuous("Total number of tests", expand=c(0, 0), limits=c(0, 8000)) +
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
  geom_bar(aes(date, value, fill=key), stat="identity", position = position_dodge2(preserve = "single")) +
  scale_x_discrete("Date") +
  scale_y_continuous("Total number of tests", expand=c(0, 0), limits=c(0, 12000)) +
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
  geom_bar(aes(date, value, fill=key), stat="identity", position = position_dodge2(preserve = "single")) +
  scale_x_discrete("Date") +
  scale_y_continuous("Total number of tests", expand=c(0, 0), limits=c(0, 10000)) +
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

gfinal <- ggarrange(g1, g2, g3, g4, nrow=4, draw=FALSE)

ggsave("figure_princeton_testing.pdf", gfinal, width=8, height=10)
