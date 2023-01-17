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

mercer_fall2020 <- mercer %>%
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

mercer_spring2020 <- mercer %>%
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

mercer_fall2021 <- mercer %>%
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

mercer_spring2021 <- mercer %>%
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
  geom_bar(aes(date, value/3000*1000, fill=key), stat="identity") +
  geom_line(data=mercer_fall2020, aes(date, cases/387340*1000, group=1), col="red") +
  geom_point(data=mercer_fall2020, aes(date, cases/387340*1000), col="red") +
  geom_segment(x=2, y=1/3, xend=2, yend=9/3, lty=3) +
  annotate("text", x=2, y=10/3, label="Classes\nbegin", vjust=0, family="Times") +
  geom_segment(x=8, y=8/3, xend=8, yend=12/3, lty=3) +
  annotate("text", x=8, y=13/3, label="Fall\nrecess", vjust=0, family="Times") +
  geom_segment(x=14, y=9/3, xend=14, yend=35/3, lty=3) +
  annotate("text", x=14, y=36/3, label="Classes\nend\n(Thanksgiving)", vjust=0, family="Times") +
  geom_segment(x=17, y=23/3, xend=17, yend=35/3, lty=3) +
  annotate("text", x=17, y=36/3, label="Exams\nend", vjust=0, family="Times") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases per 1000", expand=c(0, 0), limits=c(0, 20),
                     sec.axis = sec_axis(trans=~., "Mercer county cases per 1000")) +
  scale_fill_viridis_d() +
  ggtitle("A. Fall 2020-2021") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = c(0.15, 0.8),
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red"),
    axis.title.y.right = element_text(color="red")
  )

g2 <- ggplot(spring2020) +
  geom_bar(aes(as.factor(date), value/8000*1000, fill=key), stat="identity") +
  geom_line(data=mercer_spring2020, aes(date, cases/387340*1000, group=1), col="red") +
  geom_point(data=mercer_spring2020, aes(date, cases/387340*1000), col="red") +
  geom_segment(x=3, y=9/8, xend=3, yend=45/8, lty=3) +
  annotate("text", x=3, y=46/8, label="Classes\nbegin", vjust=0, family="Times") +
  geom_segment(x=9, y=9/8, xend=9, yend=20/8, lty=3) +
  annotate("text", x=9, y=21/8, label="Spring\nrecess", vjust=0, family="Times") +
  geom_segment(x=15, y=6/8, xend=15, yend=15/8, lty=3) +
  annotate("text", x=15, y=16/8, label="Classes\nend", vjust=0, family="Times") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases per 1000", expand=c(0, 0), limits=c(0, 10),
                     sec.axis = sec_axis(trans=~., "Mercer county cases per 1000")) +
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

g3_base <- ggplot(fall2021) +
  geom_segment(x=3, y=1/13, xend=3, yend=55/13, lty=3) +
  annotate("text", x=3, y=66/13, label="Classes\nbegin", vjust=0, family="Times") +
  geom_segment(x=10, y=1/13, xend=10, yend=35/13, lty=3) +
  annotate("text", x=10, y=46/13, label="Fall\nrecess", vjust=0, family="Times") +
  geom_segment(x=15, y=59/13, xend=15, yend=120/13, lty=3) +
  annotate("text", x=15, y=131/13, label="Thanksgiving", vjust=0, family="Times") +
  geom_segment(x=17, y=43/13, xend=17, yend=130/13, lty=3) +
  annotate("text", x=17, y=141/13, label="Classes\nend", vjust=0, family="Times") +
  geom_segment(x=19, y=43/13, xend=19, yend=155/13, lty=3) +
  annotate("text", x=19.4, y=166/13, label="Exams\nend", vjust=0, hjust=1, family="Times") +
  geom_bar(aes(as.factor(date), value/13000*1000, fill=key), stat="identity") +
  geom_line(data=mercer_fall2021, aes(date, cases/387340*1000, group=1), col="red") +
  geom_point(data=mercer_fall2021, aes(date, cases/387340*1000), col="red") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases per 1000", expand=c(0, 0), limits=c(0, 550/13),
                     sec.axis = sec_axis(trans=~., "Mercer county cases per 1000")) +
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

g3_inset <- 
  ggplot(filter(fall2021, date2 <= "2021-11-19")) +
  geom_bar(aes(as.factor(date), value/13000*1000, fill=key), stat="identity") +
  geom_line(data=merge(mercer_fall2021, filter(fall2021, date2 <= "2021-11-19")), aes(date, cases/387340*1000, group=1), col="red") +
  geom_point(data=merge(mercer_fall2021, filter(fall2021, date2 <= "2021-11-19")), aes(date, cases/387340*1000), col="red") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases per 1000", expand=c(0, 0), limits=c(0, 30/13),
                     sec.axis = sec_axis(trans=~., "Mercer county cases per 1000")) +
  scale_fill_viridis_d() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1, size=8),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line.y.right = element_line(color="red"),
    axis.ticks.y.right = element_line(color="red"),
    axis.text.y.right = element_text(color="red", size=8),
    axis.title.y.right = element_text(color="red", size=8),
    axis.text.y.left = element_text(size=8),
    axis.title.y.left = element_text(size=8)
  )
  
g3 <- g3_base + 
  annotation_custom(
    ggplotGrob(g3_inset), 
    xmin = 1, xmax = 15, ymin = 160/13, ymax = 504/13
  )

g4 <- ggplot(spring2021) +
  geom_segment(x=3, y=43/13, xend=3, yend=355/13, lty=3) +
  geom_segment(x=10, y=43/13, xend=10, yend=355/13, lty=3) +
  geom_bar(aes(as.factor(date), value/15, fill=key), stat="identity") +
  geom_line(data=mercer_spring2021, aes(date, cases/387340*1000, group=1), col="red") +
  geom_point(data=mercer_spring2021, aes(date, cases/387340*1000), col="red") +
  annotate("text", x=3, y=390/13, label="Classes\nbegin", family="Times") +
  annotate("text", x=10.5, y=380/13, label="Spring break\nReduced testing\nLifted mask mandates", vjust=0, hjust=1, family="Times") +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton cases per 1000", expand=c(0, 0), limits=c(0, 550/13),
                     sec.axis = sec_axis(trans=~., "Mercer county cases per 1000")) +
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
  merge(mercer_fall2020)

spring2020a <- spring2020 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(mercer_spring2020)

fall2021a <- fall2021 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(mercer_fall2021)

spring2021a <- spring2021 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(mercer_spring2021)

cor.test(fall2020a$value/3, fall2020a$cases/387340*1000)

g5 <- ggplot(fall2020a) +
  geom_point(aes(cases/387340*1000, value/3)) +
  geom_smooth(aes(cases/387340*1000, value/3), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

cor.test(spring2020a$value/8, spring2020a$cases/387340*1000)

g6 <- ggplot(spring2020a) +
  geom_point(aes(cases/387340*1000, value/8)) +
  geom_smooth(aes(cases/387340*1000, value/8), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

cor.test(fall2021a$value/13, fall2021a$cases/387340*1000)

g7 <- ggplot(fall2021a) +
  geom_point(aes(cases/387340*1000, value/13)) +
  geom_smooth(aes(cases/387340*1000, value/13), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("G") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

cor.test(spring2021a$value, spring2021a$cases)

g8 <- ggplot(spring2021a) +
  geom_point(aes(cases/387340*1000, value/15)) +
  geom_smooth(aes(cases/387340*1000, value/15), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("H") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

g9 <- ggplot(fall2020a) +
  geom_point(aes(date, (value/3)/(cases/387340*1000)))  +
  geom_line(aes(date, (value/3)/(cases/387340*1000), group=1)) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton to Mercer case ratio") +
  ggtitle("I")  +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g10 <- ggplot(spring2020a) +
  geom_point(aes(date, value/8/(cases/387340*1000)))  +
  geom_line(aes(date, value/8/(cases/387340*1000), group=1)) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton to Mercer case ratio") +
  ggtitle("J")  +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g11 <- ggplot(fall2021a) +
  geom_point(aes(date, value/13/(cases/387340*1000)))  +
  geom_line(aes(date, value/13/(cases/387340*1000), group=1)) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton to Mercer case ratio") +
  ggtitle("K")  +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

g12 <- ggplot(spring2021a) +
  geom_point(aes(date, value/15/(cases/387340*1000)))  +
  geom_line(aes(date, value/15/(cases/387340*1000), group=1)) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_discrete("Date") +
  scale_y_continuous("Princeton to Mercer case ratio") +
  ggtitle("L")  +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank()
  )

gcomb <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, nrow=4, byrow=FALSE,
                    widths=c(2, 0.8, 1), draw=FALSE)

# gfinal <- arrangeGrob(gcomb, g9, heights=c(4, 1))

ggsave("figure_princeton_new_per_1000.pdf", gcomb, width=12*1.2, height=10*1.2)

fall2020_pop <- data.frame(
  key=c("Faculty and Staff", "Grad", "Undergrad"),
  pop=c(2000, 1000, 300)
)

fall2020b <- fall2020 %>%
  merge(mercer_fall2020) %>%
  merge(fall2020_pop) 

spring2020_pop <- data.frame(
  key=c("Faculty and Staff", "Grad", "Undergrad"),
  pop=c(3000, 2000, 3000)
)

spring2020b <- spring2020 %>%
  merge(mercer_spring2020) %>%
  merge(spring2020_pop) 

fall2021_pop <- data.frame(
  key=c("Faculty and Staff", "Grad", "Undergrad"),
  pop=c(6000, 2000, 5000)
)

fall2021b <- fall2021 %>%
  merge(mercer_fall2021) %>%
  merge(fall2021_pop) 

spring2021_pop <- data.frame(
  key=c("Faculty and Staff", "Grad", "Undergrad"),
  pop=c(7000, 3000, 5000)
)

spring2021b <- spring2021 %>%
  merge(mercer_spring2021) %>%
  merge(spring2021_pop) 

fall2020b %>% 
  group_by(key) %>%
  summarize(
    cor.test(log(value+1), log(cases+1))[[3]]
  )

spring2020b %>% 
  group_by(key) %>%
  summarize(
    cor.test(value, cases)[[3]]
  )

fall2021b %>% 
  group_by(key) %>%
  summarize(
    cor.test(value, cases)[[3]]
  )

spring2021b %>% 
  group_by(key) %>%
  summarize(
    cor.test(value, cases)[[3]]
  )

g13 <- ggplot(fall2020b) +
  geom_point(aes(cases/387340*1000, value/pop*1000)) +
  geom_smooth(aes(cases/387340*1000, value/pop*1000), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("A. Fall 2020-2021") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

g14 <- ggplot(spring2020b) +
  geom_point(aes(cases/387340*1000, value/pop*1000)) +
  geom_smooth(aes(cases/387340*1000, value/pop*1000), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("B. Spring 2020-2021") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

g15 <- ggplot(fall2021b) +
  geom_point(aes(cases/387340*1000, value/pop*1000)) +
  geom_smooth(aes(cases/387340*1000, value/pop*1000), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("C. Fall 2021-2022") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

g16 <- ggplot(spring2021b) +
  geom_point(aes(cases/387340*1000, value/pop*1000)) +
  geom_smooth(aes(cases/387340*1000, value/pop*1000), method="lm", col="black") +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_x_continuous("Mercer county cases per 1000") +
  scale_y_continuous("Princeton cases per 1000") +
  ggtitle("D. Spring 2021-2022") +
  facet_wrap(~key, scale="free") +
  theme(
    panel.grid = element_blank()
  )

gfinal3 <- ggarrange(g13, g14, g15, g16, nrow=4, draw=FALSE)

ggsave("figure_princeton_correlation_per_1000.pdf", gfinal3, width=12, height=10)
