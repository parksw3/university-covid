library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(lubridate)
library(usmap)
library(geosphere)

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

countylonlat <- read.csv("../data/us-counties-lonlat.csv")

county2 <- county %>%
  filter(state %in% c("New Jersey", "New York", "Pennsylvania")) %>%
  group_by(state, county) %>%
  arrange(state, county, date) %>%
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

fall2020_fs <- data %>%
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
  mutate(
    date2=as.Date(paste0(year, "-", month, "-", day))
  ) %>%
  select(date, `Faculty and Staff`, year, month, day, date2)

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

spring2020_fs <- data %>%
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
  mutate(
    date2=as.Date(paste0(year, "-", month, "-", day))
  ) %>%
  select(date, `Faculty and Staff`, year, month, day, date2)

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

fall2021_fs <- data %>%
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
  mutate(
    date2=as.Date(paste0(year, "-", month, "-", day))
  ) %>%
  select(date, `Faculty and Staff`, year, month, day, date2)

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

spring2021_fs <- data %>%
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
  mutate(
    date2=as.Date(paste0(year, "-", month, "-", day))
  ) %>%
  select(date, `Faculty and Staff`, year, month, day, date2)


county2_fall2020 <- county2 %>%
  filter(date >= "2020-08-24", date<= "2021-01-01") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2020$date2)))
  ) %>%
  group_by(state, group, county) %>%
  summarize(
    cases=sum(cases),
    date=max(date),
    fips=unique(fips)
  ) %>%
  mutate(
    date=fall2020$date[match(as.Date(date), as.Date(fall2020$date2))]
  )

county2_spring2020 <- county2 %>%
  filter(date >= "2021-01-16", date<= "2021-05-14") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2020$date2)))
  ) %>%
  group_by(state, group, county) %>%
  summarize(
    cases=sum(cases),
    date=max(date),
    fips=unique(fips)
  ) %>%
  mutate(
    date=spring2020$date[match(as.Date(date), as.Date(spring2020$date2))]
  )

county2_fall2021 <- county2 %>%
  filter(date >= "2021-08-14", date <= "2022-01-14") %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(fall2021$date2)))
  ) %>%
  group_by(state, group, county) %>%
  summarize(
    cases=sum(cases),
    date=max(date),
    fips=unique(fips)
  ) %>%
  mutate(
    date=fall2021$date[match(as.Date(date), as.Date(fall2021$date2))]
  )

county2_spring2021 <- county2 %>%
  filter(date > as.Date("2021-12-31"), date <= as.Date("2022-03-18")) %>%
  mutate(
    group=cut(as.numeric(as.Date(date)), breaks=unique(as.numeric(spring2021$date2)))
  ) %>%
  group_by(state, group, county) %>%
  summarize(
    cases=sum(cases),
    date=max(date),
    fips=unique(fips)
  ) %>%
  mutate(
    date=spring2021$date[match(as.Date(date), as.Date(spring2021$date2))]
  )

county2lonlat <- countylonlat %>%
  select(state_id, county_fips, lng, lat) %>%
  rename(
    state=state_id,
    fips=county_fips
  ) %>%
  filter(state %in% c("NJ", "NY", "PA"))

mercerlonlat <- county2lonlat %>%
  filter(fips==34021)

county2lonlat2 <- county2lonlat %>%
  group_by(fips) %>%
  mutate(
    ## convert to km
    dist=distm(c(lng, lat), c(mercerlonlat$lng, mercerlonlat$lat), fun = distHaversine)[[1]]/1000
  )

fall2020_corr_raw <- fall2020 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(county2_fall2020) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(value+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  )

fall2020_corr <- fall2020_corr_raw %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

spring2020_corr_raw <- spring2020 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(county2_spring2020) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(value+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  )

spring2020_corr <- spring2020_corr_raw %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

fall2021_corr_raw <- fall2021 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(county2_fall2021) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(value+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  )

fall2021_corr <- fall2021_corr_raw %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

spring2021_corr_raw <- spring2021 %>%
  group_by(date) %>%
  summarize(
    value=sum(value)
  ) %>%
  merge(county2_spring2021) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(value+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  )

spring2021_corr <- spring2021_corr_raw %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

## fs

fall2020_corr_fs <- fall2020_fs %>%
  group_by(date) %>%
  summarize(
    `Faculty and Staff`=sum(`Faculty and Staff`)
  ) %>%
  merge(county2_fall2020) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(`Faculty and Staff`+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  ) %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

spring2020_corr_fs <- spring2020_fs %>%
  group_by(date) %>%
  summarize(
    `Faculty and Staff`=sum(`Faculty and Staff`)
  ) %>%
  merge(county2_spring2020) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(`Faculty and Staff`+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  ) %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

fall2021_corr_fs <- fall2021_fs %>%
  group_by(date) %>%
  summarize(
    `Faculty and Staff`=sum(`Faculty and Staff`)
  ) %>%
  merge(county2_fall2021) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(`Faculty and Staff`+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  ) %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

spring2021_corr_fs <- spring2021_fs %>%
  group_by(date) %>%
  summarize(
    `Faculty and Staff`=sum(`Faculty and Staff`)
  ) %>%
  merge(county2_spring2021) %>%
  group_by(state, county) %>%
  summarize(
    corr=cor(log(cases+1), log(`Faculty and Staff`+1)),
    state=state.abb[match(unique(state), state.name)],
    fips=unique(fips)
  ) %>%
  filter(!is.na(fips)) %>%
  merge(county2lonlat2)

g1 <- plot_usmap(data=fall2020_corr, value="corr", include = c("NJ", "NY", "PA"), region="counties") +
  scale_fill_viridis_c("Correlation", limits=c(-0.5, 1)) +
  ggtitle("A. Fall 2020-2021") +
  theme(
    legend.position = "right"
  )

g2 <- plot_usmap(data=spring2020_corr, value="corr", include = c("NJ", "NY", "PA"), region="counties") +
  scale_fill_viridis_c("Correlation", limits=c(-0.5, 1)) +
  ggtitle("B. Spring 2020-2021") +
  theme(
    legend.position = "right"
  )

g3 <- plot_usmap(data=fall2021_corr, value="corr", include = c("NJ", "NY", "PA"), region="counties") +
  scale_fill_viridis_c("Correlation", limits=c(-0.5, 1)) +
  ggtitle("C. Fall 2021-2022") +
  theme(
    legend.position = "right"
  )

g4 <- plot_usmap(data=spring2021_corr, value="corr", include = c("NJ", "NY", "PA"), region="counties") +
  scale_fill_viridis_c("Correlation", limits=c(-0.5, 1)) +
  ggtitle("D. Spring 2021-2022") +
  theme(
    legend.position = "right"
  )

g5 <- ggplot(fall2020_corr) +
  geom_point(aes(dist, corr)) +
  geom_smooth(aes(dist, corr), method="lm", col="black") +
  geom_point(data=fall2020_corr_fs, aes(dist, corr), col="red") +
  geom_smooth(data=fall2020_corr_fs, aes(dist, corr), method="lm", col="red", fill="red") +
  scale_x_continuous("Distance from Mercer County (km)") +
  scale_y_continuous("Correlation coefficients", limits=c(-0.5, 1)) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank()
  )

cor.test(spring2020_corr$dist, spring2020_corr$corr)

g6 <- ggplot(spring2020_corr) +
  geom_point(aes(dist, corr)) +
  geom_smooth(aes(dist, corr), method="lm", col="black") +
  geom_point(data=spring2020_corr_fs, aes(dist, corr), col="red") +
  geom_smooth(data=spring2020_corr_fs, aes(dist, corr), method="lm", col="red", fill="red") +
  scale_x_continuous("Distance from Mercer County (km)") +
  scale_y_continuous("Correlation coefficients", limits=c(-0.5, 1)) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank()
  )

cor.test(fall2021_corr$dist, fall2021_corr$corr)

g7 <- ggplot(fall2021_corr) +
  geom_point(aes(dist, corr)) +
  geom_smooth(aes(dist, corr), method="lm", col="black") +
  geom_point(data=fall2021_corr_fs, aes(dist, corr), col="red") +
  geom_smooth(data=fall2021_corr_fs, aes(dist, corr), method="lm", col="red", fill="red") +
  scale_x_continuous("Distance from Mercer County (km)") +
  scale_y_continuous("Correlation coefficients", limits=c(-0.5, 1)) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank()
  )

g8 <- ggplot(spring2021_corr) +
  geom_point(aes(dist, corr)) +
  geom_smooth(aes(dist, corr), method="lm", col="black") +
  geom_point(data=spring2021_corr_fs, aes(dist, corr), col="red") +
  geom_smooth(data=spring2021_corr_fs, aes(dist, corr), method="lm", col="red", fill="red") +
  scale_x_continuous("Distance from Mercer County (km)") +
  scale_y_continuous("Correlation coefficients", limits=c(-0.5, 1)) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank()
  )

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, byrow=FALSE, widths=c(1, 3), draw=FALSE)

ggsave("figure_princeton_map_extended.pdf", gtot, width=10, height=12)
