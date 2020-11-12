# Public Health Data Science 2019-2020 Assessment
# COVID-19 data analysis for Scandinavian countries during 9th April- 25th May 2020

# Initialize the required packages
library(tidyverse)
library(ggplot2)
install.packages("Metrics")
library(Metrics)


# IHME Norway data

IHME_Covid19_Norway_DailyDeath <- read_csv("~/Data/Norway/deaths.csv") %>%
  select(date_reported, location_id, mean) %>%
  filter(date_reported >= as.Date("2020-04-09") & date_reported <= as.Date("2020-05-25"))
head(IHME_Covid19_Norway_DailyDeath)

IHME_Covid19_Norway_TotalDeath <- read_csv('~/Data/Norway/total_death.csv') %>%
  select(date_reported, location_id, mean) %>%
  filter(date_reported >= as.Date("2020-04-09") & date_reported <= as.Date("2020-05-25"))
head(IHME_Covid19_Norway_TotalDeath)

IHME_Covid19_Norway_TotalDeath = rename(IHME_Covid19_Norway_TotalDeath, Total_predicted_death = mean)
head(IHME_Covid19_Norway_TotalDeath)

IHME_Covid19_Norway_Predicted_Death = merge(IHME_Covid19_Norway_DailyDeath, IHME_Covid19_Norway_TotalDeath)
head(IHME_Covid19_Norway_Predicted_Death)

IHME_Covid19_Norway_Predicted_Death$location_id[IHME_Covid19_Norway_Predicted_Death$location_id == '90'] <- 'Norway'
head(IHME_Covid19_Norway_Predicted_Death)

summary(IHME_Covid19_Norway_Predicted_Death)


# IHME Sweden data

IHME_Covid19_Sweden_DailyDeath <- read_csv('~/Data/Sweden/deaths.csv') %>%
  select(date_reported, location_id, mean) %>%
  filter(date_reported >= as.Date("2020-04-09") & date_reported <= as.Date("2020-05-25"))
head(IHME_Covid19_Sweden_DailyDeath)

IHME_Covid19_Sweden_TotalDeath <- read_csv('~/Data/Sweden/total_death.csv') %>%
  select(date_reported, location_id, mean) %>%
  filter(date_reported >= as.Date("2020-04-09") & date_reported <= as.Date("2020-05-25"))
head(IHME_Covid19_Sweden_TotalDeath)

IHME_Covid19_Sweden_TotalDeath = rename(IHME_Covid19_Sweden_TotalDeath, Total_predicted_death = mean)
head(IHME_Covid19_Sweden_TotalDeath)

IHME_Covid19_Sweden_Predicted_Death = merge(IHME_Covid19_Sweden_DailyDeath, IHME_Covid19_Sweden_TotalDeath)
head(IHME_Covid19_Sweden_Predicted_Death)

IHME_Covid19_Sweden_Predicted_Death$location_id[IHME_Covid19_Sweden_Predicted_Death$location_id == '93'] <- 'Sweden'
head(IHME_Covid19_Sweden_Predicted_Death)

summary(IHME_Covid19_Sweden_Predicted_Death)


# IHME Denmark data

IHME_Covid19_Denmark_DailyDeath <- read_csv('~/Data/Denmark/deaths.csv') %>%
  select(date_reported, location_id, mean) %>%
  filter(date_reported >= as.Date("2020-04-09") & date_reported <= as.Date("2020-05-25"))
head(IHME_Covid19_Denmark_DailyDeath)

IHME_Covid19_Denmark_TotalDeath <- read_csv('~/Data/Denmark/total_death.csv') %>%
  select(date_reported, location_id, mean) %>%
  filter(date_reported >= as.Date("2020-04-09") & date_reported <= as.Date("2020-05-25"))
head(IHME_Covid19_Denmark_TotalDeath)

IHME_Covid19_Denmark_TotalDeath = rename(IHME_Covid19_Denmark_TotalDeath, Total_predicted_death = mean)
head(IHME_Covid19_Denmark_TotalDeath)

IHME_Covid19_Denmark_Predicted_Death = merge(IHME_Covid19_Denmark_DailyDeath, IHME_Covid19_Denmark_TotalDeath)
head(IHME_Covid19_Denmark_Predicted_Death)

IHME_Covid19_Denmark_Predicted_Death$location_id[IHME_Covid19_Denmark_Predicted_Death$location_id == '78'] <- 'Denmark'
head(IHME_Covid19_Denmark_Predicted_Death)

summary(IHME_Covid19_Denmark_Predicted_Death)


# Combine the IHME data for all the Scandinavian countries

IHME_Predicted_Death = rbind(IHME_Covid19_Norway_Predicted_Death, IHME_Covid19_Sweden_Predicted_Death, IHME_Covid19_Denmark_Predicted_Death)
IHME_Predicted_Death = rename(IHME_Predicted_Death, Daily_predicted_death = mean)
IHME_Predicted_Death = rename(IHME_Predicted_Death, location = location_id)
view (IHME_Predicted_Death)
summary(IHME_Predicted_Death)

# Maximum value to use in graph
max_val <- sum(IHME_Predicted_Death$Total_predicted_death)
max_val

# Maximum daily predicted death for each country
IHME_max_val <- IHME_Predicted_Death %>% group_by(location) %>% filter(Daily_predicted_death == max(Daily_predicted_death))
IHME_max_val


# Plot the daily predicted death count
IHME_Daily_Predicted_Death_Plot<-
  ggplot(IHME_Predicted_Death, aes(x=date_reported, y=Daily_predicted_death, fill=location))+
  geom_bar(stat="identity")+
  #geom_point(data = IHME_max_val, colour = 'black')+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max_val, by = 20))+
  theme(axis.text.x = element_text(angle = 45), legend.position = c(.85, .80), 
        panel.background = element_rect(fill = "#d6d7e1"),
        axis.text=element_text(size=10, colour="#04045E"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent")) + #fill = "#BFD5E3"
  labs(
    title="COVID-19 Daily predicted deaths",
    #subtitle="09-Apr-2020 to 25-May-2020",
    x="Predicted date",
    y="Average death count",
    fill="Scandinavian Countries",
    caption="Source: Institute for Health Metrics & Evaluation (IHME)"
  ) + scale_fill_brewer(palette="Blues")

IHME_Daily_Predicted_Death_Plot


# Plot the cumulative predicted death count
IHME_Total_Predicted_Death_Plot<-
  ggplot(IHME_Predicted_Death, aes(x=date_reported, y=Total_predicted_death, fill=location))+
  geom_bar(stat="identity")+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max_val, by = 500))+
  theme(axis.text.x = element_text(angle = 45), legend.position = c(.15, .80),
        panel.background = element_rect(fill = "#d6d7e1"), #EDEDFF
        axis.text=element_text(size=10, colour="#04045E"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent")) + #fill = "#BFD5E3"
  labs(
    title="COVID-19 Cumulative predicted deaths",
    x="Predicted date",
    y="Average death count",
    fill="Scandinavian Countries",
    caption="Source: Institute for Health Metrics & Evaluation (IHME)"
  ) + scale_fill_brewer(palette="Blues")

IHME_Total_Predicted_Death_Plot



# To plot top 5 predicted daily death reported dates
IHME_Top5_Daily_Predicted_Death_Plot<-
  ggplot(top_n(IHME_Predicted_Death %>% group_by(location) %>% arrange(desc(Daily_predicted_death)), n=5, Daily_predicted_death),
         aes(x=reorder(date_reported, -Daily_predicted_death), y=Daily_predicted_death, fill=location)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=Daily_predicted_death), position=position_dodge(width=0.9), vjust=-0.15, size = 3) +
  theme(axis.text.x = element_text(angle = 45) , 
        axis.text = element_text(size=10, colour="#04045E"), 
        panel.background = element_rect(fill = "#d6d7e1"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(
    title="COVID-19 Top 5 Predicted death dates",
    x="Reported date",
    y="Death count",
    caption="Source: Institute for Health Metrics & Evaluation (IHME)"
  )  + scale_fill_brewer(palette="Blues") +
  facet_wrap(~location, scales="free")

IHME_Top5_Daily_Predicted_Death_Plot


# WHO Data

WHO_Covid19_Data <- read_csv('~/Data/WHO-COVID-19-global-data.csv')
WHO_Scandinavia_Death <- WHO_Covid19_Data %>%
  select(-WHO_region, -New_cases, -Cumulative_cases, -Country_code) %>%
  filter(Date_reported >= as.Date("2020-04-09") & Date_reported <= as.Date("2020-05-25")
         & (Country %in% c('Norway', 'Sweden', 'Denmark')))

tail(WHO_Scandinavia_Death)
summary(WHO_Scandinavia_Death)


# Maximum daily actual death for each country
WHO_max_val <- WHO_Scandinavia_Death %>% group_by(Country) %>% filter(New_deaths == max(New_deaths))
WHO_max_val


# COVID 19 Daily actual deaths

WHO_Scandinavia_Daily_Death_Plot<-
  ggplot(WHO_Scandinavia_Death)+
  geom_bar(aes(x=Date_reported, y=New_deaths, fill=Country), stat="identity")+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max_val, by = 25))+
  theme(axis.text.x = element_text(angle = 45), legend.position = c(.90, .80), 
        panel.background = element_rect(fill = "#d6d7e1"),
        axis.text=element_text(size=10, colour="#04045E"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent")) +
  labs(
    title="COVID-19 Daily Actual deaths",
    x="Reported date",
    y="Death count",
    fill="Scandinavian Countries",
    caption="Source: World Health Organization (WHO)"
  ) + scale_fill_brewer(palette="Blues")
  
WHO_Scandinavia_Daily_Death_Plot


# COVID 19 Cumulative actual deaths

WHO_Scandinavia_Total_Death_Plot<-
  ggplot(WHO_Scandinavia_Death)+
  geom_bar(aes(x=Date_reported, y=Cumulative_deaths, fill=Country), stat="identity")+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max_val, by = 500))+
  theme(axis.text.x = element_text(angle = 45), legend.position = c(.10, .80), 
        panel.background = element_rect(fill = "#d6d7e1"),
        axis.text=element_text(size=10, colour="#04045E"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent")) +
  labs(
    title="COVID-19 Cumulative Actual deaths",
    x="Reported date",
    y="Death count",
    fill="Scandinavian Countries",
    caption="Source: World Health Organization (WHO)"
  ) + scale_fill_brewer(palette="Blues")

WHO_Scandinavia_Total_Death_Plot


# To plot top 5 actual daily death reported dates
WHO_Scandinavia_Top5_Daily_Death_Plot<-
  ggplot(subset(top_n(WHO_Scandinavia_Death %>% group_by(Country) %>% arrange(desc(New_deaths)), n=5, New_deaths)), 
         aes(x=reorder(Date_reported, -New_deaths), y=New_deaths, fill=Country)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=New_deaths), position=position_dodge(width=0.9), vjust=-0.15, size = 3) +
  theme(axis.text.x = element_text(angle = 45) , 
        axis.text = element_text(size=10, colour="#04045E"), 
        panel.background = element_rect(fill = "#d6d7e1"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #legend.text = element_text(size = 8),
        legend.position = "none") +
  labs(
    title="COVID-19 Top 5 Actual daily death dates",
    x="Reported date",
    y="Death count",
    #fill="Scandinavian Countries",
    caption="Source: World Health Organization (WHO)"
  ) + scale_fill_brewer(palette="Blues") + 
  facet_wrap(~Country, scales="free")

WHO_Scandinavia_Top5_Daily_Death_Plot


# Combine IHME and WHO data
IHME_WHO_Scandinavia_Death <- full_join(WHO_Scandinavia_Death, IHME_Predicted_Death, 
                    by = c("Date_reported"= "date_reported", "Country" = "location"))
tail(IHME_WHO_Scandinavia_Death)


# Overall predicted and actual death count on 9th April and 25th May 2020
Minmax_Totval <- IHME_WHO_Scandinavia_Death %>% filter(Date_reported %in% c(min(Date_reported), max(Date_reported)))
Minmax_Totval


# Statistical details
summary (IHME_WHO_Scandinavia_Death)
# To check missing values
sum(is.na(IHME_WHO_Scandinavia_Death))
# No actual or predicted deaths
print(IHME_WHO_Scandinavia_Death %>% filter(New_deaths <=0 | Daily_predicted_death <=0), n=nrow(IHME_WHO_Scandinavia_Death))
# Same actual or predicted deaths
Equal_Actual_Predict = IHME_WHO_Scandinavia_Death %>% filter(New_deaths==Daily_predicted_death)
Equal_Actual_Predict

#Number of correctly identified actual and predict daily deaths (in percentage)
count(Equal_Actual_Predict)/ count(IHME_WHO_Scandinavia_Death) * 100

# To plot the predict and actual daily death
Predict_Actual_Daily_Plot<-
  ggplot(IHME_WHO_Scandinavia_Death)+
  geom_line(aes(x=Date_reported, y=New_deaths, col= 'New_deaths'), stat="identity")+
  geom_line(aes(x=Date_reported, y=Daily_predicted_death, col= 'Daily_predicted_death'))+
  scale_x_datetime(date_breaks = "4 day", date_labels = "%b %d", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, max_val, by = 10))+
  theme(axis.text.x = element_text(angle = 45) , 
        axis.text = element_text(size=10, colour="#04045E"), 
        panel.background = element_rect(fill = "#EDEDFF"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(
    title="COVID-19 Predicted vs Actual daily deaths",
    x="Reported date",
    y="Death count"#,
  ) + 
  scale_color_manual(name = "Death",
                      values = c( "New_deaths" = "darkblue", "Daily_predicted_death" = "maroon"),
                      labels = c("Predicted", "Actual")) +
  facet_wrap(~Country, scales="free_y")

Predict_Actual_Daily_Plot


# To plot the predict and actual cumulative death
Predict_Actual_Total_Plot<-
  ggplot(IHME_WHO_Scandinavia_Death)+
  geom_line(aes(x=Date_reported, y=Cumulative_deaths, col= 'Cumulative_deaths'), stat="identity")+
  geom_line(aes(x=Date_reported, y=Total_predicted_death, col= 'Total_predicted_death'))+
  scale_x_datetime(date_breaks = "4 day", date_labels = "%b %d", expand = c(0, 0)) + 
  #scale_y_continuous(breaks = seq(0, max_val, by = 500))+
  theme(axis.text.x = element_text(angle = 45) , 
        axis.text = element_text(size=10, colour="#04045E"), 
        panel.background = element_rect(fill = "#EDEDFF"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(
    title="COVID-19 Predicted vs Actual cumulative deaths",
    x="Reported date",
    y="Death count"#,
    #fill="Scandinavian Countries",
    #caption="Source: World Health Organization (WHO)"
  ) + 
  scale_color_manual(name = "Death",
                     values = c( "Cumulative_deaths" = "darkblue", "Total_predicted_death" = "maroon"),
                     labels = c("Actual", "Predicted")) +
  facet_wrap(~Country, scales="free_y")

Predict_Actual_Total_Plot


# Predicted and actual death for each country

IHME_WHO_Norway_Death = IHME_WHO_Scandinavia_Death %>% filter(Country == 'Norway')
IHME_WHO_Sweden_Death = IHME_WHO_Scandinavia_Death %>% filter(Country == 'Sweden')
IHME_WHO_Denmark_Death = IHME_WHO_Scandinavia_Death %>% filter(Country == 'Denmark')


# Performance metric evaluation for Norway using Mean Absolute Error

mae(IHME_WHO_Norway_Death$New_deaths, IHME_WHO_Norway_Death$Daily_predicted_death)
mae(IHME_WHO_Norway_Death$Cumulative_deaths, IHME_WHO_Norway_Death$Total_predicted_death)


# Metric evaluation for Sweden
mae(IHME_WHO_Sweden_Death$New_deaths, IHME_WHO_Sweden_Death$Daily_predicted_death)
mae(IHME_WHO_Sweden_Death$Cumulative_deaths, IHME_WHO_Sweden_Death$Total_predicted_death)


# Metric evaluation for Denmark
mae(IHME_WHO_Denmark_Death$New_deaths, IHME_WHO_Denmark_Death$Daily_predicted_death)
mae(IHME_WHO_Denmark_Death$Cumulative_deaths, IHME_WHO_Denmark_Death$Total_predicted_death)



