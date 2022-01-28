setwd("~/Desktop/sok-1005-v22/Oppgave1")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(data.table)
library(zoo)

tempratur_df <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")


tempratur_df <- subset(tempratur_df, select = -c(Land:AUST)) %>% 
  filter(Year !=max(Year),Year != min(Year))

sapply(tempratur_df, class)


tempratur_df$Globe <- as.numeric(tempratur_df$Globe)
tempratur_df$Mo <- as.numeric(tempratur_df$Mo)

sapply(tempratur_df, class)

tempratur_df = tempratur_df %>% 
  mutate(rolltemp = rollmean(Globe, 13,  fill = NA, align ='right'),
         Dato = as.yearmon(paste(tempratur_df$Year, tempratur_df$Mo), "%Y %m"))

tempratur_df$Dato <- as.Date(tempratur_df$Dato)
sapply(tempratur_df, class)




tempratur_df %>% 
  ggplot(aes(x=Dato, y=Globe)) +
  geom_point() +
  geom_line()+
  geom_line(aes(y=tempratur_df$rolltemp), colour = 'red')+
  labs(title="Globale temperaturer",
       x="År",
       y="Temperatur") +
  theme_bw() + 
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


#Lager den så lik som nettsiden som mulig.

tempratur_df %>%
  ggplot(aes(x=Dato, y=Globe)) + 
  geom_line(aes(x=Dato, y=Globe), colour='darkblue') +
  geom_line(aes(x=Dato, y=rolltemp), colour='red', size=1.5) +
  geom_point(aes(x=Dato, y=Globe), shape = 1, colour='darkblue') + 
  coord_cartesian(xlim = c(as.Date("1980-06-01"), as.Date("2020-01-01"))) +
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=1) +
  theme_bw() +
  scale_y_continuous(breaks = round(seq(min(tempratur_df$Globe), 1.0, by = 0.1),1)) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  labs(title = "Latest Global Temps", 
       y = "T Departure from '91-'20 Avg. (deg. C)", x = " ") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  annotate("text", x=as.Date("1987-01-01"), y=0.5, 
           label= "UAH Satellite-Based \n Temperature of the \n Global Lower
           Atmosphere \n (Version 6.0)") +
  annotate("text", x=as.Date("2018-01-01"), y=-0.5, 
           label="December 2021: \n +0.21 deg. C") +
  annotate("text", x=as.Date("2006-01-01"), y=-0.6, 
           label="Running, centered \n 13-month average", colour="red") +
  annotate("segment", x = as.Date("2006-01-01"), 
           xend = as.Date("2009-01-01"), y = -0.5, 
           yend = -0.15, colour = "red") +
  annotate("segment", x = as.Date("2018-01-01"), 
           xend = as.Date("2021-12-01"), y = -0.4, 
           yend = 0.21, colour = "black")

### Oppgave 2

Lower_Troposphere <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
Mid_Troposphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
Tropopause <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
Lower_Stratosphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

Lower_Troposphere <- subset(Lower_Troposphere, select =c(Year,Mo,NoPol)) %>%
  filter(Year != max(Year), Year != min(Year))%>%
  mutate(Atmosphere = "Lower_Troposphere")

Mid_Troposphere <- subset(Mid_Troposphere, select =c(Year,Mo,NoPol)) %>%
  filter(Year != max(Year), Year != min(Year)) %>%
  mutate(Atmosphere = "Mid_Troposhere")

Tropopause <- subset(Tropopause, select =c(Year,Mo,NoPol)) %>%
  filter(Year != max(Year), Year != min(Year)) %>%
  mutate(Atmosphere = "Tropopause")

Lower_Stratosphere <- subset(Lower_Stratosphere, select =c(Year,Mo,NoPol)) %>%
  mutate(Atmosphere = "Lower_Stratosphere")

Lower_Stratosphere <- Lower_Stratosphere %>%
  mutate_all(as.character)

df <- bind_rows(Lower_Troposphere, Mid_Troposphere,
                    Tropopause, Lower_Stratosphere)

df$NoPol <- as.numeric(df$NoPol)

df <- df %>%
  mutate(tempmean = rollmean(NoPol, k=13, fill=NA, align ='right'),
         Dato = as.yearmon(paste(df$Year, df$Mo), "%Y %m")) 
df$Dato <- as.Date(df$Dato)

ggplot(df,aes(x = Dato, y = NoPol, color = Atmosphere)) +
  geom_path() +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y=tempmean, colour='Snitt temp'), size=0.2) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("År") +
  ylab("Tempraturer") + 
  ggtitle("Varierende tempraturer fra 60 grader til 90 grader Nord") +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 6)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 11)) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  scale_y_continuous(breaks=scales::breaks_pretty(n=20),
                     expand = expansion(add = 1)) +
  theme(legend.position="bottom")









