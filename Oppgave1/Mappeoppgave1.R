setwd("~/OneDrive - UiT Office 365/Data sience")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(data.table)
library(zoo)

tabell <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")


tabell <- subset(tabell, select = -c(Land:AUST)) %>% 
  filter(Year !=max(Year),Year != min(Year))

sapply(tabell, class)


tabell$Globe <- as.numeric(tabell$Globe)
tabell$Mo <- as.numeric(tabell$Mo)

sapply(tabell, class)

tabell = tabell %>% 
  mutate(rolltemp = rollmean(Globe, 13,  fill = NA, align ='right'),
         Dato = as.yearmon(paste(tabell$Year, tabell$Mo), "%Y %m"))

tabell$Dato <- as.Date(tabell$Dato)
sapply(tabell, class)




pp <- tabell %>% 
  ggplot(aes(x=Dato, y=Globe)) +
  geom_point() +
  geom_line()+
  geom_line(aes(y=tabell$rolltemp), colour = 'red')+
  labs(title="Globale temperaturer",
       x="År",
       y="Temperatur") +
  theme_bw()

pp + scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

