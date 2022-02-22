library(tidyverse)
library(rvest)
library(dplyr)
library(janitor)
library(plotly)

# Har jobbet med Christian Karlsen og Rudi Hansen på denne oppgaven

tall <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"  %>% 
  read_html() %>% 
  html_nodes(xpath = '/html/body/article/section/div[4]/div[4]/div[1]/table') %>% 
  html_table(header = 1, fill = TRUE) %>%  
  as.data.frame() %>% 
  filter(!str_detect(Avvik, "x")) %>% 
  separate(`WLTP.tall`, sep = "/", into=c("WLTP","kWh")) %>% 
  mutate(STOPP = as.numeric(gsub("km", "", STOPP))) %>% 
  mutate(WLTP = as.numeric(gsub("km", "", WLTP)))  %>% 
  rename("Modell" = Modell..temp..varierte.fra.0..til..10..) %>%
  ggplot(., aes(x = WLTP, y = STOPP, label = Modell)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "dark orange")  +
  scale_y_continuous(limits = c(200, 700)) + 
  scale_x_continuous(limits = c(200, 700)) +
  ggtitle("Kjøreavstand til elbiler. \nTempratur varierte fra 0° til -10°") +
  theme_bw()


ggplotly(tall) 




lm(STOPP ~ WLTP, data = tall$data) 

ggplotly(tall + geom_smooth(method = lm))
