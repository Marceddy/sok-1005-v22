setwd("~/Desktop/sok-1005-v22/Oppgave 2")
library(tidyverse)
library(jsonlite)
library(rvest)
library(Countr)

data1 <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

data2 <- data1 %>% 
  mutate(Abbr = state.abb[match(name,state.name)])

data2[is.na(data2)] = "D.C"

# Oppgave 1
df2 %>%
  rename(Stat=name) %>% 
  ggplot(aes(y = deaths_per_100k, x = fully_vaccinated_pct_of_pop,)) + 
  geom_point() +
  labs(title="Dødsfall som følge av Covid-19 i forhold til vaksinerte og uvaksinerte",
       x="Andel av total befolkning som er vaksinert",
       y="Dødsfall pr 100.000") + 
  geom_text(aes(label=Abbr), size=2, hjust=0, vjust=-1, family="Georgia") + 
  scale_x_continuous(breaks=scales::breaks_pretty(n=5),labels = scales::percent) +
  theme_bw()

# Oppgave 2


lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = data2)

df2 %>%
  rename(Stat=name) %>% 
  ggplot(aes(y = deaths_per_100k, x = fully_vaccinated_pct_of_pop,)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title="Dødsfall som følge av Covid-19 i forhold til vaksinerte og uvaksinerte",
       x="Andel av total befolkning som er vaksinert",
       y="Dødsfall pr 100.000") + 
  geom_text(aes(label=Abbr), size=2, hjust=0, vjust=-1, family="Georgia") + 
  scale_x_continuous(breaks=scales::breaks_pretty(n=5),labels = scales::percent) +
  theme_bw()


            