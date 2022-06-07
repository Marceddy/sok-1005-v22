library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)

appwich<- read.csv("AppWichStoreAttributes.csv")
countycrime <- read.csv("county_crime.csv")
demographic<- read.csv("county_demographic.csv")
weeklysales <- read.csv("WEEKLY_SALES_10STORES.csv")
weeklyweather<- read.csv("WEEKLY_WEATHER.csv")
countyemployment <- read.csv("county_employment.csv")

##Hver oppgave teller ¼ av endelig karakter.
##### --------Oppgave 1 --------

appwich = appwich %>% 
  rename(Store_num =Store_Num,
         Weather_Station = Store_Weather_Station) 


weeklyweather <- weeklyweather %>% 
  mutate(Weather_Date=dmy(Weather_Date)) %>% 
  rename(Week = Weather_Week,
         Date = Weather_Date) %>% 
  mutate(Week =strftime(.$Date, format = "%U")) %>% 
  replace(is.na(.), 0)

weeklysales <- weeklysales %>% 
  mutate(Date=mdy(Date))

## Merger tabellene
##Bruk merge på df2 

df1 <- inner_join(countycrime, demographic, countyemployment, by = 'County_Name') %>% 
  replace(is.na(.), 0)

df2 <- merge(appwich, weeklysales, by = 'Store_num') %>% 
  rename(County_Name = Store_County) %>% 
  left_join(weeklyweather)

data_clean <- left_join(df2,df1)



#Den første oppgaven er å skrive R kode som slår sammen de 6 datasettene til et stort datasett.
#Du må benytte de variablene som de ulike datasettene har til felles for å gjøre dette. Denne prosessen skal kort dokumenteres og kommenteres.

#### -------- Oppgave 2 ------
#Dataene skal benyttes til en ukentlig salgsrapport til et enkelt utsalg. Gi noen eksempler på hva innholdet i en slik kortsiktig individuell rapport bør inneholde. Begrunn dine valg og tankegangen bak figurer og eventuelle tabeller.


lake_city_week <- data_clean %>%
  filter(Store_num == "16") %>% 
  filter(Month == "12") %>% 
  filter(Date >="2012-12-02", Date <="2012-12-09")



sg <- function(data_clean){
  return(x <- (data_clean) %>% 
           group_by(price_group = ifelse(Price <= 0.49, "$0", 
                                         ifelse(Price <= 1.49, "$1", 
                                                ifelse(Price > 1.49 & Price <= 2.49, "$2", 
                                                       ifelse(Price > 2.49 & Price <= 3.49, "$3", 
                                                              ifelse(Price > 3.49 & Price <= 4.49, "$4", 
                                                                     ifelse(Price > 4.49 & Price <= 5.49, "$5", 
                                                                            ifelse(Price > 5.49 & Price <= 6.49, "$6", 
                                                                                   ifelse(Price > 6.49 & Price <= 7.49, "$7",
                                                                                          ifelse(Price > 7.49 & Price <= 8.49, "$8",
                                                                                                 ifelse(Price > 8.49 & Price <= 9.49, "$9", "over_$10"))))))))))) %>% 
           summarise(Sold, Price, Sales, Profit, Margin, Store_Name) %>%
           arrange(.$price_group))
}

lake_city <- sg(lake_city_week)


lake_city %>% 
  ggplot(aes(x = price_group, y=Sales , a=Sold, b=Price, c=Profit, d=Margin  ))+
  geom_bar(stat= "identity", fill = "steelblue")  +
  labs(title = "Ukentlig salg - Lake City Stripmall",
       x="Prisklasse", y = "Ukentlig salg") +
  theme_bw()



# Hva er de mest lønnsome produktene?
lake_city_week %>% 
  group_by(Description) %>%
  summarise(Profit = sum(Profit)) %>%
  arrange(desc(Profit))

#Hva er de minst lønnsome produktene?
lake_city_week %>% 
  group_by(Description) %>%
  summarise(Profit = sum(Profit)) %>%
  arrange(Profit)

# Hva selger vi mest av?
lake_city_week %>% 
  group_by(Description) %>%
  summarise(Sold = sum(Sold)) %>%
  arrange(desc(Sold))

lake_city_week %>% 
  filter(Sold >= 50) %>% 
  ggplot(aes(x = reorder(Description,-Sold), y = Sold, colour = Date )) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Sold),position = position_stack(vjust = 0.4), colour = "White", size=3)+
  labs(title = "Mest solgte varer for uken 02-09,2012") +
  xlab("Produkt") +
  ylab("Enheter") + 
  theme_bw() +
  theme(axis.text.x = element_text(vjust= 0.6, size = 4, angle = 10))

#####------------ Oppgave 3 ------

lake_city_mnd <- data_clean %>%
  filter(Store_num == "16") %>% 
  filter(Month == "12") %>% 
  filter(Year == "2012")

mnd_data <- data_clean %>% 
  filter(Year == "2012", Month == "12") 

# Utsalgssteder og profitt

mnd_data %>%  
  group_by(Store_num) %>%
  summarise(Profit = sum(Profit)) %>%
  arrange(desc(Profit)) 

mnd_data %>% 
  group_by(Month, Store_Name) %>% 
  summarise(Mnd_profitt = sum(Profit)) %>%
  ggplot(aes(x = reorder(Store_Name,-Mnd_profitt), y = Mnd_profitt, col= Store_Name)) +
  geom_col(aes(fill= "red"),show.legend = FALSE) + 
  geom_text(aes(label = Mnd_profitt),position = position_stack(vjust = 0.4), colour = "White", size=3)+
  labs(title = "Profitt for alle utsalgsstedene desember 2012",
       subtitle = "I USD") +
  xlab("Profitt") +
  ylab("Måned") + 
  theme_bw() +
  theme(axis.text.x = element_text(vjust= 0.6, size = 5, angle = 10))


# Hvilke produkter er mest lønnsomme?
lake_city_mnd %>% 
  group_by(Description) %>%
  summarise(Profit = sum(Profit)) %>%
  arrange(desc(Profit))

#Hvilke produkter er minst lønnsomme?
lake_city_mnd %>% 
  group_by(Description) %>%
  summarise(Profit = sum(Profit)) %>%
  arrange(Profit)

#Hvilke produkter selger vi mest av?
lake_city_mnd %>%  
  group_by(Description) %>%
  summarise(Sold = sum(Sold)) %>%
  arrange(desc(Sold))

lake_city_mnd %>% 
  filter(Sold >= 50) %>% 
  ggplot(aes(x = reorder(Description,-Sold), y = Sold, colour = Date )) +
  geom_bar(stat = "identity", fill="beige") + 
  geom_text(aes(label = Sold),position = position_stack(vjust = 0.4), colour = "Black", size=3)+
  labs(title = "Mest solgte varer for uken 02-09,2012") +
  xlab("Produkt") +
  ylab("Enheter") + 
  theme_bw() +
  theme(axis.text.x = element_text(vjust= 0.6, size = 4, angle = 10))

# Plot på profitt for desember måned.
plotmnd <- lake_city_mnd 

plotmnd <- aggregate(plotmnd$Profit, by=list(Dato=plotmnd$Date), FUN=sum)
plotmnd  <- rename(plotmnd, Profitt = x) 

plotmnd %>% 
  ggplot(aes(x = Dato, y = Profitt)) +
  geom_line() + 
  labs(title = "Profitt for desember måned 2020",
       subtitle= "I USD") +
  theme_bw()

#### ------- Oppgave 4 -------

labs <- expression("Forrige uke", "Denne uke")

# Har konkurranse noe å si på lønnsomheten?
data_clean %>%
  select(Store_Name, County_Name, Profit) %>% 
  group_by(Store_Name, County_Name) %>% 
  summarise(Profit =sum(Profit)) %>% 
  ggplot(aes(x=reorder(Store_Name,-Profit) , y =Profit))+
  geom_col(aes(fill = County_Name))+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Har konkurransen en effekt på lønnsomheten?",
       subtitle = 'Farger etter hvilket county de ligger i.', 
       fill = 'fastfood marked' )+
  ylab("Profitt i USD")+
  xlab("Utsalgssted")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=15, vjust = 0.4))

# Har Drive-through noe å si på lønnsomheten?

data_clean %>% 
  select(Store_Name, Profit, Store_Drive_Through) %>% 
  group_by(Store_Name, Store_Drive_Through) %>% 
  summarise(Profit =sum(Profit)) %>% 
  ggplot(aes(x=reorder(Store_Name,-Profit) , y =Profit))+
  geom_col(aes(fill= Store_Drive_Through )) +
  labs(title = "Har drive-trough noe å si på lønnsomheten?",
       subtitle = "Burde det nye utsalgsstedet ha drive-trough?", 
       fill = 'Drive Through?' )+
  ylab("Profitt i USD")+
  xlab("Utsalgssted")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=15, vjust = 0.4))
  
  
  

