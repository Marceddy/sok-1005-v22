library(tidyverse)
library(rvest)
library(rlist)


# I samarbeid med Christian Karlsen, Rudi Hansen og Yves Sebazungu


timeplan_URL <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list", 
                     "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2032-1&View=list",
                     "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2021-1&View=list")

timeplan_scrape <- function(url) {
  return(read_html(url) %>% 
           html_nodes(.,'table') %>% 
           html_table(., fill = TRUE) %>% 
           list.stack(.) %>% # 
           janitor::row_to_names(., 1) %>% 
           separate(Dato,
                    into = c("Dag", "Dato"),
                    sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
           .[-length(.$Dag),] %>%
           filter(!str_detect(Dato, "Dato"), 
                  !str_detect(Beskrivelse, "international|(WISEFLOW)| Alta")) %>% 
           zoo::na.locf(.) %>%  
           mutate(Dato=as.Date(Dato, format = "%d.%m.%Y"), 
                  Uke = strftime(Dato, format = "%V"), 
                  Dag = strftime(Dato, format = "%A") )%>% 
           select(Dag,Dato,Uke,Tid,Rom, Lærer))
}
timeplan <- map(Timeplan_URL, timeplan_scrape) %>% 
  bind_rows() %>% 
  arrange(.$Dato, .$Tid)
          
timeplan
          
          