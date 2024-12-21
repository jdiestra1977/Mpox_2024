library(tidyverse)
library(readxl)
library(tidycensus)

setwd("~/Documents/GitHub/Mpox_2024/Data/Data_CDC/")

population_states_US <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2020#,
  #state="TX",
  #geometry = T
)

populationStates<-population_states_US %>% select(State=NAME,population=estimate) 
#I am adding two cities that are being considered in the Mpox data
populationStates<-rbind(populationStates,data.frame(State=c("New York City","Philadelphia"),
                                  population=c(8804190,1603797)))

cases<-read_xlsx("weeklyMpoxCases_CDC.xlsx")

cases %>% select(epi_date_V3_Week,sex_assigned_at_birth,COUNT) %>%
  group_by(epi_date_V3_Week,sex_assigned_at_birth) %>%
  summarise_each(sum) %>% filter(sex_assigned_at_birth %in% c("Female","Male")) %>%
  ggplot(aes(x=as.Date(epi_date_V3_Week),y=COUNT,fill=sex_assigned_at_birth))+
  geom_col(position=position_dodge()) + theme(legend.position = c(0.1,0.8))

allDates<-cases %>% pull(epi_date_V3_Week) %>% unique() %>% sort()
allStates<-cases %>% pull(jurisdiction) %>% unique() %>% sort()

dataMpoxCases<-cases %>% select(epi_date_V3_Week,jurisdiction,COUNT) %>%
  group_by(epi_date_V3_Week,jurisdiction) %>%
  summarise_each(sum) %>% arrange(jurisdiction) %>% group_by(jurisdiction) %>%
  mutate(epi_date_V3_Week=as.Date(epi_date_V3_Week)) %>%
  complete(epi_date_V3_Week = seq.Date(min(as.Date("2022-05-08")), max(as.Date("2022-08-28")), by="week")) %>%
  replace(is.na(.),0) %>% group_by(jurisdiction) %>% 
  mutate(AcumulatedCases=cumsum((COUNT)),totalCases=sum(COUNT)) %>% 
  left_join(populationStates %>% select(jurisdiction=State,population))  %>%
  mutate(casesPerCapita=AcumulatedCases/population,AcumulatedCasesPerCapita=cumsum((casesPerCapita)),
         totalCasesPerCapita=sum(casesPerCapita))

dataMpoxCases %>% filter(jurisdiction=="Philadelphia")

dataMpoxCases %>%
  #ggplot(aes(x=as.Date(epi_date_V3_Week),y=reorder(jurisdiction,totalCases),fill=log(AcumulatedCases+1))) +
  ggplot(aes(x=as.Date(epi_date_V3_Week),y=reorder(jurisdiction,totalCasesPerCapita),
             fill=log(casesPerCapita+0.000001))) +
  geom_tile() + scale_fill_gradient(low="yellow",high = "blue")

vaccines<-read_xlsx("weeklyVaccinesMpox_CDC.xlsx")

casesUSAndSex<-cases %>% select(-jurisdiction,-county) %>% 
  group_by(epi_date_V3_Week,sex_assigned_at_birth) %>%
  summarise_each(sum)

casesUSAndSex %>% filter(sex_assigned_at_birth %in% c("Male","Female")) %>%
  ggplot(aes(x=as.Date(epi_date_V3_Week),y=COUNT,fill=sex_assigned_at_birth)) +
  geom_col(position = position_dodge())

casesStateAndSex<-cases %>% select(-county) %>% 
  group_by(epi_date_V3_Week,jurisdiction,sex_assigned_at_birth) %>%
  summarise_each(sum)
  
casesStateAndSex %>% 
  filter(jurisdiction %in% c("California","Florida","Georgia","Illinois","New York City","Texas")) %>%
  filter(sex_assigned_at_birth %in% c("Male","Female")) %>%
  ggplot(aes(x=as.Date(epi_date_V3_Week),y=COUNT,fill=sex_assigned_at_birth)) +
  geom_col() + facet_wrap(~jurisdiction)

casesTexas<-cases %>% filter(jurisdiction %in% c("Texas")) %>%
#  filter(jurisdiction %in% c("California","Florida","Georgia","Illinois","New York City","Texas")) %>%
  select(-jurisdiction) %>% 
  group_by(epi_date_V3_Week,county,sex_assigned_at_birth) %>%
  summarise_each(sum)

casesTexas %>% 
  filter(county %in% c("Dallas County","Harris County","Tarrant County","Travis County","Bexar County",
                       "Bell County","Collin County","Fort Bend County","Hays County","Hidalgo County",
                       "Montgomery County")) %>%
  filter(sex_assigned_at_birth %in% c("Male","Female")) %>%
  ggplot(aes(x=as.Date(epi_date_V3_Week),y=COUNT,fill=sex_assigned_at_birth)) +
  geom_col(position = position_dodge()) + facet_wrap(~county,scales = "free_y")

casesTexas %>% 
  filter(county %in% c("Dallas County","Harris County","Tarrant County","Travis County")) %>%
  filter(sex_assigned_at_birth %in% c("Male","Female")) %>% ungroup() %>%
  select(-epi_date_V3_Week) %>% group_by(county,sex_assigned_at_birth) %>%
  summarise_each(sum) %>%
  ggplot(aes(x=county,y=COUNT,fill=sex_assigned_at_birth)) +
  geom_col(position = position_dodge())

vaccinesUS<-vaccines %>% select(-recip_address_county,-recip_address_state,-dose_num) %>%
  group_by(admin_date_Week,recip_sex) %>%
  summarise_each(sum)

vaccinesUS %>% filter(recip_sex %in% c("F","M")) %>%
  ggplot(aes(x=as.Date(admin_date_Week),y=COUNT,fill=recip_sex))+
  geom_col()

vaccinesStates<-vaccines %>% select(-recip_address_county,-dose_num) %>%
  group_by(admin_date_Week,recip_address_state,recip_sex) %>%
  summarise_each(sum)

vaccinesStates %>% filter(recip_sex %in% c("F","M")) %>%
  filter(recip_address_state %in% c("CA","FL","GA","IL","NY","TX")) %>%
  ggplot(aes(x=as.Date(admin_date_Week),y=COUNT,fill=recip_sex))+
  geom_col() + facet_wrap(~recip_address_state)










