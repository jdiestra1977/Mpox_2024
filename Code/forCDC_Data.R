library(tidyverse)
library(readxl)

setwd("~/Projects/Monkeypox/Data_CDC/")

cases<-read_xlsx("weeklyMpoxCases_CDC.xlsx")

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
  filter(county %in% c("Dallas County","Harris County","Tarrant County","Travis County")) %>%
  filter(sex_assigned_at_birth %in% c("Male","Female")) %>%
  ggplot(aes(x=as.Date(epi_date_V3_Week),y=COUNT,fill=sex_assigned_at_birth)) +
  geom_col(position = position_dodge()) + facet_wrap(~county)

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










