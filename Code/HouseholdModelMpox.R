library(tidyverse)
library(tidycensus)
library(cowplot)

# #Below, I was using a different variable from the CDC study for household size
# #No variable for Family household with 1-person
# house_cdc_vars<-c(
#   two_person_Fam="B11016_003",
#   three_person_Fam="B11016_004",
#   four_person_Fam="B11016_005",
#   five_person_Fam="B11016_006",
#   six_person_Fam="B11016_007",
#   seven_plus_Fam="B11016_008",
#   one_person_nonFam="B11016_010",
#   two_person_nonFam="B11016_011",
#   three_person_nonFam="B11016_012",
#   four_person_nonFam="B11016_013",
#   five_person_nonFam="B11016_014",
#   six_person_nonFam="B11016_015",
#   seven_plus_nonFam="B11016_016"
# )
# 
# load_variables(2020, "acs5", cache = TRUE) %>%
#   filter(str_detect(name,"B11016")) %>% print(n=20)
# 
# #Extracting race in each zip code in TX
# house_from_cdc <- get_acs(
#   geography = "zcta",
#   #  state = "TX",
#   variables = house_cdc_vars,
#   summary_var = "B11016_002",
#   year = 2020
# ) 
# 
# save(house_from_cdc,file="~/Documents/GitHub/Mpox_2024/Data/house_from_cdc.RData")


# ##### Data to get distribution of people in house using the
# ##### crowd index of the SVI
# vars_crowding<-c("B25024_007E","B25024_008E","B25024_009E","B25024_001E", #Percent of Housing Units with 10+ Units in Structure
#                  "B25033_006E","B25033_007E","B25033_012E","B25033_013E","B25033_001E",#Percent of Population Living in Mobile Homes
#                  #Percent of Population Living In Accommodations with Less Than 1 Room Per Person/Crowding - At Household Level, Occupied Housing Units, More People Than Rooms Estimate
#                  "B25014_005E","B25014_006E","B25014_007E","B25014_011E","B25014_012E","B25014_013E","B25014_001E",
#                  "B25044_003E","B25044_010E","B25044_001E", #Percent of Population with No Vehicle Available
#                  "B26001_001E","B01003_001E") #Percent of Population Living in Group Quarters
# for_crowding_index <- get_acs(
#   geography = "zcta",
#   #  state = "TX",
#   variables = vars_crowding,
# #  summary_var = "B11016_001",
#   year = 2020
# ) 
# 
# data_crowding<-for_crowding_index %>% select(Zip=GEOID,variable,estimate) %>%
#   mutate(variable=paste0(variable,"E")) %>%
#   pivot_wider(names_from = "variable",values_from = "estimate") %>%
#   mutate(MUNIT=(B25024_007E+B25024_008E+B25024_009E)/B25024_001E, #PER_MULTI_DWELL
#          MOBILE=(B25033_006E+B25033_007E+B25033_012E+B25033_013E)/B25033_001E, #PER_MOBILE_DWEL
#          CROWD=(B25014_005E+B25014_006E+B25014_007E+B25014_011E+B25014_012E+B25014_013E)/B25014_001E, #PER_CROWD_DWELL
#          NOVEH=(B25044_003E+B25044_010E)/B25044_001E, #PER_NO_VEH_AVAIL
#          GROUPQ=B26001_001E/B01003_001E) #PER_GROUP_DWELL
# j <- data_crowding$RNKMUNIT <- rank(x = -data_crowding$MUNIT, na.last = "keep", ties.method = "max")
# k <- data_crowding$RNKMOBILE <- rank(x = -data_crowding$MOBILE, na.last = "keep", ties.method = "max")
# l <- data_crowding$RNKCROWD <- rank(x = -data_crowding$CROWD, na.last = "keep", ties.method = "max")
# m <- data_crowding$RNKNOVEH <- rank(x = -data_crowding$NOVEH, na.last = "keep", ties.method = "max")
# n <- data_crowding$RNKGROUPQ <- rank(x = -data_crowding$GROUPQ, na.last = "keep", ties.method = "max")
# #Sum The Ranks
# data_crowding$SUMRANK <- j+k+l+m+n
# #Derive the Adaptive Capacity Index
# data_crowding$CROWD_INDEX <- dplyr::percent_rank(data_crowding$SUMRANK)
# save(data_crowding,file="~/Documents/GitHub/Mpox_2024/Data/data_crowding.RData")

# # This part if to get the distributions by age in all zip codes in the US
# #I will estimate the age group distribution in households
# # Get data for age groups 0-5 and 6-17
# under_18_data <- get_acs(
#   geography = "zcta",   # Adjust to "state" or "tract" if needed
#   table = "B09001",
#   year = 2020,            # Use the ACS year you need
#   survey = "acs5"
# )
# 
# # Filter and summarize for age groups
# under_18_summary <- under_18_data %>%
#   filter(variable %in% c("B09001_004", "B09001_005")) %>%
#   mutate(age_group = case_when(
#     variable == "B09001_004" ~ "0-5",
#     variable == "B09001_005" ~ "6-17"
#   )) %>%
#   group_by(GEOID, age_group) %>%
#   summarise(total = sum(estimate))
# 
# # Get data for 18+ population
# over_18_data <- get_acs(
#   geography = "zcta",   # Adjust to "state" or "tract" if needed
#   table = "B01001",
#   year = 2020,
#   survey = "acs5"
# )
# 
# # Filter for variables corresponding to age 18+ and summarize
# over_18_summary <- over_18_data %>%
#   filter(variable %in% c(
#     # Male 18+
#     "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", 
#     "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", 
#     "B01001_017", 
#     # Female 18+
#     "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", 
#     "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", 
#     "B01001_041"
#   )) %>%
#   group_by(GEOID) %>%
#   summarise(
#     age_group = "18+",
#     total = sum(estimate, na.rm = TRUE)
#   )
# # Combine all age group data
# age_distribution <- bind_rows(under_18_summary, over_18_summary)
# save(age_distribution,file="~/Documents/GitHub/Mpox_2024/Data/age_distribution.RData")

#This gives me the SVI of all US using acs5 (2016-2020)
load("~/Documents/GitHub/Mpox_2024/Data/sviAll_US.RData")
sviAllUS_ZCTA

#Number of households with number of inhabitants
load("~/Documents/GitHub/Mpox_2024/Data/house_from_cdc.RData")
house_from_cdc

total_number_homes<-house_from_cdc %>% select(type=variable,totalHomes=estimate) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(type) %>% summarise_each(sum) %>% 
  mutate(probHomes=totalHomes/sum(totalHomes))

total_number_homes %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=probHomes)) + geom_col()

#Now, I get prob of household by SVI group
total_homes_SVIgroup<-house_from_cdc %>%
  merge(sviAllUS_ZCTA %>% drop_na(),by.x="GEOID",by.y="Zip") %>%
  mutate(SVIGroup=cut(SVI,quantile(SVI),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
#  filter(SVIGroup %in% c("1","4")) %>%
  select(SVIGroup,type=variable,totalHomes=estimate) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(SVIGroup,type) %>% summarise_each(sum) %>% ungroup() %>%
  group_by(SVIGroup) %>%
  mutate(Suma=sum(totalHomes),pobHomes=totalHomes/sum(totalHomes))

total_homes_SVIgroup %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=pobHomes,fill=SVIGroup)) +
  geom_col(position = position_dodge())

#I verify that the distributions are statistically different (spoilers alert, they arent! :( )
group1<-total_homes_SVIgroup %>% filter(SVIGroup=="1") %>% pull(pobHomes)
group4<-total_homes_SVIgroup %>% filter(SVIGroup=="3") %>% pull(pobHomes)
# Wilcoxon Rank-Sum Test
wilcox.test(group1, group4)
# Kolmogorov-Smirnov Test
ks.test(group1, group4)

all_prob_of_homes<-list(for_all_US=total_number_homes %>% slice(3,7,6,2,1,5,4) %>% pull(probHomes),
                        for_SVIGroup_1=total_homes_SVIgroup %>% filter(SVIGroup=="1") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_SVIGroup_2=total_homes_SVIgroup %>% filter(SVIGroup=="2") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_SVIGroup_3=total_homes_SVIgroup %>% filter(SVIGroup=="3") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_SVIGroup_4=total_homes_SVIgroup %>% filter(SVIGroup=="4") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes)
)

#Data for crowding and crowd index
load("~/Documents/GitHub/Mpox_2024/Data/data_crowding.RData")
data_crowding

data_crowding %>% select(Zip,CROWD_INDEX) %>% arrange(desc(CROWD_INDEX))
only_crowd<-data_crowding %>% select(-contains("_0")) %>%
  arrange(CROWD_INDEX) %>% select(GEOID=Zip,CROWD) %>% arrange(desc(CROWD)) %>% drop_na() %>%
  mutate(CrowdGroup = ntile(CROWD, 4)) #%>%  # Split into 4 groups
#  mutate(CrowdGroup = as.factor(CrowdGroup)) %>% filter(CrowdGroup %in% c("1","4"))
crowding_groups<-data_crowding %>% select(GEOID=Zip,CROWD_INDEX) %>% drop_na() %>%
  mutate(CrowdGroup=cut(CROWD_INDEX,quantile(CROWD_INDEX),include.lowest=TRUE,labels=FALSE) %>% as.factor()) #%>%
#  arrange(CrowdGroup) %>% filter(CrowdGroup %in% c("1","4"))

household_dist_crowdIndex<-house_from_cdc %>% select(GEOID,variable,estimate,summary_est) %>%
  left_join(crowding_groups) %>% arrange(GEOID) %>% drop_na() %>%
  select(CrowdGroup,type=variable,estimate) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(CrowdGroup,type) %>% summarise_each(sum) %>%
  ungroup() %>% group_by(CrowdGroup) %>% mutate(total=sum(estimate)) %>%
  mutate(prob_household=estimate/total)

household_dist_crowdIndex %>%
    mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                     "four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=prob_household,fill=CrowdGroup)) +
  geom_col(position = position_dodge()) #+ facet_wrap(~name,scales = "free_y")

household_dist_crowdOnly<-house_from_cdc %>% select(GEOID,variable,estimate,summary_est) %>%
  left_join(only_crowd) %>% arrange(GEOID) %>% drop_na() %>%
  select(CrowdGroup,type=variable,estimate) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(CrowdGroup,type) %>% summarise_each(sum) %>%
  mutate(total=sum(estimate),prob_of_household=estimate/total)

household_dist_crowdOnly %>%
  mutate(type=type %>% fct_relevel(c("one_person","two_person","three_person",
                                             "four_person","five_person","six_person","seven_plus")))  %>%
  ggplot(aes(x=type,y=prob_of_household,fill=as.factor(CrowdGroup))) +
  geom_col(position = position_dodge())

group1 <- household_dist_crowdOnly %>% filter(CrowdGroup=="1") %>% pull(prob_of_household)
group2 <- household_dist_crowdOnly %>% filter(CrowdGroup=="2") %>% pull(prob_of_household)
group3 <- household_dist_crowdOnly %>% filter(CrowdGroup=="3") %>% pull(prob_of_household)
group4 <- household_dist_crowdOnly %>% filter(CrowdGroup=="4") %>% pull(prob_of_household)

group1 <- household_dist_crowdIndex %>% filter(CrowdGroup=="1") %>% pull(prob_household)
group2 <- household_dist_crowdIndex %>% filter(CrowdGroup=="2") %>% pull(prob_household)
group3 <- household_dist_crowdIndex %>% filter(CrowdGroup=="3") %>% pull(prob_household)
group4 <- household_dist_crowdIndex %>% filter(CrowdGroup=="4") %>% pull(prob_household)

# Wilcoxon Rank-Sum Test
wilcox.test(group1, group4)
# Kolmogorov-Smirnov Test
ks.test(group1, group4)

all_prob_of_homes<-list(for_all_US=total_number_homes %>% slice(3,7,6,2,1,5,4) %>% pull(probHomes),
                        for_SVIGroup_1=total_homes_SVIgroup %>% filter(SVIGroup=="1") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_SVIGroup_2=total_homes_SVIgroup %>% filter(SVIGroup=="2") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_SVIGroup_3=total_homes_SVIgroup %>% filter(SVIGroup=="3") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_SVIGroup_4=total_homes_SVIgroup %>% filter(SVIGroup=="4") %>% slice(3,7,6,2,1,5,4) %>% pull(pobHomes),
                        for_crowdIndex_1=household_dist_crowdIndex %>% filter(CrowdGroup=="1") %>% slice(3,7,6,2,1,5,4) %>% pull(prob_household),
                        for_crowdIndex_2=household_dist_crowdIndex %>% filter(CrowdGroup=="2") %>% slice(3,7,6,2,1,5,4) %>% pull(prob_household),
                        for_crowdIndex_3=household_dist_crowdIndex %>% filter(CrowdGroup=="3") %>% slice(3,7,6,2,1,5,4) %>% pull(prob_household),
                        for_crowdIndex_4=household_dist_crowdIndex %>% filter(CrowdGroup=="4") %>% slice(3,7,6,2,1,5,4) %>% pull(prob_household)
)

#### Data retrieving and cleaning ----

#Age distribution in groups in all Zip codes US
load("~/Documents/GitHub/Mpox_2024/Data/age_distribution.RData")
age_distribution

# Pivot for a cleaner view
age_distribution_final <- age_distribution %>%
  pivot_wider(names_from = age_group, values_from = total) %>%
  mutate(total_population = `0-5` + `6-17` + `18+`)

age_group_probabilitites<-age_distribution %>% ungroup() %>% select(-GEOID) %>%
  group_by(age_group) %>% summarise_each(sum) %>%
  mutate(total_pop=sum(total),prob_age_group=total/total_pop) 

age_dist_svi_groups<-age_distribution %>%
  merge(sviAllUS_ZCTA %>% drop_na(),by.x="GEOID",by.y="Zip") %>% as_tibble() %>%
  mutate(SVIGroup=cut(SVI,quantile(SVI),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
  select(-GEOID,-SVI) %>% group_by(SVIGroup,age_group) %>%
  summarise_each(sum) %>% group_by(SVIGroup) %>% 
  mutate(total_in_group=sum(total),prob_age_svi=total/total_in_group)

age_dist_svi_groups %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-5","6-17","18+"))) %>%
  ggplot(aes(x=age_group,y=prob_age_svi,fill=SVIGroup)) + 
  geom_col(position = position_dodge())

age_dist_crowdI_groups<-age_distribution %>%
  merge(crowding_groups %>% select(-CROWD_INDEX)) %>% as_tibble() %>%
  select(-GEOID) %>% group_by(CrowdGroup,age_group) %>%
  summarise_each(sum) %>% group_by(CrowdGroup) %>% 
  mutate(total_in_group=sum(total),prob_age_crowdI=total/total_in_group)

age_dist_crowdI_groups %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-5","6-17","18+"))) %>%
  ggplot(aes(x=age_group,y=prob_age_crowdI,fill=CrowdGroup)) + 
  geom_col(position = position_dodge())

#Distribution of population according to age groups
all_age_group_dists <- list(age_probs_all_US=age_group_probabilitites %>% slice(1,3,2) %>% pull(prob_age_group),
                            age_probs_group1=age_dist_svi_groups %>% filter(SVIGroup=="1") %>% slice(1,3,2) %>% pull(prob_age_svi),
                            age_probs_group2=age_dist_svi_groups %>% filter(SVIGroup=="2") %>% slice(1,3,2) %>% pull(prob_age_svi),
                            age_probs_group3=age_dist_svi_groups %>% filter(SVIGroup=="3") %>% slice(1,3,2) %>% pull(prob_age_svi),
                            age_probs_group4=age_dist_svi_groups %>% filter(SVIGroup=="4") %>% slice(1,3,2) %>% pull(prob_age_svi),
                            age_probs_crowd1=age_dist_crowdI_groups %>% filter(CrowdGroup=="1") %>% slice(1,3,2) %>% pull(prob_age_crowdI),
                            age_probs_crowd2=age_dist_crowdI_groups %>% filter(CrowdGroup=="2") %>% slice(1,3,2) %>% pull(prob_age_crowdI),
                            age_probs_crowd3=age_dist_crowdI_groups %>% filter(CrowdGroup=="3") %>% slice(1,3,2) %>% pull(prob_age_crowdI),
                            age_probs_crowd4=age_dist_crowdI_groups %>% filter(CrowdGroup=="4") %>% slice(1,3,2) %>% pull(prob_age_crowdI)
                            )

save(all_prob_of_homes,file="~/Documents/GitHub/Mpox_2024/Data/all_prob_of_homes.RData")
save(all_age_group_dists,file="~/Documents/GitHub/Mpox_2024/Data/all_age_group_dists.RData")

##

#### Simulations ----
library(dplyr)
library(ggplot2)

# Set up initial parameters (same as before)
n_households <- 1000  # Number of households to simulate
n_days <- 100  # Number of days for simulation

# Define household sizes and age groups for SVIGroup 4
household_sizes <- c("one_person", "two_person", "three_person", "four_person", "five_person", "six_person", "seven_plus")
prob_SVIGroup4 <- c(0.307, 0.234, 0.163, 0.0264, 0.0871, 0.0377, 0.0314)
age_groups <- c("0-5", "5-17", "18+")  # Age groups for household members
age_group_probs <- c(0.15, 0.20, 0.65)  # Approximate age group distribution

# Disease transmission parameters
beta_within <- 0.3  # Transmission rate within households (everyone interacts with everyone)
beta_between <- 0.05  # Transmission rate between households (more likely for 0-5 group)
gamma <- 0.1  # Recovery rate
sigma <- 0.2  # Rate of becoming infectious after exposure

# New Parameters for Incubation and Infectious Periods
incubation_mean <- 8  # Mean incubation period in days
infectious_mean <- 27  # Mean infectious period in days

# Initialize household and agent data
set.seed(123)  # For reproducibility

# Define a mapping for household sizes to numeric values
household_size_map <- c(
  "one_person" = 1,
  "two_person" = 2,
  "three_person" = 3,
  "four_person" = 4,
  "five_person" = 5,
  "six_person" = 6,
  "seven_plus" = 7  # Or you can specify a larger number like 7 or use the max number.
)

# Generate households based on SVIGroup 4 distribution
households <- data.frame(
  household_id = 1:n_households,
  size_household = sample(household_sizes, n_households, replace = TRUE, prob = prob_SVIGroup4)
)

# Generate individual agents with age groups within households
agents <- households %>%
  rowwise() %>%
  mutate(agents = list(data.frame(
    household_id = household_id,
    age_group = sample(age_groups, size = household_size_map[size_household], replace = TRUE, prob = age_group_probs),
    state = "S",  # All start as Susceptible (S)
    days_in_state = 0  # Initialize a counter for days in current state
  ))) %>%
  unnest(agents, names_repair = "unique") %>%
  mutate(row_id = row_number()) %>% # Add row number for indexing
  rename(household_id = household_id...1) %>% select(-"household_id...3") # Rename to the original household_id name

# Track infected households and agents by age group
households_infected <- numeric(n_days)  # To store the number of infected households each day
age_group_infected <- data.frame(
  day = 1:n_days,
  "0-5" = 0,
  "5-17" = 0,
  "18+" = 0
)

# Initialize one child in the 0-5 age group as exposed (E)
child_index <- agents %>%
  filter(age_group == "0-5") %>%
  slice_sample(n = 1) %>%
  pull(row_id)

agents$state[agents$row_id == child_index] <- "E"  # Set the selected child to exposed

# Function to simulate one day in the SEIR model with incubation and infectious periods
simulate_day <- function(agents) {
  
  # 1. Within-household transmission: Everyone interacts with everyone in the household
  for (household in unique(agents$household_id)) {
    household_agents <- agents %>% filter(household_id == household)
    if (any(household_agents$state == "I")) {  # If there's an infectious agent in the household
      susceptible_agents <- household_agents %>% filter(state == "S")
      # All susceptible agents within the household can be exposed to infection
      newly_exposed <- rbinom(nrow(susceptible_agents), 1, beta_within)
      agents$state[agents$household_id == household & agents$state == "S" & newly_exposed == 1] <- "E"
    }
  }
  
  # 2. Between-household transmission: Focused on the 0-5 age group, large group gatherings
  susceptible_0_5 <- agents %>% filter(age_group == "0-5" & state == "S")
  
  # Simulate large group gatherings outside households for 0-5 group (20-30 agents)
  if (nrow(susceptible_0_5) >= 20) {
    sample_size <- sample(20:30, 1)  # Random group size between 20 and 30
    gathered_agents <- sample(susceptible_0_5$row_id, sample_size)
    
    # These gathered agents interact with each other and have a chance of being exposed
    for (i in gathered_agents) {
      if (agents$state[i] == "S" && any(agents$state[gathered_agents] == "I")) {
        agents$state[i] <- ifelse(rbinom(1, 1, beta_between) == 1, "E", agents$state[i])
      }
    }
  }
  
  # 3. Update states based on SEIR transitions (using incubation and infectious periods)
  agents <- agents %>%
    mutate(
      # Transition from Exposed (E) to Infectious (I) based on incubation period
      state = case_when(
        state == "E" & days_in_state >= rexp(n(), 1 / incubation_mean) ~ "I",
        # Transition from Infectious (I) to Recovered (R) based on infectious period
        state == "I" & days_in_state >= rexp(n(), 1 / infectious_mean) ~ "R",
        TRUE ~ state
      ),
      # Increment days in current state
      days_in_state = days_in_state + 1
    )
  
  return(agents)
}

# Run the simulation for n_days and track infected households and agents
for (day in 1:n_days) {
  agents <- simulate_day(agents)
  
  # Track infected households (if any agent in the household is infected)
  households_infected[day] <- sum(agents %>%
                                    group_by(household_id) %>%
                                    filter(any(state == "I")) %>%
                                    summarise(infected = 1) %>%
                                    pull(infected))
  
  # Track infected agents by age group
  for (age_group in age_groups) {
    age_group_infected[day, age_group] <- sum(agents$state[agents$age_group == age_group] == "I")
  }
}

# Create data frames for plotting
infected_households_df <- data.frame(
  day = 1:n_days,
  infected_households = households_infected
)

# Plot the number of infected households over time
ggplot(infected_households_df, aes(x = day, y = infected_households)) +
  geom_line(color = "blue") +
  labs(title = "Number of Infected Households Over Time",
       x = "Day",
       y = "Number of Infected Households")

# Plot the number of infected agents by age group over time
age_group_infected_long <- age_group_infected %>%
  pivot_longer(cols = c("0-5", "5-17", "18+"), names_to = "age_group", values_to = "infected_agents")

ggplot(age_group_infected_long, aes(x = day, y = infected_agents, color = age_group)) +
  geom_line() +
  labs(title = "Number of Infected Agents by Age Group Over Time",
       x = "Day",
       y = "Number of Infected Agents") +
  scale_color_manual(values = c("0-5" = "red", "5-17" = "green", "18+" = "blue"))

#### Other more optimized code ------

library(dplyr)
library(ggplot2)
library(data.table)  # For faster data manipulation

#This is the main function for the disease dynamics
simulate_day <- function(agents, beta_within, beta_between, incubation_mean, infectious_mean) {
  # 1. Within-household transmission: All agents within the household interact with each other
  household_groups <- split(agents, by = "household_id")
  
  for (household in household_groups) {
    if (any(household$state == "I")) {  # If there's an infectious agent in the household
      #      print(household)
      susceptible_agents <- household[state == "S"]
      #      print(paste0("susceptible_agents: ",susceptible_agents))
      # All susceptible agents within the household can be exposed to infection
      #      print(paste0("Susceptibles in House: ",nrow(susceptible_agents)))
      newly_exposed <- rbinom(nrow(susceptible_agents), 1, beta_within)
      #      print(paste0("Value of newly_exposed: ",newly_exposed))
      agents[household_id %in% household$household_id & state == "S", `:=`(
        state = ifelse(newly_exposed == 1, "E", state),
        days_in_state = ifelse(newly_exposed == 1, 0, days_in_state)
      )]
      #      print(agents[state == "E", .(row_id, days_in_state)])
      #      household[state == "S", state := ifelse(newly_exposed == 1, "E", state)]  # Update the agents table
      #      print(household)
    }
  }
  # Increment days_in_state for all agents that didn't just transition
  agents[state %in% c("S", "E", "I"), days_in_state := days_in_state + 1]
  # 2. Between-household transmission: Large group gatherings for the 0-5 age group only
  all_agents_0_5 <- agents[age_group == "0-5"]
  if (nrow(all_agents_0_5) >= 16) {  
    sample_size <- sample(6:16, 1) # According to https://nrckids.org/files/CFOC4%20pdf-%20FINAL.pdf page 4: 6 -16
    gathered_agents <- sample(all_agents_0_5$row_id, sample_size)
    
    infected_agents_in_group <- agents[gathered_agents, state] == "I"
    if (any(infected_agents_in_group)) {
      for (i in gathered_agents) {
        if (agents[i, state] == "S") {
          agents[i, state := ifelse(rbinom(1, 1, beta_between) == 1, "E", state)]
        }
      }
    }
  }
  
  # 3. SEIR Transitions:
  # (a) Transition exposed (E) to infectious (I)
  agents[state == "E", new_state := ifelse(days_in_state >= rexp(.N, 1 / incubation_mean), "I", state)]
  agents[state == "E" & new_state == "I", days_in_state := 0]  # Reset only for those transitioning to "I"
  agents[state == "E", state := new_state]
  agents[, new_state := NULL]
  #  print(agents[state=="E"])
  
  # (b) Transition infectious (I) to recovered (R)
  agents[state == "I", new_state := ifelse(days_in_state >= rexp(.N, 1 / infectious_mean), "R", state)]
  agents[state == "I" & new_state == "R", days_in_state := 0]  # Reset only for those transitioning to "R"
  agents[state == "I", state := new_state]
  agents[, new_state := NULL]
  #  print(agents[state=="I"])
  
  # Tracking information by age group
  infected_by_age_group <- agents[state == "I", .N, by = age_group]
  recovered_by_age_group <- agents[state == "R", .N, by = age_group]
  
  # Merge infected and recovered counts by age group into a single table
  state_counts <- merge(
    infected_by_age_group[, .(age_group, infected_count = N)],
    recovered_by_age_group[, .(age_group, recovered_count = N)],
    by = "age_group",
    all = TRUE
  )
  
  # Fill NA values with 0 for any age groups without infections or recoveries
  state_counts[is.na(state_counts)] <- 0
  
  # Count households with infected members
  num_infected_households <- length(unique(agents[state == "I", household_id]))
  # Count households with infected members and calculate household sizes
  infected_households <- agents[state == "I", .(household_id)]
  infected_households <- unique(infected_households)[, .N, by = household_id]
  infected_households_sizes <- agents[household_id %in% infected_households$household_id, .N, by = household_id]
  
  # Return the updated agents, daily tracking data, and infected household count
  return(list(
    agents = agents,
    num_infected_households = num_infected_households,
    num_infected_households = nrow(infected_households),
    infected_household_sizes = infected_households_sizes,
    state_counts = state_counts
  ))
}

#These are the distributions of household size for each type (rent, own, both),
#and for each SVI group (1, 4).
#load("~/Documents/GitHub/Mpox_2024/Data/dist_of_people_in_homes.RData")
#dist_of_people_in_homes
all_prob_of_homes
#Dist of people in household by age
all_age_group_dists

# Set up initial parameters
n_households <- 5000  # Increased number of households
n_days <- 200  # Number of days for simulation
runs<-1000
# Define household sizes and age groups for SVIGroup 4
#household_sizes <- c("one_person", "two_person", "three_person", "four_person", "five_person", "six_person", "seven_plus")
#prob_SVIGroup4 <- c(0.307, 0.234, 0.163, 0.0264, 0.0871, 0.0377, 0.0314)
#prob_SVIGroup1 <- c(0.320, 0.300, 0.150, 0.0231, 0.0624, 0.0223, 0.0122)
#prob_houses<- dist_of_people_in_homes$forRent$forProbs$group1
#prob_houses <- dist_of_people_in_homes[[3]]$forProbs[[1]]

#In dist_of_people_in_homes, the first number is for type (1=Rent, 2=Own, 3=Both)
#The second number is for SVI group (1=group 1, 2, group 4)
#dist_of_people_in_homes[[1]]$forProbs[[1]]
age_groups <- c("0-5", "5-17", "18+")  # Age groups for household members
#age_group_probs <- c(0.0422, 0.0201, 0.938)  # Age group dist according to ACS

# New Parameters for Incubation and Infectious Periods
incubation_mean <- 8  # Mean incubation period in days
infectious_mean <- 27  # Mean infectious period in days

# Disease transmission parameters
SAR=0.30 # Secondary Attack Rate
beta_within <- -log(1-SAR)/infectious_mean # Assuming all household members interact equally
#beta_within <- 0.15  # Transmission rate within households
beta_between <- beta_within/3  # Transmission rate between households

# Initialize household and agent data
#set.seed(12345)  # For reproducibility

# Generate households based on SVIGroup 4 distribution (use data.table for fast indexing)
household_sizes_map <- c("one_person" = 1, "two_person" = 2, "three_person" = 3, "four_person" = 4,
                         "five_person" = 5, "six_person" = 6, "seven_plus" = 7)

households <- data.table(
  household_id = 1:n_households,
  size_household = sample(household_sizes, n_households, replace = TRUE, prob = all_prob_of_homes$for_all_US)
)

agents <- households[, .(agents = list({
  # Initialize a household's agents
  household_id <- .BY$household_id
  household_size <- household_sizes_map[size_household]
  assigned_age_groups <- sample(age_groups, size = household_size, replace = TRUE, prob = all_age_group_dists$age_probs_all_US)
  
  # Ensure 0-5 or 5-17 are not alone
  if (any(assigned_age_groups %in% c("0-5", "5-17")) && !any(assigned_age_groups == "18+")) {
    # Replace one agent's age group with 18+
    replacement_index <- sample(seq_along(assigned_age_groups), 1)
    assigned_age_groups[replacement_index] <- "18+"
  }
  # Create the data.table for agents in this household
  data.table(
    household_id = household_id,
    age_group = assigned_age_groups,
    state = "S",  # All start as Susceptible (S)
    days_in_state = 0  # Initialize a counter for days in current state
  )
})), by = household_id]

agents <- rbindlist(agents$agents)

#Mean number of people household
agents %>% select(household_id) %>% group_by(household_id) %>% count() %>% pull(n) %>% mean()

agents %>% select(household_id,age_group) %>% pull(age_group) %>% table()

#Households with (and how many) children
agents %>% select(household_id,age_group) %>%
  mutate(WithChildren=ifelse(age_group=="0-5","Yes","No")) %>%
  filter(WithChildren=="Yes") %>% select(household_id) %>% group_by(household_id) %>%
  count() %>% arrange(desc(n))

#household_sizes_map_1 <- data_frame(size_household=c("one_person","two_person","three_person","four_person",
#                                                    "five_person","six_person","seven_plus"),
#                                    number_member=c(1,2,3,4,5,6,7))
#type=1
#g=1

forAllResultsAndSimulations<-list(forAllUS_result=list(),
                                  forGroup1_result=list(),
                                  forGroup4_result=list())
all_prob_of_homes
all_age_group_dists

for (type in 1:length(all_prob_of_homes)) {
  prob_houses <- all_prob_of_homes[[type]] #Distribution of people in homes
  households <- data.table(
    household_id = 1:n_households,
    size_household = sample(household_sizes, n_households, replace = TRUE, prob = prob_houses)
  )
  all_simulations<-list()
  for(simu in 1:runs){
    # Generate individual agents with age groups within households
    agents <- households[, .(agents = list({
      # Initialize a household's agents
      household_id <- .BY$household_id
      household_size <- household_sizes_map[size_household]
#      assigned_age_groups <- sample(age_groups, size = household_size, replace = TRUE, prob = age_group_probs)
      assigned_age_groups <- sample(age_groups, size = household_size, replace = TRUE, prob = all_age_group_dists[[type]])
      
      # Ensure 0-5 or 5-17 are not alone
      if (any(assigned_age_groups %in% c("0-5", "5-17")) && !any(assigned_age_groups == "18+")) {
        # Replace one agent's age group with 18+
        replacement_index <- sample(seq_along(assigned_age_groups), 1)
        assigned_age_groups[replacement_index] <- "18+"
      }
      # Create the data.table for agents in this household
      data.table(
        household_id = household_id,
        age_group = assigned_age_groups,
        state = "S",  # All start as Susceptible (S)
        days_in_state = 0  # Initialize a counter for days in current state
      )
    })), by = household_id]
    
    # Unnest agents into a single data.table
    agents <- rbindlist(agents$agents)
    
    # Add row_id for sampling later
    agents[, row_id := .I]
    
    # Initialize one child in the 0-5 age group as exposed (E)
    # Step 1: Filter agents to get those in the 0-5 age group
    child_subset <- agents[age_group == "0-5"]
    # Step 2: Ensure the subset is not empty before sampling
    if (nrow(child_subset) > 0) {
      # Step 3: Sample one index from the subset of the 0-5 age group
      child_index <- sample(1:nrow(child_subset), 1)  # Correct way to sample
      # Step 4: Set the state of the selected agent in the full agents table to "E"
      selected_row_id <- child_subset[child_index, row_id]
      agents[row_id == selected_row_id, state := "I"]
    } else {
      print("No agents in the 0-5 age group.")
    }
    
    # Track infected households and agents by age group
    households_infected <- numeric(n_days)  # To store the number of infected households each day
    age_group_infected <- data.table(
      day = 1:n_days,
      "0-5" = 0,
      "5-17" = 0,
      "18+" = 0
    )
    
    # Initialize data storage for visualization
    households_infected_over_time <- c()
    infected_agents_by_age_group_over_time <- list()
    
    # Initialize cumulative counters
    cumulative_infected_households <- NULL
    cumulative_household_ids <- integer()
    
    # Initialize list to store results over multiple days
    time_series_log <- list()
    result<-NULL
    
    # Loop over simulation days
    for (g in 1:length(groupSizes)) {
      
    }
    for (day in 1:n_days) {
      result <- simulate_day(agents, beta_within, beta_between, incubation_mean, infectious_mean)
      agents <- result$agents
      
      # Extract daily tracking data
      daily_counts <- result$state_counts
      daily_counts[, day := day]  # Add day column for tracking
      daily_counts[, num_infected_households := result$num_infected_households]  # Add infected households count
      
      # Update cumulative data
      daily_households <- result$infected_household_sizes$household_id
      cumulative_household_ids <- unique(c(cumulative_household_ids, daily_households))
      cumulative_infected_households <- agents[household_id %in% cumulative_household_ids, .N, by = household_id]
      
      # Add cumulative counts to daily log
      daily_counts[, cumulative_infected_households := length(cumulative_household_ids)]
      
      # Append to log
      time_series_log[[day]] <- daily_counts
    }
    
    # Combine list into one data.table for full time-series analysis
    full_log <- rbindlist(time_series_log) %>% mutate(Total_ind=nrow(agents))
    
    #  full_log %>% tail()
    #  if(recuperados>10){              #No condition on size of outbreak, since we want 
    all_simulations[[simu]]<-full_log #to know how many of the simulations have large outbreaks
    #  }
    full_log<-NULL
  }
  
  forAllResultsAndSimulations[[type]]<-all_simulations
}

forAllResultsAndSimulations

#I made two runs assuming different sizes in gathering of children outside household

forAllResultsAndSimulations_with_between16 
forAllResultsAndSimulations_with_between30 
#Results for all types and SVI groups I am considering # If all good and simulations good, they are being saved here
#save(forAllResultsAndSimulations,file="~/Documents/GitHub/Mpox_2024/Data/forAllResultsAndSimulations.RData")

names(forAllResultsAndSimulations)

finalRecoveredAllSimulations_groups_16<-NULL
for(t in 1:length(forAllResultsAndSimulations_with_between16)){
  for (s in 1:length(forAllResultsAndSimulations_with_between16[[t]])) {
    uno<-forAllResultsAndSimulations_with_between16[[t]][[s]] %>% filter(day==150) %>%
      select(age_group,recovered_count,cumulative_infected_households,Total_ind) %>% 
      mutate(Simu=s,type=names(forAllResultsAndSimulations_with_between16)[t])
    finalRecoveredAllSimulations_groups_16<-rbind(finalRecoveredAllSimulations_groups_16,uno)
  }
}

finalRecoveredAllSimulations_groups_30<-NULL
for(t in 1:length(forAllResultsAndSimulations_with_between30)){
  for (s in 1:length(forAllResultsAndSimulations_with_between30[[t]])) {
    uno<-forAllResultsAndSimulations_with_between30[[t]][[s]] %>% filter(day==150) %>%
      select(age_group,recovered_count,cumulative_infected_households,Total_ind) %>% 
      mutate(Simu=s,type=names(forAllResultsAndSimulations_with_between30)[t])
    finalRecoveredAllSimulations_groups_30<-rbind(finalRecoveredAllSimulations_groups_30,uno)
  }
}

rbind(finalRecoveredAllSimulations_groups_16 %>% mutate(Between_size=16),
      finalRecoveredAllSimulations_groups_30 %>% mutate(Between_size=30)) %>%
  select(Between_size,type,Simu,recovered_count) %>% group_by(Between_size,type,Simu) %>%
  summarise_each(sum) %>% ungroup() %>% select(-Simu) %>%
  group_by(Between_size,type,recovered_count) %>% count() %>%
  ggplot(aes(x=recovered_count,y=n/1000,fill=type)) +
  geom_col(position = position_dodge()) + facet_wrap(~Between_size)

rbind(finalRecoveredAllSimulations_groups_16 %>% mutate(Between_size=16),
      finalRecoveredAllSimulations_groups_30 %>% mutate(Between_size=30)) %>%
  select(Between_size,type,Simu,cumulative_infected_households) %>% unique() %>%
  filter(cumulative_infected_households>0) %>%
  select(-Simu) %>% group_by(Between_size,type,cumulative_infected_households) %>% count() %>%
  ggplot(aes(x=as.factor(cumulative_infected_households),y=n/1000,fill=type)) +
  geom_col(position = position_dodge()) + facet_wrap(~Between_size)

#



## Individual runs -------
#This loop below is the one that is inside the loop above. I am just leaving it here to make
#sure that I did not broke anything in the functioning code
all_simulations<-list()
for(simu in 1:1000){
  # Generate individual agents with age groups within households
  agents <- households[, .(agents = list({
    # Initialize a household's agents
    household_id <- .BY$household_id
    household_size <- household_sizes_map[size_household]
    assigned_age_groups <- sample(age_groups, size = household_size, replace = TRUE, prob = age_group_probs)
    
    # Ensure 0-5 or 5-17 are not alone
    if (any(assigned_age_groups %in% c("0-5", "5-17")) && !any(assigned_age_groups == "18+")) {
      # Replace one agent's age group with 18+
      replacement_index <- sample(seq_along(assigned_age_groups), 1)
      assigned_age_groups[replacement_index] <- "18+"
    }
    # Create the data.table for agents in this household
    data.table(
      household_id = household_id,
      age_group = assigned_age_groups,
      state = "S",  # All start as Susceptible (S)
      days_in_state = 0  # Initialize a counter for days in current state
    )
  })), by = household_id]
  
  # Unnest agents into a single data.table
  agents <- rbindlist(agents$agents)
  
  # Add row_id for sampling later
  agents[, row_id := .I]
  
  # Initialize one child in the 0-5 age group as exposed (E)
  # Step 1: Filter agents to get those in the 0-5 age group
  child_subset <- agents[age_group == "0-5"]
  # Step 2: Ensure the subset is not empty before sampling
  if (nrow(child_subset) > 0) {
    # Step 3: Sample one index from the subset of the 0-5 age group
    child_index <- sample(1:nrow(child_subset), 1)  # Correct way to sample
    # Step 4: Set the state of the selected agent in the full agents table to "E"
    selected_row_id <- child_subset[child_index, row_id]
    agents[row_id == selected_row_id, state := "I"]
  } else {
    print("No agents in the 0-5 age group.")
  }
  
  # Track infected households and agents by age group
  households_infected <- numeric(n_days)  # To store the number of infected households each day
  age_group_infected <- data.table(
    day = 1:n_days,
    "0-5" = 0,
    "5-17" = 0,
    "18+" = 0
  )
  
  # Initialize data storage for visualization
  households_infected_over_time <- c()
  infected_agents_by_age_group_over_time <- list()
  
  # Initialize cumulative counters
  cumulative_infected_households <- NULL
  cumulative_household_ids <- integer()
  
  # Initialize list to store results over multiple days
  time_series_log <- list()
  result<-NULL
  
  # Loop over simulation days
  for (day in 1:n_days) {
    result <- simulate_day(agents, beta_within, beta_between, incubation_mean, infectious_mean)
    agents <- result$agents
    
    # Extract daily tracking data
    daily_counts <- result$state_counts
    daily_counts[, day := day]  # Add day column for tracking
    daily_counts[, num_infected_households := result$num_infected_households]  # Add infected households count
    
    # Update cumulative data
    daily_households <- result$infected_household_sizes$household_id
    cumulative_household_ids <- unique(c(cumulative_household_ids, daily_households))
    cumulative_infected_households <- agents[household_id %in% cumulative_household_ids, .N, by = household_id]
    
    # Add cumulative counts to daily log
    daily_counts[, cumulative_infected_households := length(cumulative_household_ids)]
    
    # Append to log
    time_series_log[[day]] <- daily_counts
  }
  
  # Combine list into one data.table for full time-series analysis
  full_log <- rbindlist(time_series_log) %>% mutate(Total_ind=nrow(agents))

#  full_log %>% tail()
#  if(recuperados>10){              #No condition on size of outbreak, since we want 
  all_simulations[[simu]]<-full_log #to know how many of the simulations have large outbreaks
#  }
  full_log<-NULL
}

finalRecoveredAllSimulations_SVIgroup4<-NULL
for (s in 1:length(all_simulations)) {
  uno<-all_simulations[[s]] %>% filter(day==200) %>%
    select(age_group,recovered_count,Total_ind) %>% mutate(Simu=s)
  finalRecoveredAllSimulations_SVIgroup4<-rbind(finalRecoveredAllSimulations_SVIgroup4,uno)
}

#This is with SVI group 4 run
all_simulationsSVIgroup4<-all_simulations

#This is with SVI group 1 run
all_simulationsSVIgroup1<-all_simulations

finalRecoveredAllSimulations_SVIgroup1
finalRecoveredAllSimulations_SVIgroup4

infectedPerSimulation_SVIgroup1<-finalRecoveredAllSimulations_SVIgroup1 %>% 
  select(Simu,recovered_count) %>% group_by(Simu)%>%
  summarise_each(sum) %>%
  mutate(new_recov=ifelse(recovered_count>5,">5",recovered_count))

infectedPerSimulation_SVIgroup4<-finalRecoveredAllSimulations_SVIgroup4 %>% 
  select(Simu,recovered_count) %>% group_by(Simu)%>%
  summarise_each(sum) %>%
  mutate(new_recov=ifelse(recovered_count>5,">5",recovered_count))

rbind(infectedPerSimulation_SVIgroup1 %>% mutate(Group="1"),
      infectedPerSimulation_SVIgroup4 %>% mutate(Group="4")) %>%
  mutate(new_recov=new_recov %>% fct_relevel(c("1","2","3","4","5",">5"))) %>%
  ggplot(aes(x=new_recov,fill=Group)) + geom_histogram(stat = "count",position = position_dodge())





