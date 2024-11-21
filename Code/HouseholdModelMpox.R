library(tidyverse)
library(tidycensus)

#### Data retrieving and cleaning ----

#B25009, B25010, B19019

load_variables(2018, "acs5", cache = TRUE) %>%
  filter(str_detect(name,"B25009")) %>% print(n=20)

### ???
household_size_tract <- get_acs(
  geography = "tract",
  state="TX",
  #  county="Travis",
  table = "B25009",
  year = 2020
)

household_size_state <- get_acs(
  geography = "state",
  # state="TX",
  #  county="Travis",
  table = "B25009",
  year = 2019
)

household_size_zip <- get_acs(
  geography = "zcta",
 # state="TX",
  #  county="Travis",
  table = "B25009",
  year = 2019
)

household_size %>% arrange(GEOID) %>% tail() %>%
  print(n=20)

#I have the data for number of households of a given size from 1 to 7+
#

household_size_zip


house_vars <- c(
  one_person = "B25009_003",
  two_person = "B25009_004",
  three_person = "B25009_005",
  four_person = "B03002_006",
  five_person = "B25009_007",
  six_person = "B25009_008",
  seven_plus= "B25009_009"
)
# 
# B25009_002 Estimate!!Total!!Owner occupied                              TENURE BY HOUSEHOLD SIZE block group
# 3 B25009_003 Estimate!!Total!!Owner occupied!!1-person household          TENURE BY HOUSEHOLD SIZE block group
# 4 B25009_004 Estimate!!Total!!Owner occupied!!2-person household          TENURE BY HOUSEHOLD SIZE block group
# 5 B25009_005 Estimate!!Total!!Owner occupied!!3-person household          TENURE BY HOUSEHOLD SIZE block group
# 6 B25009_006 Estimate!!Total!!Owner occupied!!4-person household          TENURE BY HOUSEHOLD SIZE block group
# 7 B25009_007 Estimate!!Total!!Owner occupied!!5-person household          TENURE BY HOUSEHOLD SIZE block group
# 8 B25009_008 Estimate!!Total!!Owner occupied!!6-person household          TENURE BY HOUSEHOLD SIZE block group
# 9 B25009_009

#Extracting race in each zip code in TX
test_house_owner <- get_acs(
  geography = "zcta",
#  state = "TX",
  variables = house_vars,
  summary_var = "B25009_002",
  year = 2019
) 

test_house_owner %>% arrange(GEOID) %>% print(n=20)

house_vars_rent <- c(
  one_person = "B25009_011",
  two_person = "B25009_012",
  three_person = "B25009_013",
  four_person = "B03002_014",
  five_person = "B25009_015",
  six_person = "B25009_016",
  seven_plus= "B25009_017"
)

test_house_rent <- get_acs(
  geography = "zcta",
  #  state = "TX",
  variables = house_vars_rent,
  summary_var = "B25009_010",
  year = 2019
) 

test_house_rent %>% arrange(GEOID)

load("~/Projects/SVITexas/SVI_all_zip_TX.RData")
zipsInTX<-SVI_all_zip_TX %>% pull(Zip) %>% unique()

#Removing Zip codes with 0 estimated in summary variable
quitarEstos<-test_house_rent %>% arrange(GEOID) %>% 
  merge(SVI_all_zip_TX,by.x="GEOID",by.y="Zip") %>%
  mutate(SVIGroup_whole=cut(SVI,quantile(SVI),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
  as_tibble() %>% filter(SVIGroup_whole %in% c("1","4")) %>%
  mutate(perHouseholds=estimate/summary_est) %>%
  filter(is.na(perHouseholds)) %>% pull(GEOID) %>% unique()

#This one has an infinite value
quitarEstos<-c(quitarEstos,"79344")

estimateCIs<-function(x){
  sample.mean <- mean(x)
  #  print(sample.mean)
  
  sample.n <- length(x)
  sample.sd <- sd(x)
  sample.se <- sample.sd/sqrt(sample.n)
  #  print(sample.se)
  
  alpha = 0.05
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  # print(t.score)
  
  margin.error <- t.score * sample.se
  lower.bound <- sample.mean - margin.error
  upper.bound <- sample.mean + margin.error
  list(lower.bound,upper.bound)
}

#Calculate CI of a data frame. Data has to be already clean and filtered
CI_of_dataframe<-function(x){
  grupos<-x$SVIGroup_whole %>% unique() %>% as.vector()
  sizes<-x$size_household %>% unique()
  CI_variables_after<-NULL
  CI_variables_after_same<-NULL
  for(g in 1:length(grupos)){
    for(n in 1:length(sizes)){
      ver_after_same<-x %>% ungroup() %>%
        filter(SVIGroup_whole==grupos[g],size_household==sizes[n])
      ver1_after_same<-data.frame(Lower_MR=estimateCIs(ver_after_same %>% pull(perHouseholds))[[1]],
                                  Upper_MR=estimateCIs(ver_after_same %>% pull(perHouseholds))[[2]]) %>%
        mutate(SVIGroup_whole=grupos[g],size_household=sizes[n])
#      ver2_after_same<-data.frame(Lower_visits=estimateCIs(ver_after_same %>% pull(totalVisits))[[1]],Upper_visits=estimateCIs(ver_after_same %>% pull(totalVisits))[[2]])
#      ver3_after_same<-cbind(ver1_after_same,ver2_after_same) %>% mutate(SVIGroup_whole=grupos[g],Level1=sizes[n])
      CI_variables_after_same<-rbind(CI_variables_after_same,ver1_after_same)
    }
  }
  return(CI_variables_after_same)
}

dataForProcessing<-test_house_rent %>% arrange(GEOID) %>% 
  merge(SVI_all_zip_TX,by.x="GEOID",by.y="Zip") %>%
  mutate(SVIGroup_whole=cut(SVI,quantile(SVI),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
  as_tibble() %>% filter(SVIGroup_whole %in% c("1","4")) %>%
  mutate(perHouseholds=estimate/summary_est) %>% filter(!GEOID %in% quitarEstos) %>%
  select(SVIGroup_whole,size_household=variable,perHouseholds) 

dataForProcessing<-dataForProcessing 

dataForProcessing %>% #pull(size_household) %>% unique()
#  arrange(desc(perHouseholds))
  group_by(SVIGroup_whole,size_household) %>% summarise_each(mean) %>% 
  left_join(CI_of_dataframe(dataForProcessing)) %>%
  mutate(size_household=size_household %>% 
           fct_relevel(c("one_person","two_person","three_person","four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=size_household,y=perHouseholds,fill=SVIGroup_whole)) +
  geom_col(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin=Lower_MR,ymax=Upper_MR),position = position_dodge(0.5),width=0.4)

#Data for counts of number of houses by size
data<-test_house_rent %>% arrange(GEOID) %>% 
  merge(SVI_all_zip_TX,by.x="GEOID",by.y="Zip") %>%
  mutate(SVIGroup_whole=cut(SVI,quantile(SVI),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
  as_tibble() %>% filter(SVIGroup_whole %in% c("1","4")) %>%
  mutate(perHouseholds=estimate/summary_est) %>% filter(!GEOID %in% quitarEstos) %>%
  select(SVIGroup_whole,size_household=variable,Counts=estimate) %>%
  group_by(SVIGroup_whole,size_household) %>% summarise_each(mean)

data %>%
  mutate(size_household=size_household %>% 
           fct_relevel(c("one_person","two_person","three_person","four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=size_household,y=Counts,fill=SVIGroup_whole)) +
  geom_col(position=position_dodge(0.5))

#This test is to verify if the distributions of each SVI group
#are different.
# Pivot data to wide format for chi-square test
data_wide <- data %>%
  pivot_wider(names_from = size_household, values_from = Counts) %>%
  ungroup()
# Remove the SVIGroup_whole column for the test
data_matrix <- as.matrix(data_wide %>% select(-SVIGroup_whole))
# Run chi-square test
chisq_test <- chisq.test(data_matrix)
chisq_test #According to this, distribution of counts are different
# Get the standardized residuals
residuals <- chisq_test$stdres
print(residuals)

# Now, using these results, we generate houses with these distributions

# Set up household sizes and probabilities for each SVIGroup
household_sizes <- c("one_person", "two_person", "three_person", "four_person", "five_person", "six_person", "seven_plus")

# Probabilities for SVIGroup 1 based on the observed percentages
prob_SVIGroup1 <- c(0.320, 0.300, 0.150, 0.0231, 0.0624, 0.0223, 0.0122)

# Probabilities for SVIGroup 4 based on the observed percentages
prob_SVIGroup4 <- c(0.307, 0.234, 0.163, 0.0264, 0.0871, 0.0377, 0.0314)

# Number of households to simulate for each group
n_households <- 5000

# Generate households for SVIGroup 1
set.seed(123)  # For reproducibility
households_SVIGroup1 <- sample(household_sizes, n_households, replace = TRUE, prob = prob_SVIGroup1)

# Generate households for SVIGroup 4
households_SVIGroup4 <- sample(household_sizes, n_households, replace = TRUE, prob = prob_SVIGroup4)

# Combine the data into a data frame with ordered household sizes
simulated_data <- data.frame(
  SVIGroup_whole = rep(c(1, 4), each = n_households),
  size_household = factor(c(households_SVIGroup1, households_SVIGroup4), levels = household_sizes)
)

# View a sample of the simulated data
head(simulated_data)

# Summarize the distributions, which will now be in sorted order
table(simulated_data$SVIGroup_whole, simulated_data$size_household)

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

# Set up initial parameters
n_households <- 500  # Increased number of households
n_days <- 200  # Number of days for simulation

# Define household sizes and age groups for SVIGroup 4
household_sizes <- c("one_person", "two_person", "three_person", "four_person", "five_person", "six_person", "seven_plus")
prob_SVIGroup4 <- c(0.307, 0.234, 0.163, 0.0264, 0.0871, 0.0377, 0.0314)
age_groups <- c("0-5", "5-17", "18+")  # Age groups for household members
age_group_probs <- c(0.15, 0.20, 0.65)  # Approximate age group distribution

# Disease transmission parameters
beta_within <- 1  # Transmission rate within households
beta_between <- 1  # Transmission rate between households
#gamma <- 0.1  # Recovery rate
#sigma <- 0.2  # Rate of becoming infectious after exposure

# New Parameters for Incubation and Infectious Periods
incubation_mean <- 8  # Mean incubation period in days
infectious_mean <- 27  # Mean infectious period in days

# Initialize household and agent data
#set.seed(12345)  # For reproducibility

# Generate households based on SVIGroup 4 distribution (use data.table for fast indexing)
household_sizes_map <- c("one_person" = 1, "two_person" = 2, "three_person" = 3, "four_person" = 4,
                         "five_person" = 5, "six_person" = 6, "seven_plus" = 7)

households <- data.table(
  household_id = 1:n_households,
  size_household = sample(household_sizes, n_households, replace = TRUE, prob = prob_SVIGroup4)
)

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

#agents[age_group=="0-5"] %>% nrow()

# Initialize one child in the 0-5 age group as exposed (E)
# Step 1: Filter agents to get those in the 0-5 age group
child_subset <- agents[age_group == "0-5"]
# Step 2: Ensure the subset is not empty before sampling
if (nrow(child_subset) > 0) {
  # Step 3: Sample one index from the subset of the 0-5 age group
  child_index <- sample(1:nrow(child_subset), 1)  # Correct way to sample
  # Step 4: Set the state of the selected agent in the full agents table to "E"
  selected_row_id <- child_subset[child_index, row_id]
#  agents[row_id == selected_row_id, state := "E"]
  agents[row_id == selected_row_id, state := "I"]
  # Optional: Verify the change
#  print(agents[state == "E"])
  print(agents[state == "I"])
} else {
  print("No agents in the 0-5 age group.")
}

ver<-agents[row_id==selected_row_id]$household
#print(agents[household_id==agents[row_id==selected_row_id]$household_id])
agents[household_id==ver]
# Track infected households and agents by age group
households_infected <- numeric(n_days)  # To store the number of infected households each day
age_group_infected <- data.table(
  day = 1:n_days,
  "0-5" = 0,
  "5-17" = 0,
  "18+" = 0
)

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
      # household[state == "S", `:=`(
      #   state = ifelse(newly_exposed == 1, "E", state),
      #   days_in_state = ifelse(newly_exposed == 1, 0, days_in_state)
      # )]
      agents[household_id %in% household$household_id & state == "S", `:=`(
        state = ifelse(newly_exposed == 1, "E", state),
        days_in_state = ifelse(newly_exposed == 1, 0, days_in_state)
      )]
#      print(agents[state == "E", .(row_id, days_in_state)])
#      household[state == "S", state := ifelse(newly_exposed == 1, "E", state)]  # Update the agents table
#      print(household)
    }
  }
#  print(paste0("beta_within: ",beta_within))
  
  # Increment days_in_state for all agents that didn’t just transition
#  print("Estoy aqui")
  agents[state %in% c("S", "E", "I"), days_in_state := days_in_state + 1]
#  print(agents[state == "E", .(row_id, days_in_state)])
  
  # 2. Between-household transmission: Large group gatherings for the 0-5 age group only
  all_agents_0_5 <- agents[age_group == "0-5"]
  if (nrow(all_agents_0_5) >= 20) {  
    sample_size <- sample(20:30, 1)
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
  
  # Return the updated agents, daily tracking data, and infected household count
  return(list(
    agents = agents,
    num_infected_households = num_infected_households,
    state_counts = state_counts
  ))
}

# Initialize data storage for visualization
households_infected_over_time <- c()
infected_agents_by_age_group_over_time <- list()

# Set parameters
#n_days <- 100  # Number of days to run the simulation
#incubation_mean <- 8  # Mean incubation period (days)
#infectious_mean <- 27  # Mean infectious period (days)
#beta_within <- 0.3  # Transmission rate within households
#beta_between <- 0.05  # Transmission rate between households

# Initialize list to store results over multiple days
time_series_log <- list()

# Loop over simulation days
for (day in 1:n_days) {
  result <- simulate_day(agents, beta_within, beta_between, incubation_mean, infectious_mean)
  agents <- result$agents
  
  # Extract daily tracking data
  daily_counts <- result$state_counts
  daily_counts[, day := day]  # Add day column for tracking
  daily_counts[, num_infected_households := result$num_infected_households]  # Add infected households count
  
  # Append to log
  time_series_log[[day]] <- daily_counts
}

# Combine list into one data.table for full time-series analysis
full_log <- rbindlist(time_series_log)
full_log %>% tail()
# Infected and Recovered Counts by Age Group Over Time
ggplot(full_log, aes(x = day)) +
  geom_line(aes(y = infected_count, color = age_group), size = 1) +
  geom_line(aes(y = recovered_count, color = age_group), linetype = "dashed", size = 1) +
  labs(title = "Daily Infected and Recovered Counts by Age Group",
       x = "Day",
       y = "Count",
       color = "Age Group") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal()

# Daily Number of Infected Households Over Time
ggplot(full_log, aes(x = day, y = num_infected_households)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Daily Number of Infected Households",
       x = "Day",
       y = "Number of Infected Households") +
  theme_minimal()
# 
# # Run the simulation for a number of days
# for (day in 1:n_days) {
#   # Simulate one day and store the results
#   result <- simulate_day(agents, beta_within, beta_between, incubation_mean, infectious_mean)
#   
#   # Store number of households with at least one infection
#   households_infected_over_time <- c(households_infected_over_time, result$num_households_with_infections)
#   
#   # Store the total number of infected agents by age group
#   infected_agents_by_age_group_over_time[[day]] <- result$infected_by_age_group
# }
# 
# # Convert the list of infected agents by age group into a data frame for easier plotting
# infected_agents_by_age_group_df <- do.call(rbind, infected_agents_by_age_group_over_time)
# infected_agents_by_age_group_df$day <- rep(1:n_days, each = (nrow(infected_agents_by_age_group_df)+1) / n_days)
# 
# # Ensure the data frame is formatted correctly
# infected_agents_by_age_group_df <- data.frame(
#   day = infected_agents_by_age_group_df$day,
#   age_group = infected_agents_by_age_group_df$age_group,
#   infected_count = infected_agents_by_age_group_df$N
# )
# 
# # Plot number of households with at least one infection over time
# library(ggplot2)
# 
# ggplot(data.frame(day = 1:n_days, households_infected = households_infected_over_time), aes(x = day, y = households_infected)) +
#   geom_line() +
#   labs(title = "Number of Households with At Least One Infected Agent",
#        x = "Day",
#        y = "Number of Households") +
#   theme_minimal()
# 
# # Plot total number of infected agents by age group over time
# ggplot(infected_agents_by_age_group_df, aes(x = day, y = infected_count, color = age_group)) +
#   geom_line() +
#   labs(title = "Number of Infected Agents by Age Group Over Time",
#        x = "Day",
#        y = "Number of Infected Agents") +
#   theme_minimal() +
#   scale_color_manual(values = c("0-5" = "blue", "5-17" = "red", "18+" = "green"))
# 
# 
# 
