library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(forcats)
library(reshape2)
library(readr)

library(plotly)
library(leaflet)
library(dygraphs)
library(xts)
library(sp)

library(datasets)
library(wordcloud)
library(maps)
library(dplyr)
library(tidytext)
library(ggplot2)
library(pastecs)
library(DT)
library(data.table)

# Define the theme
FR_theme <-  theme(panel.background = element_rect(fill = "white"),
                   panel.grid.major.y = element_line(colour = "grey"),
                   axis.line = element_line(size = 1.5),
                   axis.text = element_text(size = 12))

# Load datasets
top.1 = data.frame(read_csv("top1.csv"))
top.10 = data.frame(read_csv("top10.csv")) 
distance = data.frame(read_csv("distance.csv"))
random = data.frame(read_csv("random.csv"))
partners = data.frame(read_csv("partners.csv"))
census = data.frame(read_csv("census_data.csv"))
weekly = data.frame(read_csv("weekly.csv"))
human = top.10 %>% select(rescue_id, donation_id, donation_type, food_types, donor_location_id, old_recipient_location_id, old_duration, old_distance, donation_entry_time) %>% data.frame()
tar = data.frame(read_csv("tar.csv"))
tar = tar %>% mutate(id = c(1:1835))

# day_of_week: the day of the week of the donation entry time
# identifier: a string to see if the donation is recurring
# recurring: a boolean that indicates whether the donation is recurring or not
weekly = weekly %>% filter(is.na(Donor.Location.ID) == F & Donor.Location.ID != "n/a") %>% filter(is.na(Recipient.Location.ID) == F & Recipient.Location.ID != "n/a")%>% rowwise() %>% mutate(identifier = paste(Day, Donor.Location.ID, Recipient.Location.ID)) %>% ungroup()
top.1 = top.1 %>% rowwise() %>% mutate(day_of_week = weekdays(as.Date(donation_entry_time)), identifier = paste(day_of_week, donor_location_id, old_recipient_location_id), recurring = ifelse(identifier %in% weekly$identifier, TRUE, FALSE)) %>% ungroup()
human = human %>% rowwise() %>% mutate(day_of_week = weekdays(as.Date(donation_entry_time)), identifier = paste(day_of_week, donor_location_id, old_recipient_location_id), recurring = ifelse(identifier %in% weekly$identifier, TRUE, FALSE)) %>% ungroup()
top.10 = top.10 %>% rowwise() %>% mutate(day_of_week = weekdays(as.Date(donation_entry_time)), identifier = paste(day_of_week, donor_location_id, old_recipient_location_id), recurring = ifelse(identifier %in% weekly$identifier, TRUE, FALSE)) %>% ungroup()
distance = distance %>% rowwise() %>% mutate(day_of_week = weekdays(as.Date(donation_entry_time)), identifier = paste(day_of_week, donor_location_id, old_recipient_location_id), recurring = ifelse(identifier %in% weekly$identifier, TRUE, FALSE)) %>% ungroup()
random = random %>% rowwise() %>% mutate(day_of_week = weekdays(as.Date(donation_entry_time)), identifier = paste(day_of_week, donor_location_id, old_recipient_location_id), recurring = ifelse(identifier %in% weekly$identifier, TRUE, FALSE)) %>% ungroup()

excluded = partners %>% slice(which(partners$status == "Exclude" | partners$status == "exclude"))
incomplete = partners %>% slice(which(partners$status == "Incomplete" | partners$status == "incomplete"))
inactive = partners %>% slice(which(partners$status == "Inactive" | partners$status == "inactive"))
invalid_donations = human %>% slice(which(human$old_recipient_location_id %in% excluded$ID | human$old_recipient_location_id %in% incomplete$ID | human$old_recipient_location_id %in% inactive$ID))
human = human %>% slice(which(!rescue_id %in% invalid_donations$rescue_id))
top.1 = top.1 %>% slice(which(!rescue_id %in% invalid_donations$rescue_id))
random = random %>% slice(which(!rescue_id %in% invalid_donations$rescue_id))
distance = distance %>% slice(which(!rescue_id %in% invalid_donations$rescue_id))
top.10 = top.10 %>% slice(which(!rescue_id %in% invalid_donations$rescue_id))

partners = partners %>% slice(which(is.na(partners$status) == T | partners$status == "invalid hour"))

# data.frames for CDFs
human.cdf = human %>% left_join(., census, by = c("old_recipient_location_id" = "recipient_location_id")) 
top.1.cdf = top.1 %>% left_join(., census, by = c("recipient_location_id" = "recipient_location_id")) 

type_human = rep("Human dispatcher", 1760)
type_algo = rep("Algorithm", 1760)
type = c(type_human, type_algo)
poverty = c(human.cdf$zip_poverty_rate, top.1.cdf$zip_poverty_rate)
income = c(human.cdf$zip_median_income, top.1.cdf$zip_median_income)
food = c(human.cdf$zip_food_access, top.1.cdf$zip_food_access)

cdf_poverty = data.frame(poverty_rate = poverty, type = type)
cdf_poverty = cdf_poverty[order(cdf_poverty$poverty_rate), ]
cdf_poverty$type = factor(cdf_poverty$type, levels = c("Human dispatcher", "Algorithm"))
cdf_income = data.frame(median_income = income, type = type)
cdf_income = cdf_income[order(cdf_income$median_income), ]
cdf_income$type = factor(cdf_income$type, levels = c("Human dispatcher", "Algorithm"))
cdf_food = data.frame(food_access = food, type = type)
cdf_food = cdf_food[order(cdf_food$food_access), ]
cdf_food$type = factor(cdf_food$type, levels = c("Human dispatcher", "Algorithm"))

# Define functions

# for per_recipient plot
freq_donations_per_recipient = function(marginal) {
  num_of_donations = c(0:60)
  count_of_donations = rep(0, 61)
  len = nrow(marginal)
  for (ii in 1:len) {
    freq = marginal$freq[ii]
    if (freq %in% num_of_donations) {
      count_of_donations[freq+1] = count_of_donations[freq+1] + 1
    } 
  }
  return(data_frame(num_of_donations, count_of_donations))
}

get_marginal = function(data, h = FALSE) {
  if (h == T) {
    return(data %>% group_by(old_recipient_location_id) %>% summarise(freq = n()) 
           %>% select(old_recipient_location_id, freq))}
  else{
    return(data %>% group_by(recipient_location_id) %>% summarise(freq = n()) 
           %>% select(recipient_location_id, freq))
  }}

# for rescues vs. distance
get_total_distance = function(df, h = FALSE) {
  num.rows = nrow(df)
  num.rescues = 0:num.rows
  d = 0
  distance_list = rep(0, num.rows+1)
  distance_list[0] = d
  if (h == T) {
    for (ii in 1:num.rows) {
      d = d + df$old_distance[ii]
      distance_list[ii+1] = d
    }}
  else{
    for (ii in 1:num.rows) {
      d = d + df$distance[ii]
      distance_list[ii+1] = d
    }}
  return(data.frame(num.rescues, distance_list))
}

# for distribution of poverty
get_poverty_rate = function(df, h = FALSE) {
  num.rows = nrow(df)
  poverty_rate = rep(NA, num.rows)
  if (h == T) {
    for (ii in 1:num.rows) {
      id = df$old_recipient_location_id[ii]
      if(id %in% census_poverty$recipient_location_id) {
        row = slice(census_poverty,which(census_poverty$recipient_location_id == id))
        poverty_rate[ii] = row$zip_poverty_rate}}
  } else {
    for (ii in 1:num.rows) {
      id = df$recipient_location_id[ii]
      if(id %in% census_poverty$recipient_location_id) {
        row = slice(census_poverty,which(census_poverty$recipient_location_id == id))
        poverty_rate[ii] = row$zip_poverty_rate}}
  }
  poverty_rate = poverty_rate[which(is.na(poverty_rate) == F)]
  return(poverty_rate)
}

# for distribution of income
get_median_income = function(df, h = FALSE) {
  num.rows = nrow(df)
  median_income = rep(NA, num.rows)
  if (h == T) {
    for (ii in 1:num.rows) {
      id = df$old_recipient_location_id[ii]
      if(id %in% census_income$recipient_location_id) {
        index = which(census_income$recipient_location_id == id)
        row = slice(census_income, index)
        median_income[ii] = row$zip_median_income}}
  } else {
    for (ii in 1:num.rows) {
      id = df$recipient_location_id[ii]
      if(id %in% census_income$recipient_location_id) {
        index = which(census_income$recipient_location_id == id)
        row = slice(census_income, index)
        median_income[ii] = row$zip_median_income}}
  }
  median_income = median_income[which(is.na(median_income) == FALSE)]
  return(median_income)
}

# for distribution of food access
get_food_access = function(df, h = FALSE) {
  num.rows = nrow(df)
  food_access = rep(NA, num.rows)
  if (h == T) {
    for (ii in 1:num.rows) {
      id = df$old_recipient_location_id[ii]
      if(id %in% census_food$recipient_location_id) {
        index = which(census_food$recipient_location_id == id)
        row = slice(census_food, index)
        food_access[ii] = row$zip_food_access}}
  } else {
    for (ii in 1:num.rows) {
      id = df$recipient_location_id[ii]
      if(id %in% census_food$recipient_location_id) {
        index = which(census_food$recipient_location_id == id)
        row = slice(census_food, index)
        food_access[ii] = row$zip_food_access}}
  }
  food_access = food_access[which(is.na(food_access) == FALSE)]
  return(food_access)
}

# for involved recipients
get_involved_recipients = function(df, h = FALSE) {
  len = nrow(df)
  number_of_recipients = rep(0, len)
  if (h == TRUE) {
    for (ii in 1:len) {
      rows = df %>% slice(1:ii)
      number_of_recipients[ii] = length(unique(rows$old_recipient_location_id))
    }}
  else {
    for (ii in 1:len) {
      rows = df %>% slice(1:ii)
      number_of_recipients[ii] = length(unique(rows$recipient_location_id))
    }}
  return(number_of_recipients)
}

# for Gini coefficient plot
get_gini = function(marginal){
  marginal = marginal[order(marginal$freq, decreasing = FALSE),]
  total = nrow(marginal)
  total_donations = sum(marginal$freq)
  gini_x = rep(NA, total)
  gini_y = rep(NA, total)
  donations = 0
  for (ii in 1:total){
    donations = donations + marginal$freq[ii]
    gini_x[ii] = ii/total
    gini_y[ii] = donations/total_donations
  }
  return(data.frame(gini_x, gini_y))
}

# per_recipient_plot
never.human = length(which(unique(partners$ID) %in% human$old_recipient_location_id == F))
never.top.1 = length(which(unique(partners$ID) %in% top.1$recipient_location_id == F))
never.top.10 = length(which(unique(partners$ID) %in% top.10$recipient_location_id == F))
never.random = length(which(unique(partners$ID) %in% random$recipient_location_id == F))
never.distance = length(which(unique(partners$ID) %in% distance$recipient_location_id == F))

distance_marginal = get_marginal(distance)
distance_per_recipient = freq_donations_per_recipient(distance_marginal)
distance_per_recipient$count_of_donations[1] = never.distance

random_marginal = get_marginal(random)
random_per_recipient = freq_donations_per_recipient(random_marginal)
random_per_recipient$count_of_donations[1] = never.random

human_marginal = get_marginal(human, h = T)
human_per_recipient = freq_donations_per_recipient(human_marginal)
human_per_recipient$count_of_donations[1] = never.human

top1_marginal = get_marginal(top.1)
top1_per_recipient = freq_donations_per_recipient(top1_marginal)
top1_per_recipient$count_of_donations[1] = never.top.1

top10_marginal = get_marginal(top.10)
top10_per_recipient = freq_donations_per_recipient(top10_marginal)
top10_per_recipient$count_of_donations[1] = never.top.10

per_recipient = data_frame("num_of_donations" = c(0:60), "DA_freq" = distance_per_recipient$count_of_donations, "HA_freq" = human_per_recipient$count_of_donations, "RA_freq" = random_per_recipient$count_of_donations, "TenA_freq" = top10_per_recipient$count_of_donations, "TopA_freq" = top1_per_recipient$count_of_donations)

per_recipient_table = per_recipient %>% select("DA_freq", "RA_freq", "TopA_freq", "HA_freq")
frequency_summary_table = stat.desc(per_recipient_table)[c("mean", "median", "std.dev", "min", "max"),]
setnames(frequency_summary_table, old = c("DA_freq", "HA_freq", "RA_freq", "TopA_freq"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

per_recipient_adjusted = mutate(per_recipient,
                       TopA_freq = ifelse(TopA_freq==0, NA, TopA_freq),
                       DA_freq = ifelse(DA_freq==0, NA, DA_freq),
                       RA_freq = ifelse(RA_freq==0, NA, RA_freq))

# per_recipient_plot (recurring)
recur_distance = distance %>% filter(recurring == T)
never.distance.recur = length(which(unique(partners$ID) %in% recur_distance$recipient_location_id == FALSE))
recur_distance_marginal = get_marginal(recur_distance)
recur_distance_per_recipient = freq_donations_per_recipient(recur_distance_marginal)
recur_distance_per_recipient$count_of_donations[1] = never.distance.recur

recur_random = random %>% filter(recurring == T)
never.random.recur = length(which(unique(partners$ID) %in% recur_random$recipient_location_id == FALSE))
recur_random_marginal = get_marginal(recur_random)
recur_random_per_recipient = freq_donations_per_recipient(recur_random_marginal)
recur_random_per_recipient$count_of_donations[1] = never.random.recur

recur_human = human %>% filter(recurring == T)
never.human.recur = length(which(unique(partners$ID) %in% recur_human$old_recipient_location_id == FALSE))
recur_human_marginal = get_marginal(recur_human, h = T)
recur_human_per_recipient = freq_donations_per_recipient(recur_human_marginal)
recur_human_per_recipient$count_of_donations[1] = never.human.recur

recur_top.1 = top.1 %>% filter(recurring == T)
never.top.1.recur = length(which(unique(partners$ID) %in% recur_top.1$recipient_location_id == FALSE))
recur_top1_marginal = get_marginal(recur_top.1)
recur_top1_per_recipient = freq_donations_per_recipient(recur_top1_marginal)
recur_top1_per_recipient$count_of_donations[1] = never.top.1.recur

recur_top.10 = top.10 %>% filter(recurring == T)
never.top.10.recur = length(which(unique(partners$ID) %in% recur_top.10$recipient_location_id == FALSE))
recur_top10_marginal = get_marginal(recur_top.10)
recur_top10_per_recipient = freq_donations_per_recipient(recur_top10_marginal)
recur_top10_per_recipient$count_of_donations[1] = never.top.10.recur

recur_per_recipient = data_frame("num_of_donations" = c(0:60), "DA_freq" = recur_distance_per_recipient$count_of_donations, "HA_freq" = recur_human_per_recipient$count_of_donations, "RA_freq" = recur_random_per_recipient$count_of_donations, "TenA_freq" = recur_top10_per_recipient$count_of_donations, "TopA_freq" = recur_top1_per_recipient$count_of_donations)

recur_per_recipient_table = recur_per_recipient %>% select("DA_freq", "RA_freq", "TopA_freq", "HA_freq")
recur_frequency_summary_table = stat.desc(recur_per_recipient_table)[c("mean", "median", "std.dev", "min", "max"),]
setnames(recur_frequency_summary_table, old = c("DA_freq", "HA_freq", "RA_freq", "TopA_freq"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

recur_per_recipient_adjusted = mutate(recur_per_recipient,
                       TopA_freq = ifelse(TopA_freq==0, NA, TopA_freq),
                       DA_freq = ifelse(DA_freq==0, NA, DA_freq),
                       RA_freq = ifelse(RA_freq==0, NA, RA_freq))

# per_recipient_plot (regular)
ad_hoc_distance = distance %>% filter(recurring == FALSE)
never.distance.ad_hoc = length(which(unique(partners$ID) %in% ad_hoc_distance$recipient_location_id == FALSE))
ad_hoc_distance_marginal = get_marginal(ad_hoc_distance)
ad_hoc_distance_per_recipient = freq_donations_per_recipient(ad_hoc_distance_marginal)
ad_hoc_distance_per_recipient$count_of_donations[1] = never.distance.ad_hoc

ad_hoc_random = random %>% filter(recurring == FALSE)
never.random.ad_hoc = length(which(unique(partners$ID) %in% ad_hoc_random$recipient_location_id == FALSE))
ad_hoc_random_marginal = get_marginal(ad_hoc_random)
ad_hoc_random_per_recipient = freq_donations_per_recipient(ad_hoc_random_marginal)
ad_hoc_random_per_recipient$count_of_donations[1] = never.random.ad_hoc

ad_hoc_human = human %>% filter(recurring == FALSE)
never.human.ad_hoc = length(which(unique(partners$ID) %in% ad_hoc_human$old_recipient_location_id == FALSE))
ad_hoc_human_marginal = get_marginal(ad_hoc_human, h = T)
ad_hoc_human_per_recipient = freq_donations_per_recipient(ad_hoc_human_marginal)
ad_hoc_human_per_recipient$count_of_donations[1] = never.human.ad_hoc

ad_hoc_top.1 = top.1 %>% filter(recurring == FALSE)
never.top.1.ad_hoc = length(which(unique(partners$ID) %in% ad_hoc_top.1$recipient_location_id == FALSE))
ad_hoc_top1_marginal = get_marginal(ad_hoc_top.1)
ad_hoc_top1_per_recipient = freq_donations_per_recipient(ad_hoc_top1_marginal)
ad_hoc_top1_per_recipient$count_of_donations[1] = never.top.1.ad_hoc

ad_hoc_top.10 = top.10 %>% filter(recurring == FALSE)
never.top.10.ad_hoc = length(which(unique(partners$ID) %in% ad_hoc_top.10$recipient_location_id == FALSE))
ad_hoc_top10_marginal = get_marginal(ad_hoc_top.10)
ad_hoc_top10_per_recipient = freq_donations_per_recipient(ad_hoc_top10_marginal)
ad_hoc_top10_per_recipient$count_of_donations[1] = never.top.10.ad_hoc

ad_hoc_per_recipient = data_frame("num_of_donations" = c(0:60), "DA_freq" = ad_hoc_distance_per_recipient$count_of_donations, "HA_freq" = ad_hoc_human_per_recipient$count_of_donations, "RA_freq" = ad_hoc_random_per_recipient$count_of_donations, "TenA_freq" = ad_hoc_top10_per_recipient$count_of_donations, "TopA_freq" = ad_hoc_top1_per_recipient$count_of_donations)

ad_hoc_per_recipient_table = ad_hoc_per_recipient %>% select("DA_freq", "RA_freq", "TopA_freq", "HA_freq")
ad_hoc_frequency_summary_table = stat.desc(ad_hoc_per_recipient_table)[c("mean", "median", "std.dev", "min", "max"),]
setnames(ad_hoc_frequency_summary_table, old = c("DA_freq", "HA_freq", "RA_freq", "TopA_freq"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

ad_hoc_per_recipient_adjusted = mutate(ad_hoc_per_recipient,
                       TopA_freq = ifelse(TopA_freq==0, NA, TopA_freq),
                       DA_freq = ifelse(DA_freq==0, NA, DA_freq),
                       RA_freq = ifelse(RA_freq==0, NA, RA_freq))


# rescues vs. distance
total.top1 = get_total_distance(top.1)
total.top10 = get_total_distance(top.10)
total.distance = get_total_distance(distance)
total.random = get_total_distance(random)
total.human = get_total_distance(human, h = T)
rescue.vs.distance = data.frame("num_rescues" = total.top1$num.rescues, "top1" = total.top1$distance_list, "top10" = total.top10$distance_list, "human" = total.human$distance_list, "random" = total.random$distance_list, "distance" = total.distance$distance_list)

# Distribution of distance
density.distance = data.frame("DA" = distance$distance, "HA" = human$old_distance, "TopA" = top.1$distance, "TenA" = top.10$distance, "RA" = random$distance)
distance_table = density.distance %>% select("DA", "RA", "TopA", "HA")
distance_summary_table = stat.desc(distance_table)[c("mean", "median", "std.dev", "min", "max"),]
setnames(distance_summary_table, old = c("DA", "HA", "RA", "TopA"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

# Distribution of poverty rate
census_poverty = census %>% slice(which(is.na(zip_poverty_rate) == F))
poverty.top1 = get_poverty_rate(top.1)
poverty.top10 = get_poverty_rate(top.10)
poverty.random = get_poverty_rate(random)
poverty.distance= get_poverty_rate(distance)
poverty.human = get_poverty_rate(human, h = T)
density.poverty = data.frame("TopA" = poverty.top1, "TenA" = poverty.top10, "DA" = poverty.distance, "RA" = poverty.random)
density.poverty.human = data.frame("HA" = poverty.human) # different lengths

density.poverty.temp = density.poverty %>% select("DA", "RA", "TopA")
poverty_summary_table = stat.desc(density.poverty.temp)[c("mean", "median", "std.dev", "min", "max"),]
poverty_summary_table$HA = stat.desc(density.poverty.human)[c("mean", "median", "std.dev", "min", "max"),]
setnames(poverty_summary_table, old = c("DA", "HA", "RA", "TopA"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

# distribution of median income
census_income = census %>% slice(which(is.na(zip_median_income) == F))
income.top1 = get_median_income(top.1)
income.top10 = get_median_income(top.10)
income.random = get_median_income(random)
income.distance= get_median_income(distance)
income.human = get_median_income(human, h = T)
density.income = data.frame("TopA" = income.top1, "TenA" = income.top10, "DA" = income.distance, "RA" = income.random)
density.income.human = data.frame("HA" = income.human) # different lengths

density.income.temp = density.income %>% select("DA", "RA", "TopA")
income_summary_table = stat.desc(density.income.temp)[c("mean", "median", "std.dev", "min", "max"),]
income_summary_table$HA = stat.desc(density.income.human)[c("mean", "median", "std.dev", "min", "max"),]
setnames(income_summary_table, old = c("DA", "HA", "RA", "TopA"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

# distribution of food access
census_food = census %>% slice(which(is.na(zip_food_access) == F))
food.top1 = get_food_access(top.1)
food.top10 = get_food_access(top.10)
food.random = get_food_access(random)
food.distance= get_food_access(distance)
food.human = get_food_access(human, h = T)
density.food = data.frame("TopA" = food.top1, "TenA" = food.top10, "DA" = food.distance, "RA" = food.random)
density.food.human = data.frame("HA" = food.human) # different lengths

density.food.temp = density.food %>% select("DA", "RA", "TopA")
food_access_summary_table = stat.desc(density.food.temp)[c("mean", "median", "std.dev", "min", "max"),]
food_access_summary_table$HA = stat.desc(density.food.human)[c("mean", "median", "std.dev", "min", "max"),]
setnames(food_access_summary_table, old = c("DA", "HA", "RA", "TopA"), new = c("Distance-based", "Human dispatcher", "Random", "Algorithm"))

# rescues vs. involved recipients
involved.distance = get_involved_recipients(distance)
involved.random = get_involved_recipients(random)
involved.top.1 = get_involved_recipients(top.1)
involved.top.10 = get_involved_recipients(top.10)
involved.human = get_involved_recipients(human, h = TRUE)
density.involved = data.frame("number_of_rescues" = c(1:1760), "TopA" = involved.top.1, "TenA" = involved.top.10, "DA" = involved.distance, "RA" = involved.random, "HA" = involved.human)

involved.distance.recurring = get_involved_recipients(distance %>% filter(recurring == TRUE))
involved.random.recurring = get_involved_recipients(random %>% filter(recurring == TRUE))
involved.top.1.recurring = get_involved_recipients(top.1 %>% filter(recurring == TRUE))
involved.top.10.recurring = get_involved_recipients(top.10 %>% filter(recurring == TRUE))
involved.human.recurring = get_involved_recipients(human %>% filter(recurring == TRUE), h = TRUE)
density.involved.recurring = data.frame("number_of_rescues" = c(1:505), "TopA" = involved.top.1.recurring, "TenA" = involved.top.10.recurring, "DA" = involved.distance.recurring, "RA" = involved.random.recurring, "HA" = involved.human.recurring)

involved.distance.ad_hoc = get_involved_recipients(distance %>% filter(recurring == FALSE))
involved.random.ad_hoc = get_involved_recipients(random %>% filter(recurring == FALSE))
involved.top.1.ad_hoc = get_involved_recipients(top.1 %>% filter(recurring == FALSE))
involved.top.10.ad_hoc = get_involved_recipients(top.10 %>% filter(recurring == FALSE))
involved.human.ad_hoc = get_involved_recipients(human %>% filter(recurring == FALSE), h = TRUE)
density.involved.ad_hoc = data.frame("number_of_rescues" = c(1:1255), "TopA" = involved.top.1.ad_hoc, "TenA" = involved.top.10.ad_hoc, "DA" = involved.distance.ad_hoc, "RA" = involved.random.ad_hoc, "HA" = involved.human.ad_hoc)

# Gini coefficient plot
gini.never.human.ids = human %>% slice(which(unique(partners$ID) %in% old_recipient_location_id == F)) %>% select(old_recipient_location_id)
gini.human = data.frame(old_recipient_location_id = gini.never.human.ids, freq = rep(0, nrow(gini.never.human.ids)))
gini.human = rbind(human_marginal, gini.human)
gini.human = get_gini(gini.human)

gini.never.algo.ids = top.1 %>% slice(which(unique(partners$ID) %in% recipient_location_id == F)) %>% select(recipient_location_id)
gini.algo = data.frame(recipient_location_id = gini.never.algo.ids, freq = rep(0, nrow(gini.never.algo.ids)))
gini.algo = rbind(top1_marginal, gini.algo)
gini.algo = get_gini(gini.algo)

gini.never.distance.ids = distance %>% slice(which(unique(partners$ID) %in% recipient_location_id == F)) %>% select(recipient_location_id)
gini.distance = data.frame(recipient_location_id = gini.never.distance.ids, freq = rep(0, nrow(gini.never.distance.ids)))
gini.distance = rbind(distance_marginal, gini.distance)
gini.distance = get_gini(gini.distance)

gini.never.random.ids = distance %>% slice(which(unique(partners$ID) %in% recipient_location_id == F)) %>% select(recipient_location_id)
gini.random = data.frame(recipient_location_id = gini.never.random.ids, freq = rep(0, nrow(gini.never.random.ids)))
gini.random = rbind(random_marginal, gini.random)
gini.random = get_gini(gini.random)
# We are not showing TenA in all graphs for the sake of simplicity
# But we do keep all dataframes here in the server

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$donation_per_recipient <- renderPlotly({
    p = ggplot(per_recipient_adjusted, aes(num_of_donations)) + 
      geom_line(aes(y = HA_freq, colour = "Human dispatcher"), linetype = "dotted") + 
      geom_line(aes(y = TopA_freq, colour = "Algorithm")) +
      geom_line(aes(y = DA_freq, colour = "Distance-based"), linetype = "dotdash") + 
      geom_line(aes(y = RA_freq, colour = "Random"), linetype = "dashed") + 
      labs(title = "Frequency of Donations/Recipient", x = "Number of Donations/Recipient", y = "Frequency of Donations") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    if (input$recurring) {
      p = ggplot(recur_per_recipient_adjusted, aes(num_of_donations)) + 
        geom_line(aes(y = HA_freq, colour = "Human dispatcher"), linetype = "dotted") + 
        geom_line(aes(y = TopA_freq, colour = "Algorithm")) +
        geom_line(aes(y = DA_freq, colour = "Distance-based"), linetype = "dotdash") + 
        geom_line(aes(y = RA_freq, colour = "Random"), linetype = "dashed") +   
        labs(title = "Frequency of Donations/Recipient (Weekly)", x = "Number of Donations/Recipient", y = "Frequency of Donations") +
        scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
        FR_theme
    }
    if (input$ad_hoc) {
      p = ggplot(ad_hoc_per_recipient_adjusted, aes(num_of_donations)) + 
        geom_line(aes(y = HA_freq, colour = "Human dispatcher"), linetype = "dotted") + 
        geom_line(aes(y = TopA_freq, colour = "Algorithm")) +
        geom_line(aes(y = DA_freq, colour = "Distance-based"), linetype = "dotdash") + 
        geom_line(aes(y = RA_freq, colour = "Random"), linetype = "dashed") +   
        labs(title = "Frequency of Donations/Recipient (One-time)", x = "Number of Donations/Recipient", y = "Frequency of Donations") +
        scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
        FR_theme
    }
    
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(p)
  })
  
  output$frequency <- DT::renderDataTable({
    df = datatable(frequency_summary_table,
              options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
    if (input$recurring) {
      df = datatable(recur_frequency_summary_table,
                     options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
    }
    if (input$ad_hoc) {
      df = datatable(ad_hoc_frequency_summary_table,
                     options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
    }
    return(df)
  })
  
  output$rescues_vs_distance <- renderPlotly({
    p = ggplot(rescue.vs.distance, aes(num_rescues)) + 
      geom_line(aes(y = human, colour = "Human dispatcher"), linetype = "dotted") + 
      geom_line(aes(y = top1, colour = "Algorithm")) + 
      geom_line(aes(y = distance, colour = "Distance-based"), linetype = "dotdash") + 
      geom_line(aes(y = random, colour = "Random"), linetype = "dashed") + 
      labs(x = "Number of Donations", y = "Total  Distance(miles)", title = "Rescues vs. Distance") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(p)
  })
  
  output$distribution_of_distance <- renderPlotly({
    p = ggplot(density.distance) + 
      geom_density(aes(x = HA, colour = "Human dispatcher"), linetype = "dotted") + 
      geom_density(aes(x = TopA, colour = "Algorithm")) + 
      geom_density(aes(x = DA, colour = "Distance-based"), linetype = "dotdash") + 
      geom_density(aes(x = RA, colour = "Random"), linetype = "dashed") + 
      labs(title = "Distribution Plot of Distance", x = NULL, y = NULL) + 
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(p)
  })
  
  output$distance_summary <- DT::renderDataTable(
    datatable(distance_summary_table,
              options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
  )
  
  output$distribution_of_poverty_rate <- renderPlotly({
    p = ggplot() + 
      geom_density(data = density.poverty.human, aes(x = HA, colour = "Human dispatcher"), linetype = "dotted") + 
      geom_density(data = density.poverty, aes(x = TopA, colour = "Algorithm")) + 
      geom_density(data = density.poverty, aes(x = DA, colour = "Distance-based"), linetype = "dotdash") + 
      geom_density(data = density.poverty, aes(x = RA, colour = "Random"), linetype = "dashed") + 
      labs(title = "Distribution of Poverty Rate", x = NULL, y = NULL) + 
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(p)
  })
  
  output$poverty_summary <- DT::renderDataTable(
    datatable(poverty_summary_table,
              options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
  )
  
  output$distribution_of_median_income <- renderPlotly({
    p = ggplot() + 
      geom_density(data = density.income.human, aes(x = HA, colour = "Human dispatcher"), linetype = "dotted") + 
      geom_density(data = density.income, aes(x = TopA, colour = "Algorithm")) + 
      geom_density(data = density.income, aes(x = DA, colour = "Distance-based"), linetype = "dotdash") + 
      geom_density(data = density.income, aes(x = RA, colour = "Random"), linetype = "dashed") + 
      labs(title = "Distribution of Median Income", x = NULL, y = NULL) + 
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(p)
  })
  
  output$income_summary <- DT::renderDataTable(
    datatable(income_summary_table,
              options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
  )
  
  output$distribution_of_food_access <- renderPlotly({
    p = ggplot() + 
      geom_density(data = density.food.human, aes(x = HA, colour = "Human dispatcher"), linetype = "dotted") + 
      geom_density(data = density.food, aes(x = TopA, colour = "Algorithm")) + 
      geom_density(data = density.food, aes(x = DA, colour = "Distance-based"), linetype = "dotdash") + 
      geom_density(data = density.food, aes(x = RA, colour = "Random"), linetype = "dashed") + 
      labs(title = "Distribution of Food Access", x = NULL, y = NULL) + 
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(p)
  })
  
  output$food_access_summary <- DT::renderDataTable(
    datatable(food_access_summary_table,
              options = list(dom = 't')) %>% DT::formatRound(columns = c("Distance-based", "Random", "Human dispatcher", "Algorithm"), digits = 2)
  )
  
  output$involved_recipients <- renderPlotly({
    p = ggplot(density.involved, aes(x = number_of_rescues)) + 
      geom_line(aes(y = HA, color = "Human dispatcher"), linetype = "dotted") + 
      geom_line(aes(y = TopA, color = "Algorithm")) + 
      geom_line(aes(y = DA, color = "Distance-based"), linetype = "dotdash") + 
      geom_line(aes(y = RA, color = "Random"), linetype = "dashed") + 
      geom_line(data = tar, aes(x = id, y = tar), color = "grey", linetype = "dashed") + 
      labs(title = "Rescues vs. Involved Recipients", x = "Number of Rescues", y = "Number of Recipients with At Least 1 Donations") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
  if (input$recurring_2) {
    p = ggplot(density.involved.recurring, aes(x = number_of_rescues)) + 
      geom_line(aes(y = HA, color = "Human dispatcher"), linetype = "dotted") + 
      geom_line(aes(y = TopA, color = "Algorithm")) + 
      geom_line(aes(y = DA, color = "Distance-based"), linetype = "dotdash") + 
      geom_line(aes(y = RA, color = "Random"), linetype = "dashed") + 
      geom_hline(aes(yintercept = length(unique(partners$ID))), color = "grey", linetype = "dashed") +
      labs(title = "Rescues vs. Involved Recipients (Weekly)", x = "Number of Rescues", y = "Number of Recipients with At Least 1 Donations") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
  }
  if (input$ad_hoc_2) {
    p = ggplot(density.involved.ad_hoc, aes(x = number_of_rescues)) + 
      geom_line(aes(y = HA, color = "Human dispatcher"), linetype = "dotted") + 
      geom_line(aes(y = TopA, color = "Algorithm")) + 
      geom_line(aes(y = DA, color = "Distance-based"), linetype = "dotdash") + 
      geom_line(aes(y = RA, color = "Random"), linetype = "dashed") + 
      geom_hline(aes(yintercept = length(unique(partners$ID))), color = "grey", linetype = "dashed") +
      labs(title = "Rescues vs. Involved Recipients (One-time)", x = "Number of Rescues", y = "Number of Recipients with At Least 1 Donations") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
  }
  p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
  return(p)
})
  output$cdf_poverty <- renderPlotly({
    p = ggplot(data = cdf_poverty) +
      stat_ecdf(aes(x = poverty_rate, color = type, linetype = type), geom = "step") +
      labs(title = "CDF of Poverty Rate", x = "Poverty Rate", y = "Density") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm"), values = c("Human dispatcher" = "red", "Algorithm" = "blue")) +
      scale_linetype_manual(name = "", breaks = c("Human dispatcher", "Algorithm"), values = c("Human dispatcher" = "dotted", "Algorithm" = "solid")) +
      FR_theme 
    return(ggplotly(p))
  })
  output$cdf_income <- renderPlotly({
    p = ggplot(data = cdf_income) +
      stat_ecdf(aes(x = median_income, color = type, linetype = type), geom = "step") +
      labs(title = "CDF of Median Income", x = "Median Income", y = "Density") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm"), values = c("Human dispatcher" = "red", "Algorithm" = "blue")) +
      scale_linetype_manual(name = "", breaks = c("Human dispatcher", "Algorithm"), values = c("Human dispatcher" = "dotted", "Algorithm" = "solid")) +
      FR_theme
    return(ggplotly(p))
  })
  output$cdf_food <- renderPlotly({
    p = ggplot(data = cdf_food) +
      stat_ecdf(aes(x = food_access, color = type, linetype = type), geom = "step") +
      labs(title = "CDF of Food Access", x = "Food Access", y = "Density") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm"), values = c("Human dispatcher" = "red", "Algorithm" = "blue")) +
      scale_linetype_manual(name = "", breaks = c("Human dispatcher", "Algorithm"), values = c("Human dispatcher" = "dotted", "Algorithm" = "solid")) +
      FR_theme
    return(ggplotly(p))
  })
  output$gini <- renderPlotly({
    p = ggplot()+
      geom_line(data = gini.human, aes(x = gini_x, y = gini_y, color = "Human dispatcher"), linetype = "dotted") +
      geom_line(data = gini.algo, aes(x = gini_x, y = gini_y, color = "Algorithm"), linetype = "solid") +
      geom_line(data = gini.distance, aes(x = gini_x, y = gini_y, color = "Distance-based"), linetype = "dotdash") +
      geom_line(data = gini.random, aes(x = gini_x, y = gini_y, color = "Random"), linetype = "dashed") +
      labs(x = "Fraction of Recipients", y = "Fraction of Total Donations", title = "Gini Coefficient Plot") +
      scale_color_manual(name = "", breaks = c("Human dispatcher", "Algorithm", "Random", "Distance-based"), values = c("Human dispatcher" = "red", "Algorithm" = "blue", "Random" = "grey45", "Distance-based" = "burlywood4")) +
      FR_theme
    p = style(ggplotly(p),visible="legendonly", traces = c(3, 4))
    return(ggplotly(p))
  })})
