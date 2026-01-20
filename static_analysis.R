# Lanette Tyler
# Surry Home Educators Cross Country 2025
# Season Summary

# Load packages:
library(tidyverse)
library(janitor)
library(DescTools)

# Read in data
race1_data <- read_csv("Surry XC 2025 - 1 Formatted.csv")
race2_data <- read_csv("Surry XC 2025 - 2 Formatted.csv")
race3_data <- read_csv("Surry XC 2025 - 3 Formatted.csv")
race4_data <- read_csv("Surry XC 2025 - 4 Formatted.csv")
race5_data <- read_csv("Surry XC 2025 - 5 Formatted.csv")
race6_data <- read_csv("Surry XC 2025 - 6 Formatted.csv")
race7_data <- read_csv("Surry XC 2025 - 7 Formatted.csv")
race8_data <- read_csv("Surry XC 2025 - 8 Formatted.csv")
race9_data <- read_csv("Surry XC 2025 - 9 Formatted.csv")
race10_data <- read_csv("Surry XC 2025 - 10 Formatted.csv")
race11_data <- read_csv("Surry XC 2025 - 11 Formatted.csv")
race12_data <- read_csv("Surry XC 2025 - 12 Formatted.csv")
race13_data <- read_csv("Surry XC 2025 - 13 Formatted.csv")

#correct errors in column names
colnames(race7_data)[4] <- "Pace (h:mm:ss per mile)"
colnames(race9_data)[4] <- "Pace (h:mm:ss per mile)" 
colnames(race10_data)[4] <- "Pace (h:mm:ss per mile)"
colnames(race12_data)[4] <- "Pace (h:mm:ss per mile)"
colnames(race3_data)[4] <- "Pace (h:mm:ss per mile)" #fixed the error!
colnames(race3_data)[2] <- "Time (h:mm:ss)"
colnames(race6_data)[2] <- "Time (h:mm:ss)"
colnames(race13_data)[4] <- "Pace (h:mm:ss per mile)"

combine data into one file
race_data <- rbind(race1_data, race2_data, race3_data, race4_data, race5_data, 
                   race6_data, race7_data, race8_data, race9_data, race10_data,
                   race11_data, race12_data, race13_data)

race_data <- clean_names(race_data) #NEED TO FIX THIS!
race_data$percentile_ranking <- as.numeric(race_data$percentile_ranking)



data_subset <- filter(race_data, first_name == "Caroline" & distance == "2mi")
data_subset <- data_subset |>
  mutate(two_mi_race_no = 1:nrow(data_subset))
ggplot(data_subset, aes(x = two_mi_race_no, y = time_h_mm_ss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(0,nrow(data_subset),1), 
                     minor_breaks = NULL,
                     name = "Two-Mile Race Number") +
  labs(y = "time (hh:mm:ss)",
       title = "Surry XC Fall 2025",
       subtitle = "Caroline Tyler (Middle School Girls' Team)")

summarize(data_subset, mean = SecToHms(round(mean(time_h_mm_ss))), max = SecToHms(max(time_h_mm_ss)), 
          min = SecToHms(min(time_h_mm_ss)))
mean(data_subset$time_h_mm_ss)
min(data_subset$time_h_mm_ss)

ggplot(filter(race_data, first_name == "Caroline" & distance == "2mi") |>
         mutate(two_mi_race_no = 1:nrow(data_subset)), 
       aes(x = two_mi_race_no, y = time_h_mm_ss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 13, 1), 
                     minor_breaks = NULL,
                     name = "Two-Mile Race Number") +
  labs(y = "time (hh:mm:ss)",
       title = "Surry XC Fall 2025",
       subtitle = "Caroline Tyler (Middle School Girls' Team)")

ggplot(filter(race_data, division == "Middle School" &
                distance == "2mi" &
                sex == "Girls"),
       aes(x = race_no, y = time_h_mm_ss, group = first_name, color = first_name)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(breaks=seq(0, 13, 1), 
                     minor_breaks = NULL,
                     name = "Race Number") +
  labs(y = "time (hh:mm:ss)",
       title = "Surry XC Fall 2025",
       subtitle = "Middle School Girls' Team") +


# add race numbers for types of races
  
ggplot(filter(race_data, first_name == "Caroline"), 
       aes(race_no, y = pace_h_mm_ss_per_mile)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 13, 1), 
                     minor_breaks = NULL,
                     name = "Two-Mile Race Number") +
  labs(y = "pace (hh:mm:ss per mile)",
       title = "Surry XC Fall 2025",
       subtitle = "Caroline Tyler (Middle School Girls' Team)")

ggplot(race_data |> drop_na(percentile_ranking) |> filter(first_name == "Caroline"), 
       aes(x = as.factor(race_no), y = percentile_ranking, fill = short_race_name)) + 
  geom_bar(stat = "identity") #+
#  scale_y_continuous(0, 100, 1)


#Chart of Minimum, Average, and Maximum Times by Race
race_data |> 
  filter(division == "High School" & sex == "Boys" & distance == "5K") |>
  group_by(short_race_name) |>
  summarize("Fastest_Time" = SecToHms(min(time_h_mm_ss)), "Average_Time" = SecToHms(mean(time_h_mm_ss)),
            "Slowest_Time" = SecToHms(max(time_h_mm_ss)))

#Graph of Minimum, Average, and Maximum Times by Race
race_data |> 
  filter(division == "High School" & sex == "Boys" & distance == "5K") |>
  group_by(short_race_name) |>
  summarize("Fastest_Time" = SecToHms(min(time_h_mm_ss)), "Average_Time" = SecToHms(mean(time_h_mm_ss)),
            "Slowest_Time" = SecToHms(max(time_h_mm_ss))) |>
  pivot_longer(cols = Fastest_Time:Slowest_Time, names_to = "Time_Type", values_to = "Time") |>
  ggplot(aes(x = short_race_name, y = as_hms(Time), group = Time_Type, color = Time_Type)) +
  geom_point() +
  geom_line() +
  labs(y = "time (hh:mm:ss)",
       x = "Meet",
       title = "Surry XC Fall 2025: High School Boys' Team",
       subtitle = "5K Races: Minimum, Average, and Maximum Times for All Meets") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

race_data |> filter(sex == "Boys" & distance == "5K") |> 
  mutate(sorter = HmsToSec(time_h_mm_ss)) |>
  group_by(short_race_name) |>
  arrange(sorter, .by_group = TRUE) |> 
  slice_head(n=5) |>
  mutate(position_in_group = row_number()) |>
  select(position_in_group, short_race_name, time_h_mm_ss) |>
  pivot_wider(names_from = position_in_group, values_from = time_h_mm_ss) |>
  rowwise() |>
  mutate(top5_average = SecToHms(mean(c(`1`, `2`, `3`, `4`, `5`)))) |>
  ungroup()

race_data |> filter(sex == "Boys" & distance == "5K") |> 
  mutate(sorter = HmsToSec(time_h_mm_ss)) |>
  group_by(short_race_name) |>
  arrange(sorter, .by_group = TRUE) |> 
  summarize(Minimum_of_Top5 = SecToHms(min(time_h_mm_ss[1:5])),
            Average_of_Top5 = SecToHms(mean(time_h_mm_ss[1:5])),
            Maximum_of_Top5 = SecToHms(max(time_h_mm_ss[1:5])))