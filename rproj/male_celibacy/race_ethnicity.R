library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(gssr)
library(gssrdoc)

data(gss_all)

# SELECT

my_vars <- c("year", "id", "ballot", "age", "sex", "race", "hispanic", "numwomen")

wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssps",              # weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method

all_vars <- c(my_vars, wt_vars)

gss_relevant <- gss_all |>
  select(all_of(all_vars))

# FILTER

# filter criteria = male (sex=1) & 18 ≤ age < 30
# boolean flag = numwomen=0
gss_relevant <- gss_relevant |>
  drop_na(sex, age, numwomen) |>
  mutate(
    include = (sex == 1 & (age >= 18 & age < 30)),
    valid = ((!is.na(numwomen)) & (numwomen >= 0 & numwomen <= 750) & (is.na(hispanic) | (hispanic >= 1 & hispanic <= 5)) & (is.na(race) | (race >= 1 & race <= 3)))
  ) |>
  filter(include == TRUE & valid == TRUE) |>
  mutate(
    slept_with_no_women_since_18 = numwomen == 0
  )

# Converts `slept_with_no_women_since_18` column to a factor with a specified order
gss_relevant$slept_with_no_women_since_18 <- factor(gss_relevant$slept_with_no_women_since_18, levels = c(FALSE, TRUE))

# Converts `race` numeric codes to `race_str` strings
race_map <- c(`1` = "White", `2` = "Black", `3` = "Other")
gss_relevant$race_str <- race_map[gss_relevant$race]
# Converts `race` column to a factor with a specified order
gss_relevant$race_str <- factor(gss_relevant$race_str, levels = c("White", "Black", "Other"))

# Converts `hispanic` numeric codes to `hispanic_str` strings
gss_relevant <- gss_relevant |>
  mutate(
    hispanic_str = case_when(
      hispanic == 1 ~ "Not Hispanic/Latino",
      hispanic > 1 ~ "Hispanic/Latino",
      TRUE ~ NA # Default case if no other condition is met
    )
  )
# Converts `hispanic` column to a factor with a specified order
gss_relevant$hispanic_str <- factor(gss_relevant$hispanic_str, levels = c("Not Hispanic/Latino", "Hispanic/Latino"))

# CALCULATE ANNUAL STATS

options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

# integrates weights
gss_svy <- gss_relevant |>
  mutate(stratvar = interaction(year, vstrat)) |>
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = wtssps,
                   nest = TRUE)

# RACE [

gss_relevant <- gss_relevant |>
  filter(!is.na(race))
gss_svy <- gss_svy |>
  filter(!is.na(race))

# FOR SCREENSHOTS [
print((gss_relevant |> 
  group_by(year, race_str, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  mutate(proport = n / sum(n)))[67:90,], n=100)
gss_relevant |> 
  group_by(year, race_str, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  complete(slept_with_no_women_since_18, fill = list(n = 0)) |>
  mutate(proport = n / sum(n))
# ] FOR SCREENSHOTS

# Gets the breakdown for every year
proport_by_year <- gss_svy |> 
  group_by(year, race_str) |> 
  summarize(proport = survey_mean(as.numeric(slept_with_no_women_since_18 == TRUE), na.rm = TRUE, vartype = "ci")) # as.numeric makes it include zero-observation groups
  # filter(slept_with_no_women_since_18 == TRUE)
  # summarize(proport = 1 - survey_mean(na.rm = TRUE, vartype = "ci")) |>
  # filter(slept_with_no_women_since_18 == FALSE) # negation because not all years have sexless men of all races

proport_by_year_nonweighted <- gss_relevant |> 
  group_by(year, race_str, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  complete(slept_with_no_women_since_18, fill = list(n = 0)) |>
  mutate(proport = n / sum(n)) |>
  filter(slept_with_no_women_since_18 == TRUE)
  # mutate(proport = 1 - n / sum(n)) |>
  # filter(slept_with_no_women_since_18 == FALSE) # negation because not all years have sexless men of all races

# Races’ Sample Percentages
proport_by_year <- left_join(
  proport_by_year,
  gss_svy |> 
    group_by(year, race_str) |>
    summarize(count = n()) |>
    mutate(race_prevalence_in_year = 100 * count / sum(count)),
  by = c("year", "race_str"))

# PLOT

theme_set(theme_minimal())

categories_txt <- "Race"
thickness_txt <- "Race Percentage\n(In surveyed sample.\nNot population census.)"

dependent_variable_txt <- "Share of U.S. men under age 30 who report zero female sex partners since they turned 18."

races_colormap = c("White"="deeppink", "Black"="deepskyblue", "Other"="goldenrod")

proport_by_year |> 
  # select(!slept_with_no_women_since_18) |>
  
  ggplot(mapping = 
           aes(x = year, y = proport,
               ymin = proport_low, 
               ymax = proport_upp,
               color = race_str,
               group = race_str, 
               fill = race_str,
               linewidth = race_prevalence_in_year)) +
  geom_line() +
  scale_linewidth(range = c(0.1, 2.5), # adjusts the range of line widths
                  guide = guide_legend(title=thickness_txt)) +
  geom_ribbon(alpha = 0.3, color = NA, linewidth = NA) +
  scale_x_continuous(breaks = seq(1989, 2022, 3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = races_colormap,
                     guide = guide_legend(title=categories_txt)) +
  scale_fill_manual(values = races_colormap,
                    guide = guide_legend(title=categories_txt)) +
  coord_cartesian(xlim = c(1989, 2022), ylim = c(0, .5)) +
  labs(x = "Year",
       y = "%",
       subtitle = dependent_variable_txt,
       caption = "Data Source: General Social Survey") +
  theme(legend.position = "right", plot.background = element_rect(fill='white'))

ggsave("../output/male_celibacy/race.png", width=1000, height=500, units="px", dpi=144)

# ] RACE

# HISPANIC LATINO [

gss_relevant <- gss_relevant |>
  filter(!is.na(hispanic))
gss_svy <- gss_svy |>
  filter(!is.na(hispanic))

# Gets the breakdown for every year
proport_by_year <- gss_svy |> 
  group_by(year, hispanic_str) |> 
  summarize(proport = survey_mean(as.numeric(slept_with_no_women_since_18 == TRUE), na.rm = TRUE, vartype = "ci")) # as.numeric makes it include zero-observation groups
  # summarize(proport = 1 - survey_mean(na.rm = TRUE, vartype = "ci")) |>
  # filter(slept_with_no_women_since_18 == FALSE) # negation because not all years have sexless men of all races

proport_by_year_nonweighted <- gss_relevant |> 
  group_by(year, hispanic_str, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  complete(slept_with_no_women_since_18, fill = list(n = 0)) |>
  mutate(proport = n / sum(n)) |>
  filter(slept_with_no_women_since_18 == TRUE)
  # mutate(proport = 1 - n / sum(n)) |>
  # filter(slept_with_no_women_since_18 == FALSE) # negation because not all years have sexless men of all races

# Ethnic Groups’ Sample Percentages
proport_by_year <- left_join(
  proport_by_year,
  gss_svy |> 
    group_by(year, hispanic_str) |>
    summarize(count = n()) |>
    mutate(ethnic_grp_prevalance_in_year = 100 * count / sum(count)),
  by = c("year", "hispanic_str"))

# PLOT

theme_set(theme_minimal())

categories_txt <- "Ethnicity: Hispanic/Latino Specified"
thickness_txt <- "Category Percentage\n(In surveyed sample.\nNot population census.)"

# dependent_variable_txt <- "Share of U.S. men under age 30 who report zero female sex partners since they turned 18."

ethnicities_colormap = c("Not Hispanic/Latino"="orangered", "Hispanic/Latino"="darkgreen")

proport_by_year |> 
  # select(!slept_with_no_women_since_18) |>
  
  ggplot(mapping = 
           aes(x = year, y = proport,
               ymin = proport_low, 
               ymax = proport_upp,
               color = hispanic_str,
               group = hispanic_str, 
               fill = hispanic_str,
               linewidth = ethnic_grp_prevalance_in_year)) +
  geom_line() +
  scale_linewidth(range = c(0.1, 2.5), # adjusts the range of line widths
                  guide = guide_legend(title=thickness_txt)) +
  geom_ribbon(alpha = 0.3, color = NA, linewidth = NA) +
  scale_x_continuous(breaks = seq(2002, 2022, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = ethnicities_colormap,
                     guide = guide_legend(title=categories_txt)) +
  scale_fill_manual(values = ethnicities_colormap,
                    guide = guide_legend(title=categories_txt)) +
  coord_cartesian(xlim = c(2000, 2022), ylim = c(0, .5)) +
  labs(x = "Year",
       y = "%",
       subtitle = dependent_variable_txt,
       caption = "Data Source: General Social Survey") +
  theme(legend.position = "right", plot.background = element_rect(fill='white'))

ggsave("../output/male_celibacy/ethnicity.png", width=1000, height=500, units="px", dpi=144)

# ] HISPANIC LATINO
