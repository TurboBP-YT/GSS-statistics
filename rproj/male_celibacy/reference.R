library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(gssr)
library(gssrdoc)

data(gss_all)

# SELECT

my_vars <- c("year", "id", "ballot", "age", "sex", "numwomen")

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
    include = (sex == 1 & age >= 18 & age < 30),
    valid = ((!is.na(numwomen)) & numwomen >= 0 & numwomen <= 750)
  ) |>
  filter(include == TRUE & valid == TRUE) |>
  mutate(
    slept_with_no_women_since_18 = numwomen == 0
  )

# Converts `slept_with_no_women_since_18` column to a factor with a specified order
gss_relevant$slept_with_no_women_since_18 <- factor(gss_relevant$slept_with_no_women_since_18, levels = c(FALSE, TRUE))

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

# Gets the breakdown for every year
proport_by_year <- gss_svy |> 
  group_by(year) |>
  summarize(proport = survey_mean(as.numeric(slept_with_no_women_since_18 == TRUE), na.rm = TRUE, vartype = "ci"))  # as.numeric makes it include zero-observation groups
  # filter(slept_with_no_women_since_18 == TRUE)

proport_by_year_nonweighted <- gss_relevant |> 
  group_by(year, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  complete(slept_with_no_women_since_18, fill = list(n = 0)) |>
  mutate(proport = n / sum(n)) |>
  filter(slept_with_no_women_since_18 == TRUE)

# PLOT

theme_set(theme_minimal())

dependent_variable_txt <- "Share of U.S. men under age 30 who report zero female sex partners since they turned 18."

proport_by_year |> 
  # select(!slept_with_no_women_since_18) |>
  ggplot(mapping = 
           aes(x = year, y = proport,
               ymin = proport_low, 
               ymax = proport_upp)) +
  geom_line(linewidth = 1.25) +
  geom_ribbon(alpha = 0.3, color = NA) +
  scale_x_continuous(breaks = seq(1989, 2022, 3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(xlim = c(1989, 2022)) +
  labs(x = "Year",
       y = "%",
       subtitle = dependent_variable_txt,
       caption = "Data Source: General Social Survey") +
  theme(plot.background = element_rect(fill='white'))

ggsave("../output/male_celibacy/reference.png", width=1000, height=500, units="px", dpi=144)
