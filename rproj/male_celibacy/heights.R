library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(gssr)
library(gssrdoc)
library(weights)
library(stringr)

data(gss_all)

# SELECT

my_vars <- c("year", "id", "ballot", "age", "sex", "height", "numwomen")

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

CAT_LABEL_SHORT <- "Short (≤ 5'6\")"
CAT_LABEL_MED <- "Medium (5'7\" through 6'0\")"
CAT_LABEL_LONG <- "Tall (≥ 6'1\")"

# filter criteria = male (sex=1) & 18 ≤ age < 30
# boolean flag = numwomen=0
gss_relevant <- gss_relevant |>
  drop_na(sex, age, numwomen) |>
  mutate(
    include = (sex == 1 & (age >= 18 & age < 30)),
    valid = ((!is.na(numwomen)) & (numwomen >= 0 & numwomen <= 750) & (!is.na(height)))
  ) |>
  filter(include == TRUE & valid == TRUE) |>
  mutate( # Converts `height` inches to `height_grp` category labels
    slept_with_no_women_since_18 = numwomen == 0,
    height_grp = case_when(
      (height >= 0 & height <= 66) ~ CAT_LABEL_SHORT,
      (height >= 67 & height <= 72) ~ CAT_LABEL_MED,
      (height >= 73 & height <= 99) ~ CAT_LABEL_LONG,
      TRUE ~ NA # Default case if no other condition is met
    )
  )

# Converts `slept_with_no_women_since_18` column to a factor with a specified order
gss_relevant$slept_with_no_women_since_18 <- factor(gss_relevant$slept_with_no_women_since_18, levels = c(FALSE, TRUE))

# Converts `height_grp` column to a factor with a specified order
gss_relevant$height_grp <- factor(gss_relevant$height_grp, levels = c(CAT_LABEL_LONG, CAT_LABEL_MED, CAT_LABEL_SHORT))

# LINE GRAPH [

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
  group_by(year, height_grp) |> 
  summarize(proport = survey_mean(as.numeric(slept_with_no_women_since_18 == TRUE), na.rm = TRUE, vartype = "ci"))  # as.numeric makes it include zero-observation groups
  # filter(slept_with_no_women_since_18 == TRUE)

# FOR SCREENSHOT
gss_relevant |> 
  group_by(year, height_grp, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  complete(slept_with_no_women_since_18, fill = list(n = 0)) |>
  mutate(proport = n / sum(n))

proport_by_year_nonweighted <- gss_relevant |> 
  group_by(year, height_grp, slept_with_no_women_since_18) |> 
  tally() |>
  drop_na() |>
  complete(slept_with_no_women_since_18, fill = list(n = 0)) |>
  mutate(proport = n / sum(n)) |>
  filter(slept_with_no_women_since_18 == TRUE)

# Height Classes’ Percentages
proport_by_year <- left_join(
  proport_by_year,
  gss_svy |> 
    group_by(year, height_grp) |>
    summarize(count = n()) |>
    mutate(height_grp_prevalence_in_year = 100 * count / sum(count)),
  by = c("year", "height_grp"))

# PLOT

theme_set(theme_minimal())

categories_txt <- "Height Group"
thickness_txt <- "Height Group Percentage\n(In surveyed sample.\nNot population census.)"

dependent_variable_txt <- "Share of U.S. men under age 30 who report zero female sex partners since they turned 18."

height_grp_colormap = character(3)
height_grp_colormap[CAT_LABEL_LONG] <- "darkorange"
height_grp_colormap[CAT_LABEL_MED] <- "darkcyan"
height_grp_colormap[CAT_LABEL_SHORT] <- "darkslateblue"

proport_by_year |> 
  # select(!slept_with_no_women_since_18) |>
  
  ggplot(mapping = 
           aes(x = year, y = proport,
               ymin = proport_low, 
               ymax = proport_upp,
               color = height_grp,
               group = height_grp, 
               fill = height_grp,
               linewidth = height_grp_prevalence_in_year)) +
  geom_line() +
  scale_linewidth(range = c(0.1, 2.5),# adjusts the range of line widths
                  guide = guide_legend(title=thickness_txt, nrow=2)) +
  geom_ribbon(alpha = 0.3, color = NA, linewidth = NA) +
  scale_x_continuous(breaks = seq(2014, 2022, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = height_grp_colormap,
                     guide = guide_legend(title=categories_txt)) +
  scale_fill_manual(values = height_grp_colormap,
                    guide = guide_legend(title=categories_txt)) +
  coord_cartesian(xlim = c(2014, 2022), ylim = c(0, .35)) +
  labs(x = "Year",
       y = "%",
       subtitle = dependent_variable_txt,
       caption = "Data Source: General Social Survey") +
  theme(legend.position = "right", plot.background = element_rect(fill='white'))

ggsave("../output/male_celibacy/height_linegraph.png", width=1000, height=500, units="px", dpi=144)

# ] LINE GRAPH

# SCATTERPLOT [

gss_relevant <- gss_relevant |>
  mutate(wtssps_float = as.numeric(wtssps))

# CORRELATION COEFFICIENT + P-VALUE

weighted_correlation_results <- wtd.cor(gss_relevant$height, gss_relevant$numwomen, weight = gss_relevant$wtssps_float, mean1 = TRUE)
corrcoeff <- weighted_correlation_results[,"correlation"]
p_value <- weighted_correlation_results[,"p.value"]

# PLOT

theme_set(theme_minimal())

gss_relevant |> 
  ggplot(aes(x=height, y=numwomen)) + 
  geom_point(aes(size = wtssps_float), position=position_jitter(h=0.25,w=0.25)) +
  scale_size(range = c(0.1, 2.5)) +
  geom_smooth(method='lm', aes(weight = wtssps_float)) +
  scale_y_continuous(breaks = seq(0, 30, 1)) +
  coord_cartesian(ylim = c(0, 30)) +
  labs(x = "Height in Inches",
       y = "Number of Female Sexual Partners Since Respondent Turned 18",
       title = "U.S. Men (18 through 29)",
       subtitle = "Weighted. Jitter added to differentiate points.",
       caption = c(str_glue("Correlation: {round(corrcoeff,digits=5)}\nP-value: {round(p_value,digits=5)}"), "Data Source: General Social Survey")) +
  theme(plot.caption = element_text(hjust = c(0, 1)),
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill='white')
        )

ggsave("../output/male_celibacy/height_scatterplot.png", width=1000, height=1000, units="px", dpi=144)

# ] SCATTERPLOT
