# Setup

## Active packages:

 
#| echo: FALSE

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, 
  dplyr, 
  janitor, 
  renv, 
  readxl,
  lubridate,
  svglite,
  svgtools,
  knitr,
  kableExtra,
  jtools,
  plotly
  )

(.packages())

# Loading Dataset

#### first 20 variables

base::ifelse(file.exists("study/data_long.Rda"),
             base::load(file = "study/data_long.Rda"),
             base::load(file = "../../study/data_long.Rda")) #required for running quarto scripts location independent


colnames(dl_raw)[1:20]


# Calculating relevant variables

## Creating Filter for Completed Probes


#| echo: FALSE

# adding variable to raw data set that includes only
dl = dl_raw %>% 
  mutate(
    valid_probe = ifelse(
      (form == "a Situational Survey" | form == "b End of Day") & is.na(missing),
      TRUE,
      ifelse(
        is.na(form),
        NA,
        FALSE
        )
      ),
  .after = "trigger")

print("Valid probes:")
table(dl$valid_probe, useNA = "ifany")


## Calculating Duration Variables

dl = dl %>% 
  mutate(
    date_diff = ifelse(
      as.Date(day_newest) >= as.Date(day_start),
      as.Date(day_newest) - as.Date(day_start) + 1,
      0),
    max_participation = date_diff*5
  )

print("10 random start dates")
sample(dl$day_start, 10)
print("10 random most recent dates")
sample(dl$day_newest, 10)


# Creating compliance overviews

## Creating Overview Graph

# Summarising daily responses/participants
unique_daily = dl %>%
  filter(valid_probe) %>%
  # filter(
  #   probe_number_daily >= 1 &
  #   probe_number_daily <=5
  # ) %>% 
  group_by(date_day) %>% 
  summarise(
    n = length(unique(id)),
    type = "Respondents",
    probe_number_daily = NA
    )

forms_daily = dl %>%
  filter(valid_probe) %>% 
  # filter(
  #  probe_number_daily >= 1 &
  #  probe_number_daily <=5
  # ) %>% 
  group_by(date_day, probe_number_daily) %>% 
  summarise(
    n = length(id),
    type = "Forms",
    probe_number_daily = mean(probe_number_daily)
    )

n_daily = bind_rows(unique_daily, forms_daily)

rm(unique_daily, forms_daily)

daily_plot = n_daily %>% ggplot() +
  aes(x = date_day, y = n, fill = probe_number_daily) +
  geom_col(position = "stack") +
  facet_wrap(~ type)

ggplotly(daily_plot)

rm(n_daily)


### Calculating for Percentage of Filled out Surveys per Participant

fillout_overview = dl %>% 
  # filter(
  #  probe_number_daily >= 1 & probe_number_daily <= 5
  # ) %>%
  group_by(date_day, id) %>% 
  summarise(
    sum_day_fillout = sum(valid_probe == 1),
    percent_day_fillout = sum(valid_probe == 1)/5*100,
    ) %>%
  ungroup() %>% 
  group_by(date_day, percent_day_fillout) %>% 
  summarise(
    n = length(id)) %>% 
  ungroup()
  
# converting to factor
fillout_overview = fillout_overview %>% mutate(percent_day_fillout_factor = as.factor(percent_day_fillout))
  
fillout_overview = fillout_overview %>% 
  filter(
    percent_day_fillout_factor == 0 |
      percent_day_fillout_factor == 20 |
      percent_day_fillout_factor == 40 |
      percent_day_fillout_factor == 60 |
      percent_day_fillout_factor == 80 |
      percent_day_fillout_factor == 100
    )

# creating ggplot
plot_compliance_daily = fillout_overview %>%
  ggplot() +
  aes(date_day, n) +
  geom_col(#position = position_stack(reverse = TRUE)
    aes(fill = percent_day_fillout_factor)
           
    ) +
    scale_fill_brewer()

ggplotly(plot_compliance_daily)

#  
# fig = fillout_overview %>% 
#   mutate(
#     date_day = as.character(date_day)
#   ) %>% 
#   plot_ly(
#     x = ~date_day,
#     color = ~percent_day_fillout,
#     y = ~n,
#     frame = ~date_day,
#     type = 'bar',
#     showlegend = F
#   ) 
# fig
# 
# fig = fig %>% layout(barmode = 'stack')
# fig


rm(fillout_overview)

## Mapping Participant's (Relative) Compliance

#### Participants with enough compliance to receive compensation (assuming compliance continues to the same degree):

# Calculating compliance values
a = dl %>% 
  group_by(id) %>%
  summarise(
    total_valid = sum(valid_probe)
    )

# Adding compliance to data frame
dl = full_join(dl, a, by = "id") %>% 
  mutate(
    total_valid = 
      ifelse(
        !is.na(total_valid),
        total_valid,
        0),
    total_missed = max_participation - total_valid,
    compliance = ifelse(
      max_participation > 0,
      total_valid/max_participation,
      0)
    ) %>% 
  ungroup() # ungrouping

# Summarising for plot
compliance_overview = dl %>%
  filter(!is.na(valid_probe)) %>% 
  group_by(id) %>% 
  summarise(
    compliance = mean(compliance),
  )
  

# Plotting individual compliance
compliance_plot =
  compliance_overview %>%
  filter(
    compliance <= 1
  ) %>% 
  ggplot() +
  aes(reorder(id, compliance), compliance) +
  geom_col(
    #width = 1
    (aes(fill = compliance))
  )+
  scale_fill_gradient2(
    low = "red",
    high = "green",
    mid = 2,
    midpoint = .4) +
  coord_flip() +
  geom_hline(yintercept = .5) +
  geom_hline(yintercept = .6) + 
  geom_hline(yintercept = .75) +
  theme_bw()

# Compliance_plot
ggsave("compliance_plot.svg", device = NULL, width = 7.5, height = 30)

#checking compliance to see how many people would pass of those active on the current day (or the previous day,if it is after midnight and before 9 am)
dl %>%  group_by(id) %>% 
  summarise(
    pass = mean(compliance) >= 0.6 & 
      ifelse(
        str_sub(Sys.time(), -8,-4) < "09:00" &
          str_sub(Sys.time(), -8,-4) > "00:00",
        toString(date(Sys.time())-1),
        toString(date(Sys.time()))
      ) == mean(day_newest)) %>% 
  count(pass == TRUE)
 
ggplotly(compliance_plot)

## Mapping Participant's Absolute Fillout

# Creating data to visualize compliance
probes = dl %>%
  filter(valid_probe) %>%
  filter(total_valid <= max_participation) %>% # one person received more probes than should be technically possible, needs to be checked
  group_by(id) %>% 
  summarise(
    id = mean(id),
    y = mean(total_valid),
    order = mean(total_valid),
    col = "Completed probes"
    )
max = dl %>%
  filter(valid_probe) %>% 
  filter(total_valid <= max_participation) %>% # one person received more probes than should be technically possible, needs to be checked
  group_by(id) %>% 
  summarise(
    id = mean(id),
    y = mean(max_participation) - mean(total_valid),
    order = mean(total_valid),
    col = "Missing probes"
    )

start = dl %>%
  filter(valid_probe) %>% 
  filter(total_valid <= max_participation) %>% # one person received more probes than should be technically possible, needs to be checked
  group_by(id) %>%
  summarise(
    id = mean(id),
    y = 
      as.numeric(difftime(
        as.character(max(day_start)), 
        "2022-11-15", 
        units = "days"))
    *5,
    order = mean(total_valid),
    col = "Start delay"
  )

end = dl %>%
  filter(valid_probe) %>% 
  filter(total_valid <= max_participation) %>% # one person received more probes than should be technically possible, needs to be checked
  group_by(id) %>%
  summarise(
    id = mean(id),
    y = 
      as.numeric(
        difftime(
          ifelse(
            str_sub(Sys.time(), -8,-4) < "09:00" & 
              str_sub(Sys.time(), -8,-4) > "00:00",
            toString(date(Sys.time())-1),
            toString(date(Sys.time()))),
          as.character(max(day_newest)), 
          units = "days"))
    *5,
    order = mean(total_valid),
    col = "End delay"
  )

goal = dl %>%
  filter(valid_probe) %>% 
  filter(total_valid <= max_participation) %>% # one person received more probes than should be technically possible, needs to be checked
  group_by(id) %>%
  summarise(
    id = mean(id),
    y = 
      ifelse(
          42 - mean(total_valid) > 0,
        42 - mean(total_valid),
        0
        ),
    order = mean(total_valid),
    col = "Missing to goal"
  )

# joining information
abs_compliance = bind_rows(probes, max, start, end, goal) 

# ordering factor
abs_compliance$col = recode_factor(
  abs_compliance$col,
  "1" ="Missing to goal",
  "2" ="End delay",
  "3" = "Missing probes", 
  "4" = "Completed probes", 
  "5" = "Start delay",
  .ordered = T)

# Plotting individual compliance
fillout_plot =
  abs_compliance %>% ggplot() +
  aes(reorder(id, order), y) +
  geom_col(
    aes(
      fill = col,
       )
     ) +
  coord_flip() +
  ylab("Probe number") +
  xlab("ID") +
  scale_fill_manual(values = c("#00000025", "#C50F3C", "#FDB735", "#7BB725", "#DEDEDE")) +
  theme_bw()

ggplotly(fillout_plot)

# Saving plot
# ggsave("fillout_plot.svg", device = NULL, width = 7.5, height = 30)


## Getting comments


comments = dl_raw %>% 
  filter(!is.na(text_comment)) %>% 
  select(id, text_comment, form_start_date) %>% 
  mutate(
    text_comment = str_replace(text_comment, "\\n", " ↵ ") 
    #replacing new line with character
  ) %>% 
  arrange(desc(form_start_date))

table = kable(x = comments,
      #format = "simple,
      #booktabs = TRUE,
      escape = F,
      col.names = c("ID", "Comment", "Date"),
      align = c("l", "l", "c"),
      ) %>% 
  kable_paper(bootstrap_options = "responsive") %>% 
  column_spec(3, width = "20%")

table

