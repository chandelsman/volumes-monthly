# Summary of volumes by case type, client group, and pathologist


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gt)


# Load raw data -----------------------------------------------------------

volumes_raw <- 
  readxl::read_excel("data/volumes_raw.xlsx")

# Drop totals row (last row)
volumes_raw <- volumes_raw[-nrow(volumes_raw), ]

# Load group assignments
grp <- 
  readxl::read_excel("data/volumes_raw.xlsx",
                      sheet = "Sheet1")

# Pivot data to long format
vol_long <- 
  volumes_raw %>% 
  pivot_longer(
    !c(
      `Lab Test Result Type`,
      `Lab Test Sequence Group`,
      Pathologist
    ),
    names_to = "Time",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    Time = mdy(Time)
  )

grp_long <-  
  grp %>% 
  pivot_longer(
    cols = `UCH North Hospitals`:Neogenomics,
    names_to = "cl_grp",
    values_to = "cl_name",
    values_drop_na = TRUE
  )

grp_result_type <- 
  vol_long %>% 
  left_join(grp_long, by = c("Lab Test Sequence Group" = "cl_name")) %>% 
  mutate(
    yr = year(Time),
    mth = month(Time, label = TRUE, abbr = TRUE),
    type = recode(
      `Lab Test Result Type`,
      `Surgical [Prostate]` = "Surgical"
    )
  ) %>% 
  group_by(cl_grp, type, yr, mth) %>% 
  summarize(n = sum(cases))

grp_hospital <- 
  grp_result_type %>% 
  filter(is.na(cl_grp) == FALSE) %>% 
  group_by(
    cl_grp,
    yr,
    mth
  ) %>% 
  summarize(n = sum(n, na.rm = TRUE)) %>% 
  mutate(
    pct.chg = 100 * ((n - lag(n)) / lag(n))
  )
  

grp_hospital %>% 
  ggplot(aes(x = mth, y = n, group = yr, color = as.factor(yr))) +
  geom_line() + 
  geom_point() +
  scale_x_discrete(breaks = grp_result_type$mth, labels = grp_result_type$mth) +
  theme_bw() + 
  labs(title = "Total Cases by Hospital Group",
       x = "", 
       y = "Number of Cases Signed Out", 
       color = "Year") + 
  facet_wrap(~ cl_grp, scales = "free")

# grp_wide <- 
#   grp_result_type %>% 
#   pivot_wider(names_from = mth,
#               values_from = n)
# 
# grp_wide %>% 
#   group_by(yr, type) %>% 
#   gt(groupname_col = "cl_grp")
