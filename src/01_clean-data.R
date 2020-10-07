# Data cleaning to summarize volumes by case type, client, and pathologist

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gt)


# Load raw data -----------------------------------------------------------

volumes_raw <- 
  readxl::read_excel("data/volumes_raw.xlsx", sheet = "Report")

# Drop totals row (last row)
# volumes_raw <- volumes_raw[-nrow(volumes_raw), ]

# Load group assignments
grp <- 
  readxl::read_excel("data/hospital_groups.xlsx", sheet = "Sheet1")



# Format data -------------------------------------------------------------

# Pivot data to long format

# volumes
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
  filter(`Lab Test Sequence Group` != "-" | 
           `Lab Test Sequence Group` != "STONE") %>% 
  mutate(
    Time = mdy(Time),
    type = factor(
      case_when(
        `Lab Test Result Type` == "Clinical" ~ "Clinical",
        `Lab Test Result Type` == "Cytopathology [GYN]" ~ "GYN",
        `Lab Test Result Type` == "Cytopathology [NGYN]" ~ "NGYN",
        `Lab Test Result Type` == "Microscopic [Tech]" ~ "Tech Services",
        `Lab Test Result Type` == "Surgical" ~ "Surgical",
        `Lab Test Result Type` == "Surgical [Prostate]" ~ "Surgical"
      )
    )
  )

# groups
grp_long <-  
  grp %>% 
  pivot_longer(
    cols = `UCH North Hospitals`:Neogenomics,
    names_to = "cl_grp",
    values_to = "cl_name",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    facility = case_when(
      cl_name == "SURG (MCR)" ~ "Medical Center of the Rockies",
      cl_name == "CLIN - MEMHPV" ~ "Memorial Clinical Tests",
      cl_name == "NGYN (BFCMC)" ~ "Banner Fort Collins Medical Center",
      cl_name == "NGYN (CPMC)" ~ "Colorado Plains Medical Center",
      cl_name == "NGYN OP (SP)" ~ "SP",
      cl_name == "CLIN - CTNG" ~ "SPL",
      cl_name == "Cyto/Molecular" ~ "Neogenomics",
      cl_name == "SURG (PVH)" ~ "Poudre Valle Hospital",
      cl_name == "GYN Cyto (MHN)" ~ "Memorial Clinical Tests",
      cl_name == "NGYN (EMCH)" ~ "East Morgan County Hospital",
      cl_name == "NGYN (CRMC)" ~ "Cheyenne Regional Medical Center",
      cl_name == "NGYN OP (SPWY)" ~ "Summit Outreach",
      cl_name == "CLIN - HPV" ~ "SPL",
      cl_name == "Flow Cytometry" ~ "Neogenomics",
      cl_name == "SURG (UCHGH)" ~ "UCH Greeley Hospital",
      cl_name == "NGYN (GRANDVIEW)" ~ "Grandview Hospital",
      cl_name == "NGYN (McKee)" ~ "McKee Medical Center",
      cl_name == "NGYN (EPMC)" ~ "Estes Park Medical Center",
      cl_name == "SURG OP (SP)" ~ "Summit Outreach",
      cl_name == "CLIN - VPT" ~ "SPL",
      cl_name == "NGYN (MCR)" ~ "Medical Center of the Rockies",
      cl_name == "NGYN (MHC)" ~ "Memorial Hospital Central",
      cl_name == "NGYN (NCMC)" ~ "North Colorado Medical Center",
      cl_name == "NGYN (HC)" ~ "Melissa Memorial Hospital",
      cl_name == "SURG OP (SPWY)" ~ "Summit Outreach",
      cl_name == "CLIN Grp B StrepVIAL" ~ "SPL",
      cl_name == "NGYN (PVH)" ~ "Poudre Valley Hospital",
      cl_name == "NGYN (MHN)" ~ "Memorial Hospital North",
      cl_name == "NGYN (OCH)" ~ "Ogallala Community Hospital",
      cl_name == "NGYN (IMH)" ~ "Ivinson Memorial Hospital",
      cl_name == "NGYN (PEAK)" ~ "Summit Outreach",
      cl_name == "CLIN Molecular" ~ "SPL",
      cl_name == "NGYN (UCHGH)" ~ "UCH Greeley Hospital",
      cl_name == "NGYN (WOODLAND)" ~ "Pikes Peak Regional Hospital",
      cl_name == "NGYN (SRMC)" ~ "Sterling Regional Medical Center",
      cl_name == "NGYN (WY VA)" ~ "Veterans Administration",
      cl_name == "*SURG (CFG)" ~ "Centers for Gastroenterology",
      cl_name == "CLIN Pathogen - VIAL" ~ "SPL",
      cl_name == "SURG (PVH)" ~ "Poudre Valley Hospital",
      cl_name == "NGYN OP (MHN)" ~ "Memorial Hospital North",
      cl_name == "NGYN (TORRINGTON)" ~ "Torrington Community Hospital",
      cl_name == "NGYN (RAWLINS)" ~ "Memorial Hospital of Carbon County",
      cl_name == "SURG (PEAK)" ~ "Summit Outreach",
      cl_name == "CLIN Tier Two Reflex" ~ "SPL",
      cl_name == "SURG (GRANDVIEW)" ~ "Grandview Hospital",
      cl_name == "NGYN (PCMH)" ~ "Platte County Memorial Hospital",
      cl_name == "NGYN (MHCC DC)" ~ "Memorial Hospital of Converse County",
      cl_name == "SURG (SPRINGS OFFIC)" ~ "Summit Outreach",
      cl_name == "PB/CLIN BILL" ~ "SPL",
      cl_name == "SURG (MHC)" ~ "Memorial Hospital Central",
      cl_name == "SURG (BFCMC)" ~ "Banner Fort Collins Medical Center",
      cl_name == "SURG (CPMC)" ~ "Colorado Plains Medical Center",
      cl_name == "Autopsy (Tech only)" ~ "Summit Outreach",
      cl_name == "GYN Cyto (SP)" ~ "SPL",
      cl_name == "SURG (MHN)" ~ "Memorial Hospital North",
      cl_name == "SURG (EMCH)" ~ "East Morgan County Hospital",
      cl_name == "SURG (CRMC)" ~ "Cheyenne Regional Medical Center",
      cl_name == "Process Only / Tech" ~ "Summit Outreach",
      cl_name == "GYN Cyto SurePath" ~ "SPL",
      cl_name == "SURG (WOODLAND)" ~ "Pikes Peak Regional Hospital",
      cl_name == "SURG (McKee)" ~ "McKee Medical Center",
      cl_name == "SURG (EPMC)" ~ "Estes Park Medical Center",
      cl_name == "SURG (JL DERM)" ~ "Summit Outreach",
      cl_name == "SURG OP (MH)" ~ "Memorial Hospital Central",
      cl_name == "SURG (NCMC)" ~ "North Colorado Medical Center",
      cl_name == "SURG (HS)" ~ "Melissa Memorial Hospital",
      cl_name == "SURG (PROFESSIONAL)" ~ "Summit Outreach",
      cl_name == "Process/Tech-MHS" ~ "Memorial Clinical Tests",
      cl_name == "SURG (OCH)" ~ "Ogallala Community Hospital",
      cl_name == "SURG (IMH)" ~ "Ivinson Memorial Hospital",
      cl_name == "TECH ONLY FR DERM" ~ "Summit Outreach",
      cl_name == "SURG (PCMH)" ~ "Platte County Memorial Hospital",
      cl_name == "SURG (MHCC DC)" ~ "Memorial Hospital of Converse County",
      cl_name == "zzz(FR DERM)dont use" ~ "Summit Outreach",
      cl_name == "SURG (SRMC)" ~ "Sterling Regional Medical Center",
      cl_name == "SURG (KHS)" ~ "Kimball County Hospital",
      cl_name == "SURG (TORRINGTON)" ~ "Torrington Community Hospital",
      cl_name == "NGYN (KHS)" ~ "Kimball County Hospital",
      cl_name == "SURG (RAWLINS)" ~ "Memorial Hospital of Carbon County",
      cl_name == "SURG (WY VA)" ~ "Veterans Administration",
      TRUE ~ cl_name
    )
  )

# add grouping variables to volumes
volumes <- 
  vol_long %>% 
  left_join(grp_long, by = c("Lab Test Sequence Group" = "cl_name")) %>% 
  mutate(
    yr = year(Time),
    mth = month(Time, label = TRUE, abbr = TRUE),
    result_type = recode(
      `Lab Test Result Type`,
      `Surgical [Prostate]` = "Surgical"
    )
  )
