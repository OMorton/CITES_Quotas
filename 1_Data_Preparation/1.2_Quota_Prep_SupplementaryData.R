options(scipen=999)
.libPaths("C:/Packages")

library(tidyverse)
library(viridis)
library(rredlist)

Quota_codes <- read.csv("Data/CITES/Quotas/Quota_codes_raw_summary.csv")

Quota_LIV <- Quota_codes %>% filter(Term == "LIV", year < 2022) %>% group_by(FullName, party, year) %>%
  mutate(Overlapping_quotas_LIV = ifelse(n_distinct(Quota_type) > 1, "Yes", "No")) %>%
  filter(Overlapping_quotas_LIV == "No") %>% group_by(party, FullName, Purpose, Source, Term) %>%
  mutate(Quota_ID = cur_group_id()) %>%
  ungroup() %>% mutate(ROW_ID = 1:n())

