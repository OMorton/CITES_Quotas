################################
#### Quota trait analysis ####
################################

options(scipen=999)
.libPaths("C:/Packages")

library(tidyverse)
library(ggpubr)
library(ggtext)
library(viridis)

Quota_trade_listing <- read.csv("Data/CITES/Quotas/Rept_Quota_trade_listing.csv")
All_Listed_IUCN <- read.csv("Data/IUCN/ALL_REPT_ASSESSMENTS_series.csv")  
Name_trait <- read.csv("Data/Traits/Etard_quota_names.csv")

#### Data preparation ####

x <- Quota_trade_listing %>% 
  left_join(All_Listed_IUCN, by = join_by("Year", "Taxon")) %>%
  left_join( Name_trait, by = "Taxon") %>%
  mutate(Coarse_source = case_when(Source %in% c("C, D", "F") ~ "Captive",
                                   Source %in% c("W", "R", "W, R") ~ "Wild",
                                   TRUE ~ Source)) %>%
  filter(Coarse_source != "I") %>%
  mutate(IUCN_code = factor(IUCN_code, levels = c("LC", "NT", "VU", "EN", "CR", "NE")))

## Habitat breadth missing the most but we can recalculate it using more upto date assessments
sp_for_habs <- x %>% select(IUCNName) %>% distinct()

apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 code = character(),
                 habitat = character(),
                 suitability = character(),
                 season = character(), 
                 majorimportance = character())

## Get species historical statuses.
## Needs an API key and because of the delay needed between calls takes ~1 hour to run.
## Do not remove the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(sp_for_habs)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- sp_for_habs$IUCNName[i]
  iucnhabs <- rl_habitats(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnhabs$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       code = NA,
                       habitat = NA,
                       suitability = NA,
                       season = NA, 
                       majorimportance = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       code = iucnhabs$result$code,
                       habitat = iucnhabs$result$habitat,
                       suitability = iucnhabs$result$suitability,
                       season = iucnhabs$result$season,
                       majorimportance = iucnhabs$result$majorimportance)
    df <- rbind(df, spdf)
  }
}

#write.csv(df, "Data/IUCN/IUCN_habs.csv")
rept_habs <- read.csv("Data/IUCN/IUCN_habs.csv")

rept_habs_sum <- rept_habs %>% mutate(hab = ifelse(is.na(code), NA, 1)) %>%
  group_by(IUCNName) %>% reframe(hab_breadth = sum(hab))

y <- x %>% left_join(rept_habs_sum)

y %>% summarise(across(c(Adult_svl_cm:Artificial_habitat_use, hab_breadth), ~1 - sum(is.na(.x))/n()))


ggplot(y, aes(IUCN_code, Quota + 1)) +
  geom_point() +
  geom_boxplot() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(y, aes(Body_mass_g, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(y, aes(Max_longevity_d, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(y, aes(Litter_clutch_size, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(y, aes(hab_breadth, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)



