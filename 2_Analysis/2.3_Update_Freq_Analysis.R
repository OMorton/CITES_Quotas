####################################
#### Update frequency analysis #####
####################################

options(scipen=999)
source("Functions.R")

library(tidyverse)
library(ggpubr)
library(ggtext)
library(viridis)
library(cowplot)
library(brms)
library(tidybayes)

CITES_Parties <- data.table::fread("Data/CITES/CITES_Parties.csv", na.strings = "")
Quota_clean_raw <- read.csv("Data/CITES/Quotas/Quota_codes_raw_summary.csv")
Quota_trade_listing <- read.csv("Data/CITES/Quotas/Rept_Quota_trade_listing.csv")

## IUCN data
all_IUCN_series <- read.csv("Data/IUCN/IUCN_trade_threat.csv") %>% select(-X) ## new series all listed sp
iucnames <- read.csv("Data/IUCN/naming_all_traded.csv") %>% select(-X)
iucn_match <- left_join(iucnames, all_IUCN_series)%>%
  mutate(IUCN_thr = ifelse(IUCN_code %in% c("VU", "EN", "CR"), "Thr", "NonThr")) %>%
  select(Taxon, matching_name, Year, Int_trade_thr_L, IUCN_code)

#### Data prep ####

Quota_changes <- Quota_clean_raw %>% 
  left_join(iucn_match, by = c("FullName" = "Taxon", "year" = "Year")) %>%
  ## don't need to do that here as its possible to have two seperate quota series
  ## for different terms (only for live checks is it problematic)
  #filter(Other_term_quotas_in_place == "No") %>%
  #filter(Overlapping_quotas == "No") %>%
  group_by(party, Family, Genus, Order, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type, Overlapping_quotas, year, IUCN_code, Int_trade_thr_L) %>% 
  summarise(quota = sum(quota)) %>%
  group_by(party, Family, Genus, Order, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type, Overlapping_quotas) %>%
  mutate(quota_id = cur_group_id()) %>%
  group_by(quota_id, party, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type, Int_trade_thr_L) %>%
  filter(all(quota > 0)) %>%
  mutate(Change = quota != lag(quota), Change = replace_na(Change, FALSE)) %>%
  summarise(diff_quotas = sum(Change), length = n(), iucn_changes = length(unique(IUCN_code)))
#%>% 
 # left_join(iucn_2023_match, by = c("FullName" = "Taxon")) %>%
  #mutate(thr_grp = case_when(IUCN_thr == "Thr" & Int_trade_thr_L == "Yes" ~ "Globally Thr & trade Thr",
   #                          IUCN_thr == "Thr" & Int_trade_thr_L == "No" ~ "Globally Thr",
    #                         IUCN_thr == "NonThr" & Int_trade_thr_L == "Yes" ~ "Trade Thr"))

Quota_changes_zero <- Quota_clean_raw %>% 
  ## don't need to do that here as its possible to have two seperate quota series
  ## for different terms (only for live checks is it problematic)
  #filter(Other_term_quotas_in_place == "No") %>%
  #filter(Overlapping_quotas == "No") %>%
  group_by(party, Family, Genus, Order, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type, Overlapping_quotas, year) %>% 
  summarise(quota = sum(quota)) %>%
  group_by(party, Family, Genus, Order, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type, Overlapping_quotas) %>%
  mutate(quota_id = cur_group_id()) %>%
  group_by(quota_id, party, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type) %>%
  filter(any(quota == 0))

## 673
Quota_changes2 <- Quota_changes %>% filter(length > 1) %>%
  left_join(iucn_2023_match, by = c("FullName" = "Taxon"))

Quota_changes_zero %>%
  group_by(quota_id, party, Rank, FullName, Term, 
           Purpose, Source, unit,
           Quota_type) %>%
  filter(all(quota == 0)) %>% reframe(diff_quotas = n_distinct(quota), length = n()) %>% filter(length > 1)

## 272
Quota_changes_zero %>% reframe(diff_quotas = n_distinct(quota), length = n()) %>% filter(length > 1)

length_dat <- Quota_changes%>% filter(length > 1) %>% group_by(length) %>% tally()

change_stat <- Quota_changes2 %>% group_by(length) %>% 
  reframe(mean = mean(diff_quotas), sd =  sd(diff_quotas),
          sd = ifelse(is.na(sd), 0, sd))

#### Change point analysis ####
mod_change <- brm(bf(diff_quotas ~ Intercept + 
                       slope1 * length * step(change - length) +
                       (slope1 * change + slope2 * (length - change)) * step(length - change),
                     Intercept + slope1 + slope2 ~ 1,
                     change ~ 1,
                     nl = TRUE),
                  prior = prior(normal(0, 1), nlpar = "Intercept") +
                    prior(normal(1, 2), nlpar = "slope1") +
                    prior(normal(1, 2), nlpar = "slope2") +
                    prior(normal(10, 5), nlpar = "change"),
                  file = "Outputs/Models/update_changepoint.rds", 
                  data = Quota_changes2, iter = 1000, chains = 4)

fixef(mod_change, robust = TRUE, probs = c(0.05, 0.95))

change_sum <- fixef(mod_change, summary = FALSE) %>% as.data.frame() %>% select(change_Intercept) %>%
  median_hdci(change_Intercept, .width = .9)

slope_sum <- fixef(mod_change, summary = FALSE) %>% as.data.frame() %>% 
  select(slope1_Intercept, slope2_Intercept) %>%
  rename("Slope1" = "slope1_Intercept", "Slope2" = "slope2_Intercept") %>%
  pivot_longer(everything(), names_to = "Var", values_to = "Est") %>%
  group_by(Var) %>%
  mutate(pd = (sum(sign(Est) == sign(median(Est)))/n()*100)) %>%
  group_by(Var, pd) %>%
  median_hdci(Est, .width = .9)

slope_contrast <- fixef(mod_change, summary = FALSE) %>% as.data.frame() %>% 
  select(slope1_Intercept, slope2_Intercept) %>%
  mutate(diff = slope1_Intercept - slope2_Intercept,
         pd = (sum(sign(diff) == sign(median(diff)))/n()*100),
         var = "Contrast") %>%
  group_by(pd, var) %>%
  median_hdci(diff, .width = .9)


newdat <- data.frame(length = 2:27)
change_preds <- add_epred_draws(mod_change, newdata = newdat) %>% 
  group_by(length) %>% median_hdci(.epred, .width = .9)

Quota_changes_fig <- ggplot(Quota_changesv2, aes(length, diff_quotas)) +
  geom_point(alpha = .3, shape = 16, size = 2) +
  #geom_smooth(method = "loess", colour = "black") +
  #geom_point(data = change_stat, aes(length, mean), colour = "darkblue", shape = 8) +
  geom_line(data = change_preds, aes(length, .epred), colour = "darkblue", size =1) +
  geom_ribbon(data = change_preds, aes(length, ymin = .lower, ymax = .upper, y = .epred), fill = "darkblue", alpha = .2) +
  geom_point(data = change_sum, aes(x = change_Intercept, y = 5.5), colour = "darkblue", size = 5, shape = 18) +
  geom_errorbarh(data = change_sum, aes(x = change_Intercept, xmin = .lower, xmax = .upper , y = 5.5), 
                 colour = "darkblue", height = 0, size = 1) +
  #geom_line(data = change_stat, aes(length, mean), colour = "darkblue") +
  #geom_ribbon(data = change_stat, aes(y = mean, ymin = mean-sd, ymax = mean + sd), 
  #            fill = "darkblue", alpha = .1) +
  geom_abline(intercept = -1, linetype = "longdash", size = 1, colour = "darkred") +
  geom_abline(slope = .5, intercept = -1, linetype = "longdash", size = .75, colour = "darkred") +
  geom_abline(slope = .2, intercept = -1, linetype = "longdash", size = .5, colour = "darkred") +
  geom_abline(slope = .1, intercept = -1, linetype = "longdash", size = .25, colour = "darkred") +
  coord_cartesian(xlim = c(2, 26)) +
  scale_x_continuous(breaks = c(2, 10, 20)) +
  annotate(geom = "text", fontface = "bold", label = "Change every year", x = 6, y = 15, hjust = -0.25) +
  annotate(geom = "text", fontface = "bold", label = "Change every 2nd year", x = 27, y = 12, hjust = 1.25) +
  annotate(geom = "text", fontface = "bold", label = "Change every 5th year", x = 27, y = 5.4, hjust = 1) +
  annotate(geom = "text", fontface = "bold", label = "Change every 10th year", x = 27, y = 0.5, hjust = 1) +
  xlab("Quota series length (years)") +
  ylab("Number of times the quota is updated") +
  theme_minimal(base_size = 12)


#### Plotting ####
length_bb <- data.frame(length = 2:27)

zero_updates_dat <- Quota_changes2 %>%
  filter(diff_quotas == 0) %>% group_by(length) %>% tally() %>%
  right_join(length_bb) %>% mutate(n = ifelse(is.na(n), 0, n))

zero_updates_dat %>% filter(length >= 10) %>% reframe(sum(n))

yearly_updates_dat <- Quota_changes2 %>%
  filter(diff_quotas+1 == length) %>% group_by(length) %>% tally() %>%
  right_join(length_bb) %>% mutate(n = ifelse(is.na(n), 0, n))

yrly_plt <- ggplot(yearly_updates_dat, aes(length, n)) + 
  geom_col(fill = "darkblue") +
  xlab("Quota series length (year)") +
  ylab("Quotas updated yearly") +
  theme_minimal(base_size = 12)

never_plt <- ggplot(zero_updates_dat, aes(length, n)) + 
  geom_col(fill = "darkred") +
  xlab("Quota series length (year)") +
  ylab("Quotas never updated") +
  theme_minimal(base_size = 12)

## 255/673 quotas longer than a single year never change
Quota_changes2 %>% filter(diff_quotas == 0)

## average updated ever 4 years
Quota_changes2 %>% mutate(freq = diff_quotas/length) %>%
  ungroup() %>% summarise(mean(freq))

Quota_changes_sum <- Quota_changes2 %>% mutate(freq = length/diff_quotas) %>%
  mutate(freq = ifelse(freq == Inf, length, freq),
         Changes_every_10_years = ifelse(freq >= 10, 1, 0),
         Changes_every_5_years = ifelse(freq >= 5, 1, 0),
         Changes_every_2_years = ifelse(freq >= 2, 1, 0))


## 84 quotas updated every 10 or more years
## 242 quotas updated every 5 or more years
Quota_changes_sum2 <- Quota_changes_sum %>% ungroup() %>%
  summarise(Changes_every_10_years = sum(Changes_every_10_years),
            Changes_every_5_years = sum(Changes_every_5_years),
            Year10_prop = sum(Changes_every_10_years)/n() *100,
            Year5_prop = sum(Changes_every_5_years)/n() *100)

Quota_changes_sum %>% filter(length >= 5) %>% ungroup() %>%
  summarise(Changes_every_5_years = sum(Changes_every_5_years),
            Year5_prop = sum(Changes_every_5_years)/n() *100,
            length = n())

Quota_changes_sum %>% filter(length >= 10) %>% ungroup() %>%
  summarise(Changes_every_10_years = sum(Changes_every_10_years),
            Year10_prop = sum(Changes_every_10_years)/n() *100,
            length = n())

## 79 are set yearly
Quota_changes2 %>% filter(diff_quotas+1 == length)

#### arrangement ####
library(ggpubr)

Quota_change_arrange <- ggarrange(ggarrange(yrly_plt, never_plt, nrow = 2, labels = c("A.", "B.")), 
                                  Quota_changes_fig, 
                                  ncol = 2,
                                  labels = c("", "C."), widths = c(1, 1.75))

ggsave(path = "Outputs/Figures", Quota_change_arrange, filename = "Quota_changes_fig.png",  bg = "white",
       device = "png", width = 26, height = 18, units = "cm")


Quota_traded_years_sum <- Quota_trade_listing %>% 
  filter(Other_term_quotas_in_place == "No") %>%
  group_by(Name, Exporter, Family, Genus, Order, Taxon, Term, 
           Purpose, Source, Quota_type, Volume) %>% 
  mutate(quota_id = cur_group_id()) %>%
  group_by(Name, Exporter, Family, Genus, Order, Taxon, Term, 
           Purpose, Source, Quota_type) %>%
  filter(all(Zero_quota == "No")) %>%
  summarise(diff_quotas = n_distinct(Quota), 
            length = n(), traded_years = sum(Volume > 0)) %>%
  ungroup() %>%
  filter(length > 1)

ggplot(Quota_traded_years_sum, aes(traded_years, diff_quotas)) + 
  geom_point(alpha = .5) +
  geom_smooth() +
  geom_abline() +
  coord_cartesian(ylim = c(0, 25))

cor.test(Quota_traded_years_sum$traded_years, Quota_traded_years_sum$diff_quotas)


#### Quota but no trade ####
Quota_df <- Quota_trade_listing %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No") %>% 
  group_by(Taxon, Exporter, Source, Purpose) %>% 
  mutate(ID = cur_group_id()) %>%
  filter(all(Volume == 0))

check <- Quota_df %>% group_by(Taxon, Exporter, Source, Purpose) %>% tally()
