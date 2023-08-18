################################
#### Quota trait analysis ####
################################

options(scipen=999)
.libPaths("C:/Packages")

library(tidyverse)
library(ggpubr)
library(ggtext)
library(viridis)
library(brms)
library(tidybayes)

Quota_trade_listing <- read.csv("Data/CITES/Quotas/Rept_Quota_trade_listing.csv")
All_Listed_IUCN <- read.csv("Data/IUCN/ALL_REPT_ASSESSMENTS_series.csv")  
Name_trait <- read.csv("Data/Traits/Etard_quota_names.csv")

#### Data preparation ####

Quota_plus_IUCN <- Quota_trade_listing %>% 
  left_join(All_Listed_IUCN, by = join_by("Year", "Taxon")) %>%
  left_join( Name_trait, by = "Taxon") %>%
  mutate(Coarse_source = case_when(Source %in% c("C, D", "F") ~ "Captive",
                                   Source %in% c("W", "R", "W, R") ~ "Wild",
                                   TRUE ~ Source)) %>%
  filter(Coarse_source != "I") %>%
  mutate(IUCN_code = factor(IUCN_code, levels = c("LC", "NT", "VU", "EN", "CR", "NE")))

## Habitat breadth missing the most but we can recalculate it using more upto date assessments
sp_for_habs <- Quota_plus_IUCN %>% select(IUCNName) %>% distinct()

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

Quota_plus_IUCN_Traits <- Quota_plus_IUCN %>% left_join(rept_habs_sum) %>%
  filter(Coarse_source %in% c("Captive", "Wild"))

Quota_plus_IUCN_Traits %>% summarise(across(c(Adult_svl_cm:Artificial_habitat_use, hab_breadth), ~1 - sum(is.na(.x))/n()))


ggplot(Quota_plus_IUCN_Traits, aes(IUCN_code, Quota + 1)) +
  geom_point() +
  geom_boxplot() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(Quota_plus_IUCN_Traits, aes(Body_mass_g, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(Quota_plus_IUCN_Traits, aes(Max_longevity_d, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(Quota_plus_IUCN_Traits, aes(Litter_clutch_size, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

ggplot(Quota_plus_IUCN_Traits, aes(hab_breadth, Quota + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #scale_x_log10() +
  scale_y_log10() + facet_wrap(~Coarse_source)

## 755 obs
bm_dat <- Quota_plus_IUCN_Traits %>%
  filter(!is.na(Body_mass_g)) %>%
  mutate(SYear = (Year - mean(Year))/sd(Year),
         FYear = as.factor(Year),
         log_bm = log10(Body_mass_g),
         log_bm_z = (log_bm - mean(log_bm))/sd(log_bm))

## 664 obs
ml_dat <- Quota_plus_IUCN_Traits %>%
  filter(!is.na(Max_longevity_d)) %>%
  mutate(SYear = (Year - mean(Year))/sd(Year),
         FYear = as.factor(Year),
         log_ml = log10(Max_longevity_d),
         log_ml_z = (log_ml - mean(log_ml))/sd(log_ml))

## 738 obs
cl_dat <- Quota_plus_IUCN_Traits %>%
  filter(!is.na(Litter_clutch_size)) %>%
  mutate(SYear = (Year - mean(Year))/sd(Year),
         FYear = as.factor(Year),
         cl_z = (Litter_clutch_size - mean(Litter_clutch_size))/sd(Litter_clutch_size))

## 760 obs
iucn_dat <- Quota_plus_IUCN_Traits %>%
  filter(!is.na(IUCN_code)) %>%
  mutate(SYear = (Year - mean(Year))/sd(Year),
         FYear = as.factor(Year))

## 625 obs
hb_dat <- Quota_plus_IUCN_Traits %>%
  filter(!is.na(hab_breadth)) %>%
  mutate(SYear = (Year - mean(Year))/sd(Year),
         hb_z = (hab_breadth - mean(hab_breadth))/sd(hab_breadth),
         FYear = as.factor(Year))


bm_mod <- brm(Quota ~ SYear + log_bm_z + Coarse_source + Coarse_source:SYear +
                    Coarse_source:log_bm_z + 
                    (SYear + Coarse_source + Coarse_source:SYear|Taxon) +
                    (1|Exporter) + (1|FYear),
              family = negbinomial(),
              prior = c(prior(normal(0, 2), class = "b"),
                        prior(normal(0, 2), class = "Intercept"),
                        prior(normal(0, 2), class = "sd"),
                        prior(lkj(2), class = "cor")),
              file = "Outputs/Models/bm_mod.rds",
              data = bm_dat,
              iter = 1000, warmup = 500, chains = 4, cores = 4)

cl_mod <- brm(Quota ~ SYear + cl_z + Coarse_source + Coarse_source:SYear +
                Coarse_source:cl_z + 
                (SYear + Coarse_source + Coarse_source:SYear|Taxon) +
                (1|Exporter) + (1|FYear),
              family = negbinomial(),
              prior = c(prior(normal(0, 2), class = "b"),
                        prior(normal(0, 2), class = "Intercept"),
                        prior(normal(0, 2), class = "sd"),
                        prior(lkj(2), class = "cor")),
              file = "Outputs/Models/cl_mod.rds",
              data = cl_dat,
              iter = 1000, warmup = 500, chains = 4, cores = 4)

iucn_mod <- brm(Quota ~ SYear + IUCN_code + Coarse_source + Coarse_source:SYear +
                Coarse_source:IUCN_code + 
                (SYear + Coarse_source + Coarse_source:SYear|Taxon) +
                (1|Exporter) + (1|FYear),
              family = negbinomial(),
              prior = c(prior(normal(0, 2), class = "b"),
                        prior(normal(0, 2), class = "Intercept"),
                        prior(normal(0, 2), class = "sd"),
                        prior(lkj(2), class = "cor")),
              file = "Outputs/Models/iucn_mod.rds",
              data = iucn_dat,
              iter = 1000, warmup = 500, chains = 4, cores = 4)

ml_mod <- brm(Quota ~ SYear + log_ml_z + Coarse_source + Coarse_source:SYear +
                Coarse_source:log_ml_z + 
                (SYear + Coarse_source + Coarse_source:SYear|Taxon) +
                (1|Exporter) + (1|FYear),
              family = negbinomial(),
              prior = c(prior(normal(0, 2), class = "b"),
                        prior(normal(0, 2), class = "Intercept"),
                        prior(normal(0, 2), class = "sd"),
                        prior(lkj(2), class = "cor")),
              file = "Outputs/Models/ml_mod.rds",
              data = ml_dat,
              iter = 1000, warmup = 500, chains = 4, cores = 4)


hb_mod <- brm(Quota ~ SYear + hb_z + Coarse_source + Coarse_source:SYear +
                Coarse_source:hb_z + 
                (SYear + Coarse_source + Coarse_source:SYear|Taxon) +
                (1|Exporter) + (1|FYear),
              family = negbinomial(),
              prior = c(prior(normal(0, 2), class = "b"),
                        prior(normal(0, 2), class = "Intercept"),
                        prior(normal(0, 2), class = "sd"),
                        prior(lkj(2), class = "cor")),
              file = "Outputs/Models/hb_mod.rds",
              data = hb_dat,
              iter = 1000, warmup = 500, chains = 4, cores = 4)

#### Interpretation ####

bm_new <- bm_dat %>% group_by(Coarse_source) %>% 
  reframe(log_bm_z = seq(from = min(log_bm_z), to = max(log_bm_z), length.out = 25),
            SYear = 0)

cl_new <- cl_dat %>% group_by(Coarse_source) %>% 
  reframe(cl_z = seq(from = min(cl_z), to = max(cl_z), length.out = 25),
          SYear = 0)

ml_new <- ml_dat %>% group_by(Coarse_source) %>% 
  reframe(log_ml_z = seq(from = min(log_ml_z), to = max(log_ml_z), length.out = 25),
          SYear = 0)

hb_new <- hb_dat %>% group_by(Coarse_source) %>% 
  reframe(hb_z = seq(from = min(hb_z), to = max(hb_z), length.out = 25),
          SYear = 0)

iucn_new <- iucn_dat %>% distinct(Coarse_source, IUCN_code) %>%
  mutate(SYear = 0)

bm_sum <- bm_new %>% add_epred_draws(bm_mod, re_formula = NA) %>% group_by(Coarse_source,log_bm_z) %>%
  median_hdci(.epred, .width = .9) %>%
  mutate(log_bm = log_bm_z*sd(bm_dat$log_bm) + mean(bm_dat$log_bm),
         bm = 10^log_bm,
         bm_kg = bm/1000)

cl_sum <- cl_new %>% add_epred_draws(cl_mod, re_formula = NA) %>% group_by(Coarse_source,cl_z) %>%
  median_hdci(.epred, .width = .9) %>%
  mutate(cl = cl_z*sd(cl_dat$Litter_clutch_size) + mean(cl_dat$Litter_clutch_size))

ml_sum <- ml_new %>% add_epred_draws(ml_mod, re_formula = NA) %>% group_by(Coarse_source,log_ml_z) %>%
  median_hdci(.epred, .width = .9) %>%
  mutate(log_ml = log_ml_z*sd(ml_dat$log_ml) + mean(ml_dat$log_ml),
         ml = 10^log_ml)

hb_sum <- hb_new %>% add_epred_draws(hb_mod, re_formula = NA) %>% group_by(Coarse_source,hb_z) %>%
  median_hdci(.epred, .width = .9) %>%
  mutate(hb = hb_z*sd(hb_dat$hab_breadth) + mean(hb_dat$hab_breadth))

iucn_sum <- iucn_new %>% add_epred_draws(iucn_mod, re_formula = NA) %>% group_by(Coarse_source,IUCN_code) %>%
  median_hdci(.epred, .width = .9) %>%
  mutate(IUCN_code = factor(IUCN_code, levels = c("LC", "NT", "VU", "EN", "CR", "NE")))

#### plots ####

## bm
bm_capt_plt <- ggplot(filter(bm_sum, Coarse_source == "Captive"), aes(bm_kg, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Bodymass (kg)") +
  #coord_cartesian(ylim = c(0, 2200)) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  scale_y_log10() +
  theme_minimal()

bm_wild_plt <- ggplot(filter(bm_sum, Coarse_source == "Wild"), aes(bm_kg, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "chartreuse4", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Bodymass (kg)") +
  #coord_cartesian(ylim = c(0, 2200)) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  theme_minimal()

## cl
cl_capt_plt <- ggplot(filter(cl_sum, Coarse_source == "Captive"), aes(cl, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Clutch size") +
  #coord_cartesian(ylim = c(0, 6000)) +
  theme_minimal()

cl_wild_plt <- ggplot(filter(cl_sum, Coarse_source == "Wild"), aes(cl, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "chartreuse4", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Clutch size") +
  #coord_cartesian(ylim = c(0, 6000)) +
  theme_minimal()

## ml
ml_capt_plt <- ggplot(filter(ml_sum, Coarse_source == "Captive"), aes(ml/365, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Longevity (years)") +
  #coord_cartesian(ylim = c(0, 3000)) +
  scale_x_log10() +
  theme_minimal()

ml_wild_plt <- ggplot(filter(ml_sum, Coarse_source == "Wild"), aes(ml/365, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "chartreuse4", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Longevity (years)") +
  #coord_cartesian(ylim = c(0, 3000)) +
  scale_x_log10() +
  theme_minimal()

## hb
hb_capt_plt <- ggplot(filter(hb_sum, Coarse_source == "Captive"), aes(hb, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Habitat breadth") +
  #coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal()

hb_wild_plt <- ggplot(filter(hb_sum, Coarse_source == "Wild"), aes(hb, .epred)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "chartreuse4", alpha = .5, 
              colour = "black", linetype = "dashed") +
  geom_line(colour = "black") +
  ylab("Quota volume") +
  xlab("Habitat breadth") +
  #coord_cartesian(ylim = c(0, 3000)) +
  theme_minimal()

## iucn
iucn_capt_plt <- ggplot(filter(iucn_sum, Coarse_source == "Captive"), aes(IUCN_code, .epred)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), colour = "grey", width = 0, size = 1) +
  geom_point(colour = "black", size = 2) +
  ylab("Quota volume") +
  xlab("IUCN assessment") +
  #coord_cartesian(ylim = c(0, 12000)) +
  theme_minimal()

iucn_wild_plt <- ggplot(filter(iucn_sum, Coarse_source == "Wild"), aes(IUCN_code, .epred)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), colour = "chartreuse4", width = 0, size = 1) +
  geom_point(colour = "black", size = 2) +
  ylab("Quota volume") +
  xlab("IUCN assessment") +
  #coord_cartesian(ylim = c(0, 12000)) +
  theme_minimal()

trait_plt <- ggarrange(bm_wild_plt, bm_capt_plt, cl_wild_plt, cl_capt_plt, 
          ml_wild_plt, ml_capt_plt, hb_wild_plt, hb_capt_plt, iucn_wild_plt,iucn_capt_plt, 
          labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.", "J."),
          nrow = 5, ncol = 2)

final_trait_plt <- trait_plt + 
  annotation_custom(text_grob("Wild-sourced", face = "bold", size= 11), xmin = 0.27, xmax = 0.27, ymin = 0.98, ymax = 1.0) +
  annotation_custom(text_grob("Captive-sourced", face = "bold", size= 11), xmin = 0.77, xmax = 0.77, ymin = 0.98, ymax = 1.0)

  

ggsave(path = "Outputs/Figures", final_trait_plt, filename = "Trait_plt.png",  bg = "white",
       device = "png", width = 25, height = 23, units = "cm")


## bm
bm_coef_sum <- fixef(bm_mod, summary = FALSE) %>% as.data.frame() %>%
  mutate(log_bm_captive = log_bm_z,
         log_bm_wild = log_bm_z + `log_bm_z:Coarse_sourceWild`,
         log_bm_diff_wild = `log_bm_z:Coarse_sourceWild`) %>%
  select(log_bm_captive, log_bm_wild, log_bm_diff_wild) %>%
  pivot_longer(everything(), names_to = "coef", values_to = "val") %>%
  group_by(coef) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100),
         val = val/sd(bm_dat$log_bm),
         type = "sd_raw",
         trait = "bm") %>%
  group_by(coef, pd, type, trait) %>%
  median_hdci(val, .width = .9)


## ml
ml_coef_sum <- fixef(ml_mod, summary = FALSE) %>% as.data.frame() %>%
  mutate(logml_captive = log_ml_z,
         logml_wild = log_ml_z + `log_ml_z:Coarse_sourceWild`,
         logml_diff_wild = `log_ml_z:Coarse_sourceWild`) %>%
  select(logml_captive, logml_wild, logml_diff_wild) %>%
  pivot_longer(everything(), names_to = "coef", values_to = "val") %>%
  group_by(coef) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100),
         val = val/sd(ml_dat$log_ml),
         type = "sd_raw",
         trait = "ml") %>%
  group_by(coef, pd, type, trait) %>%
  median_hdci(val, .width = .9)

## cl
cl_coef_sum <- fixef(cl_mod, summary = FALSE) %>% as.data.frame() %>%
  mutate(cl_captive = cl_z,
         cl_wild = cl_z + `cl_z:Coarse_sourceWild`,
         cl_diff_wild = `cl_z:Coarse_sourceWild`) %>%
  select(cl_captive, cl_wild, cl_diff_wild) %>%
  pivot_longer(everything(), names_to = "coef", values_to = "val") %>%
  group_by(coef) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100),
         val = val/sd(cl_dat$Litter_clutch_size),
         type = "sd_raw",
         trait = "cl") %>%
  group_by(coef, pd, type, trait) %>%
  median_hdci(val, .width = .9)

## hb
hb_coef_sum <- fixef(hb_mod, summary = FALSE) %>% as.data.frame() %>%
  mutate(hb_captive = hb_z,
         hb_wild = hb_z + `hb_z:Coarse_sourceWild`,
         hb_diff_wild = `hb_z:Coarse_sourceWild`) %>%
  select(hb_captive, hb_wild, hb_diff_wild) %>%
  pivot_longer(everything(), names_to = "coef", values_to = "val") %>%
  group_by(coef) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100),
         val = val/sd(hb_dat$hab_breadth),
         type = "sd_raw",
         trait = "hb") %>%
  group_by(coef, pd, type, trait) %>%
  median_hdci(val, .width = .9)

## iucn
iucn_coef_sum <- fixef(iucn_mod, summary = FALSE) %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "coef", values_to = "val") %>%
  group_by(coef) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100),
         type = "sd_raw",
         trait = "iucn") %>%
  group_by(coef, pd, type, trait) %>%
  median_hdci(val, .width = .9)

iucn_contrast_sum <- fixef(iucn_mod, summary = FALSE) %>% as.data.frame() %>%
  mutate(LC = exp(Intercept + Coarse_sourceWild),
         NT = exp(Intercept + Coarse_sourceWild + IUCN_codeNT + `IUCN_codeNT:Coarse_sourceWild`),
         VU = exp(Intercept + Coarse_sourceWild + IUCN_codeVU +`IUCN_codeVU:Coarse_sourceWild`),
         EN = exp(Intercept + Coarse_sourceWild + IUCN_codeEN +`IUCN_codeEN:Coarse_sourceWild`),
         CR = exp(Intercept + Coarse_sourceWild + IUCN_codeCR +`IUCN_codeCR:Coarse_sourceWild`),
         NE = exp(Intercept + Coarse_sourceWild + IUCN_codeNE +`IUCN_codeNE:Coarse_sourceWild`),
         LC_VU = LC - VU,
         LC_EN = LC - EN,
         LC_CR = LC - CR,
         NT_VU = NT - VU,
         NT_EN = NT - EN,
         NT_CR = NT - CR) %>%
  select(LC_VU, LC_EN, LC_CR, NT_VU, NT_EN, NT_CR) %>%
  pivot_longer(everything(), names_to = "contr", values_to = "val") %>%
  group_by(contr) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100)) %>%
  group_by(contr, pd) %>%
  median_hdci(val, .width = .9)
  


write.csv(bm_coef_sum, "Outputs/Summary/F4/trait_coef_sum.csv")
write.csv(cl_coef_sum, "Outputs/Summary/F4/cl_coef_sum.csv")
write.csv(ml_coef_sum, "Outputs/Summary/F4/ml_coef_sum.csv")
write.csv(hb_coef_sum, "Outputs/Summary/F4/hb_coef_sum.csv")
write.csv(iucn_coef_sum, "Outputs/Summary/F4/iucn_coef_sum.csv")
write.csv(rbind(bm_coef_sum, cl_coef_sum, ml_coef_sum, hb_coef_sum, iucn_coef_sum),
          "Outputs/Summary/F4/ALL_coef_sum.csv")


write.csv(bm_dat, "Outputs/Summary/F4/bm_fitting_dat.csv")
write.csv(cl_dat, "Outputs/Summary/F4/cl_fitting_dat.csv")
write.csv(ml_dat, "Outputs/Summary/F4/ml_fitting_dat.csv")
write.csv(hb_dat, "Outputs/Summary/F4/hb_fitting_dat.csv")
write.csv(iucn_dat, "Outputs/Summary/F4/iucn_fitting_dat.csv")
