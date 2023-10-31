################################
#### Quota data preparation ####
################################

options(scipen=999)
#.libPaths("C:/Packages")
source("Functions.R")

library(tidyverse)
library(ggpubr)
library(ggtext)
library(viridis)
library(cowplot)

CITES_Parties <- data.table::fread("Data/CITES/CITES_Parties.csv", na.strings = "")
Quota_clean_raw <- read.csv("Data/CITES/Quotas/Quota_codes_raw_summary.csv")
Quota_trade_listing <- read.csv("Data/CITES/Quotas/Rept_Quota_trade_listing.csv")
All_Listed_IUCN <- read.csv("Data/IUCN/ALL_REPT_ASSESSMENTS_series.csv")  
Live_rept_trade <- read.csv("Data/CITES/WOEs/Live_Rept_Trade.csv") 
Trade_thr_species <- read.csv("Data/Challender_2023.csv")

Rept_Listings <- data.table::fread("Data/CITES/CITES_Listings_Rept_07-23.csv") %>%
  rename(Taxon = `Scientific Name`) %>% select(10:26) %>%
  mutate(Taxon = gsub("/", " ", Taxon))
Name_trait <- read.csv("Data/Traits/Etard_quota_names.csv")
Countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf", type = "countries") %>%
  select(iso_a2, continent, region_un, region_wb, subregion)

IUCN_2023 <- All_Listed_IUCN %>% filter(Year == 2023) %>% select(-X)

Quota_length <- Quota_clean_raw %>% group_by(FullName) %>% tally() %>%
  mutate(Quota = "Yes")

Quota_trade_listing2 <- Quota_trade_listing %>% left_join(Countries, by = c("Exporter" = "iso_a2"))

Quota_clean_raw2 <- Quota_clean_raw %>% mutate(Party_for_join = case_when(grepl("Ivoire", party) ~ "Cote d'Ivoire",
                                  party == "Sudan [prior to secession of South Sudan]" ~ "Sudan",
                                  TRUE ~ party)) %>% left_join(CITES_Parties, by = c("Party_for_join" = "Name")) %>% 
  mutate(ISO = case_when(Party_for_join == "Timor-Leste"~ "TL",
                         Party_for_join == "South Sudan"~ "SS",
                         TRUE ~ ISO),
         Region = case_when(Party_for_join == "Timor-Leste"~ "Asia",
                         Party_for_join == "South Sudan"~ "Africa",
                         TRUE ~ Region),
         Status = case_when(Party_for_join == "Timor-Leste"~ "Non-party",
                            Party_for_join == "South Sudan"~ "Non-party",
                            TRUE ~ `Party status`)) %>%
  left_join(Countries, by = c("ISO" = "iso_a2"))

length(unique(Quota_trade_listing$Taxon)) ## 227
length(unique(Quota_trade_listing$Exporter)) ## 29

length(unique(Quota_clean_raw$FullName)) ## 344
length(unique(Quota_clean_raw$party)) ## 70

#### Types of quotas ####

## summarising
length(unique(Quota_clean_raw$FullName))
length(unique(Quota_trade_listing$Taxon))
length(unique(Quota_length$FullName))

## 2 family, 5 genus, 333 species, 4 ssp
Quota_clean_raw %>% group_by(Rank) %>% tally(n_distinct(FullName))

Order_sum <- Quota_clean_raw %>% group_by(Order) %>% tally()
Family_sum <- Quota_clean_raw %>% group_by(Family, Order) %>% tally()

Quota_types_sum <- Quota_clean_raw %>% group_by(Quota_type) %>% tally() %>%
  mutate(Quota_label = gsub("-specific", "", Quota_type),
         Quota_label = gsub("Non", "None", Quota_label),
         Tot = sum(n),
         Prop = round(n/Tot *100, 2),
         Type = ifelse(Quota_label == "None", "Yes", "No"))

Quota_Term_sum <- Quota_clean_raw %>% group_by(Term) %>% tally() %>%
  mutate(Term_label = gsub("FLAG", "None", Term),
         Tot = sum(n),
         Prop = round(n/Tot *100, 2),
         Type = case_when(Term_label == "None" ~ "None",
                          Term_label == "ALL" ~ "All",
                          Term_label != "ALL" & Term_label != "None" ~ "Other"))

Quota_Source_sum <- Quota_clean_raw %>% group_by(Source_orig) %>% tally() %>%
  mutate(Source_label = gsub("FLAG", "None", Source_orig),
         Tot = sum(n),
         Prop = round(n/Tot *100, 2),
         Type = case_when(Source_label == "Assumed Wild" ~ "Assumed Wild",
                          Source_label == "ALL" ~ "All",
                          Source_label != "ALL" & Source_label != "None" ~ "Other"))

Quota_Purpose_sum <- Quota_clean_raw %>% group_by(Purpose) %>% tally() %>%
  mutate(Purpose_label = gsub("FLAG", "None", Purpose),
         Tot = sum(n),
         Prop = round(n/Tot *100, 2),
         Type = case_when(Purpose_label == "None" ~ "None",
                          Purpose_label == "ALL" ~ "All",
                          Purpose_label != "ALL" & Purpose_label != "None" ~ "Other"))

## Plotting
Type_plt <- ggplot(Quota_types_sum, aes(Quota_label, n, fill = Type)) +
  geom_col() +
  scale_x_discrete(limits = c("Source",
                              "Purpose-Source", "Term-Source",
                              "Term-Source-Purpose")) +
  geom_text(aes(label = paste0(Prop, "%")), vjust = -.5, fontface = "bold") +
  scale_fill_manual(values = c("grey")) +
  coord_cartesian(ylim = c(0, 6000), expand = FALSE) +
  xlab("Quota specificity") +
  ylab("Number of quotas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.4), legend.position = "none")

Term_plt <- ggplot(Quota_Term_sum, aes(n, reorder(Term_label, n), fill = Type)) +
  geom_col() +
  geom_text(aes(label = paste0(Prop, "%")), hjust = -.5, fontface = "bold") +
  coord_cartesian(xlim = c(0, 6500), expand = FALSE) +
  scale_fill_manual(values = c("black", "grey", "tomato")) +
  xlab("Number of quotas") +
  ylab("Terms") +
  theme_minimal() +
  theme(legend.position = "none")

Source_plt <- ggplot(Quota_Source_sum, aes(n, reorder(Source_label, n), fill = Type)) +
  geom_col() +
  geom_text(aes(label = paste0(Prop, "%")), hjust = -.5, fontface = "bold") +
  coord_cartesian(xlim = c(0, 6000), expand = FALSE) +
  scale_fill_manual(values = c("black", "tomato", "grey")) +
  xlab("Number of quotas") +
  ylab("Source") +
  theme_minimal() +
  theme(legend.position = "none")

Purpose_plt <- ggplot(Quota_Purpose_sum, aes(n, reorder(Purpose_label, n), fill = Type)) +
  geom_col() +
  geom_text(aes(label = paste0(Prop, "%")), hjust = -.5, fontface = "bold") +
  coord_cartesian(xlim = c(0, 12000), expand = FALSE) +
  scale_fill_manual(values = c("black", "grey", "tomato")) +
  xlab("Number of quotas") +
  ylab("Purpose") +
  theme_minimal() +
  theme(legend.position = "none")

Summary_plt <- ggarrange(Type_plt, labels = c("A.", "B."),
          ggarrange(Term_plt, ggarrange(Source_plt, Purpose_plt, ncol = 1, labels = c("C.", "D."), align = "hv"), 
          ncol = 2, widths = c(1, 0.8), labels = c("B.", "")), ncol = 1, heights = c(1, 1.5))

ggsave(path = "Outputs/Figures", Summary_plt, filename = "Summary_plt.png",  bg = "white",
       device = "png", width = 20, height = 20, units = "cm")

write.csv(Quota_Purpose_sum, "Outputs/Summary/F1/Quota_Purpose_sum.csv")
write.csv(Quota_Source_sum, "Outputs/Summary/F1/Quota_Source_sum.csv")
write.csv(Quota_Term_sum, "Outputs/Summary/F1/Quota_Term_sum.csv")
write.csv(Quota_types_sum, "Outputs/Summary/F1/Quota_Types_sum.csv")

Order_plt <- ggplot(Order_sum, aes(reorder(Order, -n), n, fill = Order)) +
  geom_col() +
  xlab("Order") +ylab("Number of quotas") +
  scale_fill_manual(values = c("chartreuse4", "brown4", "bisque3", "skyblue3")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

Family_plt <- ggplot(Family_sum, aes(reorder(Family, -n), n, fill = Order)) +
  geom_col() +
  xlab("Family") +ylab("Number of quotas") +
  scale_fill_manual(values = c("chartreuse4", "brown4", "bisque3", "skyblue3")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

Taxo_plt <- ggarrange(Order_plt, Family_plt, labels = c("A.", "B."),
                                   ncol = 2, widths = c(1, 3), align = "hv")

ggsave(path = "Outputs/SM", Taxo_plt, filename = "Taxo_sum_plt.png",  bg = "white",
       device = "png", width = 20, height = 12, units = "cm")

#### Quota compliance ####

nrow(Quota_trade_listing) ## 3158
nrow(Quota_trade_listing %>% filter(Other_term_quotas_in_place == "No")) ## 2712

ER_compliance <- compliance_plots(data = Quota_trade_listing, geo_data = Quota_trade_listing2, reporter = "ER") 
IR_compliance <- compliance_plots(data = Quota_trade_listing, geo_data = Quota_trade_listing2, reporter = "IR")

ggsave(path = "Outputs/Figures", ER_compliance$Plot, filename = "Quota_ban_plt.png",  bg = "white",
       device = "png", width = 18, height = 22, units = "cm")

ggsave(path = "Outputs/SM", IR_compliance$Plot, filename = "Quota_ban_plt_IR.png",  bg = "white",
       device = "png", width = 18, height = 22, units = "cm")

Ban_df <- ER_compliance$Ban_df
Quota_df <- ER_compliance$Quota_df

nrow(Quota_df) + nrow(Ban_df) ## 2712

Quota_df %>% filter(Traded == "No")
Quota_map_df <- select(ER_compliance$Quota_map_df, -geometry)
Quotabreach_map_df <- select(ER_compliance$Quotabreach_map_df, -geometry)
Ban_map_df <- select(ER_compliance$Ban_map_df, -geometry)
Banbreach_map_df <- select(ER_compliance$Banbreach_map_df, -geometry)

write.csv(ER_compliance$Ban_df, "Outputs/Summary/F2/Ban_df.csv")
write.csv(ER_compliance$Quota_df, "Outputs/Summary/F2/Quota_df.csv")
write.csv(select(ER_compliance$Quota_map_df, -geometry), "Outputs/Summary/F2/Quota_map_df.csv")
write.csv(select(ER_compliance$Quotabreach_map_df, -geometry), "Outputs/Summary/F2/Quotabreach_map_df.csv")
write.csv(select(ER_compliance$Ban_map_df, -geometry), "Outputs/Summary/F2/Ban_map_df.csv")
write.csv(select(ER_compliance$Banbreach_map_df, -geometry), "Outputs/Summary/F2/Banbreach_map_df.csv")


#### Modelling before after quota ####

Quota_series1 <- Quota_trade_listing %>% 
  group_by(Taxon, Exporter, Quota_type) %>% filter(n() > 2, all(Other_term_quotas_in_place == "No")) %>%
  arrange(Taxon, Exporter, Quota_type, Year) %>% 
  mutate(prior = lag(Year), diff = Year - prior) %>% 
  filter(all(diff == 1 | is.na(diff)), n() > 4)

##86
eligible_quotas <- Quota_series1 %>% 
  group_by(Taxon, Exporter, Quota_type, Term, Purpose, Source) %>% summarise(min = min(Year), max = max(Year))

write.csv(eligible_quotas, "Data/CITES/Quotas/eligible_quotas_out.csv")

eligible_quotas_plus <- read.csv("Data/CITES/Quotas/eligible_quotas_in.csv") %>%
  select(-min,-max)

##69
eligible_quotas_final <- eligible_quotas %>% left_join(eligible_quotas_plus) %>% 
  ## get how long the party was a member before hand and the species was listed to check there is a prior reporting period
  mutate(Party_prior = Party_status - min, Species_prior = Species_listed - min) %>%
  ## Keep only those that have at least 4 years listed but unquota'd 
  filter(Party_prior < -4, Species_prior < -4) %>% group_by(Taxon, Exporter) %>% 
  ## remove E. cenchria as quota changed specificity part way through
  mutate(multiple_quota_types = n()) %>%
  filter(multiple_quota_types == 1)

## Make custom series from the year the trading party became a member or from when the species was listed
## whichever is the greater, to the final year of the quota. And then cut at 1990
eligible_quotas_series <- eligible_quotas_final %>% group_by(Taxon, Exporter, Quota_type) %>% 
  reframe(Year = seq(from = max(Party_status, Species_listed), to = max)) %>%
  filter(Year >= 1990)

eligible_quotas_for_join <- Quota_series1 %>% 
  select(Taxon, Exporter, Year, Quota_type, Term, Purpose, Source, Quota)

eligible_quotas_series2 <- eligible_quotas_series %>% 
  left_join(eligible_quotas_for_join, by = join_by("Taxon", "Exporter", "Quota_type", "Year")) %>%
  group_by(Taxon, Exporter, Quota_type) %>%
  fill(Term, .direction = "up") %>%
  fill(Source, .direction = "up") %>%
  fill(Purpose, .direction = "up")

Live_rept_trade_ER <- Live_rept_trade %>% filter(Reporter.type == "E")
Live_rept_trade_IR <- Live_rept_trade %>% filter(Reporter.type == "I")


## 69 Term source specific
TS_Sp_combo <- eligible_quotas_series2 %>% filter(Quota_type == "Term-Source-specific") %>%
  separate_rows(Source) %>%
  left_join(Live_rept_trade_ER, by = c("Taxon", "Exporter", "Term", "Year", "Source")) %>%
  rename(Purpose = Purpose.x) %>%
  group_by(Taxon, Exporter, Year, Quota_type, Quota, Term, Purpose) %>%
  mutate(Traded_source = ifelse(is.na(Volume), NA, Source)) %>%
  summarise(Total_volume = sum(Volume, na.rm = TRUE), Traded_source = str_c(unique(Traded_source), collapse = ", "),
            Source = str_c(unique(Source), collapse = ", "), 
            Traded_purpose = str_c(unique(Purpose.y), collapse = ", ")) %>%
  mutate(State = ifelse(is.na(Quota), "aPre-quota", "bPost-quota-actual"))

## check no multiple recorsd for a single year.
TS_Sp_combo %>% group_by(Taxon, Exporter, Quota_type, Year) %>% tally() %>% filter(n >1)

Centred_series <-  TS_Sp_combo %>% group_by(Taxon, Exporter, Quota_type, State) %>% 
  mutate(Last_noquota_year = ifelse(State == "aPre-quota" & Year == max(Year), Year, NA),
         First_quota_year = ifelse(State == "bPost-quota-actual" & Year == min(Year), Year, NA),
         Last_noquota_volume = ifelse(State == "aPre-quota" & Year == max(Year), Total_volume, NA)) %>%
  group_by(Taxon, Exporter, Quota_type) %>%
  fill(Last_noquota_year, .direction = "updown") %>%
  fill(Last_noquota_volume, .direction = "updown") %>%
  fill(First_quota_year, .direction = "updown") %>%
  fill(First_quota_year, .direction = "updown") %>%
  mutate(FYear = as.factor(Year)) %>%
  unite("Taxon_exp", c("Taxon", "Exporter"),  remove = FALSE)

length(unique(Centred_series$Taxon)) ## 68
length(unique(Centred_series$Exporter)) ## 12
length(unique(Centred_series$Taxon_exp)) ## 69

## add the quota volumes to the listings
Centred_series_quota <- Centred_series %>% filter(State == "bPost-quota-actual") %>%
  mutate(Total_volume = Quota, 
         State = "bPost-quota-quotas") %>%
  rbind(Centred_series) %>%
  group_by(Taxon, Exporter) %>%
  mutate(Vol_cent = Total_volume - Last_noquota_volume,
         vol_sd = Vol_cent/sd(Vol_cent),
         Year_cent = Year - First_quota_year)

ggplot(Centred_series_quota, aes(Year_cent, vol_sd, colour = State)) +
  geom_point() +
  facet_wrap(~Taxon, scales = "free")


library(tidybayes)
library(brms)

## sd modelling
Centred_series_sd <- Centred_series_quota %>% filter(!is.na(vol_sd), !is.infinite(vol_sd)) %>%
  mutate(Quota_breach = ifelse(Total_volume > Quota, "Yes", "No"))

hist(Centred_series_sd$vol_sd)


Pre_Post_Quota_mod <- brm(bf(vol_sd ~ State + Year_cent + State:Year_cent + (1|Exporter) +
                      (State + Year_cent + State:Year_cent|Taxon_exp) + (1|FYear),
                    sigma ~ 1), 
                 family = gaussian(),
                 prior = c(prior(normal(0, 1), class = "b"),
                           prior(normal(0, 1), class = "Intercept", dpar = "sigma"),
                           prior(normal(0, 1), class = "Intercept"),
                           prior(normal(0, 1), class = "sd"),
                           prior(lkj(2), class = "cor")),
                 file = "Outputs/Models/Pre_Post_Quotasd5.rds",
                 data = Centred_series_sd,
                 iter = 1000, warmup = 500, chains = 4, cores = 4)

PP_mod_sum <- Centred_series_sd %>% 
  add_epred_draws(Pre_Post_Quota_mod, re_formula = NULL)

PP_lines_sum <- PP_mod_sum  %>% 
  group_by(Year_cent, vol_sd,Taxon_exp,  Taxon, Exporter, State, Quota) %>%
  median_hdci(.epred, .width = .9) %>%
  unite("ID", c("Taxon_exp", "State"), remove = FALSE)

PP_all_sp_fig1 <- ggplot(PP_lines_sum, aes(Year_cent, .epred, colour = State,
                         fill = State, group = ID)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha= .3) +
  #geom_point(aes(y = vol_sd)) +
  ggforce::facet_wrap_paginate(~Taxon_exp, scales = "free", ncol = 5, nrow = 7, page = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("grey75","black", "royalblue4"), 
                    labels = c("Pre-quota volume", "Post-quota volume", "Quota level")) +
  scale_colour_manual(values = c("grey75", "black", "royalblue4"), 
                      labels = c("Pre-quota volume", "Post-quota volume", "Quota level")) +
  xlab("Times (years)") + ylab("Estimated volume (SD scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(path = "Outputs/SM", PP_all_sp_fig, filename = "PP_all_species.png",  bg = "white",
       device = "png", width = 25, height = 30, units = "cm")

PP_all_sp_fig2 <- ggplot(PP_lines_sum, aes(Year_cent, .epred, colour = State,
                                           fill = State, group = ID)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha= .3) +
  #geom_point(aes(y = vol_sd)) +
  ggforce::facet_wrap_paginate(~Taxon_exp, scales = "free", ncol = 5, nrow = 7, page = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("grey75","black", "royalblue4"), 
                    labels = c("Pre-quota volume", "Post-quota volume", "Quota level")) +
  scale_colour_manual(values = c("grey75", "black", "royalblue4"), 
                      labels = c("Pre-quota volume", "Post-quota volume", "Quota level")) +
  xlab("Times (years)") + ylab("Estimated volume (SD scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(path = "Outputs/SM", PP_all_sp_fig1, filename = "PP_all_species_p1.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")
ggsave(path = "Outputs/SM", PP_all_sp_fig2, filename = "PP_all_species_p2.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")

new_dat <- data.frame(Year_cent = c(-10:10, 0:10),
                      State = c(rep("aPre-quota", 10), rep("bPost-quota-actual", 11),
                                         rep("bPost-quota-quotas", 11)))

fixf_coef_sum <- fixef(Pre_Post_Quota_mod, summary = FALSE) %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "coef", values_to = "val") %>%
  group_by(coef) %>%
  mutate(pd = (sum(sign(val) == sign(median(val)))/n()*100)) %>%
  group_by(coef, pd) %>%
  median_hdci(val, .width = .9)

Fixef_pred_sum <- new_dat %>% 
  add_epred_draws(Pre_Post_Quota_mod, re_formula = NA) %>% 
  group_by(Year_cent, State) %>%
  median_hdci(.epred, .width = .9)

average_quota_plt <- ggplot(Fixef_pred_sum, aes(Year_cent, .epred, 
                                                colour = State, fill = State)) +
  geom_line(aes(Year_cent, .epred), size = 1) +
  geom_ribbon(aes(Year_cent, .epred, ymin = .lower, ymax = .upper), alpha = .2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Time") +
  ylab("Volume relative to <br> pre-quota volume (SD scale)") +
  scale_color_manual(values = c("black", "grey", "dodgerblue")) +
  scale_fill_manual(values = c("black", "grey", "dodgerblue")) +
  annotate(geom = "text", label = "Pre-quota", x = -5, y = 0.6, fontface = "bold", colour = "black") +
  annotate(geom = "text", label = "Post-quota traded volumes", x = 5, y =-1, fontface = "bold", colour = "grey") +
  annotate(geom = "text", label = "Post-quota quota levels", x = 5, y =1.7, fontface = "bold", colour = "dodgerblue") +
  theme_minimal() +
  theme(axis.title.y = element_markdown(), legend.position = "none")
  
Abs_change_coef <- coef(Pre_Post_Quota_mod, summary = FALSE)$Taxon[,,c("StatebPostMquotaMactual", 
                                                                              "StatebPostMquotaMquotas")] %>% 
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "Taxon", values_to = "coef") %>%
  separate(Taxon, c("Taxon", "Trt"), sep = "\\.") %>%
  group_by(Taxon, Trt) %>%
  mutate(pd = (sum(sign(coef) == sign(median(coef)))/n()*100)) %>%
  group_by(Taxon,Trt, pd) %>%
  median_hdci(coef, .width = .9) %>%
  mutate(Interpretation = case_when(pd < 95 ~ "Uncertain",
                                    pd >= 95 & pd < 97.5 & coef > 0 ~ "Uncertain Increase",
                                    pd >= 95 & pd < 97.5 & coef < 0 ~ "Uncertain Decrease",
                                    pd >= 97.5 & coef > 0 ~ "Increase",
                                    pd >= 97.5 & coef < 0 ~ "Decrease"),
         Interpretation = factor(Interpretation, levels = c("Decrease", "Uncertain Decrease", 
                                                            "Uncertain", "Uncertain Increase", "Increase")))

b1_quota <- Abs_change_coef %>% filter(Trt == "StatebPostMquotaMquotas") %>% arrange(coef) %>%
         mutate(Order = 1:n(), 
               Trt = "b1_Quota")
b2_actual <- Abs_change_coef %>% filter(Trt == "StatebPostMquotaMactual") %>% 
        left_join(select(b1_quota, Taxon, Order))%>%
         mutate(Trt = "b2_Actual")



Trend_change_coef <- coef(Pre_Post_Quota_mod, summary = FALSE)$Taxon[,,c("StatebPostMquotaMactual:Year_cent",
                                                                                "StatebPostMquotaMquotas:Year_cent")] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "Taxon", values_to = "coef") %>%
  separate(Taxon, c("Taxon", "Trt"), sep = "\\.") %>%
  group_by(Taxon, Trt) %>%
  mutate(pd = (sum(sign(coef) == sign(median(coef)))/n()*100)) %>%
  group_by(Taxon,Trt, pd) %>%
  median_hdci(coef, .width = .9) %>%
  mutate(Interpretation = case_when(pd < 95 ~ "Uncertain",
                                    pd >= 95 & pd < 97.5 & coef > 0 ~ "Uncertain Increase",
                                    pd >= 95 & pd < 97.5 & coef < 0 ~ "Uncertain Decrease",
                                    pd >= 97.5 & coef > 0 ~ "Increase",
                                    pd >= 97.5 & coef < 0 ~ "Decrease"),
         Interpretation = factor(Interpretation, levels = c("Decrease", "Uncertain Decrease", 
                                                            "Uncertain", "Uncertain Increase", "Increase")))

b1_quota_trend <- Trend_change_coef %>% filter(Trt == "StatebPostMquotaMquotas:Year_cent") %>% arrange(coef) %>%
  mutate(Order = 1:n(), 
         Trt = "b1_Quota")
b2_actual_trend <- Trend_change_coef %>% filter(Trt == "StatebPostMquotaMactual:Year_cent") %>% 
  left_join(select(b1_quota, Taxon, Order))%>%
  mutate(Trt = "b2_Actual")

abs_change_plt_Q <- ggplot(b1_quota, aes(reorder(Taxon, Order), coef,  colour = Interpretation)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("royalblue4", "grey75", "coral", "tomato4" )) +
  annotate(geom = "text", label = "Quota level", x = 2, y = 5, fontface = "bold", hjust = 0) +
  xlab("Quota (Taxon-exporter specific)") +
  ylab("Absolute change") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.title.y = element_markdown())

abs_change_plt_A <- ggplot(b2_actual, aes(reorder(Taxon, Order), coef,  colour = Interpretation)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("royalblue4", "grey75", "coral", "tomato4" )) +
  annotate(geom = "text", label = "Traded volume", x = 2, y = 2.2, fontface = "bold", hjust = 0) +
  xlab("Quota (Taxon-exporter specific)") +
  ylab("Absolute change") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.title.y = element_markdown())

trend_change_plt_Q <- ggplot(b1_quota_trend, aes(reorder(Taxon, Order), coef,  colour = Interpretation)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("royalblue4", "grey75","coral", "tomato4" )) +
  annotate(geom = "text", label = "Quota level", x = 2, y = 0.4, fontface = "bold", hjust = 0) +
  xlab("Quota (Taxon-exporter specific)") +
  ylab("Trend change") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.title.y = element_markdown())

trend_change_plt_A <- ggplot(b2_actual_trend, aes(reorder(Taxon, Order), coef,  colour = Interpretation)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("royalblue4", "steelblue", "grey75" )) +
  annotate(geom = "text", label = "Traded volume", x = 2, y = .15, fontface = "bold", hjust = 0) +
  xlab("Quota (Taxon-exporter specific)") +
  ylab("Trend change") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.title.y = element_markdown())

abs_concept <- data.frame(period = c(rep("pre", 5), rep("post_neg", 5), rep("post_pos", 5)), time = c(1:5, 6:10, 6:10),
           y = c(rep(5, 5), rep(3, 5), rep(7, 5)))
trend_concept <- data.frame(period = c(rep("pre", 5), rep("post_neg", 5), rep("post_pos", 5)), time = c(1:5, 6:10, 6:10),
                          y = c(rep(5, 5), 4.5, 4, 3.5, 3, 2.5, 5.5, 6, 6.5, 7, 7.5))

abs_concept_plt <- ggplot(abs_concept, aes(time, y, colour = period)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_color_manual(values = c("royalblue4", "tomato4", "black")) +
  geom_vline(xintercept = 6, linetype = "dashed") +
  xlab("Time") + ylab("Volume") +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank())

trend_concept_plt <- ggplot(trend_concept, aes(time, y, colour = period)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_color_manual(values = c("royalblue4", "tomato4", "black")) +
  geom_vline(xintercept = 6, linetype = "dashed") +
  xlab("Time") + ylab("Volume") +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank())


PP_quota_plt <- ggarrange(average_quota_plt, 
                          ggarrange(
                          ggarrange(abs_concept_plt, trend_concept_plt, labels = c("B.", "E."),
                                    nrow = 2),
                          ggarrange(abs_change_plt_Q, abs_change_plt_A,
                                    trend_change_plt_Q, trend_change_plt_A, 
                                    nrow = 4, labels = c("C.", "D.", "F.", "G."), align = "hv"),
                          ncol = 2, widths= c(.6, 1)),
                          labels = c("A.", ""), heights =  c(.4, 1), nrow = 2)
                                    

ggsave(path = "Outputs/Figures", PP_quota_plt, filename = "PP_quota.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")

write.csv(Centred_series_sd, "Outputs/Summary/F3/Model_fitting_data.csv")
write.csv(Trend_change_coef, "Outputs/Summary/F3/Trend_change_coef.csv")
write.csv(Abs_change_coef, "Outputs/Summary/F3/Abs_change_coef.csv")
write.csv(fixf_coef_sum, "Outputs/Summary/F3/fixf_coef_sum.csv")
write.csv(Centred_series_sd, "Outputs/Summary/F3/fitting_data.csv")






#### ER and IR misalignment ####


IR_breaches <- Quota_trade_listing %>% filter(Other_term_quotas_in_place == "No") %>% ## 2712
  filter(Volume_IR > Quota, Volume <= Quota) %>% 
  mutate(IR_perc = (Volume_IR/Quota *100)-100)

length(unique(IR_breaches$Taxon)) ## 36 species
range(IR_breaches$IR_perc, finite = 1) 

IR_breach_out <- IR_breaches %>% 
  arrange(-Perc_of_quota_IR) %>%
  mutate(Perc_of_quota = round(Perc_of_quota, 1),
         Perc_of_quota_IR = round(Perc_of_quota_IR, 1),
         Perc = paste0(Volume," (", Perc_of_quota, "%)"),
         Perc_IR = paste0(Volume_IR," (", Perc_of_quota_IR, "%)")) %>%
  select(Taxon, Family, Exporter, Year, Quota, 
         Perc, Perc_IR) 

write.csv(IR_breach_out, "Outputs/SM/IR_quota_breaches.csv")
