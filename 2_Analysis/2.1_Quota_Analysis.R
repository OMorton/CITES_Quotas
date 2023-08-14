################################
#### Quota data preparation ####
################################

options(scipen=999)
.libPaths("C:/Packages")

library(tidyverse)
library(ggpubr)
library(ggtext)
library(viridis)

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

length(unique(Quota_trade_listing$Taxon)) ## 236
length(unique(Quota_clean_raw$FullName)) ## 351
length(unique(Quota_clean_raw$party)) ## 70

#### Types of quotas ####

## summarising
length(unique(Quota_clean_raw$FullName))
length(unique(Quota_trade_listing$Taxon))
length(unique(Quota_length$FullName))

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

Quota_Source_sum <- Quota_clean_raw %>% group_by(Source) %>% tally() %>%
  mutate(Source_label = gsub("FLAG", "None", Source),
         Tot = sum(n),
         Prop = round(n/Tot *100, 2),
         Type = case_when(Source_label == "None" ~ "None",
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
  scale_x_discrete(limits = c("None", "Purpose", "Source", "Term",
                              "Purpose-Source", "Term-Source", "Term-Purpose",
                              "Term-Source-Purpose")) +
  geom_text(aes(label = paste0(Prop, "%")), vjust = -.5, fontface = "bold") +
  scale_fill_manual(values = c("grey", "tomato")) +
  coord_cartesian(ylim = c(0, 4000), expand = FALSE) +
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
  scale_fill_manual(values = c("black", "grey", "tomato")) +
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

#### Quota compliance ####

## absolute quota compliance
Ban_df <- Quota_trade_listing %>% filter(Zero_quota == "Yes", Other_term_quotas_in_place == "No")
Quota_df <- Quota_trade_listing %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No") %>%
  mutate(Traded = case_when(Volume == 0 ~ "No",
                            Volume > 0 & Quota_breach == "No" ~ "Yes, no breach",
                            Volume > 0 & Quota_breach == "Yes" ~ "Yes, breach"))

length(unique(Ban_df$Taxon))
length(unique(Ban_df$Name))
length(unique(Quota_df$Taxon))
length(unique(Quota_df$Name))

Ban_df_sum <- Ban_df %>% group_by(Quota_breach) %>% tally() %>% mutate(Perc = round(n/sum(n) * 100, 2))
Quota_df_sum <- Quota_df %>% group_by(Quota_breach) %>% tally()%>% mutate(Perc = round(n/sum(n) * 100, 2))

Ban_sum_plt <- ggplot(Ban_df_sum, aes(n, Quota_breach, fill = Quota_breach)) + 
  geom_col() +
  geom_text(aes(label = paste0(Perc, "%")), hjust = -.5) +
  scale_fill_manual(values = c("royalblue4", "tomato")) +
  coord_cartesian(xlim = c(0, 200)) +
  xlab("Number of quotas") +
  ylab("Breached <br> bans") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.y = element_markdown())

Quota_sum_plt <- ggplot(Quota_df_sum, aes(n, Quota_breach,fill = Quota_breach)) + 
  geom_col() +
  geom_text(aes(label = paste0(Perc, "%")), hjust = -.5) +
  scale_fill_manual(values = c("royalblue4", "tomato")) +
  coord_cartesian(xlim = c(0, 3200)) +
  xlab("Number of quotas") +
  ylab("Breached <br> quotas") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.y = element_markdown())

Ban_map_df <- Quota_trade_listing2 %>% filter(Zero_quota == "Yes", Other_term_quotas_in_place == "No") %>%
  group_by(Exporter, geometry) %>% tally()
Banbreach_map_df <- Quota_trade_listing2 %>% filter(Zero_quota == "Yes", Other_term_quotas_in_place == "No", Quota_breach == "Yes") %>%
  group_by(Exporter, geometry) %>% tally()
Quota_map_df <- Quota_trade_listing2 %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No") %>%
  group_by(Exporter, geometry) %>% tally()
Quotabreach_map_df <- Quota_trade_listing2 %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No", Quota_breach == "Yes") %>%
  group_by(Exporter, geometry) %>% tally()


ban_map_plt <- ggplot() + 
  geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
  geom_sf(data = Ban_map_df, aes(fill = n, geometry = geometry)) +
  scale_fill_gradient(name = "Live bans", na.value="", breaks = c(10, 90), 
                      low = "white", high = "royalblue4") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
        legend.title=element_text(size=10))

banbreach_map_plt <- ggplot() + 
  geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
  geom_sf(data = Banbreach_map_df, aes(fill = n, geometry = geometry)) +
  scale_fill_gradient(name = "Live ban breaches", na.value="", breaks = c(1, 10),
                      low = "white", high = "tomato") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
        legend.title=element_text(size=10))

quota_map_plt <- ggplot() + 
  geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
  geom_sf(data = Quota_map_df, aes(fill = n, geometry = geometry)) +
  scale_fill_gradient(name = "Live quotas", na.value="",breaks = c(100, 600), 
                      low = "white", high = "royalblue4") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
        legend.title=element_text(size=10))

quotabreach_map_plt <- ggplot() + 
  geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
  geom_sf(data = Quotabreach_map_df, aes(fill = n, geometry = geometry)) +
  scale_fill_gradient(name = "Live quota breaches", na.value="", breaks = c(50, 250),
                      low = "white", high = "tomato") +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
        legend.title=element_text(size=10))

## The really high quotas for ptyas mucosus are likely because of odd use of term codes
## in quotas. E.g. quotas are termed specifically for live individuals and skins are quota'd
## seperately but in reality the skins are alos traded live
## also true for Amyda cartilaginea, Naja sputatrix
## all examples so far are from Indonesia
All_mean <- Quota_df %>% filter(Other_term_quotas_in_place == "No") %>% summarise(mean = round(mean(Perc_of_quota), 2))
Traded_mean <- Quota_df %>% filter(Other_term_quotas_in_place == "No", Perc_of_quota > 0) %>% summarise(mean = round(mean(Perc_of_quota), 2))

Inset_quota_perc <- ggplot(Quota_df, 
                           aes(Perc_of_quota, fill = Traded, colour = Traded)) +
  geom_histogram(bins = 100, alpha = .9) +
  geom_vline(xintercept = 100, colour = "red") +
  scale_fill_manual(values = c("black", "tomato", "royalblue4")) +
  scale_colour_manual(values = c("black", "tomato", "royalblue4")) +
  xlab("% of quota") +
  ylab("Number of quotas") +
  theme_minimal() +
  theme(legend.position = "none", axis.title = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"))

All_quota_perc <- ggplot(filter(Quota_df, Perc_of_quota < 250), 
       aes(Perc_of_quota, fill = Traded)) +
  geom_histogram(bins = 75) +
  geom_vline(xintercept = 100, colour = "red") +
  geom_vline(xintercept = All_mean$mean, linetype = "dashed") +
  geom_vline(xintercept = Traded_mean$mean, linetype = "dashed", colour = "darkblue") +
  geom_text(aes(x = All_mean$mean - 10, y = 90, label = paste0( All_mean$mean, "%")), angle = 90) +
  geom_text(aes(x = Traded_mean$mean - 10, y = 90, label = paste0( Traded_mean$mean, "%")), angle = 90, colour = "darkblue") +
  coord_cartesian(ylim = c(0, 110)) +
  scale_fill_manual(values = c("black", "tomato", "royalblue4")) +
  scale_colour_manual(values = c("black", "tomato", "royalblue4")) +  xlab("% of quota used") +
  ylab("Number of quotas") +
  theme_minimal() +
  theme(legend.position = "none")

library(cowplot)
quota_arrange <- ggdraw() +
  draw_plot(All_quota_perc) +
  draw_plot(Inset_quota_perc, x = 0.5, y = 0.6, width = .5, height = .4)

Quota_ban_arrange <- ggarrange(ggarrange(ban_map_plt, quota_map_plt, banbreach_map_plt, quotabreach_map_plt,
                               Ban_sum_plt, Quota_sum_plt,  nrow = 3, ncol = 2,
                               labels = c("A.", "B.", "C.", "D.", "E.", "F."), label.x = -0.01,
                               heights = c(1, 1, 0.7)),
                               quota_arrange, ncol = 1, labels = c("", "G."), heights = c(1.6, 1))

ggsave(path = "Outputs/Figures", Quota_ban_arrange, filename = "Quota_ban_plt.png",  bg = "white",
       device = "png", width = 18, height = 22, units = "cm")

write.csv(Quota_df, "Outputs/Summary/F2/Quota_df.csv")
write.csv(select(Quota_map_df, -geometry), "Outputs/Summary/F2/Quota_map_df.csv")
write.csv(select(Quotabreach_map_df, -geometry), "Outputs/Summary/F2/Quotabreach_map_df.csv")
write.csv(select(Ban_map_df, -geometry), "Outputs/Summary/F2/Ban_map_df.csv")
write.csv(select(Banbreach_map_df, -geometry), "Outputs/Summary/F2/Banbreach_map_df.csv")


#### Modelling before after quota ####

Quota_series1 <- Quota_trade_listing %>% 
  group_by(Taxon, Exporter, Quota_type) %>% filter(n() > 2, all(Other_term_quotas_in_place == "No")) %>%
  arrange(Taxon, Exporter, Quota_type, Year) %>% 
  mutate(prior = lag(Year), diff = Year - prior) %>% 
  filter(all(diff == 1 | is.na(diff)), n() > 4)

eligible_quotas <- Quota_series1 %>% 
  group_by(Taxon, Exporter, Quota_type, Term, Purpose, Source) %>% summarise(min = min(Year), max = max(Year))

write.csv(eligible_quotas, "Data/CITES/Quotas/eligible_quotas_out.csv")

eligible_quotas_plus <- read.csv("Data/CITES/Quotas/eligible_quotas_in.csv")
eligible_quotas_final <- eligible_quotas_plus %>% 
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

## 46 Term specific quotas
T_Sp_combo <- eligible_quotas_series2 %>% filter(Quota_type == "Term-specific") %>%
  left_join(Live_rept_trade_ER, by = c("Taxon", "Exporter", "Term", "Year")) %>%
  rename(Purpose = Purpose.x, Source = Source.x) %>%
  group_by(Taxon, Exporter, Year, Quota_type, Quota, Term, Purpose, Source) %>%
  reframe(Total_volume = sum(Volume, na.rm = TRUE),
          Traded_source = str_c(unique(Source.y), collapse = ", "), 
          Traded_purpose = str_c(unique(Purpose.y), collapse = ", ")) %>%
  mutate(State = ifelse(is.na(Quota), "aPre-quota", "bPost-quota-actual"))

## 20 Term source specific
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
rbind(T_Sp_combo, TS_Sp_combo) %>% group_by(Taxon, Exporter, Quota_type, Year) %>% tally() %>% filter(n >1)

Centred_series <- rbind(T_Sp_combo, TS_Sp_combo) %>% group_by(Taxon, Exporter, Quota_type, State) %>% 
  mutate(Last_noquota_year = ifelse(State == "aPre-quota" & Year == max(Year), Year, NA),
         Last_noquota_volume = ifelse(State == "aPre-quota" & Year == max(Year), Total_volume, NA)) %>%
  group_by(Taxon, Exporter, Quota_type) %>%
  fill(Last_noquota_year, .direction = "updown") %>%
  fill(Last_noquota_volume, .direction = "updown") %>%
  mutate(FYear = as.factor(Year),
         sd = sd(Total_volume)) %>%
  unite("Taxon_exp", c("Taxon", "Exporter"),  remove = FALSE)

length(unique(Centred_series$Taxon)) ## 65
length(unique(Centred_series$Exporter)) ## 12
length(unique(Centred_series$Taxon_exp)) ## 66

## add the quota volumes to the listings
Centred_series_quota <- Centred_series %>% filter(State == "bPost-quota-actual") %>%
  mutate(Total_volume = Quota, 
         State = "bPost-quota-quotas") %>%
  rbind(Centred_series) %>%
  mutate(Vol_cent = Total_volume - Last_noquota_volume,
         vol_sd = Vol_cent/sd,
         Year_cent = Year - Last_noquota_year)


library(tidybayes)
library(brms)

## sd modelling
Centred_series_sd <- Centred_series_quota %>% filter(!is.na(vol_sd), !is.infinite(vol_sd)) %>%
  mutate(Quota_breach = ifelse(Total_volume > Quota, "Yes", "No")) %>%
  ## remove this species as it was traded in frequently and in very low volumes
  ## and then had very high quotas set (1800 sd away from the traded mean)
  filter(Taxon_exp != "Pelusios castaneus_MZ")

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
                 file = "Outputs/Models/Pre_Post_Quota2.rds",
                 data = Centred_series_sd,
                 iter = 1000, warmup = 500, chains = 4, cores = 4)

PP_mod_sum <- Centred_series_sd %>% 
  add_epred_draws(Pre_Post_Quota_mod, re_formula = NULL)

PP_lines_sum <- PP_mod_sum  %>% 
  group_by(Year_cent, vol_sd,Taxon_exp,  Taxon, Exporter, State, Quota, sd) %>%
  median_hdci(.epred, .width = .9) %>%
  unite("ID", c("Taxon_exp", "State"), remove = FALSE)

ggplot(PP_lines_sum, aes(Year_cent, .epred, colour = State,
                         fill = State, group = ID)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha= .3) +
  geom_point(aes(y = vol_sd)) +
  facet_wrap(~Taxon_exp, scales = "free", ncol = 6) +
  #scale_fill_manual(values = c("grey75", "royalblue4")) +
  #scale_colour_manual(values = c("grey75", "royalblue4")) +
  theme_minimal() +
  theme(legend.position = "none")

new_dat <- data.frame(Year_cent = c(-10:10, 1:10),
                      State = c(rep("aPre-quota", 11), rep("bPost-quota-actual", 10),
                                         rep("bPost-quota-quotas", 10)))

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
  annotate(geom = "text", label = "Post-quota traded volumes", x = 5, y =0.6, fontface = "bold", colour = "grey") +
  annotate(geom = "text", label = "Post-quota quota levels", x = 5, y =3.5, fontface = "bold", colour = "dodgerblue") +
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
  scale_color_manual(values = c("royalblue4", "grey75", "tomato4" )) +
  annotate(geom = "text", label = "Quota level", x = 2, y = 20, fontface = "bold", hjust = 0) +
  xlab("Quota (Taxon-exporter specific)") +
  ylab("Absolute change") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.title.y = element_markdown())

abs_change_plt_A <- ggplot(b2_actual, aes(reorder(Taxon, Order), coef,  colour = Interpretation)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("royalblue4", "steelblue", "grey75", "coral", "tomato4" )) +
  annotate(geom = "text", label = "Traded volume", x = 2, y = 2.2, fontface = "bold", hjust = 0) +
  xlab("Quota (Taxon-exporter specific)") +
  ylab("Absolute change") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none", axis.title.y = element_markdown())

trend_change_plt_Q <- ggplot(b1_quota_trend, aes(reorder(Taxon, Order), coef,  colour = Interpretation)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("royalblue4", "steelblue", "grey75", "tomato4" )) +
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
  geom_vline(xintercept = 5, linetype = "dashed") +
  xlab("Time") + ylab("Volume") +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_blank())

trend_concept_plt <- ggplot(trend_concept, aes(time, y, colour = period)) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_color_manual(values = c("royalblue4", "tomato4", "black")) +
  geom_vline(xintercept = 5, linetype = "dashed") +
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


