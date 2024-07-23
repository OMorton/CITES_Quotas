
library(tidyverse)

trade_quota_genjoin <- read.csv("Data/CITES/Quotas/Trade_data_general_quota_join.csv")

## alternate figure

time_sum_int <- trade_quota_genjoin %>% filter(is.na(n_quotas), Int_trade_thr_L == "Yes") %>% group_by(Year) %>%
  summarise(vol = sum(Volume), n = n())
time_sum_thr <- trade_quota_genjoin %>% filter(is.na(n_quotas), Threat_st == "Threatened") %>% group_by(Year) %>%
  summarise(vol = sum(Volume), n = n())
time_sum_NE <- trade_quota_genjoin %>% filter(is.na(n_quotas), 
                                              IUCN_code %in% c("Not assessed", "DD")) %>% 
  group_by(Year) %>%
  summarise(vol = sum(Volume), n = n())

trade_quota_genjoin %>% filter(is.na(n_quotas), IUCN_code %in% c("Not assessed", "DD")) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)), sum(Volume))
trade_quota_genjoin %>% filter(is.na(n_quotas), IUCN_code %in% c("Not assessed", "DD"), Year < 2016) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)))
trade_quota_genjoin %>% filter(is.na(n_quotas), IUCN_code %in% c("Not assessed", "DD"), Year > 2015) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)))

trade_quota_genjoin %>% filter(is.na(n_quotas), Threat_st %in% c("Threatened")) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)), sum(Volume))
trade_quota_genjoin %>% filter(is.na(n_quotas), Threat_st %in% c("Threatened"), Year < 2016) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)))
trade_quota_genjoin %>% filter(is.na(n_quotas), Threat_st %in% c("Threatened"), Year > 2015) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)))

trade_quota_genjoin %>% filter(is.na(n_quotas), Int_trade_thr_L %in% c("Yes")) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)), sum(Volume))
trade_quota_genjoin %>% filter(is.na(n_quotas), Int_trade_thr_L %in% c("Yes"), Year < 2016) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)))
trade_quota_genjoin %>% filter(is.na(n_quotas), Int_trade_thr_L %in% c("Yes"), Year > 2015) %>% 
  summarise(length(unique(Taxon)), length(unique(Exporter)))

Int_line_plt <- ggplot(time_sum_int, aes(Year)) +
  geom_line(aes(y = n), colour = "darkred", size = 1) +
  geom_line(aes(y = vol/2000), colour = "darkred", alpha = .5, linetype = "dashed", size = .5) +
  scale_y_continuous(name = "Richness", 
                     sec.axis = sec_axis(~.*2000, name="Volume")) +
  theme_minimal(base_size = 8)

Thr_line_plt <- ggplot(time_sum_thr, aes(Year)) +
  geom_line(aes(y = n), colour = "darkorange", size = 1) +
  geom_line(aes(y = vol/2000), colour = "darkorange", alpha = .5, linetype = "dashed", size = .5) +
  scale_y_continuous(name = "Richness", 
                     sec.axis = sec_axis(~.*2000, name="Volume")) +
  coord_cartesian(ylim = c(0, 35)) +
  theme_minimal(base_size = 8)

DD_line_plt <- ggplot(time_sum_NE, aes(Year)) +
  geom_line(aes(y = n), colour = "darkblue", size = 1) +
  geom_line(aes(y = vol/2000), colour = "darkblue", alpha = .5, linetype = "dashed", size = .5) +
  scale_y_continuous(name = "Richness", 
                     sec.axis = sec_axis(~.*2000, name="Volume")) +
  theme_minimal(base_size = 8)


library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 

## Likely threatened
## 41 countries
time_sp_int_hist <- trade_quota_genjoin %>% 
  filter(is.na(n_quotas), Int_trade_thr_L == "Yes", Year < 2016) %>% group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) %>% right_join(world, by = c("Exporter" = "iso_a2"))
## 8 countries
time_sp_int_rec <- trade_quota_genjoin %>% 
  filter(is.na(n_quotas), Int_trade_thr_L == "Yes", Year > 2015) %>% group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) %>% right_join(world, by = c("Exporter" = "iso_a2"))

## Thr
## 64 countries
time_sp_thr_hist <- trade_quota_genjoin %>% filter(is.na(n_quotas), Threat_st == "Threatened", Year < 2016) %>%
  group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) %>% right_join(world, by = c("Exporter" = "iso_a2"))
## 28 countries
time_sp_thr_rec <- trade_quota_genjoin %>% filter(is.na(n_quotas), Threat_st == "Threatened", Year > 2015) %>%
  group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) %>% right_join(world, by = c("Exporter" = "iso_a2"))

## dd
## 80 countries
time_sp_NE_hist <- trade_quota_genjoin %>% filter(is.na(n_quotas),IUCN_code %in% c("Not assessed", "DD"),  Year < 2016) %>% 
  group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) %>% right_join(world, by = c("Exporter" = "iso_a2"))
## 19
time_sp_NE_rec <- trade_quota_genjoin %>% filter(is.na(n_quotas),IUCN_code %in% c("Not assessed", "DD"),  Year > 2015) %>% 
  group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) %>% right_join(world, by = c("Exporter" = "iso_a2"))

## Check
filter(time_sp_int_hist, !is.na(n))

tot_sum <- trade_quota_genjoin %>% filter(is.na(n_quotas), Year > 2015, 
                                          IUCN_code %in% c("Not assessed", "DD")|Threat_st == "Threatened"|Int_trade_thr_L == "Yes") %>% 
  group_by(Exporter) %>%
  summarise(vol = sum(Volume), n = n()) 

## DD
DD_hist_plt <- ggplot() + 
  geom_sf(data = time_sp_NE_hist, aes(geometry = geometry, fill = n), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_steps(low = "skyblue", high = "darkblue", na.value="grey90",
                   limits = c(1, 80)) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"))

DD_rec_plt <- ggplot() + 
  geom_sf(data = time_sp_NE_rec, aes(geometry = geometry, fill = n), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_steps(low = "skyblue", high = "darkblue", na.value="grey90",
                   limits = c(1, 80)) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"))

## Thr
Thr_hist_plt <- ggplot() + 
  geom_sf(data = time_sp_thr_hist, aes(geometry = geometry, fill = n), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_steps(low = "lightgoldenrod1", high = "darkorange", na.value="grey90",
                   limits = c(1, 66)) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"))

Thr_rec_plt <- ggplot() + 
  geom_sf(data = time_sp_thr_rec, aes(geometry = geometry, fill = n), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_steps(low = "lightgoldenrod1", high = "darkorange", na.value="grey90",
                   limits = c(1, 66)) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"))

## L Thr
Int_hist_plt <- ggplot() + 
  geom_sf(data = time_sp_int_hist, aes(geometry = geometry, fill = n), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_steps(low = "lightgoldenrod1", high = "darkred", na.value="grey90",
                   limits = c(1, 42)) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"))

Int_rec_plt <- ggplot() + 
  geom_sf(data = time_sp_int_rec, aes(geometry = geometry, fill = n), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_steps(low = "lightgoldenrod1", high = "darkred", na.value="grey90",
                   limits = c(1, 42)) +
  theme_classic(base_size = 10) +
  theme(panel.grid = element_blank(), legend.position = "right", legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"))

library(ggpubr)

empty <- ggplot() + theme_void()

coverage_plt_alt <- ggarrange(
  ggarrange(DD_line_plt, DD_hist_plt, DD_rec_plt, nrow = 1, common.legend = TRUE, legend = "right",
            labels = c("A.", "B.", "C.")),
  ggarrange(Thr_line_plt, Thr_hist_plt, Thr_rec_plt, nrow = 1, common.legend = TRUE, legend = "right",
            labels = c("D.", "E.", "F.")),
  ggarrange(Int_line_plt, Int_hist_plt, Int_rec_plt, nrow = 1, common.legend = TRUE, legend = "right",
            labels = c("G.", "H.", "I.")),
  nrow = 3)

coverage_plt_alt2 <- coverage_plt_alt + 
  annotation_custom(text_grob("Historical (1997 - 2015)", face = "bold", size= 9, hjust = 0), 
                    xmin = 0.37, xmax = 0.37, ymin = 0.97, ymax = 0.97) +
  annotation_custom(text_grob("Recent (2016 - 2021)", hjust = 0,face = "bold", size= 9), 
                    xmin = 0.68, xmax = 0.68, ymin = 0.97, ymax = 0.97) +
  annotation_custom(text_grob("80 exporters, 248 taxa", hjust = 0,face = "italic", size= 7), 
                    xmin = 0.37, xmax = 0.37, ymin = 0.67, ymax = 0.67) +
  annotation_custom(text_grob("19 exporters, 27 taxa", hjust = 0,face = "italic", size= 7), 
                    xmin = 0.68, xmax = 0.68, ymin = 0.67, ymax = 0.67) +
  annotation_custom(text_grob("64 exporters, 54 taxa", hjust = 0,face = "italic", size= 7), 
                    xmin = 0.37, xmax = 0.37, ymin = 0.34, ymax = 0.34) +
  annotation_custom(text_grob("28 exporters, 27 taxa", hjust = 0,face = "italic", size= 7), 
                    xmin = 0.68, xmax = 0.68, ymin = 0.34, ymax = 0.34) +
  annotation_custom(text_grob("41 exporters, 43 taxa", hjust = 0,face = "italic", size= 7), 
                    xmin = 0.37, xmax = 0.37, ymin = 0.01, ymax = 0.01) +
  annotation_custom(text_grob("8 exporters, 11 taxa", hjust = 0,face = "italic", size= 7), 
                    xmin = 0.68, xmax = 0.68, ymin = 0.01, ymax = 0.01)

ggsave(path = "Outputs/FINAL_FIGURES", coverage_plt_alt2, filename = "Figure4.png",  bg = "white",
       device = "png", width = 17, height = 8, units = "cm", dpi = 600)
ggsave(path = "Outputs/FINAL_FIGURES", coverage_plt_alt2, filename = "Figure4.pdf",  bg = "white",
       device = "pdf", width = 17, height = 8, units = "cm", dpi = 600)
