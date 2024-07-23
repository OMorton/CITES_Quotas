
###################
#### Functions ####
###################


#### Dist object to data frame ####

## takes a dist type object (e.g. from gawdis(), dist() etc. and outputs a dataframe)
dist.to.df <- function(inDist) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    value = as.vector(inDist))
}





#### Add trade data and plot compliance ####

compliance_plots <- function(data, geo_data, reporter) {
  
  if(reporter == "IR") {
    data <- data %>% select(-Volume, -Traded_purpose, -Traded_source, -Traded_taxa, -Perc_of_quota,
                            -Quota_breach)
    names(data) = gsub(pattern = "_IR", replacement = "", x = names(data))
    
    geo_data <- geo_data %>% select(-Volume, -Traded_purpose, -Traded_source, -Traded_taxa, -Perc_of_quota,
                                    -Quota_breach)
    names(geo_data) = gsub(pattern = "_IR", replacement = "", x = names(geo_data))
  }
  
  Ban_df <- data %>% filter(Zero_quota == "Yes", Other_term_quotas_in_place == "No")
  Quota_df <- data %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No") %>%
    mutate(Traded = case_when(Volume == 0 ~ "No",
                              Volume > 0 & Quota_breach == "No" ~ "Yes, no breach",
                              Volume > 0 & Quota_breach == "Yes" ~ "Yes, breach"))
  
  Ban_df_sum <- Ban_df %>% group_by(Quota_breach) %>% tally() %>% mutate(Perc = round(n/sum(n) * 100, 1))
  Quota_df_sum <- Quota_df %>% group_by(Quota_breach) %>% tally()%>% mutate(Perc = round(n/sum(n) * 100, 1))
  
  Ban_sum_plt <- ggplot(Ban_df_sum, aes(n, Quota_breach, fill = Quota_breach)) + 
    geom_col() +
    geom_text(aes(label = paste0(Perc, "%")), hjust = -.5) +
    scale_fill_manual(values = c("royalblue4", "tomato")) +
    coord_cartesian(xlim = c(0, 200)) +
    xlab("Number of quotas") +
    ylab("Breached <br> zero quotas") +
    theme_minimal(base_size = 9.5) +
    theme(legend.position = "none", axis.title.y = element_markdown())
  
  Quota_sum_plt <- ggplot(Quota_df_sum, aes(n, Quota_breach,fill = Quota_breach)) + 
    geom_col() +
    geom_text(aes(label = paste0(Perc, "%")), hjust = -.5) +
    scale_fill_manual(values = c("royalblue4", "tomato")) +
    coord_cartesian(xlim = c(0, 3200)) +
    xlab("Number of quotas") +
    ylab("Breached <br> non-zero quotas") +
    theme_minimal(base_size = 9.5) +
    theme(legend.position = "none", axis.title.y = element_markdown())
  
  Ban_map_df <- geo_data %>% filter(Zero_quota == "Yes", Other_term_quotas_in_place == "No") %>%
    group_by(Exporter, geometry) %>% tally()
  Banbreach_map_df <- geo_data %>% filter(Zero_quota == "Yes", Other_term_quotas_in_place == "No", Quota_breach == "Yes") %>%
    group_by(Exporter, geometry) %>% tally()
  Quota_map_df <- geo_data %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No") %>%
    group_by(Exporter, geometry) %>% tally()
  Quotabreach_map_df <- geo_data %>% filter(Zero_quota != "Yes", Other_term_quotas_in_place == "No", Quota_breach == "Yes") %>%
    group_by(Exporter, geometry) %>% tally()
  
  
  ban_map_plt <- ggplot() + 
    geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
    geom_sf(data = Ban_map_df, aes(fill = n, geometry = geometry)) +
    scale_fill_gradient(name = "Live zero-quotas", na.value="",
                        breaks = c(min(Ban_map_df$n), max(Ban_map_df$n)), 
                        low = "white", high = "royalblue4") +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    theme_classic(base_size = 10) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
          legend.title=element_text(size=9))
  
  banbreach_map_plt <- ggplot() + 
    geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
    geom_sf(data = Banbreach_map_df, aes(fill = n, geometry = geometry)) +
    scale_fill_gradient(name = "Live zero-quota breaches", na.value="", 
                        breaks = c(min(Banbreach_map_df$n), max(Banbreach_map_df$n)),
                        low = "white", high = "tomato") +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    theme_classic(base_size = 10) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
          legend.title=element_text(size=9))
  
  quota_map_plt <- ggplot() + 
    geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
    geom_sf(data = Quota_map_df, aes(fill = n, geometry = geometry)) +
    scale_fill_gradient(name = "Live non-zero quotas", na.value="",
                        breaks = c(min(Quota_map_df$n), max(Quota_map_df$n)), 
                        low = "white", high = "royalblue4") +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    theme_classic(base_size = 10) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
          legend.title=element_text(size=9))
  
  quotabreach_map_plt <- ggplot() + 
    geom_sf(data = Countries, aes(geometry = geometry), colour = NA) +
    geom_sf(data = Quotabreach_map_df, aes(fill = n, geometry = geometry)) +
    scale_fill_gradient(name = "Live non-zero quota breaches", na.value="",
                        breaks = c(min(Quotabreach_map_df$n), max(Quotabreach_map_df$n)),
                        low = "white", high = "tomato") +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    theme_classic(base_size = 10) +
    theme(panel.grid = element_blank(), legend.position = "bottom", legend.key.height = unit(.2, "cm"),
          legend.title=element_text(size=9))
  
  ## The really high quotas for ptyas mucosus are likely because of odd use of term codes
  ## in quotas. E.g. quotas are termed specifically for live individuals and skins are quota'd
  ## seperately but in reality the skins are alos traded live
  ## also true for Amyda cartilaginea, Naja sputatrix
  ## all examples so far are from Indonesia
  All_mean <- Quota_df %>% filter(Other_term_quotas_in_place == "No") %>% summarise(mean = round(mean(Perc_of_quota), 1))
  Traded_mean <- Quota_df %>% filter(Other_term_quotas_in_place == "No", Perc_of_quota > 0) %>%
    summarise(mean = round(mean(Perc_of_quota), 1),
              mean.c = format(round(mean(Perc_of_quota), 1), nsmall = 1))
  
  Inset_quota_perc <- ggplot(Quota_df, 
                             aes(Perc_of_quota, fill = Traded)) +
    geom_histogram(bins = 100, alpha = .9, colour = NA) +
    geom_vline(xintercept = 100, colour = "red") +
    scale_fill_manual(values = c("black", "tomato", "royalblue4")) +
    scale_colour_manual(values = c("black", "tomato", "royalblue4")) +
    xlab("% of quota") +
    ylab("Number of quotas") +
    theme_minimal(base_size = 10) +
    coord_cartesian(xlim = c(0, 1050)) +
    theme(legend.position = "none", axis.title = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "white"), 
          plot.background = element_rect(colour = "black", fill=NA))
  
  All_quota_perc <- ggplot(filter(Quota_df, Perc_of_quota < 250), 
                           aes(Perc_of_quota, fill = Traded)) +
    geom_histogram(bins = 75) +
    geom_vline(xintercept = 100, colour = "red") +
    geom_vline(xintercept = All_mean$mean, linetype = "dashed") +
    geom_vline(xintercept = Traded_mean$mean, linetype = "dashed", colour = "darkblue") +
    geom_text(aes(x = All_mean$mean - 10, y = 150, label = paste0( All_mean$mean, "%")), angle = 90) +
    geom_text(aes(x = Traded_mean$mean - 10, y = 150, label = paste0( Traded_mean$mean.c, "%")), angle = 90, colour = "darkblue") +
    coord_cartesian(ylim = c(0, 240)) +
    scale_fill_manual(values = c("black", "tomato", "royalblue4")) +
    scale_colour_manual(values = c("black", "tomato", "royalblue4")) +  xlab("% of quota used") +
    ylab("Number of quotas") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none")
  
  quota_arrange <- ggdraw() +
    draw_plot(All_quota_perc) +
    draw_plot(Inset_quota_perc, x = 0.52, y = 0.5, width = .45, height = .4)
  
  Quota_ban_arrange <- ggarrange(ggarrange(ban_map_plt, quota_map_plt, banbreach_map_plt, quotabreach_map_plt,
                                           Ban_sum_plt, Quota_sum_plt,  nrow = 3, ncol = 2,
                                           labels = c("A.", "B.", "C.", "D.", "E.", "F."), label.x = -0.01,
                                           heights = c(1, 1, 0.7)),
                                 quota_arrange, ncol = 1, labels = c("", "G."), heights = c(1.6, 1))
  
  return(list("Plot" = Quota_ban_arrange, "Ban_df" = Ban_df, "Quota_df" = Quota_df,
              "Quota_map_df" = Quota_map_df, "Quotabreach_map_df" = Quotabreach_map_df, 
              "Ban_map_df" = Ban_map_df, "Banbreach_map_df" = Banbreach_map_df))
}

#### Rounding function ####

# from https://gist.github.com/sotoattanito/8e6fad4b7322ceae9f14f342985f1681
# round() uses the round to even protocol, to maintain ease of interpretation rounding
# on .5 was done upwards

round.off <- function (x, digits=0) 
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}


  

#### Make concept plots ####

make_concept <- function(col = "blue", size = 1) {
# none
p1 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 5, yend = 5), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 5, yend = 5), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("1") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())

# step up
p2 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 5, yend = 5), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 10, yend = 10), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("2") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())

# step down
p3 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 6, yend = 6), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 1, yend = 1), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("3") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())

# Declining to plateau
p8 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 7, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 3, yend = 3), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("8") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())

# Increases to plateau
p5 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 3, yend = 7), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 7, yend = 7), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("5") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Decreasing from plateau
p7 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 7, yend = 7), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 7, yend = 3), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("7") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# increasing from plateau
p4 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 3, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 3, yend = 7), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("4") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# trend shift
p6 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 3, yend = 7), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 7, yend = 3), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("6") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Continuous temporal decrease, step increase
p9 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 6, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 7, yend = 4), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("9") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Continuous temporal increase, step decrease
p10 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 4, yend = 7), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 3, yend = 6), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("10") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Continuous temporal increase, step increase
p11 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 0, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 6, yend = 9), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("11") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
#Increases to elevated plateau
p12 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 0, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 6, yend = 6), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("12") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Increases to reduced plateau
p13 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 3, yend = 6), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 3, yend = 3), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("13") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Post-quota temporal and step increase
p14 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 3, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 6, yend = 9), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("14") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Step increase, subsequent decline
p15 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 3, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 6, yend = 3), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("15") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Temporal decrease with elevated plateau
p16 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 6, yend = 3), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 6, yend = 6), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("16") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())
# Trend shift, increase to decrease, with step decrease
p17 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 4, yend = 7), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 4, yend = 1), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("17") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())

# Trend shift, increase to decrease, with step increase
p18 <- ggplot() +
  geom_segment(aes(x = 0, xend = 5, y = 1, yend = 4), colour = "black", size = size) +
  geom_segment(aes(x = 5, xend = 10, y = 7, yend = 4), colour = col, size = size) +
  geom_vline(xintercept = 5, linetype = "dashed", size = size) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_classic(base_size = 8) +
  xlab("18") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())

return(list("p1" = p1, "p2" = p2, "p3" = p3, "p4" = p4, "p5" = p5, "p6" = p6, 
            "p7" = p7, "p8" = p8, "p9" = p9, "p10" = p10, "p11" = p11, "p12" = p12,
            "p13" = p13, "p14" = p14, "p15" = p15, "p16" = p16, "p17" = p17, "p18" = p18))
}
