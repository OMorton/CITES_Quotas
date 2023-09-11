
################################
#### Quota data preparation ####
################################

options(scipen=999)
.libPaths("C:/Packages")

library(brms)
library(tidyverse)
library(viridis)
library(rredlist)

source("Functions.R")

#### Data read in ####
Quota_raw <- read_csv("Data/CITES/Rept_Quotas_RAW_Jun2023.csv") %>% select(!publication_date)
CITES_Parties <- data.table::fread("Data/CITES/CITES_Parties.csv")

## Example being here
Quota_raw %>% filter(party == "Ethiopia", year == 2005, `Full Name` == "Varanus niloticus")
Quota_raw[duplicated(Quota_raw),]

Quota_raw <- Quota_raw[!duplicated(Quota_raw),]

####
## 8445 annual quotas
## Trim trailing ws and remove regex patterns
Quota_raw <- Quota_raw %>% mutate(notes = str_trim(notes),
                                  notes = gsub("<i>", "", notes),
                                  notes = gsub("<i/>", "", notes),
                                  notes = gsub("</i>", "", notes),
                                  notes = gsub("<p>", "", notes),
                                  notes = gsub("<p/>", "", notes),
                                  notes = gsub("</p>", "", notes),
                                  notes = gsub("\r", "", notes),
                                  notes = gsub("\n", " ", notes))

## 690 distinct phrases used in the notes
length(unique(Quota_raw$notes))
Note_terms <- Quota_raw %>% distinct(notes)

## 303 distinct species-notes phrases where there is suspected
## taxanomic ambiguity.
Taxa_uncert <- Quota_raw %>% filter(grepl("originally", notes)|grepl("synon", notes)|
                                      grepl("split", notes)|grepl("taxonomic changes", notes)|
                                      grepl("lumped", notes)) %>% distinct(`Full Name`, notes)

## write out for classification
write.csv(Note_terms, "Data/CITES/Formatting/Note_terms_OUT.csv")
write.csv(Taxa_uncert, "Data/CITES/Formatting/Taxa_uncert_OUT.csv")

## read in and bind
Notes_terms_in <- read_csv("Data/CITES/Formatting/Note_terms_IN.csv")
Taxa_uncert_in <- read_csv("Data/CITES/Formatting/Taxa_uncert_IN.csv")

Quota_codes <- left_join(Quota_raw, Notes_terms_in, by = join_by(notes)) %>%
  left_join(Taxa_uncert_in, by = join_by(`Full Name`, notes)) %>%
  mutate(Purpose = ifelse(is.na(Purpose), "FLAG", Purpose),
         Source_orig = ifelse(is.na(Source)|Source == "FLAG", "Assumed Wild", Source),
         Source = ifelse(is.na(Source)|Source == "FLAG", "W", Source),
         Term = ifelse(is.na(Term), "FLAG", Term),
         Purpose_verbose = ifelse(is.na(Purpose_verbose), "FLAG", Purpose_verbose),
         Source_verbose = ifelse(is.na(Source_verbose), "W", Source_verbose),
         Term_verbose = ifelse(is.na(Term_verbose), "FLAG", Term_verbose),
         Taxa_Remove = ifelse(is.na(Taxa_Remove), 0, Taxa_Remove),
         #Rank = ifelse(!is.na(Ssp_specific), "SUBSPECIES", Rank), ## see comment below
         `Full Name` = ifelse(!is.na(Ssp_specific), Ssp_specific, `Full Name`),
         ## despite the notes having text detail specifying the quota applies to a specific 
         ## ssp, the ssp are never actually traded and the quotas are still recorded in the 
         ## data base as species level rather than sub species.
         `Full Name` = ifelse(`Full Name` == "Boa constrictor constrictor" , "Boa constrictor", `Full Name`),
         `Full Name` = ifelse(`Full Name` == "Morelia spilota variegata" , "Morelia spilota", `Full Name`),
         `Full Name` = ifelse(`Full Name` == "Morelia spilota harrisoni" , "Morelia spilota", `Full Name`),
         `Full Name` = ifelse(`Full Name` == "Liasis mackloti mackloti" , "Liasis mackloti", `Full Name`)) %>%
  rename(FullName = `Full Name`)

## Check instances where the note terms have not matched. Should be 0.
Quota_codes %>% filter(!is.na(notes), is.na(Term))

## Remove quotas for -1 these often link to specific notifications and concern the 
## issuance of permits under certain  conditions that cannot be verified from the available data.
## Similarly remove all unusable quotas
## 7761 
Quota_codes <- Quota_codes %>% filter(quota >= 0, is.na(Usability_check), Taxa_Remove == 0) %>%
  mutate(Quota_type = case_when(Term != "FLAG" & Purpose != "FLAG" & Source != "FLAG" ~ "Term-Source-Purpose-specific",
                                Term == "FLAG" & Purpose != "FLAG" & Source != "FLAG" ~ "Purpose-Source-specific",
                                Term != "FLAG" & Purpose == "FLAG" & Source != "FLAG" ~ "Term-Source-specific",
                                Term != "FLAG" & Purpose != "FLAG" & Source == "FLAG" ~ "Term-Purpose-specific",
                                Term == "FLAG" & Purpose == "FLAG" & Source != "FLAG" ~ "Source-specific",
                                Term != "FLAG" & Purpose == "FLAG" & Source == "FLAG" ~ "Term-specific",
                                Term == "FLAG" & Purpose != "FLAG" & Source == "FLAG" ~ "Purpose-specific",
                                Term == "FLAG" & Purpose == "FLAG" & Source == "FLAG" ~ "Non-specific")) %>%
  group_by(FullName, party, year) %>%
  ## assess for overlapping uncertain quotas e.g. where a party has multiple quotas for a single species
  ## in a single year at differing levels - ex. 100 ranched individuals and 1000 live individuals
  ## the quotas contradict
  mutate(Overlapping_quotas = ifelse(n_distinct(Quota_type) > 1, "Yes", "No")) %>% 
  group_by(FullName, party, year) %>% 
  mutate(Other_term_quotas_in_place = ifelse(n_distinct(Term) > 1, "Yes", "No"))

## most quotas concern live individuals
Quota_codes %>% group_by(Other_term_quotas_in_place) %>% tally() %>% arrange(-n)
Quota_codes %>% group_by(Term) %>% tally() %>% arrange(-n)
Quota_codes %>% group_by(Term, Quota_type) %>% tally() %>% arrange(-n)
Quota_codes %>% group_by(Quota_type) %>% tally() %>% arrange(-n)
Quota_codes %>% group_by(FullName, party, year) %>% tally() %>% filter(n >1)
Quota_codes %>% group_by(FullName, party) %>% tally() %>% arrange(-n)

write.csv(Quota_codes, "Data/CITES/Quotas/Quota_codes_raw_summary.csv")

Quota_LIV <- Quota_codes %>% filter(Term == "LIV", year < 2022) %>% group_by(FullName, party, year) %>%
  mutate(Overlapping_quotas_LIV = ifelse(n_distinct(Quota_type) > 1, "Yes", "No")) %>%
  filter(Overlapping_quotas_LIV == "No") %>% group_by(party, FullName, Purpose, Source, Term) %>%
  mutate(Quota_ID = cur_group_id()) %>%
  ungroup() %>% mutate(ROW_ID = 1:n())

## 25 issues
## 2 manually removed (records where the quota was reissued in a single year due to taxo changes)
## Kinyongia fischeri from Tanzania in 2010 line 702
## Tupinambis teguixin from Colombia in 2000 line 2044
## 8 summed over
## so the total number is 10. So 21 - 10 = 11 (total number of records should be 11 less than 3074)
Manual_check <- Quota_LIV %>% group_by(Quota_ID, year) %>% filter(n() > 1)
Manual_check %>% filter(!ROW_ID %in% c(664, 1294)) %>% group_by(party, year, Family, Order, Rank, FullName, Term, Term_verbose, 
                                                               Purpose, Purpose_verbose, Source, Source_verbose, unit,
                                                               Quota_type, Overlapping_quotas, Overlapping_quotas_LIV) %>%
  summarise(Quota = sum(quota))

check2 <- Manual_check %>% filter(!ROW_ID %in% c(664, 1294)) %>% group_by(party, year, Family, Order, Rank, FullName, Term, Term_verbose, 
                                                                Purpose, Purpose_verbose, Source, Source_verbose, unit,
                                                                Quota_type, Overlapping_quotas, Overlapping_quotas_LIV) %>%
  tally()

## Manually check for duplicated quotas where an update was provided for taxonomc updates
## Malaysia splits quotas between sabah, sarawak and peninsular from some species 
## - no data for these so sum over them.
## Similarly some split out the live purposes e.g. live (pets) and live (consumption)
## so sum over these as well.
## 3324 quota records
Quota_LIV_clean <- Quota_LIV %>% filter(!ROW_ID %in% c(664, 1294)) %>% 
  group_by(party, year, Family, Genus, Order, Rank, FullName, Term, Term_verbose, 
                       Purpose, Purpose_verbose, Source, unit,
                       Quota_type, Overlapping_quotas, Overlapping_quotas_LIV, Other_term_quotas_in_place) %>%
  summarise(Quota = sum(quota), Comb_raw_notes = str_c(unique(notes), collapse = " // ")) %>% 
  group_by(party, FullName, Purpose, Source, Term) %>%
  mutate(Quota_ID = cur_group_id())

length(unique(Quota_LIV_clean$FullName)) ## 227
length(unique(Quota_LIV_clean$party)) ## 29
Quota_LIV_clean %>% group_by(Quota_ID) %>% tally() %>% group_by(n) %>% tally()



#### Read in the CITES data ####
## Read in the 49 seperate .csv's from CITES latest Bulk release 2022 v1
## Takes a few minutes to run, total records 25,021,290
CITES_MASTER <- list.files(path="G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2023.1", 
                           full.names = TRUE, pattern="*.csv") %>% 
  lapply(read_csv, na = "", col_types = cols(Unit = col_character(), 
                                             Import.permit.RandomID = col_character(),
                                             Export.permit.RandomID = col_character(), 
                                             Origin.permit.RandomID = col_character(),
                                             Purpose = col_character())) %>% 
  bind_rows

## remove re exports and focus on live records
CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter  | is.na(Origin), 
                                      Class == "Reptilia", Year >= 1990 & Year < 2022,
                                      Term == "live", is.na(Unit)|Unit == "Number of specimens") %>%
  mutate(Purpose = ifelse(is.na(Purpose), "BLANK", Purpose),
         Source = ifelse(is.na(Source), "BLANK", Source))

## summarise the data to per term, source, purpose etc.
Trade_quota_sp <- CITES_TRUE %>% filter(Taxon %in% unique(Quota_LIV_clean$FullName)) %>%
  group_by(Year, Taxon, Term, Unit, Exporter, Purpose, Source, Reporter.type) %>% summarise(Volume = sum(Quantity))

write.csv(Trade_quota_sp, "Data/CITES/WOEs/Live_Rept_Trade.csv")

## ER and IR versions
Trade_quota_spER <- Trade_quota_sp %>% filter(Reporter.type ==  "E") %>% select(-Reporter.type)
Trade_quota_spIR <- Trade_quota_sp %>% filter(Reporter.type ==  "I") %>% select(-Reporter.type)

## Tidy up the quota dat.
## Split out the source, term, purpose comma seperated values.
## add Party ISO codes.
Quota_clean_expanded <- Quota_LIV_clean %>% separate_rows(Source, Term, Purpose) %>%
  rename(Taxon = FullName, Name = party, Year = year, Term = Term_verbose, Term_verbose = Term) %>%
  left_join(select(CITES_Parties, Name, ISO)) %>%
  rename(Exporter = ISO)

## 8 TSP, 3150 TS, (3,332 total)
Quota_LIV_clean %>% group_by(Quota_type) %>% tally()
Quota_LIV_clean %>% group_by(Rank) %>% tally()
Quota_clean_expanded_sp <- Quota_clean_expanded %>% filter(Rank %in% c("SPECIES", "SUBSPECIES"))
unique(Quota_clean_expanded_sp$Quota_type)

Genus_taxa <- Quota_LIV_clean %>% ungroup() %>% filter(Rank == "GENUS") %>% distinct(FullName)

## Term specific quotas
Quota_TermE <- left_join(filter(Quota_clean_expanded_sp, Quota_type == "Term-specific"), Trade_quota_spER,
                         by = c("Taxon", "Exporter", "Year", "Term")) %>%
  rename(Purpose = Purpose.x, Source = Source.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Taxon, Rank, 
           Term, Term_verbose, Purpose, Purpose_verbose, Source, 
           Overlapping_quotas_LIV,Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  summarise(Volume = sum(Volume), 
            Traded_purpose = str_c(unique(Purpose.y), collapse = ", "),
            Traded_source = str_c(unique(Source.y), collapse = ", ")) %>%
  mutate(Traded_taxa = Taxon)

Quota_TermI <- left_join(filter(Quota_clean_expanded_sp, Quota_type == "Term-specific"), Trade_quota_spIR,
                         by = c("Taxon", "Exporter", "Year", "Term")) %>%
  rename(Purpose = Purpose.x, Source = Source.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Taxon, Rank, 
           Term, Term_verbose, Purpose, Purpose_verbose, Source, 
           Overlapping_quotas_LIV,Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  summarise(Volume_IR = sum(Volume), 
            Traded_purpose_IR = str_c(unique(Purpose.y), collapse = ", "),
            Traded_source_IR = str_c(unique(Source.y), collapse = ", ")) %>%
  mutate(Traded_taxa_IR = Taxon)

Term_all <- left_join(Quota_TermE, Quota_TermI)

## Term-source specific quotas
Quota_TermSourceE <- left_join(filter(Quota_clean_expanded_sp, Quota_type == "Term-Source-specific"), 
                               Trade_quota_spER,
                               by = c("Taxon", "Exporter", "Year", "Term", "Source")) %>%
  rename(Purpose = Purpose.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Taxon, Rank, 
           Term, Term_verbose, Purpose, Purpose_verbose, 
           Overlapping_quotas_LIV, Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  mutate(Traded_source = ifelse(!is.na(Volume), Source, NA),
         Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  summarise(Volume = sum(Volume),
            Source = str_c(unique(Source), collapse = ", "), 
            Traded_source = str_c(unique(Traded_source), collapse = ", "),
            Traded_purpose = str_c(unique(Purpose.y), collapse = ", ")) %>%
  mutate(Traded_taxa = Taxon)

Quota_TermSourceI <- left_join(filter(Quota_clean_expanded_sp, Quota_type == "Term-Source-specific"), 
                               Trade_quota_spIR,
                               by = c("Taxon", "Exporter", "Year", "Term", "Source")) %>%
  rename(Purpose = Purpose.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Taxon, Rank, 
           Term, Term_verbose, Purpose, Purpose_verbose, 
           Overlapping_quotas_LIV, Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  mutate(Traded_source = ifelse(!is.na(Volume), Source, NA),
         Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  summarise(Volume_IR = sum(Volume), 
            Source = str_c(unique(Source), collapse = ", "), 
            Traded_source_IR = str_c(unique(Traded_source), collapse = ", "),
            Traded_purpose_IR = str_c(unique(Purpose.y), collapse = ", ")) %>%
  mutate(Traded_taxa_IR = Taxon)

TermSource_all <- left_join(Quota_TermSourceE, Quota_TermSourceI)

## Quota term purpose
Quota_TermPurposeE <- left_join(filter(Quota_clean_expanded_sp, Quota_type == "Term-Source-Purpose-specific"),
                                Trade_quota_spER,
                                by = c("Taxon", "Exporter", "Year", "Term", "Purpose")) %>%
  rename(Source = Source.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Taxon, Rank, 
           Term, Term_verbose, Purpose_verbose, Source, 
           Overlapping_quotas_LIV, Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  mutate(Traded_purpose = ifelse(!is.na(Volume), Purpose, NA),
         Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  summarise(Volume = sum(Volume),
            Purpose = str_c(unique(Purpose), collapse = ", "), 
            Traded_purpose = str_c(unique(Traded_purpose), collapse = ", "),
            Traded_source = str_c(unique(Source.y), collapse = ", ")) %>%
  mutate(Traded_taxa = Taxon)

Quota_TermPurposeI <- left_join(filter(Quota_clean_expanded_sp, Quota_type == "Term-Source-Purpose-specific"),
                                Trade_quota_spIR,
                                by = c("Taxon", "Exporter", "Year", "Term", "Purpose")) %>%
  rename(Source = Source.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Taxon, Rank, 
           Term, Term_verbose, Purpose_verbose, Source, 
           Overlapping_quotas_LIV, Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  mutate(Traded_purpose = ifelse(!is.na(Volume), Purpose, NA),
         Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  summarise(Volume_IR = sum(Volume),
            Purpose = str_c(unique(Purpose), collapse = ", "), 
            Traded_purpose_IR = str_c(unique(Traded_purpose), collapse = ", "),
            Traded_source_IR = str_c(unique(Source.y), collapse = ", ")) %>%
  mutate(Traded_taxa = Taxon)

TermPurpose_all <- left_join(Quota_TermPurposeE, Quota_TermPurposeI)

## genus sp
Genus_lvl <- CITES_TRUE %>% filter(Genus %in% Genus_taxa$FullName, Reporter.type == "E") %>%
  group_by(Year, Taxon, Genus, Term, Unit, Exporter, Purpose, Source) %>% 
  summarise(Volume = sum(Quantity))

Genus_lvl_IR <- CITES_TRUE %>% filter(Genus %in% Genus_taxa$FullName, Reporter.type == "I") %>%
  group_by(Year, Taxon, Genus, Term, Unit, Exporter, Purpose, Source) %>% 
  summarise(Volume = sum(Quantity))

Quota_genusE <- Quota_clean_expanded %>% filter(Rank == "GENUS") %>%
  left_join(Genus_lvl, by = c("Genus", "Exporter", "Year", "Term")) %>%
  rename(Purpose = Purpose.x, Source = Source.x, Taxon = Taxon.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Genus, Taxon, Rank, 
           Term, Term_verbose, Purpose, Purpose_verbose, Source, 
           Overlapping_quotas_LIV, Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  mutate(Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  summarise(Volume = sum(Volume), 
            Traded_purpose = str_c(unique(Purpose.y), collapse = ", "),
            Traded_source = str_c(unique(Source.y), collapse = ", "),
            Traded_taxa = str_c(unique(Taxon.y), collapse = ", "))

Quota_genusI <- Quota_clean_expanded %>% filter(Rank == "GENUS") %>%
  left_join(Genus_lvl_IR, by = c("Genus", "Exporter", "Year", "Term")) %>%
  rename(Purpose = Purpose.x, Source = Source.x, Taxon = Taxon.x) %>%
  group_by(Quota_type, Quota_ID, Name, Exporter, Year, Order, Family, Genus, Taxon, Rank, 
           Term, Term_verbose, Purpose, Purpose_verbose, Source, 
           Overlapping_quotas_LIV, Other_term_quotas_in_place, Comb_raw_notes, Quota) %>%
  mutate(Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  summarise(Volume_IR = sum(Volume), 
            Traded_purpose_IR = str_c(unique(Purpose.y), collapse = ", "),
            Traded_source_IR = str_c(unique(Source.y), collapse = ", "),
            Traded_taxa_IR = str_c(unique(Taxon.y), collapse = ", "))

Genus_all <- left_join(Quota_genusE, Quota_genusI)


Quota_traded_volumes <- rbind(Term_all, TermPurpose_all, TermSource_all, Genus_all) %>%
  mutate(Volume = ifelse(is.na(Volume), 0, Volume),
         Volume_IR = ifelse(is.na(Volume_IR), 0, Volume_IR),
         Perc_of_quota = Volume/Quota *100, 
         Perc_of_quota_IR = Volume_IR/Quota *100, 
         Quota_breach = ifelse(Volume > Quota, "Yes", "No"),
         Quota_breach_IR = ifelse(Volume_IR > Quota, "Yes", "No"),
         Zero_quota = ifelse(Quota == 0, "Yes", "No"), cs = 1) %>%
  arrange(Year) %>%
  rename(Quota_Rank = Rank)
  

write.csv(Quota_traded_volumes, "Data/CITES/Quotas/Cleaned_live_rept.csv")


#### Match Listings and IUCN data ####

Rept_Listings <- data.table::fread("Data/CITES/CITES_Listings_Rept_07-23.csv") %>%
  rename(Taxon = `Scientific Name`) %>% select(10:26) %>%
  mutate(Taxon = gsub("/", " ", Taxon))

## Check species with no match
Listing_match <- Quota_traded_volumes %>% left_join(select(Rept_Listings, Taxon, Listing, `Listed under`), by = c("Taxon"))
Check <- Listing_match %>% filter(is.na(Listing)) %>% ungroup() %>% select(Taxon, Year)
unique(Check$Taxon)
Quota_trade_listing <- Listing_match %>% mutate(Listing = case_when(Taxon == "Bothrops asper" ~ "II",
                                             Taxon == "Caiman crocodilus crocodilus" ~ "II",
                                             Taxon == "Caiman crocodilus yacare" ~ "II",
                                             Taxon == "Caiman crocodilus fuscus" ~ "II",
                                             Taxon == "Epicrates cenchria cenchria" ~ "II",
                                             Taxon == "Pelomedusa subrufa" ~ "III",
                                             Taxon == "Pelusios castaneus" ~ "III",
                                             Taxon == "Pelusios gabonensis" ~ "III",
                                             Taxon == "Pelusios niger" ~ "III",
                                             Taxon == "Naja" ~ "Genus-level quota",
                                             Taxon == "Salvator" ~ "Genus-level quota",
                                             Taxon == "Tupinambis" ~ "Genus-level quota",
                                             Taxon == "Colubridae" ~ "Genus-level quota",
                                             Taxon == "Lanthanotidae" ~ "Genus-level quota",
                                             Taxon == "Terrapene" ~ "Genus-level quota",
                                             Taxon == "Varanus" ~ "Genus-level quota",
                                             .default = Listing),
                                             Taxon = gsub("/", " ", Taxon)) %>%
  group_by(Taxon) %>%
  mutate(Quota_time = case_when(max(Year) > 2014 ~ "Quota >2014",
                                max(Year) > 2009 & max(Year) < 2015 ~ "Quota 2010-14",
                                max(Year) < 2010 ~ "Quota <2010"))

write.csv(Quota_trade_listing, "Data/CITES/Quotas/Rept_Quota_trade_listing.csv")

## 1212
Listed_names <- Rept_Listings %>% select(Taxon) %>% 
  rbind(data.frame(Taxon = c("Bothrops asper", "Caiman crocodilus crocodilus", "Caiman crocodilus fuscus",
                             "Caiman crocodilus yacare", "Epicrates cenchria cenchria",
                             "Pelomedusa subrufa", "Pelusios castaneus", "Pelusios gabonensis",
                             "Pelusios niger", "Naja", "Salvator", "Tupinambis", "Colubridae",
                             "Lanthanotidae", "Terrapene", "Varanus")))

NA_update <- Listed_names %>% mutate(IUCNName = case_when(Taxon == "Caiman crocodilus apaporiensis" ~ "Caiman crocodilus",
                                                     Taxon == "Crocodylus cataphractus" ~ "Mecistops cataphractus",
                                                     Taxon == "Ctenophorus caudicinctus" ~ "Ctenophorus caudicinctus",
                                                     Taxon == "Ctenophorus modestus" ~ "Ctenophorus modestus", ## NA
                                                     Taxon == "Ctenophorus spinodomus" ~ "Ctenophorus spinodomus", ## NA
                                                     Taxon == "Tympanocryptis argillosa" ~ "Tympanocryptis argillosa", ## NA 
                                                     Taxon == "Tympanocryptis fictilis" ~ "Tympanocryptis fictilis", ## NA
                                                     Taxon == "Tympanocryptis macra" ~ "Tympanocryptis macra", ## NA
                                                     Taxon == "Tympanocryptis mccartneyi" ~ "Tympanocryptis mccartneyi", ## NA
                                                     Taxon == "Tympanocryptis osbornei" ~ "Tympanocryptis osbornei", ## NA
                                                     Taxon == "Tympanocryptis petersi" ~ "Tympanocryptis petersi", ## NA
                                                     Taxon == "Tympanocryptis rustica" ~ "Tympanocryptis rustica", ## NA
                                                     Taxon == "Tympanocryptis tolleyi" ~ "Tympanocryptis tolleyi", ## NA
                                                     Taxon == "Uromastyx nigriventris" ~ "Uromastyx nigriventris", ## NA
                                                     Taxon == "Abronia cuetzpali" ~ "Abronia cuetzpali", ## NA
                                                     Taxon == "Abronia morenica" ~ "Abronia morenica", ## NA
                                                     Taxon == "Calumma gehringi" ~ "Calumma gehringi", ## NA
                                                     Taxon == "Calumma juliae" ~ "Calumma juliae", ## NA
                                                     Taxon == "Calumma lefona" ~ "Calumma lefona", ## NA
                                                     Taxon == "Calumma roaloko" ~ "Calumma roaloko", ## NA
                                                     Taxon == "Calumma uetzi" ~ "Calumma uetzi", ## NA
                                                     Taxon == "Kinyongia itombwensis" ~ "Kinyongia itombwensis", ## NA
                                                     Taxon == "Kinyongia msuyae" ~ "Kinyongia msuyae", ## NA
                                                     Taxon == "Kinyongia rugegensis" ~ "Kinyongia rugegensis", ## NA
                                                     Taxon == "Kinyongia tolleyae" ~ "Kinyongia tolleyae", ## NA
                                                     Taxon == "Karusaurus jordani" ~ "Karusaurus jordani", ## NA
                                                     Taxon == "Karusaurus polyzonus" ~ "Karusaurus polyzonus", ## NA
                                                     Taxon == "Pseudocordylus subviridis" ~ "Pseudocordylus subviridis", ## NA
                                                     Taxon == "Goniurosaurus chengzheng" ~ "Goniurosaurus chengzheng", ## NA
                                                     Taxon == "Goniurosaurus gezhi" ~ "Goniurosaurus gezhi", ## NA
                                                     Taxon == "Goniurosaurus gollum" ~ "Goniurosaurus gollum", ## NA
                                                     Taxon == "Goniurosaurus kwanghua" ~ "Goniurosaurus kwanghua", ## NA
                                                     Taxon == "Goniurosaurus sengokui" ~ "Goniurosaurus sengokui", ## NA
                                                     Taxon == "Goniurosaurus sinensis" ~ "Goniurosaurus sinensis", ## NA
                                                     Taxon == "Goniurosaurus varius" ~ "Goniurosaurus varius", ## NA
                                                     Taxon == 'Dactylocnemis ""Matapia""' ~ 'Dactylocnemis ""Matapia""', ## NA
                                                     Taxon == 'Dactylocnemis ""Mokohinaus""' ~ 'Dactylocnemis ""Mokohinaus""', ## NA
                                                     Taxon == 'Dactylocnemis ""North Cape""' ~ 'Dactylocnemis ""North Cape""', ## NA
                                                     Taxon == 'Dactylocnemis ""Poor Knights""' ~ 'Dactylocnemis ""Poor Knights""', ## NA
                                                     Taxon == 'Dactylocnemis ""Three Kings""' ~ 'Dactylocnemis ""Three Kings""', ## NA
                                                     Taxon == 'Naultinus ""north cape""' ~ 'Naultinus ""north cape""', ## NA
                                                     Taxon == "Nephrurus cinctus" ~ "Nephrurus cinctus", ## NA
                                                     Taxon == "Phelsuma andamanense" ~ "Phelsuma andamanensis", 
                                                     Taxon == "Sphaerodactylus nigropunctatus alayoi" ~ "Sphaerodactylus nigropunctatus", 
                                                     Taxon == "Sphaerodactylus nigropunctatus granti" ~ "Sphaerodactylus nigropunctatus",
                                                     Taxon == "Sphaerodactylus nigropunctatus lissodesmus" ~ "Sphaerodactylus nigropunctatus",
                                                     Taxon == "Sphaerodactylus nigropunctatus ocujal" ~ "Sphaerodactylus nigropunctatus",
                                                     Taxon == "Sphaerodactylus nigropunctatus strategus" ~ "Sphaerodactylus nigropunctatus",
                                                     Taxon == "Sphaerodactylus notatus atactus" ~ "Sphaerodactylus notatus",
                                                     Taxon == "Strophurus trux" ~ "Strophurus trux", ## NA
                                                     Taxon == "Uroplatus fiera" ~ "Uroplatus fiera", ## NA
                                                     Taxon == "Uroplatus fotsivava" ~ "Uroplatus fotsivava", ## NA
                                                     Taxon == "Uroplatus kelirambo" ~ "Uroplatus kelirambo", ## NA
                                                     Taxon == "Woodworthia brunneus" ~ "Woodworthia cf. brunnea", 
                                                     Taxon == 'Woodworthia ""Central Otago""' ~ 'Woodworthia ""Central Otago""',## NA
                                                     Taxon == "Woodworthia chrysosireticus" ~ "Woodworthia chrysosiretica",
                                                     Taxon == 'Woodworthia ""Cromwell Gorge""' ~ 'Woodworthia ""Cromwell Gorge""',## NA
                                                     Taxon == 'Woodworthia ""Kaikouras""' ~ 'Woodworthia ""Kaikouras""',## NA
                                                     Taxon == "Woodworthia maculatus" ~ "Woodworthia maculata",
                                                     Taxon == 'Woodworthia ""Marlborough Mini""' ~ 'Woodworthia ""Marlborough Mini""',## NA
                                                     Taxon == 'Woodworthia ""Mt. Arthur""' ~ 'Woodworthia ""Mt. Arthur""',## NA
                                                     Taxon == 'Woodworthia ""Otago Southland""' ~ 'Woodworthia ""Otago Southland""',## NA
                                                     Taxon == 'Woodworthia ""Pygmy""' ~ 'Woodworthia ""Pygmy""',## NA
                                                     Taxon == 'Woodworthia ""Southern Alps""' ~ 'Woodworthia ""Southern Alps"" ',## NA
                                                     Taxon == 'Woodworthia ""Southern Mini""' ~ 'Woodworthia ""Southern Mini""',## NA
                                                     Taxon == 'Heloderma horridum charlesbogerti' ~ 'Heloderma charlesbogerti',
                                                     Taxon == 'Cyclura ricordi' ~ 'Cyclura ricordii',
                                                     Taxon == 'Phrynosoma bauri' ~ 'Phrynosoma bauri', ## NA
                                                     Taxon == 'Phrynosoma brevirostris' ~ 'Phrynosoma brevirostris',## NA
                                                     Taxon == 'Phrynosoma cerroense' ~ 'Phrynosoma cerroense',## NA
                                                     Taxon == 'Phrynosoma diminutum' ~ 'Phrynosoma diminutum',## NA
                                                     Taxon == 'Phrynosoma ornatissimum' ~ 'Phrynosoma ornatissimum',## NA
                                                     Taxon == 'Phrynosoma sherbrookei' ~ 'Phrynosoma sherbrookei',## NA
                                                     Taxon == 'Phrynosoma wigginsi' ~ 'Phrynosoma wigginsi',## NA
                                                     Taxon == 'Egernia roomi' ~ 'Egernia roomi',## NA
                                                     Taxon == 'Tiliqua scincoides intermedia' ~ 'Tiliqua scincoides',
                                                     Taxon == 'Tiliqua scincoides scincoides' ~ 'Tiliqua scincoides',
                                                     Taxon == 'Tupinambis cryptus' ~ 'Tupinambis cryptus',## NA
                                                     Taxon == 'Tupinambis cuzcoensis' ~ 'Tupinambis cuzcoensis',## NA
                                                     Taxon == 'Tupinambis zuliensis' ~ 'Tupinambis zuliensis',## NA
                                                     Taxon == 'Varanus cerambonensis' ~ 'Varanus cerambonensis',## NA
                                                     Taxon == 'Varanus douarrha' ~ 'Varanus douarrha',## NA
                                                     Taxon == 'Varanus nebulosus' ~ 'Varanus bengalensis',
                                                     Taxon == 'Varanus nesterovi' ~ 'Varanus nesterovi', ## NA 
                                                     Taxon == 'Varanus ornatus' ~ 'Varanus ornatus', ## NA 
                                                     Taxon == 'Varanus semotus' ~ 'Varanus semotus', ## NA 
                                                     Taxon == 'Varanus similis' ~ 'Varanus scalaris',  
                                                     Taxon == 'Boa constrictor occidentalis' ~ 'Boa constrictor',  
                                                     Taxon == 'Gongylophis colubrinus' ~ 'Eryx colubrinus',  
                                                     Taxon == 'Gongylophis conicus' ~ 'Eryx conicus',  
                                                     Taxon == 'Gongylophis muelleri' ~ 'Eryx muelleri',  
                                                     Taxon == 'Cyclagras gigas' ~ 'Hydrodynastes gigas',  
                                                     Taxon == 'Ptyas mucosus' ~ 'Ptyas mucosa',  
                                                     Taxon == 'Xenochrophis piscator' ~ 'Fowlea piscator',  
                                                     Taxon == 'Xenochrophis schnurrenbergeri' ~ 'Fowlea schnurrenbergeri',  
                                                     Taxon == 'Xenochrophis tytleri' ~ 'Fowlea tytleri',  
                                                     Taxon == 'Leiopython bennettorum' ~ 'Leiopython bennettorum', ## NA 
                                                     Taxon == 'Leiopython hoserae' ~ 'Leiopython meridionalis',
                                                     Taxon == 'Morelia boeleni' ~ 'Simalia boeleni',
                                                     Taxon == 'Morelia clastolepis' ~ 'Simalia clastolepis',
                                                     Taxon == 'Morelia kinghorni' ~ 'Morelia kinghorni', ## NA
                                                     Taxon == 'Morelia nauta' ~ 'Simalia nauta',
                                                     Taxon == 'Morelia oenpelliensis' ~ 'Simalia oenpelliensis',
                                                     Taxon == 'Morelia tracyae' ~ 'Simalia tracyae',
                                                     Taxon == 'Chelus fimbriatus' ~ 'Chelus fimbriatus', ## NA
                                                     Taxon == 'Mauremys iversoni' ~ 'Mauremys iversoni', ## NA
                                                     Taxon == 'Mauremys pritchardi' ~ 'Mauremys reevesii',
                                                     Taxon == 'Ocadia glyphistoma' ~ 'Mauremys sinensis', 
                                                     Taxon == 'Ocadia philippeni' ~ 'Mauremys sinensis', 
                                                     Taxon == 'Rhinoclemmys diademata' ~ 'Rhinoclemmys diademata', ## NA
                                                     Taxon == 'Rhinoclemmys melanosterna' ~ 'Rhinoclemmys melanosterna', ## NA
                                                     Taxon == 'Rhinoclemmys pulcherrima' ~ 'Rhinoclemmys pulcherrima', ## NA
                                                     Taxon == 'Rhinoclemmys punctularia' ~ 'Rhinoclemmys punctularia', ## NA
                                                     Taxon == 'Sacalia pseudocellata' ~ 'Sacalia quadriocellata',
                                                     Taxon == 'Kinosternon cora' ~ 'Kinosternon cora', ## NA
                                                     Taxon == 'Kinosternon leucostomum' ~ 'Kinosternon leucostomum', ## NA
                                                     Taxon == 'Kinosternon scorpioides' ~ 'Kinosternon scorpioides', ## Now split
                                                     Taxon == 'Kinosternon steindachneri' ~ 'Kinosternon steindachneri', ## NA
                                                     Taxon == 'Kinosternon stejnegeri' ~ 'Kinosternon stejnegeri', ## NA
                                                     Taxon == 'Podocnemis vogli' ~ 'Podocnemis vogli', ## NA
                                                     Taxon == 'Aldabrachelys gigantea' ~ 'Geochelone gigantea',
                                                     Taxon == 'Chelonoidis carbonarius' ~ 'Chelonoidis carbonarius', ## NA
                                                     Taxon == 'Chelonoidis denticulatus' ~ 'Chelonoidis denticulata',
                                                     Taxon == 'Gopherus morafkai' ~ 'Gopherus morafkai', ## NA
                                                     Taxon == 'Kinixys belliana' ~ 'Kinixys belliana', ## Now split
                                                     Taxon == 'Kinixys nogueyi' ~ 'Kinixys nogueyi', ## NA
                                                     Taxon == 'Kinixys spekii' ~ 'Kinixys spekii', ## NA
                                                     Taxon == 'Kinixys zombensis' ~ 'Kinixys zombensis', ## NA
                                                     Taxon == 'Psammobates oculifer' ~ 'Kinixys zombensis', ## NA
                                                     Taxon == 'Apalone spinifera atra' ~ 'Apalone spinifera ssp. atra', ## MANUALLY
                                                     Taxon == 'Pelodiscus axenaria' ~ 'Pelodiscus axenaria', ## NA
                                                     Taxon == 'Pelodiscus maackii' ~ 'Pelodiscus maackii', ## NA
                                                     Taxon == 'Pelodiscus parviformis' ~ 'Pelodiscus maackii', ## NA
                                                     Taxon == 'Caiman crocodilus crocodilus' ~ 'Caiman crocodilus', 
                                                     Taxon == 'Caiman crocodilus yacare' ~ 'Caiman yacare',
                                                     Taxon == 'Epicrates cenchria cenchria' ~ 'Epicrates cenchria',
                                                     Taxon == 'Pelomedusa subrufa' ~ 'Pelomedusa galeata',
                                                     Taxon == 'Pelusios castaneus' ~ 'Pelusios castaneus', ## NA
                                                     Taxon == 'Pelusios gabonensis' ~ 'Pelusios gabonensis', ## NA
                                                     Taxon == 'Caiman crocodilus fuscus' ~ 'Caiman crocodilus',
                                                     Taxon == 'Naja' ~ 'Naja sp.', ## NA
                                                     Taxon == 'Salvator' ~ 'Salvator sp.', ## NA
                                                     Taxon == 'Tupinambis' ~ 'Tupinambis sp.', ## NA
                                                     Taxon == 'Colubridae' ~ 'Colubridae sp.', ## NA
                                                     Taxon == 'Lanthanotidae' ~ 'Lanthanotidae sp.', ## NA
                                                     Taxon == 'Terrapene' ~ 'Terrapene sp.', ## NA
                                                     Taxon == 'Varanus' ~ 'Varanus sp.', ## NA
                                                     TRUE ~ Taxon))

## key
apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

## Dummy data frame for the loops
df <- data.frame(Taxon = character(),
                 Year = character(),
                 IUCN_code = character(),
                 IUCN_cat = character())

## Get species historical statuses.
## Needs an API key and because of the delay needed between calls takes ~1 hour to run.
## Do not remove the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(NA_update)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- NA_update$IUCNName[i]
  iucnHistory <- rl_history(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnHistory$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Year = NA,
                       IUCN_code = NA,
                       IUCN_cat = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Year = iucnHistory$result$year,
                       IUCN_code = iucnHistory$result$code,
                       IUCN_cat = iucnHistory$result$category)
    df <- rbind(df, spdf)
  }
}

NA_List <- df %>% filter(is.na(Year))
#write.csv(NA_List, "Data/IUCN/IUCN_API_NA_ASSESSMENTS.csv")  

df2 <- df %>% filter(IUCNName != "Apalone spinifera ssp. atra") %>% 
  rbind(data.frame(IUCNName = "Apalone spinifera ssp. atra", Year = c(1996, 2016),
                   IUCN_code = "CR", IUCN_cat = "Critically Endangered"))

#write.csv(df2, "Data/IUCN/ALL_REPT_ASSESSMENTS.csv")  
df2 <- read.csv("Data/IUCN/ALL_REPT_ASSESSMENTS.csv")  

Historic_IUCN <- df2 %>% 
  ## Set Not assessed species as not assessed in 2000 otherwise they are also NA for Year and get removed later when they shouldnt
  mutate(Year = if_else(is.na(Year), 2000, as.numeric(Year))) %>%
  filter(!IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt"))

length(unique(Historic_IUCN$IUCNName)) ## 1194

## Keep these but flag them
Historic_IUCN %>% filter(IUCN_code %in% c("EX", "EW"))

## Convert the 1994 system to post-2000
Historic_IUCN_up <- Historic_IUCN %>%
  mutate(IUCN_code = replace(IUCN_code, IUCN_code == "LR/lc", "LC"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/nt", "NT"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/cd", "NT"),
         IUCN_code = replace(IUCN_code, IUCN_code == "Ex", "EX"),
         IUCN_code = replace(IUCN_code, IUCN_code == "Ex?", "EX"))

## Check removal and conversion left only the post 2001 framework
unique(Historic_IUCN_up$IUCN_code)
unique(Historic_IUCN_up$IUCNName) ## 1194

## Backbone of values 1975 - 2023
backbone <- expand.grid(Year = as.integer(1975:2023), IUCNName = unique(Historic_IUCN_up$IUCNName))

## left join this and create your unrolled status
## Some species in trade before being IUCN assessed these are the NA values
Historic_IUCN_up$Year <- as.integer(Historic_IUCN_up$Year)
backbone$Year <- as.integer(backbone$Year)

## Here we add the backbone of species and dates to the IUCN data
df_new <- left_join(backbone, Historic_IUCN_up) %>%
  arrange(IUCNName, Year) %>% group_by(IUCNName) %>% 
  fill(IUCN_code , .direction = "down") %>% 
  fill(IUCNName, .direction = "down") %>% 
  fill(IUCNName, .direction = "up") %>% 
  filter(Year %in% c(1997:2023)) %>% ungroup() %>%
  mutate(Year = as.numeric(Year)) %>% 
  select(Year, IUCN_code, IUCNName) %>% 
  mutate(IUCN_code = replace_na(IUCN_code, "Not assessed")) %>% distinct()

## Now we have a clean time series of all statuses for all species 1999 - 2023
length(unique(df_new$IUCNName)) ## 1194 species

## Check no species were assessed twice in one year (2 records for 1 year interfere with the joins)
df_new %>% group_by(Year, IUCNName) %>% tally() %>% filter(n != 1)
df_new <- df_new %>% mutate(ID = paste(IUCNName, Year, IUCN_code, sep = "_")) %>% 
  filter(ID != "Chersobius signatus_2018_VU")

## all IUCN statuses through time
All_Listed_IUCN <- left_join(NA_update, df_new, relationship = "many-to-many") %>%
  mutate(IUCN_code = ifelse(IUCN_code %in% c("DD", "Not assessed"), "NE", IUCN_code))
IUCN_2023 <- All_Listed_IUCN %>% filter(Year == 2023)
write.csv(All_Listed_IUCN, "Data/IUCN/ALL_REPT_ASSESSMENTS_series.csv")  

#### Trait data ####

## trait data
Etard_dat <- read.csv("Data/Traits/Etard_reptiles.csv")

Test_name <- Etard_dat %>% select(Best_guess_binomial, Genus)
Quota_sp <- Quota_trade_listing %>% select(Taxon) %>% distinct

Check <- left_join(Quota_sp, Test_name, by = c("Taxon" = "Best_guess_binomial"))
Checklist <- Check %>% filter(is.na(Genus)) ## 28 species not already matched

## names matched using Etards 2018 synonyms
Quota_sp2 <- Quota_sp %>% mutate(Etard_name = case_when(Taxon == "Tupinambis teguixin" ~ "Salvator merianae",
                                           Taxon == "Gongylophis colubrinus" ~ "Eryx colubrinus",
                                           Taxon == "Kinixys belliana" ~ "Kinixys lobatsiana",
                                           Taxon == "Boa constrictor" ~ "Boa imperator",
                                           Taxon == "Caiman crocodilus crocodilus" ~ "Caiman crocodilus",
                                           Taxon == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
                                           Taxon == "Apodora papuana" ~ "Liasis papuana",
                                           Taxon == "Morelia boeleni" ~ "Simalia boeleni",
                                           Taxon == "Phelsuma lineata" ~ "Phelsuma dorsivittata", 
                                           Taxon == "Pelusios castaneus" ~ "Pelusios seychellensis",
                                           Taxon == "Cyclagras gigas" ~ "Hydrodynastes gigas",
                                           Taxon == "Malayopython reticulatus" ~ "Python reticulatus",
                                           Taxon == "Naja sputatrix" ~ "Naja siamensis", 
                                           Taxon == "Python curtus" ~ "Python brongersmai",
                                           Taxon == "Caiman crocodilus yacare" ~ "Caiman yacare",
                                           Taxon == "Naja" ~ "Not_in_data", ## non specific quota
                                           Taxon == "Salvator" ~ "Not_in_data", ## non specific quota
                                           Taxon == "Tupinambis" ~ "Not_in_data", ## non specific quota
                                           Taxon == "Uroplatus sikorae" ~ "Uroplatus sameiti",
                                           Taxon == "Malayemys khoratensis" ~ "Not_in_data", ## not in data
                                           Taxon == "Naja naja" ~ "Naja kaouthia",
                                           Taxon == "Ptyas mucosus" ~ "Ptyas mucosa",
                                           Taxon == "Brookesia minima" ~ "Brookesia tuberculata",
                                           Taxon == "Morelia clastolepis" ~ "Simalia clastolepis",
                                           Taxon == "Morelia nauta" ~ "Simalia nauta",
                                           Taxon == "Morelia tracyae" ~ "Simalia tracyae",
                                           Taxon == "Varanus ornatus" ~ "Varanus olivaceus",
                                           Taxon == "Varanus similis" ~ "Varanus scalaris",
                                           Taxon == "Leiopython hoserae" ~ "Leiopython meridionalis",
                                           TRUE ~ Taxon))

Name_trait <- left_join(Quota_sp2, Etard_dat, by = c("Etard_name" = "Best_guess_binomial"))

write.csv(Name_trait, "Data/Traits/Etard_quota_names.csv")

