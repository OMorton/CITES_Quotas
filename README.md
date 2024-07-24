## CITES Reptile quota coverage and compliance analysis

The output of this is accepted in principle as **International wildlife trade quotas are characterised by high compliance and coverage but insufficient adaptive management** at *Nature Ecology and Evolution*.

### File summary

#### 1. Data Preparation 

***1.1_Quota_Prep.R*** Classifies and processes the raw CITES quotas and then compiles relevant trade data to assess each quota against. Also appends trait data and temporally correct IUCN assessments.

***1.2_Quota_Prep_SupplementaryData.R*** Minimally used to write out data.

***1.3_Get_IUCN_statuses.R*** Takes the raw CITES trade data for all reptiles and matches up trade records with quotas and then
appends correct IUCN statuses for all species whether quota managed or not. Also adds additional data from published sources on species
threat from international wildlife trade.

#### 2. Analysis

***2.1_Quota_Analysis.R*** Main analysis for Figures 1, 2 and 3 presented in the main paper. Also produces all figures for these sections.

***2.3_Update_Freq_Analysis.R*** For the main analysis presented in Figure 4. Assesses the rate of updating for quota series using custom temporal change point models written in *brms*.

***2.4_Quota_coverage_gaps.R*** Uses the output of script 1.3 to map the historic and current gaps in quota coverage. 

***Functions.R*** Contains a number of convenience functions written to streamline plotting.

All the data used is publicly accessible and clearly linked within the manuscript. In the data folder here we present the processed data used for the analysis steps.