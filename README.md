## CITES Reptile quota coverage and compliance analysis

The output of this is in submission as **Paper quotas:  High compliance but ineffective quota-setting in the international wildlife trade** at *Nature Ecology and Evolution*.

### File summary

#### 1. Data Preparation 

***1.1_Quota_Prep.R*** Classifies and processes the raw CITES quotas and then compiles relevant trade data to assess each quota against. Also appends trait data and temporally correct IUCN assessments.

***1.2_Quota_Prep_SUpplementaryData.R*** Minimally used to write out data.


#### 2. Analysis

***2.1_Quota_Analysis.R*** Main analysis for Figures 1, 2 and 3 presented in the main paper. Also produces all figures for these sections.

***2.2_Trait_Analysis.R*** NOT USED. Initial exploratory analysis of trait volume correlations.

***2.3_Update_Freq_Analysis.R*** For the main analysis presented in Figure 4. Assesses the rate of updating for quota series using custom temporal change point models written in *brms*.

***Functions.R*** Contains a number of convenience functions written to streamline plotting.

All the data used is publicly accessible and clearly linked within the manuscript. In the data folder here we present the processed data used for the analysis steps.