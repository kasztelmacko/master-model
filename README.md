## Master Thesis: Survey Data Extraction and DCE Modeling

This repository contains all scripts, data, and outputs used in the Master Thesis project on survey data analysis and Discrete Choice Experiment (DCE) modeling. The project focuses on extracting, cleaning, preparing, and modeling survey data, including DCE designs and brand awareness measures.

## Project Structure
```
   master-model
   ├─ DCE_design/                # Scripts and data for DCE design generation and extraction
   │  ├─ data/                   # Raw and optimized DCE designs
   │  │  ├─ DCE_design_optimzied.csv
   │  │  ├─ DCE_design_start.csv
   │  │  ├─ raw_design_optimized.csv
   │  │  └─ raw_design_start.csv
   │  └─ extract_DCE_design.R    # R script to extract and prepare DCE designs
   └─ Modeling/                  # Main survey and modeling workflow
      ├─ data/                   # Collected and cleaned survey datasets
      │  ├─ 1/                   # First survey wave / dataset
      │  │  ├─ brand_lists_cleaned.csv
      │  │  ├─ brand_recall.csv
      │  │  ├─ brand_recognition.csv
      │  │  ├─ clean_data.csv
      │  │  ├─ DCE_design.csv
      │  │  └─ survey_results.csv
      │  ├─ 2/                   # Second survey wave / dataset
      │  │  ├─ brand_lists_cleaned.csv
      │  │  ├─ brand_recall.csv
      │  │  ├─ brand_recognition.csv
      │  │  ├─ clean_data.csv
      │  │  ├─ DCE_design_new.csv
      │  │  └─ survey_results.csv
      │  ├─ 3/                   # Third survey wave / dataset
      │  │  ├─ brand_lists_cleaned.csv
      │  │  ├─ brand_recall.csv
      │  │  ├─ brand_recognition.csv
      │  │  ├─ clean_data.csv
      │  │  ├─ DCE_design_newest.csv
      │  │  └─ survey_results.csv
      │  └─ final_data/           # Consolidated data for modeling
      │     ├─ brand_recall.csv
      │     ├─ brand_recognition.csv
      │     ├─ clean_data.csv
      │     └─ model_data.csv
      │
      ├─ data_exploration_and_plots.R   # Script for EDA and visualization
      ├─ data_modeling.R                # Script for statistical & choice modeling
      ├─ data_preparation.R             # Script for cleaning and structuring survey data
      │
      └─ plots/                         # Generated plots and visualizations
         ├─ age_distribution_plot.png
         ├─ marekt_awareness_stats.png
         └─ recall_recognition_plot.png
```
