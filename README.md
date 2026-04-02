# Replication Materials — Platform freedom: Financial subjectivity at the nexus of investment and social media

**Authors:** Desiree Fields, Julien Migozzi  
**Journal:** Environment and Planning A: Economy and Space | **Year:** 2026 | **DOI:** https://doi.org/10.1177/0308518X261429864

---

## Overview

This repository contains all code necessary to replicate the analyses and figures reported in *Platform freedom*.

---

## Repository structure
```
.
├── README.md
├── LICENSE
├── scripts/
│   ├── 1_extract_data_from_reddit.R   # Data extraction from Reddit
│   ├── 2_create_data_fundrise.R       # Fundrise dataset construction
│   ├── 3_data_prep.R                  # Data cleaning and preparation
│   ├── 4_comparing_models.R           # STM comparison and selection
│   ├── 5_model15k.R                   # Main STM model estimation
│   └── 6_figs.R                       # Figure and table generation
└── data/
    ├── raw/                           # Original data cannot be made available online
    └── processed/                     # Cleaned data produced by 3_data_prep.R
```
---

## Requirements

- **R version:** [e.g. 4.3.1]  
- **Key packages:** `tidyverse`,`stm`, `spacyr`, `quanteda`, `RedditExtractoR`


---

## Instructions

Run the scripts in order from the project root.


---

## Data

The data were collected from Reddit via the RedditExtractoR package.
Raw data cannot be redistributed due to Reddit's Terms of Service.
To replicate the data collection step, run `1_extract_data_from_reddit.R`, 
which retrieves the data directly from Reddit.

---

## Citation

If you use these materials, please cite:

Desiree Fields and Julien Migozzi (2026) "Platform freedom: Financial subjectivity at the nexus of investment and social media", *Environment and Planning A: Economy and Space*.

---

## Licence

Code is released under the [MIT License](LICENSE).  