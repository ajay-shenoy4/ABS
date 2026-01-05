# ABS Global: Dairy Herd Genetics & Genomic Auditing
## Project Overview
This repository contains a suite of advanced R-based tools designed for the dairy industry. These applications automate the retrieval of pedigree data, calculate genomic performance scores, and perform large-scale audits (CDCB, CI, AHDB) to optimize herd genetics and reproductive efficiency.

## Core Applications
### 1. GMS Pedigree Navigator (ui-GMS.R & server-GMS.R)
A dynamic R Shiny application that serves as a portal into cattle lineage and reproductive health.

Data Integration: Connects to SQL Server via YAML-encrypted credentials.

Processing Engine: Merges multiple relational tables (Cows, Grandsires, Calf Forecasts) to calculate derived fields like Age in Months and Days Carrying Calf.

Features: Rule-based reproductive status assignment, lineage view toggles, and automated CSV exporting.

### 2. GeneAdvance Buckets (Herd Scoring Pipeline)
A highly technical pipeline for dairy herd categorization based on production and conformation traits.

Advanced Wrangling: Utilizes dplyr and purrr for functional programming across multi-source datasets (SQL & Starburst via RJDBC).

Scoring Logic: Implements distance-to-bounding-box logic to assign animals to custom genetic "buckets."

Visual Analytics: Generates publication-quality ggplot2 visualizations, including faceted temporal trends and trait range distributions.

### 3. Multi-Standard Genomic Audits (CDCB, CI, AHDB)
An automated reporting framework for international genomic and phenotypic auditing.

Audit Scope: Conducts audits for Council on Dairy Cattle Breeding (CDCB), CI, and AHDB standards.

Metrics: Calculates indicators for production, fertility, longevity, body composition, and udder health.

Output: Automatically generates zipped report folders containing processed datasets and custom summary tables tailored to specific breeds.

## Data Pipeline Architecture
The scripts follow a rigorous ETL (Extract, Transform, Load) process to ensure data integrity across various global standards:

Extraction: Secure connection to distributed databases (SQL Server, Starburst).

Transformation: Alignment of disparate CSV data, genomic indicator calculation, and parentage verification.

Visualization: Automated plot generation for udder health, fertility, and production trends.

Reporting: Generation of reproducible audits for customer decision-making.

Contact & Credits

ABS Global 

Ajay Shenoy 
