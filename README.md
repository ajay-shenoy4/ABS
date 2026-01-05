# ABS Global: Dairy Herd Genetics & Genomic Auditing
## Project Overview
This repository contains a collection of R-based tools designed for the dairy industry. These applications automate the retrieval of pedigree data, calculate genomic performance scores, and perform large-scale audits (CDCB, CI, AHDB) to optimize herd genetics and reproductive efficiency.

## Core Applications
### 1. GMS Pedigree Navigator (`ui-GMS.R` & `server-GMS.R`)
A full-stack **R Shiny** dashboard and data-processing pipeline to reconstruct multi-generational cattle pedigrees and track real-time reproductive physiology.

#### Technical Architecture
* **Multi-Tier Data Integration:** Connects to **MS SQL Server** using `odbc` and `DBI` protocols with secure `YAML` configuration management.
    * Implements a modular SQL architecture to query `farm_account_lookup`, `animal_data`, `calf_forecast`, and `reproduction_data` asynchronously.
* **Pedigree Reconstruction:** The system utilizes specialized join logic (`CowtoMgs` → `GetMggs` → `GetGreatGrandSires`) to recursively traverse the maternal line.
    * Transforms flat relational data into a structured **Sire / Maternal Grand Sire (MGS) / Maternal Great Grand Sire (MGGS)** lineage view.
* **Automated Reproductive State Machine:** **Heuristic Status Assignment:** Implements complex `case_when` logic to dynamically assign reproductive states (`PREG`, `BRED`, `OPEN`, `DNB`, `FRSH`, `ABRT`) based on `ReproType` and `ResultedInPregnancy` flags.
    * **Biological Computation:** Calculates derived fields including **Days Carrying Calf** (using `difftime` logic between Service and Due dates) and **Age in Months** via `lubridate` intervals.

#### Data Flow & Lineage Logic
The application flattens a 3-generation lineage into a single row-major format by joining `animal_data` onto itself via Dam IDs. This allows breeding advisors to evaluate three generations of genetic impact alongside current reproductive metrics simultaneously.

| Pedigree Level | Script Function | Logic & Data Source |
| :--- | :--- | :--- |
| **Generation 1** | `CowtoMgs` | Maps the Cow to her immediate Sire and Dam. |
| **Generation 2** | `GetMggs` | Joins the Dam's ID to identify the Maternal Grand Sire. |
| **Generation 3** | `GetGreatGrandSires` | Joins the Grand-Dam's ID to identify the Great Grand Sire. |

#### User Interface Features
* **Toggleable View System:** Users can shift between a comprehensive "Full Data" audit trail (40+ variables) and a condensed "GMS Lineage" view focused on genetic markers.
* **Branded Dashboard:** Custom CSS-styled `shinydashboard` featuring a fixed-header sidebar, asynchronous `withProgress` loading indicators, and high-performance `DT` (DataTables) integration for large dataset exploration.
* **Validated Exporting:** Integrated `downloadHandler` for generating cleaned CSV reports containing only the user’s currently selected filtered view.

---
### 2. GeneAdvance Buckets (Dairy Herd Genetic Scoring Pipeline)
An analytical pipeline designed for scoring, categorization, and longitudinal tracking of dairy herd performance across global markets.

#### Technical Architecture & ETL
* **Hybrid Database Orchestration:** Implements a centralized `scapi_setup()` function to manage simultaneous connections to **MS SQL Server** and **Starburst (Trino)** via `odbc` and `RJDBC`.
    * Utilizes secure YAML-based credential injection for scalable multi-environment deployment.
* **Automated Data Lifecycle:**
    * **Dynamic Alignment:** The `clean_and_align_herd_lists` engine standardizes disparate CSV inputs, handles missing columns by comparing against reference dataframes, and sanitizes metadata (filtering "test" countries and invalid Farm IDs).
    * **Longitudinal Batch Processing:** Maps processing functions across a time-series of datasets (from 2023 to 2025) to maintain consistent genetic classification over multi-year periods.

#### The "Genetic Bucketing" Engine
The core of the pipeline is a classification system that maps herds into specific market-relevant indices (e.g., NM$like, TPIlike, HEALTH, PRODUCTION).
* **3D State Space Scoring:** Scores every herd across three primary dimensions: **Production**, **Fitness**, and **Conformation**.
* **Euclidean Distance Logic:** Features a specialized **Distance-to-Bounding-Box** algorithm. If a herd falls outside a strict rule-based category, the script calculates the Euclidean distance between the herd's coordinates and the nearest defined "Genetic Box" to ensure 100% classification coverage.
* **Anomaly Detection:** Identifies "Unassigned" or "Multi-Bucket" rows to flag inconsistencies in farm reporting or extreme genetic outliers.

#### Advanced Visualization & Analytics
The pipeline integrates a dedicated suite of `ggplot2` plotting functions to transform high-dimensional data into publication-quality insights:

* **Global Performance Trends:** Faceted line charts and stacked bar graphs showing how genetic proportions (by both **Herd Count** and **Herd Size**) shift over time across different countries.
* **Trait-Specific Faceting:** Specialized trend plots for:
    * **Production Traits:** Milk, Fat, Protein.
    * **Health/Fitness:** Productive Life (PL), Livability, SCS, and AHI.
    * **Conformation:** Udder Composite (UDC), Feet & Legs (FLC), and Body Weight (BWC).
* **Trait Range Verification:** Visualization of "Bucket Rules" vs. "Actual Performance" to validate that classified herds align with their intended genetic targets.

### I was able to present the findings in a presentation to several product line managers every sire summary. 

* **Executive Summaries:** Generation of `combined_bucket_summary.csv`, which provides an analysis of the global herd distribution without requiring deep-dives into raw SQL tables.
* **Regional Performance Faceting:** Using `plot_production_trait_trends`, we provide PLMs with country-specific benchmarks (e.g., Brazil vs. USA), allowing them to identify underperforming regions or emerging genetic trends.
* **Validation of Strategic Rules:** By visualizing `plot_trait_ranges`, we demonstrate to product managers that our classification logic ("Bucketing") aligns with real-world biological performance, building trust in the algorithm's accuracy.
* **Trait Trend Facets:** A "health-check" on biological markers (Fertility, Udder Health, Longevity) to ensure that genetic progress is consistent with global sustainability and animal welfare goals.
* **The "Market Shift" Story:** Line charts showing the year-over-year growth of specific buckets (like `HEALTH` or `NM$like`) to help PLMs pivot marketing and inventory strategies.
---
### 3. Multi-Standard Genomic Audits (CDCB, CI, AHDB)

### AHDB National Evaluation & Genomic Audits
A high-performance pipeline aligned with **AHDB (UK)** national standards. This tool synchronizes farm-level milk recording data (MRO) with national genomic evaluations to analyze genetic merit and economic potential. The audit runs for Europe, the Middle East, and Africa (EMEA).

#### **Key Enhancements (EMEA Portfolio)**
* **Expanded Trait Analysis:** Increased included traits from **26 to 46**, providing the most granular genetic view in the industry.
* **Advanced Data Science:** Developed 10+ new visualizations including `geom_density` histograms, longitudinal scatterplots, and 305-day normalization models.
* **Operational Support:** Generated specialized dataframes for sire info and parent averages to support global sales teams and analysts.

#### **Technical Workflow & Synthesis**
* **Multi-Source Ingestion:** Integrates data from **CIS, NMR, and UDF**, alongside internal **GMS MasterPlan** records.
* **Intelligent Data Merging:** Resolves animal identities via a dual-key matching engine (`HBNLink`) using National IDs and Herd Book Numbers.
* **Recursive Trait Recovery:** Implements `AppendTraitDB` to reconstruct genetic profiles using a weighted ancestral formula
* **305-Day ME Normalization:** Standardizes diverse records into **305-Day Milk Equivalents** using biological scaling factors for fair comparison across lactation groups.

#### **Visual Analytics & Benchmarking**
* **Genetic Lag Visualization:** Compares herd-specific progress against the national UK industry benchmark.
* **Birthdate Trend Analysis:** Faceted plots showing progress for **Production**, **Fitness**, and **EnviroCow** indices.
* **Index Support:** Full support for the three major UK rankings: **PLI** (Profitable Lifetime Index), **SCI** (Spring Calving Index), and **ACI** (Autumn Calving Index).
  
### Custom Index (CI) Audits
A comprehensive automated pipeline enabling retrieval, cleaning, and reporting of herd performance against international standards across EMEA and Latin America (e.g., Italy, UK, Brazil).

* **Orchestration & Integration:** Resolves unique customer metadata through a dual-database lookup across EMEA and GeneAdvance production servers.
* **Merging & State-Space Review:** Merges Parent Average (PA) phenotypic data with Genomic Testing (GT) results. It assigns a **Genomic Indicator (G or P)** to every animal, ensuring that failing genomic samples default to Parent Average indices to maintain data continuity.
* **Visual Intelligence:** Uses `KappPlotting` to generate audit images, including matrix correlation plots and "stiching" functions that combine distribution graphs with numerical summary tables into a single image.

### CDCB Genomic & Performance Audits
An analytical pipeline for conducting genomic audits based on **CDCB (North America)** standards. This tool automates pedigree validation and generates longitudinal genetic progress reports.

* **Proprietary HBN Conversion:** Translates **Sexed-semen identifiers** to **Conventional HBNs** across Holstein and Jersey breeds, ensuring seamless trait-merging with national bull databases.
* **Recursive Inheritance Logic:** Implements `CDCBAppendTrait` which falls back to secondary generation data (MGS/MGGS) if primary sire data is missing, ensuring zero data gaps in the genetic profile.
* **Economic Value Tracking:** Includes a specialized **ROI Analysis (ROIPngs)** that factors in farm-specific **Beef-on-Dairy** percentages to calculate the financial impact of breeding decisions.
* **SQL Integration:** Automatically manages the `CDCBAuditData` warehouse, clearing legacy snapshots and appending fresh audit results for PowerBI consumption.
---

## Technical Requirements
* **Environment:** R 4.x+
* **Core Libraries:** `tidyverse`, `ggplot2`, `odbc`, `rJava`, `DBI`, `data.table`, `shiny`.
* **Memory:** Requires high memory allocation for Java (`-Xmx8000m`) for large-scale genomic processing.
* **Configuration:** Requires a `config.yml` in `~/.scapi/` for database credentials.

---
**Author:** Ajay Shenoy, as part of ABS Global  

