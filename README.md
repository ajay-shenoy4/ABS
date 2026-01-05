# ABS Global: Dairy Herd Genetics & Genomic Auditing
## Project Overview
This repository contains a suite of advanced R-based tools designed for the dairy industry. These applications automate the retrieval of pedigree data, calculate genomic performance scores, and perform large-scale audits (CDCB, CI, AHDB) to optimize herd genetics and reproductive efficiency.

## Core Applications
### 1. GMS Pedigree Navigator (`ui-GMS.R` & `server-GMS.R`)
A full-stack **R Shiny** dashboard and data-processing engine designed to reconstruct multi-generational cattle pedigrees and track real-time reproductive physiology.

#### Technical Architecture
* **Multi-Tier Data Integration:** * Connects to **MS SQL Server** using `odbc` and `DBI` protocols with secure `YAML` configuration management.
    * Implements a modular SQL architecture to query `farm_account_lookup`, `animal_data`, `calf_forecast`, and `reproduction_data` asynchronously.
* **Pedigree Reconstruction Engine:** * The system utilizes specialized join logic (`CowtoMgs` → `GetMggs` → `GetGreatGrandSires`) to recursively traverse the maternal line.
    * Transforms flat relational data into a structured **Sire / Maternal Grand Sire (MGS) / Maternal Great Grand Sire (MGGS)** lineage view.
* **Automated Reproductive State Machine:** * **Heuristic Status Assignment:** Implements complex `case_when` logic to dynamically assign reproductive states (`PREG`, `BRED`, `OPEN`, `DNB`, `FRSH`, `ABRT`) based on `ReproType` and `ResultedInPregnancy` flags.
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
An end-to-end analytical pipeline designed for high-throughput scoring, categorization, and longitudinal tracking of dairy herd performance across global markets.

#### Technical Architecture & ETL
* **Hybrid Database Orchestration:** * Implements a centralized `scapi_setup()` function to manage simultaneous connections to **MS SQL Server** and **Starburst (Trino)** via `odbc` and `RJDBC`.
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
* **Trait Range Verification:** Automated visualization of "Bucket Rules" vs. "Actual Performance" to validate that classified herds align with their intended genetic targets.

| Core Function | Responsibility |
| :--- | :--- |
| `scapi_setup` | Orchestrates DB connections and environment paths. |
| `process_and_bucket_herds` | Primary scoring engine using 3D coordinate logic. |
| `generate_and_save_summaries` | Batch processes longitudinal data into a standardized CSV library. |
| `plot_health_trait_trends` | High-fidelity facet plotting of biological markers across global regions. |

### I was able to present the findings in a presentation to several product line managers every sire summary. 

* **Executive Summaries:** Automated generation of `combined_bucket_summary.csv`, which provides a high-level "pulse" of the global herd distribution without requiring deep-dives into raw SQL tables.
* **Regional Performance Faceting:** Using `plot_production_trait_trends`, we provide PLMs with country-specific benchmarks (e.g., Brazil vs. USA), allowing them to identify underperforming regions or emerging genetic trends.
* **Validation of Strategic Rules:** By visualizing `plot_trait_ranges`, we demonstrate to product managers that our classification logic ("Bucketing") aligns with real-world biological performance, building trust in the algorithm's accuracy.
* **Trait Trend Facets:** A "health-check" on biological markers (Fertility, Udder Health, Longevity) to ensure that genetic progress is consistent with global sustainability and animal welfare goals.
* **The "Market Shift" Story:** Line charts showing the year-over-year growth of specific buckets (like `HEALTH` or `NM$like`) to help PLMs pivot marketing and inventory strategies.

### Impact on Product Line Management
* **Resource Allocation:** Data identifies which genetic lines are most dominant in key markets, helping managers allocate laboratory and sire resources more efficiently.
* **Standardization:** The pipeline provides a single "Source of Truth," ensuring that managers in different countries are looking at the same KPIs and audit results.
* **Predictive Forecasting:** By tracking longitudinal trends from 2023–2025, we provide PLMs with a predictive view of where the market is heading by 2026.

---
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
