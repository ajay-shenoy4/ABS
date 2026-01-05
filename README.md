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
