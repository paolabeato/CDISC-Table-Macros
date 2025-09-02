# 💾 CDISC TLF Macro Toolkit (SAS)

## 📌 Overview

A growing library of **SAS macros** for generating **regulatory-aligned TLFs** (Tables, Listings, Figures) from CDISC ADaM datasets.

Currently includes a suite of macros for **ADSL demographic tables**, with planned expansion to **ADAE, ADLB, ADVSV, ADTTE, and more**.

‼️ **No real clinical trial or patient data are included** ‼️

## 🤔 Current Focus

Macros for baseline demographic summaries (Table 14.1.x style):

-   `%adsl_basic` – Demographic tables without Overall or p-values

-   `%adsl_advanced` – With Overall column + p-values (ANOVA/KW)

-   `%adsl_assumptions` – Assumption checks to recommend ANOVA vs Kruskal–Wallis

## ⚙️ Macro Parameters

### 🔹 `%adsl_basic` and `%adsl_advanced`

These macros share the same parameter set. `%adsl_advanced` extends `%adsl_basic` by including **Overall columns** and **p-values**.

| Parameter        | Description                                                                 |
|------------------|-----------------------------------------------------------------------------|
| **pop**          | Population flag variable (e.g., `ITTFL`, `SAFFL`, `FASFL`).                 |
| **popdesc**      | Population description for titles (e.g., *Intent-to-Treat*, *Safety*).      |
| **popval**       | Value indicating inclusion in population (typically `'Y'`).                 |
| **trtvar**       | Treatment variable for grouping (e.g., `ARM`, `TRT01P`).                    |
| **outrtf**       | Output RTF file path (**required**).                                        |
| **tablenum**     | Table number for title (e.g., `14.1.1`).                                    |
| **denom_mode**   | Denominator for percentages: `FULL` (all in pop) \| `NONMISS` (non-missing only). |
| **show_missing** | Show missing categories in counts/percents: `Y` \| `N`.                     |
| **dostat**       | Calculate p-values (for advanced table): `Y` \| `N`.                        |
| **test_type**    | Continuous var test: `ANOVA` \| `KW` (Kruskal–Wallis).                      |
| **dooverall**    | Include an Overall column: `Y` \| `N`.                                      |
| **trt_order**    | Explicit arm order (e.g., `Placebo DrugA`).                                 |
| **tab_title**    | Main table title.                                                           |
| **tab_subtitle** | Optional subtitle.                                                          |
| **src**          | Source footnote text (e.g., `ADSL`).                                        |
| **output_type**  | Output type: `RTF` (default) \| `PDF`.  

### 🔹 `%adsl_assumptions`

This macro checks assumptions for continuous variables to guide the choice of statistical test.

| Parameter     | Description                                                                          |
|---------------|--------------------------------------------------------------------------------------|
| **data**      | Input dataset containing analysis population (default: `work_population`).           |
| **contvars**  | Space-separated list of continuous variables to check.                               |
| **trtvar**    | Treatment variable for grouping (**required**).                                      |
| **outdir**    | Output directory for HTML report; if blank, creates a temporary report in `WORK`.    |
| **alpha**     | Significance level for tests (default: `0.05`).    

## 📊 Example Workflow

### Basic Demographics (no overall, no p-values)

``` sas
%adsl_basic(
  pop=ITTFL,
  popdesc=Intent-to-Treat,
  popval=Y,
  trtvar=ARM,
  outrtf=./Table_14_1_1_ITT.rtf,
  tablenum=14.1.1,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=RTF
);
```

### Advanced Demographics (overall column + p-values)

``` sas
%adsl_advanced(
  pop=SAFFL,
  popdesc=Safety,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Advanced_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.rtf,
  tablenum=14.1.2,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=Y,
  test_type=KW,
  dooverall=Y,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=RTF
);
```

## 📄 Example Outputs  

Click below to expand and view sample outputs (**mock data, demonstration only**).  

<details>
<summary><strong>Basic ADSL Tables</strong></summary>

- [ITT Population (Table 14.1.1, PDF)](Output/ADSL_Tables/Basic_ADSL_Tables/ITT_Population/Table_14_1_1_ITT.pdf)  
- [ITT Population (Table 14.1.1, RTF)](Output/ADSL_Tables/Basic_ADSL_Tables/ITT_Population/Table_14_1_1_ITT.rtf)  
- [SAF Population (Table 14.1.2, PDF)](Output/ADSL_Tables/Basic_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.pdf)  
- [SAF Population (Table 14.1.2, RTF)](Output/ADSL_Tables/Basic_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.rtf)  
- [FAS Population (Table 14.1.3, PDF)](Output/ADSL_Tables/Basic_ADSL_Tables/FAS_Population/Table_14_1_3_FAS.pdf)  
- [FAS Population (Table 14.1.3, RTF)](Output/ADSL_Tables/Basic_ADSL_Tables/FAS_Population/Table_14_1_3_FAS.rtf)  

</details>

<details>
<summary><strong>Advanced ADSL Tables (with Overall & p-values)</strong></summary>

- [ITT Population (Table 14.1.1, PDF)](Output/ADSL_Tables/Advanced_ADSL_Tables/ITT_Population/Table_14_1_1_ITT.pdf)  
- [ITT Population (Table 14.1.1, RTF)](Output/ADSL_Tables/Advanced_ADSL_Tables/ITT_Population/Table_14_1_1_ITT.rtf)  
- [SAF Population (Table 14.1.2, PDF)](Output/ADSL_Tables/Advanced_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.pdf)  
- [SAF Population (Table 14.1.2, RTF)](Output/ADSL_Tables/Advanced_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.rtf)  
- [FAS Population (Table 14.1.3, PDF)](Output/ADSL_Tables/Advanced_ADSL_Tables/FAS_Population/Table_14_1_3_FAS.pdf)  
- [FAS Population (Table 14.1.3, RTF)](Output/ADSL_Tables/Advanced_ADSL_Tables/FAS_Population/Table_14_1_3_FAS.rtf)  

</details>

<details>
<summary><strong>Assumption Checks</strong></summary>

- [PDF Report](Output/ADSL_Tables/ADSL_Assumptions/ADSL_Assumption_Checks.pdf)  
- [Log File (TXT)](Output/ADSL_Tables/ADSL_Assumptions/ADSL_Assumptions_Log.txt)  

</details>

## 🚀 Future Roadmap

This project will evolve into a **full CDISC-aligned TLF generation toolkit**, expanding support to other ADaM domains:

-   **ADAE (Adverse Events):** Treatment-emergent adverse event summaries by SOC/PT

-   **ADLB (Labs):** Lab shift tables, grade changes, and lab listings

-   **ADVSV (Vitals):** Vital sign summaries and abnormality flags

-   **ADTTE (Time-to-Event):** Kaplan–Meier plots, risk tables, Cox model outputs

-   **ADEX (Exposure):** Drug exposure and compliance tables

Planned features include:

-   Harmonized **macro parameter conventions** across domains

-   Consistent **output styling** (titles, footnotes, shell alignment)

-   Support for **dual-programming QC pipelines**

-   Modular design to drop into **regulatory submission workflows**

## 📜 License

MIT License — feel free to use, modify, and share.

### ⚠️ Disclaimer

This repository uses **mock CDISC-style data** and synthetic examples created for **portfolio and educational purposes only**.

## ✍️ Author Notes

I built this project as part of my **programming portfolio** to demonstrate regulatory-style table generation in SAS using the **CDISC ADaM framework**.

-   I started with **ADSL demographic tables** because they are among the most universal deliverables across clinical studies.

-   The `%adsl_basic` and `%adsl_advanced` macros show how to handle **population flags, treatment arms, denominators, missing data, and statistical tests**, while `%adsl_assumptions` ensures **assumptions are verified** before reporting p-values.

-   My focus was on writing **clear, parameterized macros** that mimic sponsor-ready outputs while staying flexible for multiple studies.

-   Future expansions will extend the macros to **ADAE, ADLB, ADVSV, ADTTE, and ADEX**, aiming for a reproducible pipeline that covers the full suite of CDISC tables.

-   This project is both a **learning exercise and a professional showcase** — demonstrating how I approach **regulatory-aligned programming**, **automation**, and **quality control** in clinical data analysis.
