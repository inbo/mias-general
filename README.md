<!-- badges: start -->
![GitHub](https://img.shields.io/github/license/inbo/monitoring-ias)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/inbo/monitoring-ias/check-project)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/monitoring-ias)
<!-- badges: end -->

# Monitoring invasive alien species of Union concern

[Adolf, Janne![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-6064-9803)[^aut][^cre][^inbo.be]
Research Institute for Nature and Forest (INBO)[^cph][^fnd]

[^cph]: copyright holder
[^fnd]: funder
[^aut]: author
[^cre]: contact person
[^inbo.be]: Research Institute for Nature and Forest (INBO)

**keywords**: invasive alien species; monitoring schemes

<!-- community: inbo -->

<!-- description: start -->
Repository accompanying the development of monitoring schemes for the invasive alien species of Union concern in Flanders.

This is an overarching project repository.
For a child repository concerning the Chinese muntjac, see the repository [mias-muntjac](https://github.com/inbo/mias-muntjac).

Associated with this project is the following INBO publication:\
Adolf, J., Strubbe, D., Adriaens, T., & Onkelinx, T. (2025). *Prioritising invasive alien species for monitoring in Flanders: Results of an expert survey.* (Rapporten van het Instituut voor Natuur- en Bosonderzoek; Nr. 59). Instituut voor Natuur- en Bosonderzoek. [https://doi.org/10.21436/inbor.133486163](https://pureportal.inbo.be/nl/publications/prioritising-invasive-alien-species-for-monitoring-in-flanders-re/)
<!-- description: end -->

<!-- Anything below here is visible in the README but not in the citation. -->

## Repository structure 

The repository is organised as follows:

```
.
├── data/processed
├── inst/
├── media/
├── renv/
└── source/
```


### `data/processed`
Contains processed data used in the project's analyses.

### `renv/`
Contains files created by the **renv** R package to manage the R package environment and ensure reproducibility.

### `source/`
Contains the source code for the project, including:
- `analysis`: R script to join species lists from GBIF and PrIUS 
- `export/survey_experts`: scripts and files associated with the above cited expert survey, spanning all workflow phases (i.e., setup, distribution, data collection, data analysis, reporting). 
Scripts are R or JavaScript (automatically generated and to be deployed via Google Apps Script).
The Quarto project and Quarto files used to generate the above cited publication are located in `export/survey_experts/docu_report`.
- `functions`: R functions used throughout the project
- `gbif_occ_maps`: R scripts for occurrence maps based on GBIF data
- `presentation_ec`: Quarto project and Quarto files used to generate an overview presentation over the project (contributing to the INBO-European Commission Invasive Alien Species Meeting on Oct 17, 2025)
- `report`: R scripts and R Markdown files associated with some early-stage documentation of the project

## Reproducibility

This project uses **renv** to manage R package dependencies.
The `renv.lock` file (and its commit history) provides detailed information on the R version and all R packages used.
To restore the project dependencies from the lockfile, use the indicated R version and run:

```r
renv::restore()
```


