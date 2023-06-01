---
---
---

# README: for Replication Materials for an Extension of 'Pride amid Prejudice' (Ayoub et al., 2021)

This is a replication project based on Ayoub, P.M., Page, D. and Whitt, S. (2021) 'Pride amid Prejudice: The Influence of LGBT+ Rights Activism in a Socially Conservative Society', American Political Science Review, 115(2), pp. 467--485. Available at: <https://doi.org/10.1017/S0003055420001082.>

## Project Documentation

### 1. Not in Files:

-   *.gitignore*

-   **README.md**: this file

-   **SMI205 Extension Pride amid Prejudice.Rmd:** written assignment for the project, including snippets of code and plots from the scripts included here

-   **SMI205_Preregistration_form.Rmd** and **SMI205_Preregistration_form.html:** preregistration document for this replication extension

-   **SMI205_git.Rproj:** .RProject for this replication study

### 2. Data:

The datasets used are openly available for download, but are also provided within the Data folder. The 'Created' folder contains datasets that are created in the first section of the script (**1_Data_Cleaning.R**). The .zip files open to a .csv of the same name; this was a measure taken to deal with GitHub's file size limit.

-   Created:

    -   **rep_data.csv:** cleaned data from Pride_Amid_Prejudice_replication data_final.dta:
    -   **shape.zip:** cleaned shapefile data of Bosnia and Herzegovina municipal boundaries, from geoBoundaries-BIH-ADM2.geojson; names converted to Latin characters and adjusted to match the survey data
    -   **rep_shape.zip:** merge of the shapefile data of municipality boundaries (**shape**.csv) and **rep**\_data.csv
    -   **rep_shape_m.zip:** data used in **m**odelling
    -   **rep_shape_p.zip:** data used in **p**lotting

-   **Pride_Amid_Prejudice_replication data_final.dta:** the original dataset as accessed from <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IROJ12>

-   **geoBoundaries-BIH-ADM2.geojson:** original shapefile of Bosnia and Herzegovina municipal boundaries, as accessed from <https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-bosnia-and-herzegovina>

    In the preregistration I reference a dataset provided in the replication materials for 'How Exposure to Violence Affects Ethnic Voting' ([Hadzic et al., 2020](https://doi.org/10.1017/S0007123417000448.)). This was not ultimately used for this reproduction, but the dataset can be accessed from <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EUAIFA>

### 3. Outputs:

Folder of the plots produced in section three of the script (**3_Plots.R**), named in correspondence with their names in **SMI205 Extension Pride amid Prejudice.Rmd**

-   **Figure_1.png:** histogram of number of respondents per municipality
-   **Figure_2.png:** map of number of respondents per municipality
-   **Figure_3.png:** facet grid of distribution of each variable
-   **Figure_4.png:** random effects for treatment in the random slopes model, before adding other variables
-   **Figure_5.png:** random effects of the final model
-   **Figure_6.png:** fixed effects of the final model
-   **Figure_7.png:** facet of three maps - support before, support after, and change in support
-   **Null_Pred.png:** plot of intercept estimates in a null model, not included in **SMI205 Extension Pride amid Prejudice.Rmd**

### 4. Scripts:

The full script covers everything from **SMI205 Extension Pride amid Prejudice.Rmd** as well as further tests for each model and additional plots. This script is also cut into three sections to facilitate users; they are designed to be run in order (data cleaning, modelling, plotting) as objects are created throughout and referenced later.

-   **0_Full Script.R:** full R code for the replication extension; internally split into four sections that correspond with the other three scripts: library and set up, data, modelling, plots and results
-   **1_Data_Cleaning.R:** first section of the full script; read, clean, recode, rename data; creates datasets from **'Data/Created/'**
-   **2_Modelling.R:** creates null models, models 1-11, and the final model; comparison tests and test of significance
-   **3_Plots.R:** creates plots used in the replication extension; output from this saved as .png in **'/Outputs'**

### 5. My Environment:

Versions of R and specific packages used to create this project

```{r}
sessionInfo()
```
