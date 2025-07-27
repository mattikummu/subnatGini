# global subnational dataset for income inequality (Gini coefficient) over 1990-2023

These codes were used to create the subnational dataset for Gini coefficient that is part of the following preprint. Please do cite this paper if using the codes. 

Chrisendo D, Niva V, Hoffman R, Sayyar SM, Rocha J, Sandström V, Solt F, Kummu M. 2024. Income inequality has increased for over two-thirds of the global population. Preprint. doi: https://doi.org/10.21203/rs.3.rs-5548291/v1


**SOFTWARE**

We used R (version 4.3.2) to develop and test the code. On top of R, also following softwares are needed:

**QGIS**: open-source Geographic Information System.
Installation Instructions: qgis.org/en/site/forusers/download.html 

**Node.js**: A JavaScript runtime environment that is required to run Mapshaper.
Installation Instructions: nodejs.org/en/download

**Mapshaper**: A tool for editing and simplifying geospatial data. After installing Node.js, you can install Mapshaper by running a single command in your system's terminal (like Terminal on macOS/Linux or Command Prompt/PowerShell on Windows).
Installation Command: npm install -g mapshaper
Homepage: mapshaper.org

All of these are open source softwares, and can be installed on the most common operating systems. Istalling of these will take ca 15-30 min. 


**CODE SCRIPTS**

The code is numbered with the order it should be run. Below each code is briefly explained. 

**0_install_packages.R**: install the needed packages

**1_gini_processing_adm0.R**: puts together admin 0 level (national) data. 

**2_gini_interpExtrap_adm0_v3.R**: Interpolates and extrapolates the missing values for SWIID dataset.

**2.1_gini_interpExtrap_adm0_v3_WID.R**: Interpolates and extrapolates the missing values for WID dataset.

**3_gini_processing_adm1.R**: puts together admin 1 level (sbunational) data, interpolates and extrapolates the missing values

**4_gini_adm1_ratio_over_adm0**: calculates the ratio between admin 1 and admin 0 level

**5_gini_prepare_spatial.R**:  combines the admin 0 and admin 1 level to a global grid and gpkg file; puts data to raster and polygon files

**6_gini_plot_maps.R**: script to plot maps shown in the manuscript

**7_storeFinalFiles.R**: store final files

**8_sensitivityAnalysisAdm0Extrap.R**: sensitivity analysis for extrapolation of admin 0 level data

**9_uncertainty_v4.R**: Monte Carlo simulations for uncertainty estimates 

**9_1_uncertainty_analysis.R**: analysis of Monte Carlo results

**9_2_uncertainty_plot.R**: plotting the uncertainty results

**10_comparison_adm0data.R**: compares two national datasets: WID and SWIID


**functions**: the functions used in the scripts above are in this folder


**REPRODUCTION**

The input data needed to run the code is available in the repository: http://doi.org/10.5281/zenodo.14056855. Please extract the zip-file (data_in_gini.zip) under the same folder with the code. To run all the scripts, will take on average 12 hours to complete. Without the uncertainty Monte Carlo, the code running time is much less, ca one hour. 

The final output data files, i.e. the expected outcome from the code, are within the same repository than the input data. 


For more information, please contact Matti Kummu (matti.kummu@aalto.fi)