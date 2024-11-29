# global subnational dataset for income inequality (Gini coefficient) over 1990-2021

These codes were used to create the subnational dataset for Gini coefficient that is part of the following preprint. Please do cite this paper if using the codes. 

Chrisendo, Venla, Hoffman, Masoumzadeh Sayyar, Rocha, Sandström, Solt, Kummu. 2024. Income inequality has increased for over two-thirds of the global population. Preprint. 

The input data needed to run the code is available in the repository: http://doi.org/10.5281/zenodo.14056855. Please extract the zip-file (data_in_gini.zip) under the same folder with the code. 

The final output data files are within the same repository. 


The code is numbered with the order it should be run. Below each code is briefly explained. We used R (version 4.3.2) to develop the code.

**0_install_packages.R**: install the needed packages

**1_gini_processing_adm0.R**: puts together admin 0 level (national) data. 

**2_gini_interpExtrap_adm0_v2**: Interpolates and extrapolates the missing values.

**3_gini_processing_adm1.R**: puts together admin 1 level (sbunational) data, interpolates and extrapolates the missing values

**4_gini_adm1_ratio_over_adm0**: calculates the ratio between admin 1 and admin 0 level

**5_gini_prepare_spatial.R**:  combines the admin 0 and admin 1 level to a global grid and gpkg file; puts data to raster and polygon files

**6_gini_plot_maps.R**: script to plot maps shown in the manuscript

**7_storeFinalFiles.R**: store final files


**functions**: the functions used in the scripts above are in this folder


For more information, please contact Matti Kummu (matti.kummu@aalto.fi)