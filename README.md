# Wild Bird Analysis and Risk Assessment

This project primarily analyzes highly pathogenic H5 avian influenza in wild birds in the United States, consisting of four main components: visualization of influenza genotype changes, dispersal capability assessment, seasonal analysis, spatiotemporal analysis, and correlation analysis with meteorological factors. The hosted project contains 6 folders with original code and visualization graphics for analyzing these aspects.

## Project Structure

### Bioinformatics Section
Creates Sankey diagrams for different genotypes and infected orders of wild birds infected with avian influenza, showing changes in infected genes over time. This includes genotype data (`gisaid_epiflu_isolates.xls`), original wild bird order classification information (`order name.xlsx`), and graphics processing and plotting code (`Sankey diagram.R`).

### Estimation of Dispersal Capability
Estimates the dispersal capability of wild birds and includes two R files and one CSV file. The `hpai-wild-birds.csv` is the original research data, `R0_Calculation_bird01.R` is the code for calculating dispersal capability R0, and `bird_plot_bird02.R` is the code for graphics visualization.

### Seasonal Analysis
This folder contains one R file and one dataset file. The dataset file has an additional column of data called "order" showing different bird orders. The R code file is mainly used to plot time series bar charts for different orders, STL decomposition results, and box plots comparing different orders across different seasons.

### Heat Map Drawing
This folder includes one R file and one original dataset, mainly used to draw heat maps of the total number and numbers by different years and seasons of wild birds infected with H5 avian influenza in various U.S. states.

### Spatial Analysis of Different Migration Routes across the United States
Primarily conducts spatiotemporal and seasonal analysis of migratory and non-migratory birds in four flyway migration corridors. This contains 5 subfolders: adding migratory bird information folder, adding four migration route folders, seasonal analysis folder for migratory and non-migratory birds on different migration routes, dispersal capability calculation folder, and spatiotemporal analysis folder for migratory and non-migratory birds across the U.S. and four migration routes.

### Analysis of Meteorological Factors
This folder contains one R file and one original dataset. The R file is used to calculate the correlation between different meteorological factors and wild bird H5 subtype avian influenza infection and plot related graphics. The original dataset contains meteorological factor data, where the count column represents the number of wild bird infections.