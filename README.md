# ClimMap Toolkit
The toolkit includes various functions for downloading, processing (e.g. aggregated climatology creation), and visualisation of satellite-derived marine and terrestrial climate data in NetCDF format. Implementation was carried out in [RStudio](https://www.rstudio.com/products/rstudio/download/) (R v3.5.0). The following repositories are supported as of version 0.9 (released 02.11.2019): 
1. [National Snow and Ice Data Centre (NSIDC)](https://nsidc.org), including [Sea Ice Index](https://nsidc.org/data/seaice_index/archives) and [NOAA/NSIDC Sea Ice Concentration](https://nsidc.org/data/g02202).
2. The [full complement](https://oceancolor.gsfc.nasa.gov/atbd/) of [Moderate Resolution Imaging Spectroradiometer (MODIS) Aqua](https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/) and [Sea-Viewing Wide Field-of-View Sensor (SeaWifs)](https://oceandata.sci.gsfc.nasa.gov/SeaWiFS/) products.
3. Precipitation, air temperature, and [other data](https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets) from the [HadUK-Grid](https://catalogue.ceda.ac.uk/uuid/4dc8450d889a491ebb20e724debe2dfb).
4. [All](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php) climate variables from [TerraClimate](http://www.climatologylab.org/terraclimate.html).
5. Contemporary [Climate Research Unit Time Series (CRU TS) v4.03](https://crudata.uea.ac.uk/cru/data/hrg/) [data](https://catalogue.ceda.ac.uk/uuid/10d3e3640f004c578403419aac167d82).

Please send any suggestions for inclusion of future datasets to [deniz.koseoglu@plymouth.ac.uk](mailto: deniz.koseoglu@plymouth.ac.uk).

The toolkit was previously used to calculate a temporally-averaged climatology, as well as a spatially-averaged daily time series of chlorophyll-a (and a record of its percentage change) in the Barents Sea. Chlorophyll concentrations were also determined at point locations where surface sediments were collected in preparation for allomerative hierarchical clustering (AHC) and dimensionality reduction (via PCA). The results are published in:

Belt, S.T., Smik, L., Köseoğlu, D., Knies, J., Husum, K. (2019), "A novel biomarker-based proxy of the spring phytoplankton bloom in Arctic and sub-arctic settings — HBI T<sub>25</sub>", *Quaternary Science Reviews* **523**, 115703.

**NOTE**: A portfolio of figures and data tables generated via ClimMap Toolkit is available [here]().

The toolkit is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Functionality
The ClimMap Toolkit contains the following functions as of v0.9 (02/11/2019):
1. `clim_download` automatically downloads files from various HTTP, THREDDS, or FTP repositories according to the chosen temporal range (month and/or year) and desired variables.
2. `clim_summary` extracts data from downloaded NetCDF4 or HDF files, with or without further calculations (mean, SD, summed/aggregated climatologies, and/or anomalies).
3. `clim_plot` visualises the data created with `clim_summary` (or that from any suitable .csv file). Please **note** that this function in a Work In Progress (WIP).
4. `clim_region` spatially aggregates extracted/summarised data from any .csv via ESRI shapefiles.
5. `clim_locate` determines values of extracted/summarised data at any number of point locations.
6. `clim_cphyto` calculates phytoplankton biomass from particulate backscattering coefficient (b<sub>bp</sub>) data according to linear calibrations of [Behrenfeld et al., (2005)](https://doi.org/10.1029/2004GB002299), [Graff et al. (2016)](https://doi.org/10.3354/meps11539), or any custom linear regression function.
7. `clim_day` calculates theoretical day length as a function of latitude and day of year according to [Kirk et al., 2010](https://doi.org/10.1017/CBO9781139168212) and based on the C implementation [here](http://orca.science.oregonstate.edu/faq01.php).
8. `clim_divrate` models phytoplankton division/growth rates according to [Behrenfeld et al., 2005](https://doi.org/10.1029/2004GB002299) and/or [Behrenfeld et al. (2016](https://doi.org/10.1038/NCLIMATE2838), [2017](https://doi.org/10.1038/NGEO2861)).
9. `clim_btrack` calculates a record of relative change from a time series (e.g. daily/8-daily chlorophyll concentration or phytoplankton biomass) using various methods ([Behrenfeld et al., 2017](https://doi.org/10.1038/NGEO2861); [McKibben et al., 2012](https://doi.org/10.1029/2012JC008114)).
10. `clim_bloom` detects bloom start/end dates, duration ([Hopkins et al., 2015](https://doi.org/10.1002/2014GB004919)), intensity, magnitude ([Friedland et al., 2018](https://doi.org/10.1111/geb.12717)), and relative contribution to the summed timeseries. Works best with daily chlorophyll concentration timeseries characterised by a pronounced spring bloom peak (e.g. in the Barents Sea and other Arctic shelf areas).
11. `clim_melt` calculates Inner and Outer Melt Season Length (MSL) from the melt season [NOAA data](https://neptune.gsfc.nasa.gov/uploads/files/melt_update_2019.zip) as outlined by [Stroeve et al., 2014](https://doi.org/10.1002/2013GL058951).
12. `clim_trend` derives the slope (absolute or percentage) of a least-squares trend from timeseries data (e.g. interannual or daily Sea Ice Concentration, SIC) and calculates the significance (*p*-value) of linear regression.
13. `clim_ndate` converts dates within filenames from a yyyy-mm-dd to a yyyy-day_of_year format for compatibility with `clim_summary`.
14. `multMerge` is a helper function that merges all .csv files from a given directory by column(s). 

Other auxiliary functions not used separately include `is.even`, `is.odd`, `get_free_ram`, and `showMemoryUse`.

# Dependencies
R (>=3.5.0), [data.table](https://cran.r-project.org/web/packages/data.table/index.html), [pracma](https://cran.r-project.org/web/packages/pracma/index.html), [XML](https://cran.r-project.org/web/packages/XML/index.html), [rvest](https://cran.r-project.org/web/packages/rvest/index.html), [RCurl](https://cran.r-project.org/web/packages/RCurl/index.html), [curl](https://cran.r-project.org/web/packages/curl/index.html), [purrr](https://cran.r-project.org/web/packages/purrr/index.html), [devtools](https://cran.r-project.org/web/packages/devtools/index.html), [svMisc](https://cran.r-project.org/web/packages/svMisc/index.html), [threddscrawler](https://github.com/BigelowLab/threddscrawler), [obpgcrawler](https://github.com/BigelowLab/obpgcrawler), [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html), [raster](https://cran.r-project.org/web/packages/raster/index.html), [scatterpie](https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html), [maps](https://cran.r-project.org/web/packages/maps/index.html), [mapdata](https://cran.r-project.org/web/packages/mapdata/index.html), [lubridate](https://cran.r-project.org/web/packages/lubridate/index.html), [lattice](https://cran.r-project.org/web/packages/lattice/index.html), [zoo](https://cran.r-project.org/web/packages/zoo/index.html), [bigmemory](https://cran.r-project.org/web/packages/bigmemory/index.html), [bigalgebra](https://cran.r-project.org/web/packages/bigalgebra/index.html), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [ggmap](https://cran.r-project.org/web/packages/ggmap/index.html), [ggalt](https://cran.r-project.org/web/packages/ggalt/index.html), [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html), [maptools](https://cran.r-project.org/web/packages/maptools/index.html), [cowplot](https://cran.r-project.org/web/packages/cowplot/index.html), [rgeos](https://cran.r-project.org/web/packages/rgeos/index.html), [sp](https://cran.r-project.org/web/packages/sp/index.html), [sf](https://cran.r-project.org/web/packages/sf/index.html), [broom](https://cran.r-project.org/web/packages/broom/index.html), [marmap](https://cran.r-project.org/web/packages/marmap/index.html), [animation](https://cran.r-project.org/web/packages/animation/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html), [EnvStats](https://cran.r-project.org/web/packages/EnvStats/index.html), [gtools](https://cran.r-project.org/web/packages/gtools/index.html).

Any missing dependencies should install automatically when first using any ClimMap function. Otherwise, these can be installed manually via `install.packages()`.

# Getting started
1. Install both [R](https://cran.r-project.org/mirrors.html) and [RStudio](https://www.rstudio.com/products/rstudio/download/).
2. Download the latest ClimMap Toolkit release from the [repository]() and unpack the .zip archive into a directory of your choice. Examples herein use **D:/** as a directory.
3. Open RStudio and create a new R script via **File -> New File -> R Script**.
4. Source the ClimMap Toolkit functions  from the "ClimMap_Toolkit_v09.R" file located in the directory to which the .zip archive was unpacked. For example, assuming the "CT_Toolkit.R" file is located in D:/, the following command may be used:
```r
source("D:/Climate Data/ClimMap_Toolkit_v09.R")
```
You are now ready to use the functions.

5. Note that although the "ClimMap_Toolkit_v09.R" file does not have to be opened directly in R (or RStudio), it contains the source code which can be examined and altered by the user where required.


# Functions
### The `clim_download` function
#### Description
Automatically downloads files from various HTTP, THREDDS, or FTP repositories according to the chosen temporal range (month and/or year) and desired variables.

#### Usage
```r

```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **arg** |desc|
| **arg** |desc|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
The function returns NULL and as it serves only to download user-requested files.
<br/><br/>
### The `clim_summary` function
#### Description
Extracts data from downloaded NetCDF4 or HDF files, with or without further calculations (mean, SD, summed/aggregated climatologies, and/or anomalies).

#### Usage
```r

```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **arg** |desc|
| **arg** |desc|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
The function returns a list containing the `data.frame` of extracted/summarised climatology and/or yearly data (`$yearly_summary`), monthly data (`$monthly_summary`; optional), more visually interpretable/formatted column names (`$clim_pretty_names` and `$clim_pretty_names_monthly`; optional) map projection information (`$projection_info`), and — depending on the type of data processed — a `data.frame` of grid coordinates only (`$coords_total`).
