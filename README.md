# ClimMap Toolkit
The toolkit includes various functions for downloading, processing (e.g. aggregated climatology creation), and visualisation of satellite-derived marine and terrestrial climate data in NetCDF format. Implementation was carried out in [RStudio](https://www.rstudio.com/products/rstudio/download/) (R v3.5.0). The following repositories are supported as of version 0.9 (released 04.11.2019): 

#### Marine Datasets
1. [National Snow and Ice Data Centre (NSIDC)](https://nsidc.org), including [Sea Ice Index](https://nsidc.org/data/seaice_index/archives) and [NOAA/NSIDC Sea Ice Concentration](https://nsidc.org/data/g02202).
2. The [majority](https://oceancolor.gsfc.nasa.gov/atbd/) of [Moderate Resolution Imaging Spectroradiometer (MODIS) Aqua](https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/) and [Sea-Viewing Wide Field-of-View Sensor (SeaWifs)](https://oceandata.sci.gsfc.nasa.gov/SeaWiFS/) products.
3. [Ocean Colour CCI v4.0 (OC CCI)](https://esa-oceancolour-cci.org) data (specialised data processing currently in development).

#### Terrestrial datasets
4. Precipitation, air temperature, and [other data](https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets) from the [HadUK-Grid](https://catalogue.ceda.ac.uk/uuid/4dc8450d889a491ebb20e724debe2dfb).
5. [All](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php) climate variables from [TerraClimate](http://www.climatologylab.org/terraclimate.html).
6. Contemporary [Climate Research Unit Time Series (CRU TS) v4.03](https://crudata.uea.ac.uk/cru/data/hrg/) [data](https://catalogue.ceda.ac.uk/uuid/10d3e3640f004c578403419aac167d82).

Please send any suggestions for inclusion of future datasets to deniz.koseoglu@plymouth.ac.uk.

The toolkit was previously used to calculate a temporally-averaged climatology, as well as a spatially-averaged daily time series of chlorophyll-a (and a record of its percentage change) in the Barents Sea. Chlorophyll concentrations were also determined at point locations where surface sediments were collected in preparation for allomerative hierarchical clustering (AHC) and dimensionality reduction (via PCA). The results are published in:

Belt, S.T., Smik, L., Köseoğlu, D., Knies, J., Husum, K. (2019), "A novel biomarker-based proxy of the spring phytoplankton bloom in Arctic and sub-arctic settings — HBI T<sub>25</sub>", *Quaternary Science Reviews* **523**, 115703.

**NOTE**: A portfolio of figures and data tables generated via ClimMap Toolkit is available [here]().

The toolkit was tested solely on the Windows 10 operating system as of 11/2019, and is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Functionality
The ClimMap Toolkit contains the following functions as of v0.9 (02/11/2019):
1. `clim_download` automatically downloads files from various HTTP, THREDDS, or FTP repositories according to the chosen temporal range (month and/or year) and desired variables.
2. `clim_summary` extracts data from downloaded NetCDF4 or HDF files, with or without further calculations (mean, SD, summed/aggregated climatologies, and/or anomalies).
3. `clim_plot` visualises the data created with `clim_summary` (or that from any suitable .csv file). Please **note** that this function is a Work In Progress (WIP).
4. `clim_region` spatially aggregates extracted/summarised data from any .csv via ESRI shapefiles.
5. `clim_locate` determines values of extracted/summarised satellite data at any number of point locations.
6. `clim_cphyto` calculates phytoplankton biomass from particulate backscattering coefficient (b<sub>bp</sub>) data according to linear calibrations of [Behrenfeld et al., (2005)](https://doi.org/10.1029/2004GB002299), [Graff et al. (2016)](https://doi.org/10.3354/meps11539), or any custom linear regression function.
7. `clim_day` calculates theoretical day length as a function of latitude and day of year according to [Kirk et al., 2010](https://doi.org/10.1017/CBO9781139168212) and based on the C implementation [here](http://orca.science.oregonstate.edu/faq01.php).
8. `clim_divrate` models phytoplankton division/growth rates according to [Behrenfeld et al., 2005](https://doi.org/10.1029/2004GB002299) and/or [Behrenfeld et al. (2016](https://doi.org/10.1038/NCLIMATE2838), [2017](https://doi.org/10.1038/NGEO2861)).
9. `clim_btrack` calculates a record of relative change from a time series (e.g. daily/8-daily chlorophyll concentration or phytoplankton biomass) using various methods ([Behrenfeld et al., 2017](https://doi.org/10.1038/NGEO2861); [McKibben et al., 2012](https://doi.org/10.1029/2012JC008114)).
10. `clim_bloom` detects bloom start/end dates, duration ([Hopkins et al., 2015](https://doi.org/10.1002/2014GB004919)), intensity, magnitude ([Friedland et al., 2018](https://doi.org/10.1111/geb.12717)), and relative contribution to the summed timeseries. Works best with daily chlorophyll concentration timeseries characterised by a pronounced spring bloom peak (e.g. in the Barents Sea and other Arctic shelf areas).
11. `clim_melt` calculates Inner and Outer Melt Season Length (MSL) from the melt season [NOAA data](https://neptune.gsfc.nasa.gov/uploads/files/melt_update_2019.zip) as outlined by [Stroeve et al. (2014)](https://doi.org/10.1002/2013GL058951).
12. `clim_trend` derives the slope (absolute or percentage) of a least-squares trend from timeseries data (e.g. interannual or daily Sea Ice Concentration, SIC) and optionally calculates the significance (*p*-value) and standard error of the regression.
13. `clim_ndate` converts dates within filenames from a yyyy-mm-dd to a yyyy-day_of_year format for compatibility with `clim_summary`.
14. `multMerge` is a helper function that merges all .csv files from a given directory by column(s). 

Other auxiliary functions not used separately include `is.even`, `is.odd`, `get_free_ram`, and `showMemoryUse`.

# Dependencies
R (≥3.5.0), [data.table](https://cran.r-project.org/web/packages/data.table/index.html), [pracma](https://cran.r-project.org/web/packages/pracma/index.html), [XML](https://cran.r-project.org/web/packages/XML/index.html), [rvest](https://cran.r-project.org/web/packages/rvest/index.html), [RCurl](https://cran.r-project.org/web/packages/RCurl/index.html), [curl](https://cran.r-project.org/web/packages/curl/index.html), [purrr](https://cran.r-project.org/web/packages/purrr/index.html), [devtools](https://cran.r-project.org/web/packages/devtools/index.html), [svMisc](https://cran.r-project.org/web/packages/svMisc/index.html), [threddscrawler](https://github.com/BigelowLab/threddscrawler), [obpgcrawler](https://github.com/BigelowLab/obpgcrawler), [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html), [raster](https://cran.r-project.org/web/packages/raster/index.html), [scatterpie](https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html), [maps](https://cran.r-project.org/web/packages/maps/index.html), [mapdata](https://cran.r-project.org/web/packages/mapdata/index.html), [lubridate](https://cran.r-project.org/web/packages/lubridate/index.html), [lattice](https://cran.r-project.org/web/packages/lattice/index.html), [zoo](https://cran.r-project.org/web/packages/zoo/index.html), [bigmemory](https://cran.r-project.org/web/packages/bigmemory/index.html), [bigalgebra](https://cran.r-project.org/web/packages/bigalgebra/index.html), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [ggmap](https://cran.r-project.org/web/packages/ggmap/index.html), [ggalt](https://cran.r-project.org/web/packages/ggalt/index.html), [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html), [maptools](https://cran.r-project.org/web/packages/maptools/index.html), [cowplot](https://cran.r-project.org/web/packages/cowplot/index.html), [rgeos](https://cran.r-project.org/web/packages/rgeos/index.html), [sp](https://cran.r-project.org/web/packages/sp/index.html), [sf](https://cran.r-project.org/web/packages/sf/index.html), [broom](https://cran.r-project.org/web/packages/broom/index.html), [marmap](https://cran.r-project.org/web/packages/marmap/index.html), [animation](https://cran.r-project.org/web/packages/animation/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html), [EnvStats](https://cran.r-project.org/web/packages/EnvStats/index.html), [gtools](https://cran.r-project.org/web/packages/gtools/index.html).

Any missing dependencies should install automatically when first using a given ClimMap function. Otherwise, these can be installed manually via `install.packages()`.

# Getting started
1. Install both [R](https://cran.r-project.org/mirrors.html) and [RStudio](https://www.rstudio.com/products/rstudio/download/).
2. Download the latest ClimMap Toolkit release from the [repository]() and unpack the .zip archive into a directory of your choice. Examples herein use **D:/** as a directory.
3. Open RStudio and create a new R script via **File -> New File -> R Script**. Alternatively, use the console.
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
clim_download <- function(repository="NSIDC", hemisphere="north", frequency="monthly", data_type="Chla", grid_res="9km", year, month, res_path, usrname=NA, pword=NA, suppr_msgs=TRUE, opt_down=TRUE, down_mode="wb", dcores=detectCores(), max_retries=6, shared_folder=TRUE)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **repository** |Which repository to download from? One of: "NSIDC", "MODIS_A", "SeaWifs", "CEDA_MO" (HadUK-Grid), "CRU4" (CRU TS v4.03), "OC_CCI" (Ocean Colour CCI v4.0), or "TClim" (TerraClimate).|
| **hemisphere** |One of: "north" or "south"; only effective when `repository` is `"NSIDC"`.|
| **frequency** |The frequency of data to download. One of: "daily", "monthly", "mon-20y" (if `repository` is "CEDA_MO", "CRU4"), "mon-30y" (if repository is "CEDA_MO", "CRU4", "TClim").|
| **data_type** |Which variable to download data for. One of: "SIC", "SIE_polyline", "SIE_polygon" (when `repository="NSIDC"`); "Chla", "SST", "NSST", "PAR", "iPAR", "PIC", "POC", "NFLH", "Zeu", "KD490", "BBP_GIOP", "BBP_s_GIOP", "Adg_GIOP", "Aph_GIOP", "BBP_GSM", "BBP_QAA" (`repository="MODIS_A"` or `"SeaWifs"`; SST data is not available from the latter); "Chla", "IOP", "RRS", "KD490" (`repository="OC_CCI"`); "groundfrost", "hurs", "psl", "pv", "rainfall", "sfcWind", "sun", "tas", "tasmax", "tasmin" (`repository="CEDA_MO"`); "aet", "def", "swe", "q", "soil", "PDSI", "pet", "ppt", "srad", "tmax", "tmin", "vap", "vpd", "ws" (`repository="TClim"`); "cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet" (`repository="CRU4"`).|
| **grid_res** |Character value of grid resolution at which to download the data. One of: "4km" or "9km". Only useful when `repository` is `"MODIS_A"` or `"SeaWifs"`.|
| **year** |A numeric vector of years for which to download data.|
| **month** |A numeric vector between 1 and 12, or character vector (any of "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") denoting months for which to download data. A value of "all" downloads every month of available data included in `year`.|
| **res_path** |Character directory path where ClimMap Toolkit is located (e.g. "D:/Climate Data").|
| **usrname** |Username (character), required when `repository` is one of: `"CEDA_MO"` or `"OC_CCI"`.|
| **pword** |Password (character), required when `repository` is one of: `"CEDA_MO"` or `"OC_CCI"`.|
| **suppr_msgs** |A TRUE/FALSE logical. Suppresses user updates when downloading files (`TRUE` by default).|
| **opt_down** |A TRUE/FALSE logical. Should multi-threaded (parallelised) downloading be used? Significantly speeds up execution by downloading multiple files simultaneously.|
| **down_mode** |File writing mode. One of: "wb" (default), "w", "a", "ab". See `?download.file` for more information.|
| **dcores** |How many CPU threads should be used for processing? Automatically detects and uses all available threads by default. Only effective when `opt_down` is `TRUE`.|
| **max_retries** |The number of server connections attempted before stopping the function.|
| **shared_folder** |An extra option to specify whether SeaWifs or MODIS Aqua data should be downloaded to the same (shared) folder. Defaults to `TRUE`.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
The function returns NULL as it serves only to download user-requested files.
<br/><br/>
### The `clim_summary` function
#### Description
Extracts data from downloaded NetCDF4 or HDF files, with or without further calculations (mean, SD, summed/aggregated climatologies, and/or anomalies).

#### Usage
```r
clim_summary <- function(remove_miss="all", frequency="daily", repository="NSIDC", hemisphere="north", data_type="Chla", nc_path, year_rng="all", month_rng="all", day_rng=NULL, var_names="default", coord_subset=NULL, subset_order=c("year", "month", ""), summary_func="total", export_path=NULL, big_data, good_names=TRUE, calc_anom=FALSE, calc_sum=FALSE, n_limit=0, mode="summary")
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **repository** |desc|
| **hemisphere** |desc|
| **frequency** |desc|
| **data_type** |desc|
| **nc_path** |desc|
| **year_rng** |desc|
| **month_rng** |desc|
| **day_rng** |desc|
| **var_names** |desc|
| **coord_subset** |desc|
| **subset_order** |desc|
| **summary_func** |desc|
| **export_path** |desc|
| **big_data** |desc|
| **good_names** |desc|
| **calc_anom** |desc|
| **calc_sum** |desc|
| **n_limit** |desc|
| **mode** |desc|
| **remove_miss** |Should missing values be removed? **This argument is not currently used**, and values marked as missing in the original data are always removed.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
The function returns a list containing the `data.frame` of extracted/summarised climatology and/or yearly data (`$yearly_summary`), monthly data (`$monthly_summary`; optional), more visually interpretable/formatted column names (`$clim_pretty_names` and `$clim_pretty_names_monthly`; optional) map projection information (`$projection_info`), and — depending on the type of data processed — a `data.frame` of grid coordinates only (`$coords_total`).
<br/><br/>
### The `clim_plot` function
#### Description
Visualises the data created with `clim_summary` (or that from any suitable .csv file). Please **note** that this function is a **Work In Progress (WIP)** and is not feature-complete.

#### Usage
```r
clim_plot <- function(core_dir = getwd(), sat_data, point_data=NA, point_vars=NA, plot_aes = list(c("fill", "size"), NULL), pie_plot=c(FALSE, 1, 0.005), separ = ",", coord_vars = c("Longitude", "Latitude"), sat_vars, sat_varlabs=sat_vars, scale_alpha=c(0.3, 1), scale_symbol=c(21:25), scale_size="default", sat_sub = list(NULL, "transparent"), sat_rasterize = TRUE, rast_res = c(NA, NA, 3), rast_type = "raster", sat_contours=NULL, satcont_type = c("lines",  1, 1.2), plot_cols=rep(list("default"),10), proj_orig, proj_init="WGS84", proj_final, bathy_res="none", bathy_contours=NULL, coast_res=c("low", "ifb"), bathy_sub = c(-6000:0), coord_sub=NULL, sat_values = "default", sat_breaks="default", break_num=5, sat_lims="default", scale_opts=c(0, 3, FALSE, FALSE), size_lims = c("default", FALSE), leg_labs=waiver(), x_lab="Longitude", y_lab="Latitude", grat=list("WGS84", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7), export_results="all", print_plots="all", export_path, width=10, height=10, point_size=12)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **core_dir** |desc|
| **sat_data** |desc|
| **point_data** |desc|
| **point_vars** |desc|
| **plot_aes** |desc|
| **pie_plot** |desc|
| **separ** |desc|
| **coord_vars** |desc|
| **sat_vars** |desc|
| **sat_varlabs** |desc|
| **scale_alpha** |desc|
| **scale_symbol** |desc|
| **scale_size** |desc|
| **sat_sub** |desc|
| **sat_rasterize** |desc|
| **rast_res** |desc|
| **rast_type** |desc|
| **sat_contours** |desc|
| **satcont_type** |desc|
| **plot_cols** |desc|
| **proj_orig** |desc|
| **proj_init** |desc|
| **proj_final** |desc|
| **bathy_res** |desc|
| **bathy_contours** |desc|
| **coast_res** |desc|
| **bathy_sub** |desc|
| **coord_sub** |desc|
| **sat_values** |desc|
| **sat_breaks** |desc|
| **sat_lims** |desc|
| **scale_opts** |desc|
| **size_lims** |desc|
| **leg_labs** |desc|
| **x_lab** |desc|
| **y_lab** |desc|
| **grat** |desc|
| **export_results** |desc|
| **print_plots** |desc|
| **export_path** |desc|
| **width** |desc|
| **height** |desc|
| **point_size** |desc|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
The function returns two lists containing `ggplot2` objects with maps of satellite data and (optionally) point/sample data.
<br/><br/>
### The `clim_region` function
#### Description
Spatially aggregates extracted/summarised data from any .csv (e.g. that created by `clim_summary`) via ESRI shapefiles.

#### Usage
```r
clim_region <- function(core_dir, poly_path, poly_type="shp_file", poly_list="all", poly_pat="\\.shp$", separ=",", sat_data, sat_vars, sat_varlabs=NA, mean_col=NA, bar_varlabs="default", coord_vars=c("Longitude", "Latitude"), check_inter=FALSE, proj_init="WGS84", proj_final, coast_res=c("low", "ifb"), coord_sub=NULL, grat=list("none", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7, 5), plot_cols=rep(list("default"), 7), plot_type="heat_rank", plot_labs=coord_vars, y_lab="Mean", plot_by="varib", print_plots=TRUE, facet_plots="summarise", plot_opts=c(45, -0.2, 7), plot_extras="sd", plot_oob=FALSE, extra_trends=FALSE, export_plots="pdf", export_path, height=10, width=10, point_size=12, dpi=500)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **core_dir** |desc|
| **poly_path** |desc|
| **poly_type** |desc|
| **poly_list** |desc|
| **poly_pat** |desc|
| **sat_data** |desc|
| **sat_vars** |desc|
| **sat_varlabs** |desc|
| **mean_col** |desc|
| **bar_varlabs** |desc|
| **coord_vars** |desc|
| **check_inter** |desc|
| **proj_init** |desc|
| **proj_final** |desc|
| **coast_res** |desc|
| **coord_sub** |desc|
| **grat** |desc|
| **plot_cols** |desc|
| **plot_type** |desc|
| **plot_labs** |desc|
| **y_lab** |desc|
| **plot_by** |desc|
| **print_plots** |desc|
| **facet_plots** |desc|
| **plot_opts** |desc|
| **plot_extras** |desc|
| **plot_oob** |desc|
| **extra_trends** |desc|
| **export_plots** |desc|
| **export_path** |desc|
| **height** |desc|
| **width** |desc|
| **point_size** |desc|
| **dpi** |desc|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A list containing a `data.frame` with spatially-averaged data and `ggplot2` plots as specified by the user.
<br/><br/>
### The `clim_locate` function
#### Description
Determines values of extracted/summarised data at any number of point locations (only WGS84-projected Latitude/Longitude coordinates are supported at present).

#### Usage
```r
clim_locate <- function(sat_data, point_data, ex_method="bilinear", coord_vars=c("Longitude", "Latitude"), sat_vars=sat_data[,!colnames(sat_data) %in% coord_vars], sat_varlabs=NA, separ=",", proj_init="WGS84", proj_orig, rast_res=c(NA, NA, 3), export_path=getwd())
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **sat_data** |Character filepath to satellite data, or an equivalent R `data.frame` object.|
| **point_data** |Character denoting a filepath, or an R `data.frame` object containing coordinates for which `sat_data` values are to be determined.|
| **ex_method** |One of "simple" or "bilinear"; denotes the method used for determining satellite data value at given point locations. See `?raster::extract` for further information.|
| **coord_vars** |Character vector of length 2 with column names corresponding to x and y coordinates. Defaults to `c("Longitude", "Latitude")`.|
| **sat_vars** |Character vector of satellite data variable column names. Identifies variables whose values are to be determined for `point_data` coordinates.|
| **sat_varlabs** |An optional character vector of alternative satellite variable names, ordered in the same way as (and of equivalent length to) `sat_vars`.|
| **separ** |Separator value to use for imported files. Defaults to a comma for .csv files.|
| **proj_init** |Character string of current map projection information for `sat_data`. |
| **proj_orig** |Character vector of the original map projection of `sat_data`.|
| **rast_res** |A vector of length 3 containing horizontal and vertical raster resolution for `sat_data`, and the number of decimal places to which the coordinates are rounded (3 by default).|
| **export_path** |Character value denoting the directory to which function output is exported.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` of input coordinates and associated variable values (e.g. chlorophyll concentration) determined by the function. 
<br/><br/>
### The `clim_cphyto` function
#### Description
Calculates phytoplankton biomass from particulate backscattering coefficient (b<sub>bp</sub>) data according to linear calibrations of [Behrenfeld et al., (2005)](https://doi.org/10.1029/2004GB002299), [Graff et al. (2016)](https://doi.org/10.3354/meps11539), or any custom linear regression function.

#### Usage
```r
clim_cphyto <- function(bbp_data, algo="BH05", slp=NA, incpt=0, bkgrd=NA, coord_vars=c("Longitude", "Latitude"), ex_path=getwd())
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **bbp_data** |Character filepath or R `data.frame` containing b<sub>bp</sub> data.|
| **algo** |Which algorithm to use for C<sub>phyto</sub> calculation from `bbp_data`? One of: "BH05" ([Behrenfeld et al., 2005](https://doi.org/10.1029/2004GB002299)), "GF16" ([Graff et al., 2016](https://doi.org/10.3354/meps11539)). Ignored if custom values of `slp` and `bkgrd` are provided.|
| **slp** |Optional numeric value of the regression slope.|
| **incpt** |Optional numeric value of the y-intercept.|
| **bkgrd** |Optional numeric value of background b<sub>bp</sub>.|
| **coord_vars** |A character vector of column names containing latitude and longitude coordinates. Defaults to `c("Longitude", "Latitude")`.|
| **ex_path** |Character value denoting the directory to which function output is exported.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` including original input data and phytoplankton biomass (C<sub>phyto</sub>; g C m<sup>-3</sup>) as determined by the function.
<br/><br/>
### The `clim_day` function
#### Description
Calculates theoretical day length as a function of latitude and day of year according to [Kirk et al., 2010](https://doi.org/10.1017/CBO9781139168212) and based on the C implementation [here](http://orca.science.oregonstate.edu/faq01.php).

#### Usage
```r
clim_day <- function(lat_data, lat_var="Latitude", long_var="Longitude", day_rng, ex_path=NA)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **lat_data** |Character filepath or R `data.frame` containing latitude data in decimal degrees.|
| **lat_var** |Character column name containing latitude data. Defaults to "Latitude".|
| **long_var** |Character column name containing longitude data. Defaults to "Longitude".|
| **day_rng** |A numeric vector of days (from 1 to 365) for which to calculate day length.|
| **ex_path** |Character value denoting the directory to which function output is exported.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` with input data and calculated day length (in hours). 
<br/><br/>
### The `clim_divrate` function
#### Description
Models phytoplankton division/growth rates (μ; d<sup>-1</sup>) according to [Behrenfeld et al., 2005](https://doi.org/10.1029/2004GB002299) and/or [Behrenfeld et al. (2016](https://doi.org/10.1038/NCLIMATE2838), [2017](https://doi.org/10.1038/NGEO2861)).

#### Usage
```r
clim_divrate <- function(chla=NA, cphyto, kd490, ipar, par, sst, zeu=NA, dayl, mld, aph, q_val=2, method=c("BH05", "BH17"), chl_c="sat", coord_vars=c("Longitude", "Latitude"), cons_par=TRUE, div_nums=c(64,1), ex_path=getwd(), res="depth_only")
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **chla** |Character filepath or R `data.frame` containing chlorophyll data (mg m<sup>-3</sup>) used to calculate the chlorophyll/C<sub>phyto</sub> ratio. If not provided, the ratio is estimated using either the [original Carbon-Based Productivity Model (CbPM)](http://www.science.oregonstate.edu/ocean.productivity/carbon2.code.php) with `aph`, or the method of [Behrenfeld et al., 2016](https://doi.org/10.1038/NCLIMATE2838); see the `chl_c` argument for more information.|
| **cphyto** |Character filepath or R `data.frame` containing C<sub>phyto</sub> data (e.g. calculated via `clim_cphyto`.|
| **kd490** |Character filepath of R `data.frame` containing diffuse attenuation coefficient for downwelling irradiance data at 490 nm (Kd<sub>490</sub>).|
| **ipar** |Character filepath or R `data.frame` containing Instantaneous Photosynthetically Active Radiation (iPAR) data (mol photon m<sup>-2</sup> day<sup>-1</sup>).|
| **par** |Character filepath or R `data.frame` containing daily integrated Photosynthetically Active Radiation (PAR) data (mol photon m<sup>-2</sup> day<sup>-1</sup>).|
| **sst** |Character filepath or R `data.frame` containing sea surface temperature (SST) data (°C).|
| **zeu** |Character filepath of R `data.frame` containing euphotic zone depth (Z<sub>eu</sub>; m). If not provided, Z<sub>eu</sub> is estimated as the depth of the 0.415 mol photon m<sup>-2</sup> day<sup>-1</sup> isolume from `par` and `kd490` ([Behrenfeld et al., 2017](https://doi.org/10.1038/NGEO2861)).|
| **dayl** |Character filepath of R `data.frame` containing day length (hours), as calculated via `clim_day`.|
| **mld** |Character filepath of R `data.frame` containing Mixed Layer Depth (MLD) data (m).|
| **aph** |Character filepath of R `data.frame` containing absorption coefficients for phytoplankton (a<sub>ph</sub>) at 443 nm (m<sup>-1</sup>).|
| **q_val** |The *Q<sub>10</sub>* value used to estimate the chlorophyll-specific light-saturated rate of photosynthesis from an exponential relationship with `sst` ([Eppley, 1972](https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/1972/704/eppley.pdf)).|
| **method** |Which method to use for division/growth rate estimation? Any of "BH05" ([(Behrenfeld et al., 2005)](https://doi.org/10.1029/2004GB002299) and/or "BH17" ([(Behrenfeld et al., 2017)](https://doi.org/10.1038/NGEO2861).|
| **chl_c** |Determines the method used to estimate the chlorophyll/C<sub>phyto</sub> ratio. One of "sat" (satellite chlorophyll), "aph_ratio" (CbPM model), or "BH16" (the model of Behrenfeld et al., 2016).|
| **coord_vars** |A character vector of column names containing latitude and longitude coordinates. Defaults to `c("Longitude", "Latitude")`.|
| **cons_par** |If `TRUE`, ensures that the integral of iPAR throughout the day length (`dayl`), which is used to calculate division rate μ (d<sup>-1</sup>) when `method="BH17"`, is equal to the daily integrated PAR value (`par`). Otherwise, `ipar` is assumed to represent maximum PAR at solar noon.|
| **div_nums** |Numeric vector of length 2. When `method="BH17"` and `res="time_depth"`, the first and second elements denote the number of time steps throughout day length (`dayl`) and the interval between MLD (`mld`) used for the double integration, respectively. Defaults to `c(64, 1)`.|
| **ex_path** |Character value denoting the directory to which function output is exported.|
| **res** |One of "time_depth" or "depth_only" (default; much less computationally expensive). Determines whether division rate μ (d<sup>-1</sup>) is resolved for time and depth or only the latter, respectively ([Behrenfeld et al., 2017](https://doi.org/10.1038/NGEO2861)).|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A list of `data.frame` objects containing input data and division/growth rates (d<sup>-1</sup>) calculated via the chosen algorithm(s).
<br/><br/>
### The `clim_btrack` function
#### Description
Calculates a record of relative change from a time series (e.g. daily/8-daily chlorophyll concentration or phytoplankton biomass) using various methods ([Behrenfeld et al., 2017](https://doi.org/10.1038/NGEO2861); [McKibben et al., 2012](https://doi.org/10.1029/2012JC008114)).

#### Usage
```r
clim_btrack <- function(data, sat_vars, coord_vars=c("xcoord", "ycoord", "Longitude", "Latitude"), run_window = 8, mean_fun=c("arithm", "arithm"), var_lab="default", method="mckibben", times=c("days", NA), year_val=2009, monthly_aggr=TRUE, export_path=NA, extras=TRUE, smoothing=NA)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **data** |Character filepath or R `data.frame` containing data (e.g. chlorophyll concentration) from which to derive a relative change record.|
| **sat_vars** |Character column names of daily-resolution satellite data variables to use.|
| **coord_vars** |A character vector of column names containing latitude and longitude coordinates. Defaults to `c("Longitude", "Latitude")`.|
| **run_window** |A numeric value denoting the number of data points (e.g. daily satellite data values) averaged before calculating relative change between them. Defaults to 8 (i.e. relative change is calculated between averages of 8 data points, and a constant time step of 1 data point).|
| **mean_fun** |A character vector of length 2. Determines the type of averaging technique used when calculating relative change (first element) and aggregating by month when `monthly_aggr=TRUE` (second element). One of "arithm" and/or "geo" for arithmetic and geometric mean, respectively.|
| **var_lab** |A character value used to identify the satellite data variable processed. Added to output column names for user convenience. If equal to `"default"`, attemps to identify the variable name automatically under the assumption that data was summarised via `clim_summary`. If `NA`, defaults to "ArbitraryVar".|
| **method** |One of "mckibben", "bh_r", or "bh_dudt". Calculates relative change according to the methods of [McKibben et al. (2012)](https://doi.org/10.1029/2012JC008114) Equation (1), [Behrenfeld et al. (2017)](https://doi.org/10.1038/NGEO2861) Equations (2) and (3), respectively.|
| **times** |A vector used to convert column names of `sat_vars` to the correct format for function compatibility. The first element is a character denoting the type of time step (e.g. "days" for daily data). The remaining elements are numeric and denote the start and end points of temporal coverage (e.g. 60:273 is appropriate if the data spans days 60 through to 273 of the year at a daily resolution).|
| **year_val** |**Currently not utilised.**|
| **monthly_aggr** |A TRUE/FALSE logical. Should relative change be averaged by month? Defaults to `TRUE`.|
| **export_path** |Character value denoting the directory to which function output is exported.|
| **smoothing** |If numeric, applies LOESS smoothing to the data as a pre-processing step and denotes the span value used. If `NA` (default), no smoothing is applied.|
| **extras** |A TRUE/FALSE logical. If `TRUE` (default), calculates additional parameters from the relative change record, including: maximum and minimum value above 0 (Pos_MAX and Pos_MIN), mean and median value above 0 (Pos_MEAN and Pos_MEDIAN), duration (in number of data points) of values above 0 and their percentage from the total (Pos_DUR and Pos_Dur%); most of these parameters, with the exception of maximum value, are also calculated for values below 0 (Neg_MIN, Neg_MEAN, Neg_MEDIAN, Neg_DUR, Neg_Dur%). Finally, sample size of non-missing values is also calculated (SSize). Parameters calculated from non-smoothed data have the prefix "RAW", while a second set of parameters with the prefix "LOESS" is additionally calculated from LOESS-smoothed data when `smoothing` is numeric.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` with timeseries of relative change of the input variable (e.g. in chlorophyll concentration) and additional calculations as specified by the user (e.g. maximum/minimum/mean/median/duration of positive and negative change).
<br/><br/>
### The `clim_bloom` function
#### Description
Detects bloom start/end dates, duration ([Hopkins et al., 2015](https://doi.org/10.1002/2014GB004919)), intensity, magnitude ([Friedland et al., 2018](https://doi.org/10.1111/geb.12717)), and percentage contribution to the summed timeseries. Works best with daily chlorophyll concentration timeseries characterised by a pronounced spring bloom peak (e.g. in the Barents Sea and other Arctic shelf areas).

#### Usage
```r
clim_bloom <- function(data, coord_vars=c("Longitude", "Latitude"), consec_num=2, thres_percent=0.05, thres_num=2, grep_patt=".*days[[:digit:]]{1,3}.([[:digit:]]{1,3}).$", varlab="chlor_a", export_path, bloom_dur=FALSE, smoothing=NA, smooth_max=NA)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **data** |Character filepath or R `data.frame` containing time series of data (e.g. daily-resolution chlorophyll concentration) from which to derive bloom descriptors.|
| **coord_vars** |A character vector of column names containing latitude and longitude coordinates. Defaults to `c("Longitude", "Latitude")`.|
| **consec_num** |The number of consecutive data points below the set threshold required to identify bloom start and end dates (see [Hopkins et al., 2015](https://doi.org/10.1002/2014GB004919)).|
| **thres_percent** |desc|
| **thres_num** |desc|
| **grep_patt** |desc|
| **varlab** |desc|
| **export_path** |desc|
| **bloom_dur** |desc|
| **smoothing** |desc|
| **smooth_max** |desc|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
Returns a `data.frame` containing original input data and calculated bloom descriptors (see **Arguments**).
<br/><br/>
### The `clim_melt` function
#### Description
Calculates Inner and Outer Melt Season Length (MSL) from the melt season [NOAA data](https://neptune.gsfc.nasa.gov/uploads/files/melt_update_2019.zip), as outlined by [Stroeve et al., 2014](https://doi.org/10.1002/2013GL058951).

#### Usage
```r
clim_melt <- function(data)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **data** |An R object containing a 4-column `data.frame` of yearly [Melt Season Data](https://neptune.gsfc.nasa.gov/uploads/files/melt_update_2019.zip), e.g. that processed via `clim_summary`.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` with input MSL data and calculated Inner MSL ("Inmelt") and Outer MSL ("Outmelt"), in days. 
<br/><br/>
### The `clim_trend` function
#### Description
Derives the slope (absolute or percentage) of a least-squares trend from timeseries data (e.g. interannual or daily Sea Ice Concentration, SIC) and calculates the significance (*p*-value) of linear regression.

#### Usage
```r
clim_trend <- function(data, trend_vars, trend_unit="default", add_stats = TRUE, coord_vars = c("Longitude", "Latitude"), time_span)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **data** |An R object containing the input data (e.g. from `clim_summary`). The function is designed for use with yearly data, but any time step can be used.|
| **trend_vars** |A character vector of column names to use for least-squares regression.|
| **trend_unit**|The format to use for calculated least-squares slopes. The default value leaves the slope unaltered; `"decade"` multiplies the slope by 10, providing a decadal trend assuming input data is yearly; `"%"` normalises the slope to the mean value; `"% decade"` additionally multiplies the slope by 10, yielding a decadal trend of relative change.|
| **add_stats**|A TRUE/FALSE logical determining whether standard error and *p*-value are calculated for each regression (`TRUE` by default; switch to `FALSE` to considerably accelerate function execution).|
| **coord_vars**|A character vector of column names containing latitude and longitude coordinates. Defaults to `c("Longitude", "Latitude")`.|
| **time_span** |A character vector of length 2 designed to contain the first and last year of input data coverage, which are added to column names for the user's convenience. Otherwise, can take any value.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` with input data and calculated slope (absolute or percentage change per year/decade as specified by the user), *p*-value, and standard error (optional).
<br/><br/>
### The `clim_ndate` function
#### Description
Converts dates within filenames from a yyyy-mm-dd to a yyyy-day_of_year format for compatibility with `clim_summary`.

#### Usage
```r
clim_ndate <- function(file_path)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **file_path** |A character vector containing the directory path. Filenames where a date in the yyyy-mm-dd format is found will be renamed (e.g. 20020701 changes to 2002182).|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
Returns NULL. The files within the specified directory are renamed.
<br/><br/>
### The `multMerge` function
#### Description
A helper function that merges all .csv files from a given directory by column(s).

#### Usage
```r
multMerge <- function(mypath, use_dt=TRUE, patt="\\.csv$", which.files="all", by_cols=FALSE, which.cols=NULL)
```

#### Arguments
| Argument | Description |
| ------------- |-------------|
| **mypath** |A character vector denoting the filepath of files to be merged.|
| **use_dt** |A TRUE/FALSE logical. When `TRUE` (default), the `data.table` package is used for significantly quicker importing of large files into R.|
| **patt** |A `grep` pattern to filter the files by name within the target directory. The default value ensures only .csv files are merged.|
| **arg**| A numeric vector of file IDs (assigned based on their order within `mypath`). Used to further filter the files to be merged. The default value, "all", merges all files detected in `mypath` using `patt`.|
| **by_cols**| A character vector of column names to use for merging. By default, merging is carried out by columns common to all files in `mypath`.|
| **which.cols**| A character vector of column names to keep in the final output. Defaults to NULL, keeping all columns.|

#### Details
Please refer to the [ClimMap Toolkit vignette]() for *reproducible* usage examples of functions.

#### Values
A `data.frame` containing columns from all files merged (e.g. by latitude/longitude coordinates). 
<br/><br/>
