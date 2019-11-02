# ClimMap Toolkit
The toolkit includes various functions for downloading, processing (e.g. aggregated climatology creation), and visualisation of satellite-derived marine and terrestrial climate data in NetCDF format. Implementation was carried out in [RStudio](https://www.rstudio.com/products/rstudio/download/) (R v3.5.0). The following repositories are supported as of version 0.9 (released 02.11.2019): 
1. [National Snow and Ice Data Centre (NSIDC)](https://nsidc.org), including [Sea Ice Index](https://nsidc.org/data/seaice_index/archives) and [NOAA/NSIDC Sea Ice Concentration](https://nsidc.org/data/g02202).
2. The [full complement](https://oceancolor.gsfc.nasa.gov/atbd/) of [Moderate Resolution Imaging Spectroradiometer (MODIS) Aqua](https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/) and [Sea-Viewing Wide Field-of-View Sensor (SeaWifs)](https://oceandata.sci.gsfc.nasa.gov/SeaWiFS/) products.
3. Precipitation, air temperature, and [other data](https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets) from the [HadUK-Grid](https://catalogue.ceda.ac.uk/uuid/4dc8450d889a491ebb20e724debe2dfb).
4. [All](https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php) climate variables from [TerraClimate](http://www.climatologylab.org/terraclimate.html).
5. Contemporary [Climate Research Unit Time Series (CRU TS) v4.03](https://crudata.uea.ac.uk/cru/data/hrg/) [data](https://catalogue.ceda.ac.uk/uuid/10d3e3640f004c578403419aac167d82).

The toolkit was previously used to calculate a temporally-averaged climatology, as well as a spatially-averaged daily time series of chlorophyll-a (and a record of its percentage change) in the Barents Sea. Chlorophyll concentrations were also determined at point locations where surface sediments were collected in preparation for allomerative hierarchical clustering (AHC) and dimensionality reduction (via PCA). The results are published in:

Belt, S.T., Smik, L., Köseoğlu, D., Knies, J., Husum, K. (2019), "A novel biomarker-based proxy of the spring phytoplankton bloom in Arctic and sub-arctic settings — HBI T<sub>25</sub>", *Quaternary Science Reviews* ***523***, 115703.

The toolkit is provided as-is under the terms of the MIT licence (see LICENCE.md in the repository root).

# Functionality
The ClimMap Toolkit contains the following functions as of v0.9 (02/11/2019):
1. `clim_download`
2. `clim_summary`
3. `clim_ndate`
4. `clim_cphyto`
5. `clim_region`
6. `clim_locate`
7. `clim_bloom`
8. `clim_btrack`
9. `clim_divrate`
10. `clim_plot`
11. `clim_day`
12. `clim_trend`
13. `clim_melt`
14. `multMerge`

Other auxiliary functions not used separately include `is.even`, `is.odd`, `get_free_ram`, and `showMemoryUse`.

# Dependencies
R (>=3.5.0), [data.table](https://cran.r-project.org/web/packages/data.table/index.html), [pracma](https://cran.r-project.org/web/packages/pracma/index.html), [XML](https://cran.r-project.org/web/packages/XML/index.html), [rvest](https://cran.r-project.org/web/packages/rvest/index.html), [RCurl](https://cran.r-project.org/web/packages/RCurl/index.html), [curl](https://cran.r-project.org/web/packages/curl/index.html), [purrr](https://cran.r-project.org/web/packages/purrr/index.html), [devtools](https://cran.r-project.org/web/packages/devtools/index.html), [svMisc](https://cran.r-project.org/web/packages/svMisc/index.html), [threddscrawler](https://github.com/BigelowLab/threddscrawler), [obpgcrawler](https://github.com/BigelowLab/obpgcrawler), [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html), [raster](https://cran.r-project.org/web/packages/raster/index.html), [scatterpie](https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html), [maps](https://cran.r-project.org/web/packages/maps/index.html), [mapdata](https://cran.r-project.org/web/packages/mapdata/index.html), [lubridate](https://cran.r-project.org/web/packages/lubridate/index.html), [lattice](https://cran.r-project.org/web/packages/lattice/index.html), [zoo](https://cran.r-project.org/web/packages/zoo/index.html), [bigmemory](https://cran.r-project.org/web/packages/bigmemory/index.html), [bigalgebra](https://cran.r-project.org/web/packages/bigalgebra/index.html), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [ggmap](https://cran.r-project.org/web/packages/ggmap/index.html), [ggalt](https://cran.r-project.org/web/packages/ggalt/index.html), [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html), [maptools](https://cran.r-project.org/web/packages/maptools/index.html), [cowplot](https://cran.r-project.org/web/packages/cowplot/index.html), [rgeos](https://cran.r-project.org/web/packages/rgeos/index.html), [sp](https://cran.r-project.org/web/packages/sp/index.html), [sf](https://cran.r-project.org/web/packages/sf/index.html), [broom](https://cran.r-project.org/web/packages/broom/index.html), [marmap](https://cran.r-project.org/web/packages/marmap/index.html), [animation](https://cran.r-project.org/web/packages/animation/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html), [EnvStats](https://cran.r-project.org/web/packages/EnvStats/index.html), [gtools](https://cran.r-project.org/web/packages/gtools/index.html).

# Getting started
