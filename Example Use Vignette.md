# ClimMap Toolkit Vignette
This example use vignette showcases some of the functionality included in the ClimMap Toolkit, and depends on the user having [downloaded](https://drive.google.com/file/d/1XAEcbGg-2_GwzQXVF2M05I_B7eZ_JghE/view?usp=sharing) and [set up](https://github.com/Deniz-Koseoglu/ClimMap_Toolkit#getting-started) the latest release (currently v0.9.1) using provided instructions, complete with [dependencies](https://github.com/Deniz-Koseoglu/ClimMap_Toolkit#dependencies). While the toolkit may be unpacked to any directory of your choice, this vignette assumes that the "ClimMap_Toolkit" folder contained in the downloaded .ZIP archive was extracted to **D:/**.

Note that all initial function output presented herein (except individual daily Chla files due to their large size) is available in the extracted **"D:/ClimMap_Toolkit/Example/Vignette Outputs"** folder, while the code is summarised in the **"D:/ClimMap_Toolkit/ClimMap_Toolkit_v091_EXAMPLES.R"** file.
<br/><br/>
## EXAMPLE 1: Working with monthly data
In this example, [NOAA/NSIDC Sea Ice Concentration (SIC)]() and [MODIS Aqua chlorophyll-*a* (Chla)]() data will be downloaded, summarised to interannual and annual average climatologies, visualised on maps using the Orthographic North Polar (ONP) projection, spatially averaged for various Arctic regions defined by ESRI shapefiles. Finally, SIC and Chla concentrations will be determined for a set of point locations representing surface sediments.

### Downloading monthly SIC and Chla data
We will download data via `clim_download` as follows:

```r
#Make sure you have sourced the ClimMap_Toolkit functions!
source("D:/ClimMap_Toolkit/ClimMap_Toolkit_v091.R")

#SIC data (no security credentials required)
clim_download(repository = "NSIDC",
              hemisphere = "north",
              frequency = "monthly",
              data_type = "SIC",
              year = 1988:2018,
              month = "all",
              res_path = "D:/ClimMap_Toolkit",
              suppr_msgs = TRUE,
              opt_down = TRUE)

#Chla data (Note Earth Data Login information required!)
clim_download(repository = "MODIS_A",
              frequency = "monthly",
              data_type = "Chla",
              year = 2003:2018,
              month = "all",
              res_path = "D:/ClimMap_Toolkit",
              usrname="dckoseoglu",
              pword="Example_Pword01",
              suppr_msgs = TRUE,
              opt_down = FALSE,
              shared_folder = TRUE)

```

The files should appear in **D:/ClimMap_Toolkit/Data/NSIDC/Data/SIC/nh_monthly** (SIC) and **D:/ClimMap_Toolkit/Data/SeaWifs_MODISA/Data/Chla/monthly** (Chla).

**NOTE:** As MODIS Aqua and SeaWifs data now require Earth Data user login, `usrname` and `pword` arguments containing the user name and password are now utilised in the second example for downloading chlorophyll (Chla) data. While working example credentials are provided here, please [register on Earth Data](https://urs.earthdata.nasa.gov/users/new) and use your own whenever possible.

<br/><br/>
### Creating average climatologies
We will derive April-June (i.e. "spring") SIC and April-May Chla climatologies using `clim_summary` using downloaded data. First, the SIC data, which is summarised for the period of 1988-2018 and months 4-6 (i.e. April-June). Since we also want the annual mean for each year, we specify `summary_func` to be `"yearly"`. The argument `var_names` specifies only the mean SIC values to work with, and ignores their SDs included in the netCDF files. The .CSV output can be checked at the `export_path` directory, and the climatology is also assigned as a `satlist` element named `"SIC_north"`.

```r
#Create a list to store Clim_Summary output
satlist <- list()

#Extract and summarize April-June SIC data from 1988-2018
satlist[["SIC_north"]] <- clim_summary(repository = "NSIDC",
                                       hemisphere = "north",
                                       data_type = "SIC",
                                       frequency = "monthly",
                                       nc_path = "D:/ClimMap_Toolkit/Data/NSIDC/Data/SIC/nh_monthly",
                                       year_rng = 1988:2018,
                                       month_rng = 4:6,
                                       summary_func = "yearly",
                                       var_names = "seaice_conc_monthly_cdr",
                                       export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Summary output",
                                       big_data = c("D:/ClimMap_Toolkit/Satellite Processing Results/Big Data", 7000, 140000, 3),
                                       mode = "summary")[[1]] #Only the first element (summarised data) is extracted!
```

The Chla data is summarised in a similar manner, but for April-May of 2003-2018. However, NOTE the changed `subset_order` argument to reflect the format of netCDF filenames (which are now yyyyddd instead of yyyymm). Additionally, we are only interested in higher latitudes, so `coord_subset` prevents extraction of data between the equator and 30°N.

```r
#Extract and summarize April-May Chla data from 2003-2018.
satlist[["Chla"]] <- clim_summary(repository = "SeaWifs_MODISA",
                                  data_type = "Chla",
                                  frequency = "monthly",
                                  nc_path = "D:/ClimMap_Toolkit/Data/SeaWifs_MODISA/Data/Chla/monthly",
                                  year_rng = 2003:2018,
                                  month_rng = 4:5,
                                  subset_order = c("year", "day", ""),
                                  summary_func = "yearly",
                                  coord_subset = c(30, 90, -180, 180),
                                  export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Summary output",
                                  big_data = c("D:/ClimMap_Toolkit/Satellite Processing Results/Big Data", 7000, 1000000, 3),
                                  mode = "summary")[[1]]

```
<br/><br/>
### Plotting interannual climatologies
Below, SIC and Chla maps are created using the April-June (1988-2018) and April-May (2003-2018) climatologies, respectively, that we obtained from `clim_summary`. This is done via [Ocean Data View](https://odv.awi.de), as shown below for SIC (April-June 1988-2018) and Chla (April-May 2003-2018) data:
![Image3](https://i.ibb.co/6tMVBqv/SIC-ODV.png)
![Image4](https://i.ibb.co/MMV70vs/Chla-ODV.png)
<br/><br/>
### Spatially averaging data and determining interannual evolution
SIC and Chla data will be spatially averaged for various regions of the Arctic Ocean using the `clim_region` function. The code below uses the `heat_rank` as a `plot_type` to plot interannual evolution/progression of the data. Abbreviated labels are provided for both regions (defined by polygon .SHP files) and timesteps (years, in this case) via the `bar_varlabs` and `sat_varlabs` arguments, respectively. Not providing these may result in unwieldy labels in the output plots, depending on column names of `sat_data`. The argument `sat_vars` is used here to exclude irrelevant column names from `sat_data` which are not meant to be spatially averaged, e.g. coordinates and SD values. Both .PDF and .CSV output files can be found at the `export_path`. Note that less Arctic regions are used for spatial averaging in case of Chla data, which is scarcely available in some heavily ice-covered Arctic shelf areas.

```r
#Creating a list to store data
climreg_list <- list()

#Spatially average April-June SIC data and determine record highs/lows throughout 1988-2018.
climreg_list[["SIC"]] <- clim_region(core_dir = "D:/ClimMap_Toolkit",
                                     poly_path = "D:/ClimMap_Toolkit/Mapping/regions/AO_MASIE",
                                     sat_data = satlist[[1]],
                                     sat_vars = colnames(satlist[[1]])[-grep("xcoord|ycoord|Longitude|Latitude|Mean.of.*1988.2018|SD.of", colnames(satlist[[1]]))],
                                     sat_varlabs = 1988:2018,
                                     bar_varlabs = c("BB", "BS", "BeaS", "BerS", "CAA", "CA", "CS", "ESS", "GS", "HB", "KS", "LS", "SofO"),
                                     proj_final = "ONP",
                                     coord_sub = c(0.6, 0.5, 0.5, 0.7),
                                     plot_type = "heat_rank",
                                     plot_by = "varib",
                                     facet_plots = "summarise",
                                     print_plots = FALSE,
                                     plot_oob = FALSE,
                                     export_plots = "pdf",
                                     plot_opts = c(45, -0.2, 12),
                                     width=15,
                                     export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Region output/SIC")
                                     
#Spatially average April-May Chla data and determine record highs/lows throughout 2003-2018.
climreg_list[["Chla"]] <- clim_region(core_dir = "D:/ClimMap_Toolkit",
                                      poly_path = "D:/ClimMap_Toolkit/Mapping/regions/AO_MASIE",
                                      poly_list = c("Baffin_Bay_St_Lawrence", "Barents_Sea", "Bering_Sea_IHO", "Chukchi_Sea", "Greenland_Sea", "Sea_of_Okhotsk"),
                                      sat_data = satlist[[2]],
                                      sat_vars = colnames(satlist[[2]])[-grep("Longitude|Latitude|Mean.of.*2003.2018|SD.of", colnames(satlist[[2]]))],
                                      sat_varlabs = 2003:2018,
                                      bar_varlabs = c("BB", "BS", "BeaS", "BerS", "CS", "GS", "SofO"),
                                      proj_final = "ONP",
                                      coord_sub = c(0.6, 0.5, 0.5, 0.7),
                                      plot_type = "heat_rank",
                                      plot_by = "varib",
                                      facet_plots = "summarise",
                                      print_plots = FALSE,
                                      plot_oob = FALSE,
                                      export_plots = "pdf",
                                      plot_opts = c(45, -0.2, 12),
                                      width=15,
                                      export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Region output/Chla")
```
Examples of `clim_region` output below illustrate the regions identified from polygon shapefiles, and how April-June SIC changed in these regions throughout 1988-2018. In this case, values highlighted in white and blue denote record highs and lows, respectively.
![Image5](https://i.ibb.co/QNQDp5B/CLIM-Regional-2019-11-14-19hr-16min-Page-2.png)
![Image6](https://i.ibb.co/vsXp0q8/CLIM-Regional-2019-11-14-19hr-16min-Page-3.png)
<br></br>
### Determining values at point locations
We will use `clim_locate` to derive, bilinearly, SIC and Chla values at several surface sediment locations. Note that **it is necessary to check whether all point locations are covered by satellite data**. Otherwise, the function returns `NA` for locations where no satellite data is available! As always, the output data is located at `export_path`.

```r
#SIC data
clim_locate(sat_data = satlist[[1]],
            point_data = "D:/ClimMap_Toolkit/Example/Input Data/Example_Data.csv",
            sat_vars = colnames(satlist[[1]])[-grep("xcoord|ycoord|Longitude|Latitude|SD.of", colnames(satlist[[1]]))],
            proj_orig="SNP",
            export_path="D:/ClimMap_Toolkit/Example/Example 1/Clim_Locate output")

#Chla data
clim_locate(sat_data = satlist[[2]],
            point_data = "D:/ClimMap_Toolkit/Example/Input Data/Example_Data.csv",
            sat_vars = colnames(satlist[[2]])[-grep("xcoord|ycoord|Longitude|Latitude|SD.of", colnames(satlist[[2]]))],
            proj_orig="WGS84",
            export_path="D:/ClimMap_Toolkit/Example/Example 1/Clim_Locate output")
```
<br></br>
## EXAMPLE 2: Working with daily Chla data
This exercise will present a way to derive daily satellite-derived Chla timeseries from netCDF/HDF files, and use this to obtain a record of relative (e.g. %) change in Chla (`clim_btrack`), as well as various bloom descriptors (bloom start, end, and duration; see the description and arguments of `clim_bloom`). **NOTE** that certain processing steps in this example are quite time-consuming (in the order of hours).
<br></br>
### Downloading daily Chla data
We will use `clim_download` as before:

```r
clim_download(repository = "MODIS_A",
              frequency = "daily",
              data_type = "Chla",
              year = 2003:2018,
              month = "all",
              res_path = "D:/ClimMap_Toolkit",
              suppr_msgs = TRUE,
              opt_down = TRUE,
              shared_folder = TRUE)
```
<br></br>
### Calculating and merging daily average climatologies for days (of year) 60-273
We will use `clim_summary` within a `for` loop for this purpose. There should then be 214 output .CSV files at `export_path` (one for each day). Days 60 (beginning of March) to 273 (end of September) were chosen since MODIS Aqua provides northern hemisphere coverage during (roughly) this period each year.

```r
for(i in 60:273) {
  print(paste0("Processing Chla data for day ", i, " of ", max(60:273), "..."))
daily_chla_res <- clim_summary(repository = "SeaWifs_MODISA",
                               data_type = "Chla",
                               frequency = "daily",
                               nc_path = "D:/ClimMap_Toolkit/Data/SeaWifs_MODISA/Data/Chla/daily",
                               year_rng = 2003:2018,
                               day_rng = i,
                               subset_order = c("year", "day", ""),
                               summary_func = "total",
                               coord_subset = c(59, 86, -10, 80),
                               export_path = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Summary output/Individual Daily Files",
                               big_data = c("D:/ClimMap_Toolkit/Satellite Processing Results/Big Data", 7000, 1000000, 3),
                               mode = "summary")
rm(daily_chla_res)
}
```
Finally, we will use `multMerge` to merge all 214 daily files into a single dataset by their coordinates, check the column order and export the result as a .CSV file.

```r
chla_list <- list()

chla_list[["Chla_Concentration"]] <- multMerge(mypath = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Summary output/Individual Daily Files",
                                               use_dt = TRUE)

#Make sure the columns are ordered ascendingly according to day of year
chla_vars <- chla_list[["Chla_Concentration"]][,grep("Mean.of", colnames(chla_list[["Chla_Concentration"]]))]
chla_meta <- chla_list[["Chla_Concentration"]][,grep("Longitude|Latitude", colnames(chla_list[["Chla_Concentration"]]))]
chla_vars <- chla_vars[, order(as.numeric(gsub(".*\\.([[:digit:]]{1,3}).$", "\\1", colnames(chla_vars))))]

chla_list[["Chla_Concentration"]] <- cbind.data.frame(chla_meta, chla_vars)

fwrite(chla_list[["Chla_Concentration"]], file = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Summary output/Final Aggregate/DAILY_Chla_mgm3_2003-2018.csv", na=NA)

rm(chla_meta)
rm(chla_vars)
```
<br></br>
### Deriving a spatial average daily Chla timeseries for the Barents Sea
We can use `clim_region` for this purpose as follows:

```r
daily_chla_climreg <- clim_region(core_dir = "D:/ClimMap_Toolkit",
                                  poly_path = "D:/ClimMap_Toolkit/Mapping/regions/AO_MASIE",
                                  poly_list = "Barents_Sea",
                                  sat_data = chla_list[["Chla_Concentration"]],
                                  sat_vars = colnames(chla_list[["Chla_Concentration"]])[-grep("Longitude|Latitude|SD.of", colnames(chla_list[["Chla_Concentration"]]))],
                                  sat_varlabs = paste("day", 60:273),
                                  bar_varlabs = "BS",
                                  proj_final = "ONP",
                                  coord_sub = c(0.6, 0.5, 0.5, 0.7),
                                  plot_type = "point",
                                  plot_by = "varib",
                                  facet_plots = "summarise",
                                  print_plots = FALSE,
                                  plot_oob = FALSE,
                                  export_plots = "pdf",
                                  plot_opts = c(45, -0.2, 12),
                                  width=20,
                                  export_path = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Region output")
```

An example of what the output may look like (once plotted) can be seen in **Figure 8a** here: [Belt, S.T., Smik, L., Köseoğlu, D., Knies, J., Husum, K. (2019), "A novel biomarker-based proxy of the spring phytoplankton bloom in Arctic and sub-arctic settings — HBI T<sub>25</sub>", *Quaternary Science Reviews* **523**, 115703](https://doi.org/10.1016/j.epsl.2019.06.038), and in the roughly formatted point plot exported by `clim_region`:
![Image7](https://i.ibb.co/0q7FxD4/CLIM-Regional-2019-11-15-20hr-25min-Page-3.png)
<br></br>
### Deriving a record of relative change in Chla
This can be done using `clim_btrack`. LOESS smoothing is not applied as a pre-processing step in this case, and the McKibben et al. (2012) method is used. The day range is specified to be 60:273 (as before), and differences are calculated between 8-daily averages (set by the `run_window` argument).

```r
chla_list[["Chla_RelChange"]] <- clim_btrack(data = chla_list[["Chla_Concentration"]],
                                             sat_vars = colnames(chla_list[["Chla_Concentration"]])[grep("Mean.of", colnames(chla_list[["Chla_Concentration"]]))],
                                             run_window = 8,
                                             method = "mckibben",
                                             times = c("days", 60:273),
                                             monthly_aggr = FALSE,
                                             export_path = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Btrack output",
                                             smoothing = NA)
```
<br></br>
### Calculating bloom descriptors from Chla data
Finally, `clim_bloom` is used below to calculate bloom start, end, and duration. The above Chla timeseries shows that high values in September and outliers in March may be erroneously identified as the peak bloom. Thus, the day range was cut down from 60:273 to 75:244 in this case, using the `data` argument.

```r
chla_list[["Chla_BloomDesc"]] <- clim_bloom(data = chla_list[["Chla_Concentration"]][,grep(paste0("Longitude|Latitude|Mean.of|", paste0("days", 75:244, collapse="|")), colnames(chla_list[["Chla_Concentration"]]))],
                                            bloom_dur = TRUE,
                                            export_path = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Bloom output")
```
This is what the bloom duration (in days) map looks like when plotted in Ocean Data View:
![Image7](https://i.ibb.co/gy56fVC/Default-1.png)
It is apparent that on the Barents Sea shelf, within and south of the Marginal Ice Zone, bloom duration is significantly shorter overall.
