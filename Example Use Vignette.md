# ClimMap Toolkit Vignette
This example use vignette showcases some of the functionality included in the ClimMap Toolkit, and depends on the user having [downloaded]() and [set up](https://github.com/Deniz-Koseoglu/ClimMap_Toolkit#getting-started) the latest release (currently v0.9) using provided instructions, complete with [dependencies](https://github.com/Deniz-Koseoglu/ClimMap_Toolkit#dependencies). While the toolkit may be unpacked to any directory of your choice, this vignette assumes that the "ClimMap_Toolkit" folder contained in the downloaded .ZIP archive was extracted to **D:/**.

Note that all initial function output presented herein is available in the extracted **"D:/ClimMap_Toolkit/Example/Vignette Outputs"** folder, while the code is summarised in the **"D:/ClimMap_Toolkit/ClimMap_Toolkit_v09_EXAMPLES.R"** file.
<br/><br/>
## EXAMPLE 1: Working with monthly data
In this example, [NOAA/NSIDC Sea Ice Concentration (SIC)]() and [MODIS Aqua chlorophyll-*a* (Chla)]() data will be downloaded, summarised to interannual and annual average climatologies, visualised on maps using the Orthographic North Polar (ONP) projection, spatially averaged for various Arctic regions defined by ESRI shapefiles. Finally, SIC and Chla concentrations will be determined for a set of point locations representing surface sediments.

### Downloading monthly SIC and Chla data
We will download data via `clim_download` as follows:

```r
#Make sure you have sourced the ClimMap_Toolkit functions!
source("D:/ClimMap_Toolkit/ClimMap_Toolkit_v09.R")

#SIC data
clim_download(repository = "NSIDC",
              hemisphere = "north",
              frequency = "monthly",
              data_type = "SIC",
              year = 1988:2018,
              month = "all",
              res_path = "D:/ClimMap_Toolkit",
              suppr_msgs = TRUE,
              opt_down = TRUE)

#Chla data
clim_download(repository = "MODIS_A",
              frequency = "monthly",
              data_type = "Chla",
              year = 2003:2018,
              month = "all",
              res_path = "D:/ClimMap_Toolkit",
              suppr_msgs = TRUE,
              opt_down = TRUE,
              shared_folder = TRUE)

```

The files should appear in **D:/ClimMap_Toolkit/Data/NSIDC/Data/SIC/nh_monthly** (SIC) and **D:/ClimMap_Toolkit/Data/SeaWifs_MODISA/Data/Chla/monthly** (Chla).
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
                                  big_data = c("D:/ClimMap_Toolkit/Satellite Processing Results/Big Data", 7000, 140000, 3),
                                  mode = "summary")[[1]]

```
<br/><br/>
### Plotting interannual climatologies
Below, SIC and Chla maps are created using the April-June (1988-2018) and April-May (2003-2018) climatologies, respectively, that we obtained from `clim_summary`. Below, we specify `satlist[[1]]` (i.e. the first element) to obtain plot data from, and further narrow this down to a single climatology using the `sat_vars` argument. Three contours at 1%, 15%, and 50% SIC are defined in `sat_contours`, but these are not exported, as defined by the `export_results` argument. We also refrain from plotting graticules (`grat`), and exclude SIC values of 0 from the maps (`sat_sub`). Having specified the original (`proj_orig`) and final (`proj_final`) map projections as Stereographic North Polar (SNP) and Orthographic North Polar (ONP), respectively, we also zoom in on the Arctic Ocean using the `coord_sub` argument. As before, output files (.PDF maps, in this case) can be found at `export_path`.

```r
#Determine the name of SIC/Chla columns containing interannual average values (this is what we want to plot!)
meancol_SIC <- colnames(satlist[[1]])[grep("Mean.of.*1988.2018", colnames(satlist[[1]]))]
meancol_Chla <- colnames(satlist[[2]])[grep("Mean.of.*2003.2018", colnames(satlist[[2]]))]

#Create list to store Clim_Plot output within the R environment
maplist <- list()

#Visualize April-June (1988-2018) SIC data
maplist[["SIC"]] <- clim_plot(core_dir = "D:/ClimMap_Toolkit",
                              sat_data = satlist[[1]],
                              coord_vars = c("Longitude", "Latitude"),
                              sat_vars = meancol_SIC,
                              sat_varlabs = "Mean of April-June SIC (1988-2018)",
                              sat_contours = c(1, 15, 50),
                              sat_breaks = seq(0, 100, 20),
                              sat_lims = c(0, 100),
                              proj_orig = "SNP",
                              proj_final = "ONP",
                              leg_labs = c("SIC (%)", "Contours"),
                              sat_sub = c(0, "transparent"),
                              grat = list("none", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7),
                              coord_sub = c(0.6, 0.5, 0.5, 0.7),
                              export_results = "plots",
                              print_plots = "none",
                              export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Plot output")
```

The Chla data map follows a similar procedure to that above, but contours are not plotted and the plotted value range is adjusted to prevent outliers from defining the scale. Thus, `sat_values` is set to colour-code values within the `"IQR"` (Inter-Quartile Range), and only Chla concentration between 0 and 10 mg m<sup>-3</sup> are plotted, as defined by `sat_lims` and `sat_breaks`.
```r
maplist[["Chla"]] <- clim_plot(core_dir = "D:/ClimMap_Toolkit",
                               sat_data = satlist[[2]],
                               coord_vars = c("Longitude", "Latitude"),
                               sat_vars = meancol_Chla,
                               plot_cols = "MODIS",
                               sat_varlabs = "Mean of April-May Chla (2003-2018)",
                               sat_contours = NULL,
                               sat_lims = c(0, 10),
                               sat_breaks = seq(0, 10, 2),
                               sat_values = "IQR",
                               proj_orig = "WGS84",
                               proj_final = "ONP",
                               leg_labs = c("Chla (mg/m^3)", "Contours"),
                               grat = list("none", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7),
                               coord_sub = c(0.6, 0.5, 0.5, 0.7),
                               export_results = "plots",
                               print_plots = "none",
                               export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Plot output")
```
The above `clim_plot` code produces these SIC and Chla maps:
![Image1](https://i.ibb.co/HxVYMDD/CLIM-Plots-2019-11-14-15hr-31min-35sec.png)
![Image2](https://i.ibb.co/Ks82L2s/CLIM-Plots-2019-11-14-17hr-32min-11sec.png)

Note that data exported from `clim_summary` may just as easily be plotted using dedicated software, such as [Ocean Data View](https://odv.awi.de), as shown below:
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
![Image5](https://i.ibb.co/St7m0x9/CLIM-Regional-2019-11-14-19hr-16min-Page-2.png)
![Image6](https://i.ibb.co/DgSWYgj/CLIM-Regional-2019-11-14-19hr-16min-Page-3.png)
<br/><br/>
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