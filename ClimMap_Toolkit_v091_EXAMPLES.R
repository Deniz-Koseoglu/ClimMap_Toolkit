#ClimMap Toolkit v0.9.1 GitHub EXAMPLES
#Author: Deniz Can Koseoglu
#Date: 11.06.2020

#Source ClimMap Toolkit v0.9.1
source("D:/ClimMap_Toolkit/ClimMap_Toolkit_v091.R")

#~~~~~~~~~
#EXAMPLE 1
#~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 1: Download monthly NSIDC SIC data from 1988-2018 and MODIS Aqua chlorophyll-a (Chla) data from 2003-2018
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#Chla data (Earth Data Login information required!)
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 2: Create summaries of Arctic SIC and Chla data for April-June and April-May, respectively
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#Extract and summarize April-May Chla data from 2003-2018
satlist[["Chla"]] <- clim_summary(repository = "SeaWifs_MODISA",
                                  data_type = "Chla",
                                  frequency = "monthly",
                                  nc_path = "D:/ClimMap_Toolkit/Data/SeaWifs_MODISA/Data/Chla/monthly",
                                  year_rng = 2003:2018,
                                  month_rng = 4:5,
                                  subset_order = c("year", "day", ""),
                                  summary_func = "yearly",
                                  coord_subset = c(40, 90, -180, 180),
                                  export_path = "D:/ClimMap_Toolkit/Example/Example 1/Clim_Summary output",
                                  big_data = c("D:/ClimMap_Toolkit/Satellite Processing Results/Big Data", 7000, 1000000, 3),
                                  mode = "summary")[[1]]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 3: Map Arctic SIC and Chla data for April-June and April-May, respectively
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#Visualize April-May (2003-2018) Chla data
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 4: Spatially aggregate data and derive record low and high values from annual averages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 5: Determine values of SIC and Chla data at specific point locations (representing surface sediment sampling sites in this case)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


#~~~~~~~~~
#EXAMPLE 2
#~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 1: Download daily MODIS Aqua Chla data. WARNING: Requies ca. 26 GB of free space!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_download(repository = "MODIS_A",
              frequency = "daily",
              data_type = "Chla",
              year = 2003:2018,
              month = "all",
              res_path = "D:/ClimMap_Toolkit",
              usrname="dckoseoglu",
              pword="Example_Pword01",
              suppr_msgs = TRUE,
              opt_down = TRUE,
              shared_folder = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 2: Derive daily Chla data averaged over the 2003-2018 period
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 3: Merge daily Chla files into one (and export to disk)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 4: Spatially aggregate daily Chla data for the Barents Sea
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 5: Derive relative change in daily Chla using the McKibben et al. (2012) method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chla_list[["Chla_RelChange"]] <- clim_btrack(data = chla_list[["Chla_Concentration"]],
                                             sat_vars = colnames(chla_list[["Chla_Concentration"]])[grep("Mean.of", colnames(chla_list[["Chla_Concentration"]]))],
                                             run_window = 8,
                                             method = "mckibben",
                                             times = c("days", 60:273),
                                             monthly_aggr = FALSE,
                                             export_path = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Btrack output",
                                             smoothing = NA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#STEP 6: Derive bloom descriptors of daily Chla concentration timeseries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chla_list[["Chla_BloomDesc"]] <- clim_bloom(data = chla_list[["Chla_Concentration"]][,grep(paste0("Longitude|Latitude|Mean.of|", paste0("days", 75:244, collapse="|")), colnames(chla_list[["Chla_Concentration"]]))],
                                            bloom_dur = TRUE,
                                            export_path = "D:/ClimMap_Toolkit/Example/Example 2/Clim_Bloom output")
