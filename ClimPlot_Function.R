#install.packages("marmap")
#install.packages("ggalt")
#install.packages("akima")
#install.packages("interp")
#install.packages("rgeos")

#POTENTIAL FUTURE CHALLENGES!!!
#The "tidy" function from package "broom" now returns tibbles (ver. 0.5.0)... 
#last version returning data.frames was 0.4.5 --> KEEP IT IF YOU DON'T WANT WARNINGS...

clim_plot <- function(core_dir = getwd(), sat_data, point_data=NA, point_vars=NA, plot_aes = list(c("fill", "size"), NULL), pie_plot=c(FALSE, 1, 0.005),
                      separ = ",", coord_vars = c("Longitude", "Latitude"), sat_vars, sat_varlabs=sat_vars, scale_alpha=c(0.3, 1), scale_symbol=c(21:25), scale_size="default",
                      sat_sub = list(NULL, "transparent"), sat_rasterize = TRUE, rast_res = c(NA, NA, 3),
                      rast_type = "raster", sat_contours=NULL, satcont_type = c("lines",  1, 1.2), plot_cols=rep(list("default"),10),
                      proj_orig, proj_init="WGS84", proj_final, bathy_res="none",
                      bathy_contours=NULL, coast_res=c("low", "ifb"), bathy_sub = c(-6000:0), coord_sub=NULL, sat_values = "default",
                      sat_breaks="default", break_num=5, sat_lims="default", scale_opts=c(0, 3, FALSE, FALSE), size_lims = c("default", FALSE), leg_labs=waiver(), x_lab="Longitude", y_lab="Latitude", 
                      grat=list("WGS84", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7), export_results="all", print_plots="all",
                      export_path, width=10, height=10, point_size=12) {
  
  #Install and load the required libraries
  list.of.packages <- c("RColorBrewer", "ggplot2", "ggmap", "ggalt", "rgdal", "maptools", "maps", "mapdata", "raster", "scatterpie", "cowplot",
                        "rgeos", "sp", "sf", "broom", "marmap", "data.table", "animation")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  library(RColorBrewer)
  library(ggplot2)
  library(ggmap)
  library(ggalt)
  library(rgdal)
  library(maptools)
  library(maps)
  library(mapdata)
  library(raster)
  library(scatterpie)
  library(cowplot)
  library(rgeos)
  library(sp)
  library(sf)
  library(broom)
  library(marmap)
  library(data.table)
  library(animation)
  
  #PREP STAGE: Sorting and assigning projections
  proj_ids <- c("ONP", "LANP", "SNP", "AENP", "OSP", "SSP", "SSP_alt", "LASP", "WGS84")
  proj_defs <- c("+proj=ortho +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                 "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs +init=epsg:3408",
                 "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs +init=epsg:3411",
                 "+proj=aeqd +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                 "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +units=m +no_defs",
                 "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs +init=epsg:3031",
                 "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +init=epsg:3031",
                 "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                 "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs")
  
  proj_mapped <- setNames(proj_defs, proj_ids)
  
  if(any(proj_ids %in% proj_orig)) {
    
    proj_orig <- proj_mapped[[proj_orig]]
    
  } else if(!any(proj_ids %in% proj_orig) & !is.character(proj_orig)) {
    print("Projection was not provided in the correct format! Switching to the default: Stereographic North Polar (EPSG 3411)...")
    proj_orig <- proj_defs[3]
  }
  
  if(any(proj_ids %in% proj_init)) {
    
    proj_init <- proj_mapped[[proj_init]]
    
  } else if(!any(proj_ids %in% proj_init) & !is.character(proj_init)) {
    print("Projection was not provided in the correct format! Switching to the default: WGS84 (EPSG 4326)...")
    proj_init <- proj_defs[9]
  }
  
  if(any(proj_ids %in% proj_final)) {
    
    proj_final <- proj_mapped[[proj_final]]
    
  } else if(!any(proj_ids %in% proj_final) & !is.character(proj_final)) {
    print("Projection was not provided in the correct format! Switching to the default: Orthographic North Polar...")
    proj_final <- proj_defs[1]
  }
  
  #PREP STAGE: Running error checks
  plot_cols <- c(plot_cols, as.list(c(rep("default", 10-length(plot_cols)))))
  
  if(length(plot_cols[[4]])>1 | length(plot_cols[[5]])>1 | length(plot_cols[[6]])>1 | length(plot_cols[[8]])>1 | length(plot_cols[[9]])>1 | length(plot_cols[[10]])>1) {
    print("The 4th to 10th elements of plot_cols may only be of length 1 (excl. 7). Switching to default colours for offending elements!")
    
    which_len <- which(lengths(plot_cols)>1)
    which_len <- which_len[which_len %in% c(4:6,8:10)]
    
    print(paste0("Default colours were used for these plot_cols elements: ", paste(which_len, collapse=","), "!"))
    
    for(i in which_len) {
      plot_cols[[i]] <- "default"
    }
  }
  
  if(any(grepl("epsg:3411|epsg:3031", proj_init)) & bathy_res!="none" | any(grepl("epsg:3411|epsg:3031", proj_final)) & bathy_res!="none") {
    print("Bathymetry plotting is not currently supported with NSIDC stereographic projections! Arguments bathy_res and bathy_contours will be ignored.")
    #Bathymetry data cannot be reprojected using EPSG:3411 nor EPSG:3031... Remove the bathymetry option when proj_str contains either.
    bathy_res <- "none"
  }
  
  if(!any(satcont_type[1] %in% c("filled", "lines")) & satcont_type[1]!="none") {
    print("Unsupported values presented for sat_contours! Changed to default contour lines.")
    
    satcont_type[1] <- "lines"
  }
  
  if(length(satcont_type)<3 & any(satcont_type[1] %in% c("filled", "lines"))) {
    print("Assigning default values for satellite contours linetype and width (length of satcont_type is less than 3)...")
    satcont_type[2] <- 1
    satcont_type[3] <- 1.5
  }
  
  if(!is.null(sat_contours) & !isTRUE(sat_rasterize)) {
    print("Contours can only be calculated for rasterised/regularly gridded data! The sat_contours argument will be ignored.")
    
    sat_contours <- NULL
  }
  
  if(!coast_res[1] %in% c("crude", "low", "medium", "high", "full") & coast_res[1]!="none") {
    print("Unsupported value of coast_res! Please use one of: crude, low, medium, high, full, or none. Switching to medium as default...")
    coast_res[1] <- "medium"
  }
  
  if(length(coast_res)==1 | !coast_res[2] %in% c("ifb", "gl", "none")) {
    print("The second element of coast_res was not provided or incorrect... Switching to the default value for SO coastlines (Ice Front Boundary-based!")
    coast_res[2] <- "ifb"
  }
  
  if(coast_res[1]=="none" & grat[[1]]!="none") {
    print("Graticule plotting is only supported when coastlines are plotted! Please adjust coast_res...")
    grat[[1]] <- "none"
  }
  
  if(length(grat)<7) {
    print("Graticule aesthetic list (grat) is incomplete (of insufficient length)... Adding default values.")
  def_gratvals <- list("WGS84", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7, 5)
  
  grat <- c(grat, as.list(c(rep("default", 7-length(grat)))))
  
  if(any(grat=="default")) {
    which_grats <- which(grat=="default")
    
    for(i in which_grats) {
      grat[[i]] <- def_gratvals[[i]]
    }
  }
  }

  if(grepl("epsg:3411|epsg:3408|+proj=aeqd", proj_final)) {
    print("The SO coastlines will not be plotted due to the NSIDC North Polar Stereographic projection being used as proj_final!")
    coast_res[2] <- "none"
  }
  
  if(bathy_res!="none" & !all(c(sat_lims, sat_values, sat_breaks) %in% "default")) {
    print("Scales options were all set to default values since bathymetry is enabled!")
    sat_lims <- sat_breaks <- sat_values <- "default"
  }
  
  if(!any(c("default", "IQR", "quartile") %in% sat_lims) & !is.numeric(sat_lims)) {
    print("Scale limits (sat_lims) argument not in correct format! Defaulting to data range...")
    sat_lims <- "default"
  }
  
  if(!any(c("default", "IQR", "quartile") %in% sat_values) & !is.numeric(sat_lims)) {
    print("Scale limits (sat_lims) argument not in correct format! Defaulting to data range...")
    sat_values <- "default"
  }
  
  if(!any(c("default", "IQR", "quartile") %in% sat_breaks) & !is.numeric(sat_lims)) {
    print("Scale limits (sat_lims) argument not in correct format! Defaulting to data range...")
    sat_breaks <- "default"
  }
  
  if(pie_plot[2]<=0.5 & pie_plot[2]>0) {
    print("Values of pie_plot[2] less than or equal to 0.5 are not allowed! changing to the minimum of 0.51...")
    pie_plot[2] <- 0.51
  }
  
  if(any(is.na(sat_varlabs)) & !any(is.character(sat_varlabs))) {
    print("Using variable names as graph titles since a vector of labels was not provided in sat_varlabs...")
    sat_varlabs <- sat_vars
  }
  
  
  # PREP STAGE: Sorting and assigning plot colours
  defcol_list <- list(c("#093C70", "#137AE3", "#1684EB", "#178CF2", "#1994F9", "#1A9BFC", "#23A3FC", "#31ABFC", 
                        "#45B4FC", "#57BCFC", "#6AC4FC", "#7DCCFD", "#94D5FD", "#A8DCFD", "#BCE4FE", "#D0ECFE", "#E4F4FE", "#F7FCFF"),
                      c("#00008Bff", "#1E90FFff", "#1EFAA0ff", "#228B22ff", "#00FA00ff", "#7DFA00ff", "#ADFF2Fff", 
                        "#FAFA00ff", "#FA0000ff", "#78005Aff"),
                      c("#053061", "#4F4F4F", "#1D5FA2", "#1D5FA2", "#2B73B3", "#4F9BC7", "#71B0D3", "#93C6DE", "#B1D5E7", "#CCE2EF", 
                        "#DEEBF2", "#F8F1ED", "#FBE5D8", "#FCD7C2", "#F8BFA4", "#F4A683", "#E8896C", "#DB6B55", "#CC4C44", "#A81529", "#870A24"),
                      c("purple", "#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                      c("#d16cfa", "#7139fe", "#0db5e6", "#27ab19", "#ffc000", "#d31f2a", "#feb483"),
                      c("#9881F3", "#9286F2", "#8B89F4", "#8693F1", "#809AEE", "#8BA9E9", "#99B9E2", 
                        "#A1C6E0", "#A4D0D9", "#B3D3D2","#BED3CC", "#D2D2C6", "#D9D1C6", "#D6CABE", "#D0BEB0"),
                      c("#000000", "#000413", "#000728", "#002650", "#005E8C","#0096C8", "#45BCBB", "#8AE2AE", 
                        "#BCF8B9", "#DBFBDC"),
                      c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"),
                      c("#E91E24", "#F04F9D", "#F38EBA", "#F7BBD4", "#8DD5E3", "#63B7E6", "#4375BA", "#394FA4"))
  
  defcol_ids <- c("SIC", "SIC_heat", "SIC_anom", "MODIS", "ODV", "ODV_bathy", "Alter_bathy", "RYG", "Melt_Trend")
  
  for(i in seq_along(defcol_ids)) {
    if(any(plot_cols==defcol_ids[i])) { plot_cols[[which(plot_cols==defcol_ids[i])]] <- defcol_list[[i]] }
  }
  
  def_colvals <- list(defcol_list[[1]],
                      c("red", "orange", "green", "grey", "purple", "yellow", "white", "coral4", "deeppink", "slategray1", "lightblue"),
                      defcol_list[[6]],
                      "#093C70", "#AD9284", "black", 
                      c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"),
                      "grey60", "#A4D0D9", "white")
  
  if(any(plot_cols=="default")) {
    which_cols <- which(plot_cols=="default")
    
    for(i in which_cols) {
      plot_cols[[i]] <- def_colvals[[i]]
    }
  }
  
  #STAGE 1: Importing and processing the bathymetry file, checking for reprojection problems
  #SET UP A LIST TO COLLECT ALL GGPLOT LAYERS!
  Layer_List <- list()
  
  if(bathy_res!="none") {
    bathy_map <- as.data.frame(data.table::fread(paste0(core_dir, "/Mapping/bathymetry/ETOPO_1min/", bathy_res, ".csv")))
    colnames(bathy_map) <- c("Longitude", "Latitude", "Depth")
    z_bathy <- paste(colnames(bathy_map)[3])
    
    
    bathy_raster <- rasterFromXYZ(as.matrix(bathy_map), crs="+init=epsg:4326 +proj=longlat +datum=WGS84 +nodefs")
    
    if(!is.na(rast_res[2])) {
      bathy_raster <- try(projectRaster(bathy_raster, crs=proj_final, res=rast_res[2]), silent=TRUE)
    } else if(is.na(rast_res[2])) {
      bathy_raster <- try(projectRaster(bathy_raster, crs=proj_final), silent=TRUE)
    }
    
    
    if(inherits(bathy_raster, "try-error")) {
      
      print("Bathymetry file could not be rasterised and will not be plotted!")
      warning("Bathymetry was not plotted due to rasterisation failure...")
      bathy_res <- "none"
      
    } else if(!inherits(bathy_raster, "try-error")) {
      
      #bathy_map_coord.old <- SpatialPoints(bathy_map[,coord_vars], proj4string = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs"))
      #bathy_map_coord.new <- spTransform(bathy_map_coord.old, CRS(proj_final))
      #bathy_map <- as.data.frame(do.call(cbind, list(coordinates(bathy_map_coord.new), bathy_map[,"Depth"])))
      bathy_map <- as.data.frame(bathy_raster, xy=TRUE)
      colnames(bathy_map) <- c(coord_vars[1], coord_vars[2], "Depth")
      
      bathy_map[,z_bathy] <- as.integer(bathy_map[,z_bathy])
      
      if(!is.null(bathy_sub)) {
        bathy_map[!round(bathy_map[,z_bathy],0) %in% bathy_sub, z_bathy] <- NA
      }
      
      if(rast_type=="raster") {
        Layer_List[["B_Bathy_L"]] <- bathy_layer <- geom_raster(data=bathy_map, aes_string(fill=z_bathy), na.rm=TRUE)
      } else if(rast_type=="tile") {
        Layer_List[["B_Bathy_L"]] <- bathy_layer <- geom_tile(data=bathy_map, aes_string(fill=z_bathy), na.rm=TRUE)
      }
      
      if(!is.null(bathy_contours)) {
        Layer_List[["C_Bathycont_L"]] <- bathycont_layer <- geom_contour(data=bathy_map, aes_string(z=z_bathy), colour= plot_cols[[8]], na.rm=TRUE) 
      }
      
    }
  }
  
  #STAGE 2: Importing and processing the sat_data file
  if(is.object(sat_data)) {
    satmap <- sat_data[,c(coord_vars, sat_vars)]
  } else if(is.character(sat_data)) {
    satmap <- data.table::fread(sat_data, sep=separ, select=c(coord_vars, sat_vars), data.table = FALSE)
  }
  
  #Convert projection from proj_init to proj_final, while attempting rasterisation on both projections whilst sat_rasterize = TRUE!
  if(isTRUE(sat_rasterize)) {
    print("Converting satellite data to a raster...")
    
    if(!is.na(rast_res[1])) {
      satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap), crs=proj_init, res=rast_res[1], digits=rast_res[3]), silent=TRUE))
    } else if(is.na(rast_res[1])) {
      satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap), crs=proj_init, digits=rast_res[3]), silent=TRUE))
    }
    
    if(inherits(satmap_rast, "try-error")) {
      print("Failed to rasterize data via proj_init... Trying to force-convert to proj_orig, then rasterize!")
      satmap_coord.init <- SpatialPoints(satmap[,coord_vars], proj4string = CRS(proj_init))
      satmap_coord.orig <- spTransform(satmap_coord.init, CRS(proj_orig))
      satmap_origproj <- cbind(coordinates(satmap_coord.orig), satmap[,sat_vars])
      colnames(satmap_origproj) <- c(coord_vars, sat_vars)
      
      satmap_origproj[,coord_vars] <- round(satmap_origproj[,coord_vars], 5) #FOR SOME REASON THIS PREVENTS RASTERISATION FAILURES... WHY?
      
      if(!is.na(rast_res[2])) {
        satmap_rast <- satmap_rast2 <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_origproj), crs=proj_orig, res=rast_res[2], digits=rast_res[3]), silent=TRUE))
      } else if(is.na(rast_res[2])) {
        satmap_rast <- satmap_rast2 <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_origproj), crs=proj_orig, digits=rast_res[3]), silent=TRUE))
      }
    } else if(!inherits(satmap_rast, "try-error")) {
      
      satmap_rast2 <- satmap_rast
    }
    
    if(inherits(satmap_rast, "try-error")) {
      print("Failed to rasterize the satellite data using the initial projection (proj_init), most likely due to an irregular grid. Trying with proj_final...")
      
      satmap_coord.old <- SpatialPoints(satmap[,coord_vars], proj4string = CRS(proj_init))
      satmap_coord.new <- spTransform(satmap_coord.old, CRS(proj_final))
      satmap_newproj <- cbind(coordinates(satmap_coord.new), satmap[,sat_vars])
      colnames(satmap_newproj) <- c(coord_vars, sat_vars)
      
      satmap_newproj[,coord_vars] <- round(satmap_newproj[,coord_vars], 5) #TRICK TO AVOID WEIRD RASTER ERROR! Seems to work with anything below 6...
      #satmap_proj[is.nan(satmap_proj)] <- NA
      
      if(!is.na(rast_res[2])) {
        satmap_rast2 <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_newproj), crs=proj_final, res=rast_res[2], digits=rast_res[3]), silent=TRUE))
      } else if(is.na(rast_res[2])) {
        satmap_rast2 <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_newproj), crs=proj_final, digits=rast_res[3]), silent=TRUE))
      }
      
      
      if(!inherits(satmap_rast2, "try-error")) {
        satmap_final <- as.data.frame(satmap_rast2, xy=TRUE)
        colnames(satmap_final)[which(colnames(satmap_final) %in% c("x", "y"))] <- coord_vars
        print("Conversion to raster successful!")
        
        #Put together the satellite data ggplot layer and color palette!
        print("Compiling satellite data map layer, contours and colour palettes...")
        sat_layer <- list()
        sat_scales <- list()
        satcont_layer <- list()
        satcont_scale <- list()
        contour_obj <- list()
        contour_res <- list()
        
        for(i in seq_along(sat_vars)) {
          print(paste("Subsetting satellite data variable", sat_vars[i], "according to criteria..."))
          
          if(is.numeric(sat_sub[[1]]) & !is.null(sat_sub[[1]])) {
            satmap_final[round(satmap_final[,sat_vars[i]],0) %in% sat_sub[[1]], sat_vars[i]] <- NA
          }
          
          if(rast_type=="raster") {
            Layer_List[[paste0("D_Sat_L_", sat_vars[i])]] <- sat_layer[[sat_vars[i]]] <- geom_raster(data=satmap_final, aes_string(fill=sat_vars[i]))
          } else if(rast_type=="tile") {
            Layer_List[[paste0("D_Sat_L_", sat_vars[i])]] <- sat_layer[[sat_vars[i]]] <- geom_tile(data=satmap_final, aes_string(fill=sat_vars[i]))
          }
          
          #Contours layer
          if(!is.null(sat_contours) & is.numeric(sat_contours) & satcont_type[1]!="none") {
            if(satcont_type[1]=="lines") {
              
              if(length(plot_cols[[7]])==1) {
                sat_scalevar <- sat_vars[i]
                Layer_List[[paste0("F_Satcont_L_", sat_vars[i])]] <- satcont_layer[[sat_vars[i]]] <- geom_contour(data=satmap_final, aes_string(z=sat_scalevar), colour=plot_cols[[7]], breaks=sat_contours, na.rm = TRUE, linetype=as.numeric(satcont_type[2]), size=as.numeric(satcont_type[3]))
              } else if(length(plot_cols[[7]])>1) {
                sat_scalevar <- sat_vars[i]
                Layer_List[[paste0("F_Satcont_L_", sat_vars[i])]] <- satcont_layer[[sat_vars[i]]] <- geom_contour(data=satmap_final, aes_string(z=sat_scalevar, colour="..level.."), breaks=sat_contours, na.rm = TRUE, linetype=as.numeric(satcont_type[2]), size=as.numeric(satcont_type[3]))
                Layer_List[[paste0("G_Satcont_SC_", sat_vars[i])]] <- satcont_scale[[sat_vars[i]]] <- scale_colour_gradientn(colours=plot_cols[[7]], name=leg_labs[2], breaks=sat_contours)
              }
              
            } else if(satcont_type[1]=="filled") {
              sat_scalevar <- sat_vars[i]
              Layer_List[[paste0("F_Satcont_L_", sat_vars[i])]] <- satcont_layer[[sat_vars[i]]] <- stat_contour(data=satmap_final, geom="polygon", aes_string(z=sat_scalevar, fill="..level.."), breaks=sat_contours, na.rm = TRUE) 
              
            }
          }
          
          #Drawing contours and confining them to an object for export
          if(!is.null(sat_contours) & !any(export_results %in% c("plots", "none"))) {
            
            print("Exporting contours set in sat_contours as ESRI shapefiles...")
            contour_obj[[sat_vars[i]]] <- rasterToContour(satmap_rast2[[sat_vars[i]]], levels=sat_contours)
            feature_num <- length(contour_obj[[sat_vars[i]]]["level"])
            
            for(j in 1:feature_num) {
              cont_lname <- paste(sat_vars[i], "; level", j)
              contour_res[[cont_lname]] <- spTransform(contour_obj[[sat_vars[i]]][j,], CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs"))
              writeOGR(obj=contour_res[[cont_lname]], dsn=export_path, layer=names(contour_res)[which(names(contour_res) %in% cont_lname)], driver="ESRI Shapefile")
            }
            
          }
          
          #NEW COLOUR PALETTE CODE!
          #Setting up values for manipulating the fill scale
          if(bathy_res=="none") {
            
            quart_vals <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[2:4])
            min_val <- min(na.omit(satmap_final[,sat_vars[i]]))
            quart1 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[2])
            quart2 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[3])
            quart3 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[4])
            iqr_val <- quart3 - quart1
            max_val <- max(na.omit(satmap_final[,sat_vars[i]]))
            
            #Setting up limits for fill scale
            if(any(sat_lims %in% "quartile")) {
              
              satscale_lims <- c(quart1, quart3)
              
            } else if(any(sat_lims %in% "IQR")) {
              
              satscale_lims <- c(quart1-iqr_val, quart3+iqr_val)
              if(scale_opts[3]==FALSE) {
                satscale_lims[satscale_lims<0] <- 0
              }
              
            } else if(any(sat_lims %in% "default")) {
              
              satscale_lims <- c(min_val, max_val)
              
            } else if(!any(sat_lims %in% c("default", "quartile", "IQR")) & is.numeric(sat_lims)) {
              
              satscale_lims <- sat_lims
            }
            
            #Setting up values for fill scale
            if(any(sat_values %in% "quartile")) {
              
              satscale_vals <- c(seq(quart1, quart3, length.out = length(plot_cols[[1]])-1), max(satscale_lims))
              
            } else if(any(sat_values %in% "IQR")) {
              
              satscale_vals <- c(seq(quart1-iqr_val, quart3, length.out = length(plot_cols[[1]])-2), quart3+iqr_val) #c(quart1-iqr_val, seq(quart1, quart3, length.out = length(plot_cols[[2]])-2), quart3+iqr_val) 
              
              if(scale_opts[3]==FALSE) {
                satscale_vals[satscale_vals<0] <- 0
              }
              
            } else if(any(sat_values %in% "default")) {
              
              satscale_vals <- seq(min(satscale_lims), max(satscale_lims), length.out=length(plot_cols[[2]]))
              
            } else if(!any(sat_values %in% c("default", "quartile", "IQR")) & is.numeric(sat_values)) {
              satscale_vals <- sat_values
            }
            
            #Setting up breaks for the fill scale
            if(any(sat_breaks %in% "quartile")) {
              
              satscale_breaks <- unique(c(min(satscale_lims), quart_vals, max(satscale_lims)))
              
            } else if(any(sat_breaks %in% "IQR")) {
              
              satscale_breaks <- unique(c(min(satscale_lims), quart1-iqr_val, quart_vals, quart3+iqr_val, max(satscale_lims)))
              
            } else if(any(sat_breaks %in% "default")) {
              
              satscale_breaks <- pretty(range(satscale_lims), n=break_num)
              
            } else if(!any(sat_breaks %in% c("default", "quartile", "IQR")) & is.numeric(sat_breaks)) {
              
              satscale_breaks <- sat_breaks
            }
            
            satscale_labs <- round(satscale_breaks, scale_opts[2])
            
            
          } else if(bathy_res!="none") {
            if(!all(c(sat_values, sat_breaks, sat_lims) %in% "default")) {
              print("Please note that transformed satellite data colour bar is only available when bathymetry is disabled!")
            }
            
            #Colour palette (depending on the value of bathy_res!)
            if(any(sat_values %in% c("default", "quartile", "IQR"))) {
              satscale_vals <- c(seq(min(na.omit(bathy_map[,z_bathy])), max(na.omit(bathy_map[,z_bathy])), length.out = length(plot_cols[[3]])), 0, seq(1e-32, max(na.omit(satmap_final[, sat_vars[i]])), length.out = length(plot_cols[[1]])))
            } else if(!any(sat_values %in% c("default", "quartile", "IQR"))) {
              satscale_vals <- sat_values
            }
            
            if(any(sat_breaks %in% c("default", "quartile", "IQR"))) {
              satscale_breaks <- c(seq(round(min(na.omit(bathy_map[,z_bathy])), 0), round(max(na.omit(bathy_map[,z_bathy])), 0), length.out = 4), 0, seq(round(min(na.omit(satmap_final[, sat_vars[i]])), scale_opts[1]), round(max(na.omit(satmap_final[, sat_vars[i]])), scale_opts[1]), length.out = 4))
            } else if(!any(sat_breaks %in% c("default", "quartile", "IQR"))) {
              satscale_breaks <- sat_breaks
            }
            
            if(any(sat_lims %in% "default") | any(sat_lims %in% "quartile") | any(sat_lims %in% "IQR")) {
              satscale_lims <- c(round(min(na.omit(bathy_map[,z_bathy])), 0), round(max(na.omit(satmap_final[, sat_vars[i]])), 0))
            } else if(!any(sat_lims %in% "default") | !any(sat_lims %in% "quartile") | !any(sat_lims %in% "IQR")) {
              satscale_lims <- sat_lims
            }
            satscale_labs <- round(satscale_breaks, scale_opts[2])
          }
          
          if(bathy_res!="none") {
            Layer_List[[paste0("E_Sat_SC_", sat_vars[i])]] <- sat_scales[[sat_vars[i]]] <- scale_fill_gradientn(colours=c(plot_cols[[3]], plot_cols[[1]]), values=scales::rescale(satscale_vals), breaks=satscale_breaks, name=leg_labs[1], na.value = sat_sub[[2]], limits=satscale_lims, labels=satscale_labs)
          } else if(bathy_res=="none") {
            Layer_List[[paste0("E_Sat_SC_", sat_vars[i])]] <- sat_scales[[sat_vars[i]]] <- scale_fill_gradientn(colours=plot_cols[[1]], values=scales::rescale(satscale_vals), breaks=satscale_breaks, name=leg_labs[1], na.value = sat_sub[[2]], limits=satscale_lims, labels=satscale_labs)
          }
          #END OF NEW COLOUR PALETTE CODE!
        }
      }
      
    } else if (!inherits(satmap_rast, "try-error")) {
      
      if(!is.na(rast_res[2])) {
        satmap_final <- projectRaster(satmap_rast, crs=proj_final, res=rast_res[2]) #Points not finite warning does NOT pose a problem for the data!
      } else if(is.na(rast_res[2])) {
        satmap_final <- projectRaster(satmap_rast, crs=proj_final)
      }
      
      satmap_final <- as.data.frame(satmap_final, xy=TRUE)
      colnames(satmap_final)[which(colnames(satmap_final) %in% c("x", "y"))] <- coord_vars
      print("Conversion to raster successful!")
      
      #Put together the satellite data ggplot layer and color palette!
      print("Compiling satellite data map layer, contours and colour palettes...")
      sat_layer <- list()
      sat_scales <- list()
      satcont_layer <- list()
      satcont_scale <- list()
      contour_obj <- list()
      contour_res <- list()
      
      for(i in seq_along(sat_vars)) {
        
        print(paste("Subsetting satellite data variable", sat_vars[i], "according to criteria..."))
        
        if(is.numeric(sat_sub[[1]]) & !is.null(sat_sub[[1]])) {
          satmap_final[round(satmap_final[,sat_vars[i]],0) %in% sat_sub[[1]], sat_vars[i]] <- NA
        }
        
        if(rast_type=="raster") {
          Layer_List[[paste0("D_Sat_L_", sat_vars[i])]] <- sat_layer[[sat_vars[i]]] <- geom_raster(data=satmap_final, aes_string(fill=sat_vars[i]))
        } else if(rast_type=="tile") {
          Layer_List[[paste0("D_Sat_L_", sat_vars[i])]] <- sat_layer[[sat_vars[i]]] <- geom_tile(data=satmap_final, aes_string(fill=sat_vars[i]))
        }
        
        #Contours layer
        if(!is.null(sat_contours) & is.numeric(sat_contours) & satcont_type[1]!="none") {
          if(satcont_type[1]=="lines") {
            
            if(length(plot_cols[[7]])==1) {
              sat_scalevar <- sat_vars[i]
              Layer_List[[paste0("F_Satcont_L_", sat_vars[i])]] <- satcont_layer[[sat_vars[i]]] <- geom_contour(data=satmap_final, aes_string(z=sat_scalevar), colour=plot_cols[[7]], breaks=sat_contours, na.rm = TRUE, linetype=as.numeric(satcont_type[2]), size=as.numeric(satcont_type[3]))
            } else if(length(plot_cols[[7]])>1) {
              sat_scalevar <- sat_vars[i]
              Layer_List[[paste0("F_Satcont_L_", sat_vars[i])]] <- satcont_layer[[sat_vars[i]]] <- geom_contour(data=satmap_final, aes_string(z=sat_scalevar, colour="..level.."), breaks=sat_contours, na.rm = TRUE, linetype=as.numeric(satcont_type[2]), size=as.numeric(satcont_type[3]))
              Layer_List[[paste0("G_Satcont_SC_", sat_vars[i])]] <- satcont_scale[[sat_vars[i]]] <- scale_colour_gradientn(colours=plot_cols[[7]], name=leg_labs[2], breaks=sat_contours)
            }
            
          } else if(satcont_type[1]=="filled") {
            sat_scalevar <- sat_vars[i]
            Layer_List[[paste0("F_Satcont_L_", sat_vars[i])]] <- satcont_layer[[sat_vars[i]]] <- stat_contour(data=satmap_final, geom="polygon", aes_string(z=sat_scalevar, fill="..level.."), breaks=sat_contours, na.rm = TRUE) 
            
          }
        }
        
        #Drawing contours and confining them to an object for export
        if(!is.null(sat_contours) & !any(export_results %in% c("plots", "none"))) {
        
        print("Exporting contours set in sat_contours as ESRI shapefiles...")
        contour_obj[[sat_vars[i]]] <- rasterToContour(satmap_rast[[sat_vars[i]]], levels=sat_contours)
        feature_num <- length(contour_obj[[sat_vars[i]]]["level"])
        
        for(j in 1:feature_num) {
          cont_lname <- paste(sat_vars[i], "; level", j)
          contour_res[[cont_lname]] <- spTransform(contour_obj[[sat_vars[i]]][j,], CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs"))
          writeOGR(obj=contour_res[[cont_lname]], dsn=export_path, layer=names(contour_res)[which(names(contour_res) %in% cont_lname)], driver="ESRI Shapefile")
        }
        }
        
        #NEW COLOUR PALETTE CODE!
        #Setting up values for manipulating the fill scale
        if(bathy_res=="none") {
          
          quart_vals <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[2:4])
          min_val <- min(na.omit(satmap_final[,sat_vars[i]]))
          quart1 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[2])
          quart2 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[3])
          quart3 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[4])
          iqr_val <- quart3 - quart1
          max_val <- max(na.omit(satmap_final[,sat_vars[i]]))
          
          #Setting up limits for fill scale
          if(any(sat_lims %in% "quartile")) {
            
            satscale_lims <- c(quart1, quart3)
            
          } else if(any(sat_lims %in% "IQR")) {
            
            satscale_lims <- c(quart1-iqr_val, quart3+iqr_val)
            if(scale_opts[3]==FALSE) {
              satscale_lims[satscale_lims<0] <- 0
            }
            
          } else if(any(sat_lims %in% "default")) {
            
            satscale_lims <- c(min_val, max_val)
            
          } else if(!any(sat_lims %in% c("default", "quartile", "IQR")) & is.numeric(sat_lims)) {
            
            satscale_lims <- sat_lims
          }
          
          #Setting up values for fill scale
          if(any(sat_values %in% "quartile")) {
            
            satscale_vals <- c(seq(quart1, quart3, length.out = length(plot_cols[[1]])-1), max(satscale_lims))
            
          } else if(any(sat_values %in% "IQR")) {
            
            satscale_vals <- c(seq(quart1-iqr_val, quart3, length.out = length(plot_cols[[1]])-2), quart3+iqr_val) #c(quart1-iqr_val, seq(quart1, quart3, length.out = length(plot_cols[[2]])-2), quart3+iqr_val) 
            
            if(scale_opts[3]==FALSE) {
              satscale_vals[satscale_vals<0] <- 0
            }
            
          } else if(any(sat_values %in% "default")) {
            
            satscale_vals <- seq(min(satscale_lims), max(satscale_lims), length.out=length(plot_cols[[2]]))
            
          } else if(!any(sat_values %in% c("default", "quartile", "IQR")) & is.numeric(sat_values)) {
            satscale_vals <- sat_values
          }
          
          #Setting up breaks for the fill scale
          if(any(sat_breaks %in% "quartile")) {
            
            satscale_breaks <- unique(c(min(satscale_lims), quart_vals, max(satscale_lims)))
            
          } else if(any(sat_breaks %in% "IQR")) {
            
            satscale_breaks <- unique(c(min(satscale_lims), quart1-iqr_val, quart_vals, quart3+iqr_val, max(satscale_lims)))
            
          } else if(any(sat_breaks %in% "default")) {
            
            satscale_breaks <- pretty(range(satscale_lims), n=break_num)
            
          } else if(!any(sat_breaks %in% c("default", "quartile", "IQR")) & is.numeric(sat_breaks)) {
            
            satscale_breaks <- sat_breaks
          }
          
          satscale_labs <- round(satscale_breaks, scale_opts[2])
          
          
        } else if(bathy_res!="none") {
          if(!all(c(sat_values, sat_breaks, sat_lims) %in% "default")) {
            print("Please note that transformed satellite data colour bar is only available when bathymetry is disabled!")
          }
          
          #Colour palette (depending on the value of bathy_res!)
          if(any(sat_values %in% c("default", "quartile", "IQR"))) {
            satscale_vals <- c(seq(min(na.omit(bathy_map[,z_bathy])), max(na.omit(bathy_map[,z_bathy])), length.out = length(plot_cols[[3]])), 0, seq(1e-32, max(na.omit(satmap_final[, sat_vars[i]])), length.out = length(plot_cols[[1]])))
          } else if(!any(sat_values %in% c("default", "quartile", "IQR"))) {
            satscale_vals <- sat_values
          }
          
          if(any(sat_breaks %in% c("default", "quartile", "IQR"))) {
            satscale_breaks <- c(seq(round(min(na.omit(bathy_map[,z_bathy])), 0), round(max(na.omit(bathy_map[,z_bathy])), 0), length.out = 4), 0, seq(round(min(na.omit(satmap_final[, sat_vars[i]])), scale_opts[1]), round(max(na.omit(satmap_final[, sat_vars[i]])), scale_opts[1]), length.out = 4))
          } else if(!any(sat_breaks %in% c("default", "quartile", "IQR"))) {
            satscale_breaks <- sat_breaks
          }
          
          if(any(sat_lims %in% "default") | any(sat_lims %in% "quartile") | any(sat_lims %in% "IQR")) {
            satscale_lims <- c(round(min(na.omit(bathy_map[,z_bathy])), 0), round(max(na.omit(satmap_final[, sat_vars[i]])), 0))
          } else if(!any(sat_lims %in% "default") | !any(sat_lims %in% "quartile") | !any(sat_lims %in% "IQR")) {
            satscale_lims <- sat_lims
          }
          satscale_labs <- round(satscale_breaks, scale_opts[2])
        }
        
        if(bathy_res!="none") {
          Layer_List[[paste0("E_Sat_SC_", sat_vars[i])]] <- sat_scales[[sat_vars[i]]] <- scale_fill_gradientn(colours=c(plot_cols[[3]], plot_cols[[1]]), values=scales::rescale(satscale_vals), breaks=satscale_breaks, name=leg_labs[1], na.value = sat_sub[[2]], limits=satscale_lims, labels=satscale_labs)
        } else if(bathy_res=="none") {
          Layer_List[[paste0("E_Sat_SC_", sat_vars[i])]] <- sat_scales[[sat_vars[i]]] <- scale_fill_gradientn(colours=plot_cols[[1]], values=scales::rescale(satscale_vals), breaks=satscale_breaks, name=leg_labs[1], na.value = sat_sub[[2]], limits=satscale_lims, labels=satscale_labs)
        }
        #END OF NEW COLOUR PALETTE CODE!
        
      }
    }
    
  } 
  
  if(!isTRUE(sat_rasterize) | inherits(satmap_rast2, "try-error")) {
    
    if(inherits(satmap_rast2, "try-error")) {
      print("Could not rasterize the satellite grid in either the initial or final projections... Cancelling rasterisation!")
      sat_contours <- NULL
    }
    
    satmap_coord.old <- SpatialPoints(satmap[,coord_vars], proj4string = CRS(proj_init))
    satmap_coord.new <- spTransform(satmap_coord.old, CRS(proj_final))
    satmap_final <- as.data.frame(do.call(cbind, list(coordinates(satmap_coord.new), satmap[,sat_vars])))
    colnames(satmap_final) <- c(coord_vars, sat_vars)
    
    #Put together the satellite data ggplot layer and color palette!
    print("Compiling satellite data map layer colour palettes...")
    sat_layer <- list()
    sat_scales <- list()
    
    for(i in seq_along(sat_vars)) {
      
      print(paste("Subsetting satellite data variable", sat_vars[i], "according to criteria..."))
      
      if(is.numeric(sat_sub[[1]]) & !is.null(sat_sub[[1]])) {
        satmap_final[round(satmap_final[,sat_vars[i]],0) %in% sat_sub[[1]], sat_vars[i]] <- NA
      }
      
      Layer_List[[paste0("D_Sat_L_", sat_vars[i])]] <- sat_layer[[sat_vars[i]]] <- geom_point(data=satmap_final, aes_string(fill=sat_vars[i]), pch=21, colour=alpha("white",0), stroke=0)
      
      #NEW COLOUR PALETTE CODE!
      #Setting up values for manipulating the fill scale
      if(bathy_res=="none") {
        
        quart_vals <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[2:4])
        min_val <- min(na.omit(satmap_final[,sat_vars[i]]))
        quart1 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[2])
        quart2 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[3])
        quart3 <- as.numeric(quantile(satmap_final[,sat_vars[i]], na.rm=TRUE)[4])
        iqr_val <- quart3 - quart1
        max_val <- max(na.omit(satmap_final[,sat_vars[i]]))
        
        #Setting up limits for fill scale
        if(any(sat_lims %in% "quartile")) {
          
          satscale_lims <- c(quart1, quart3)
          
        } else if(any(sat_lims %in% "IQR")) {
          
          satscale_lims <- c(quart1-iqr_val, quart3+iqr_val)
          if(scale_opts[3]==FALSE) {
            satscale_lims[satscale_lims<0] <- 0
          }
          
        } else if(any(sat_lims %in% "default")) {
          
          satscale_lims <- c(min_val, max_val)
          
        } else if(!any(sat_lims %in% c("default", "quartile", "IQR")) & is.numeric(sat_lims)) {
          
          satscale_lims <- sat_lims
        }
        
        #Setting up values for fill scale
        if(any(sat_values %in% "quartile")) {
          
          satscale_vals <- c(seq(quart1, quart3, length.out = length(plot_cols[[1]])-1), max(satscale_lims))
          
        } else if(any(sat_values %in% "IQR")) {
          
          satscale_vals <- c(seq(quart1-iqr_val, quart3, length.out = length(plot_cols[[1]])-2), quart3+iqr_val) #c(quart1-iqr_val, seq(quart1, quart3, length.out = length(plot_cols[[2]])-2), quart3+iqr_val) 
          
          if(scale_opts[3]==FALSE) {
            satscale_vals[satscale_vals<0] <- 0
          }
          
        } else if(any(sat_values %in% "default")) {
          
          satscale_vals <- seq(min(satscale_lims), max(satscale_lims), length.out=length(plot_cols[[2]]))
          
        } else if(!any(sat_values %in% c("default", "quartile", "IQR")) & is.numeric(sat_values)) {
          satscale_vals <- sat_values
        }
        
        #Setting up breaks for the fill scale
        if(any(sat_breaks %in% "quartile")) {
          
          satscale_breaks <- unique(c(min(satscale_lims), quart_vals, max(satscale_lims)))
          
        } else if(any(sat_breaks %in% "IQR")) {
          
          satscale_breaks <- unique(c(min(satscale_lims), quart1-iqr_val, quart_vals, quart3+iqr_val, max(satscale_lims)))
          
        } else if(any(sat_breaks %in% "default")) {
          
          satscale_breaks <- pretty(range(satscale_lims), n=break_num)
          
        } else if(!any(sat_breaks %in% c("default", "quartile", "IQR")) & is.numeric(sat_breaks)) {
          
          satscale_breaks <- sat_breaks
        }
        
        satscale_labs <- round(satscale_breaks, scale_opts[2])
        
        
      } else if(bathy_res!="none") {
        if(!all(c(sat_values, sat_breaks, sat_lims) %in% "default")) {
          print("Please note that transformed satellite data colour bar is only available when bathymetry is disabled!")
        }
        
        #Colour palette (depending on the value of bathy_res!)
        if(any(sat_values %in% c("default", "quartile", "IQR"))) {
          satscale_vals <- c(seq(min(na.omit(bathy_map[,z_bathy])), max(na.omit(bathy_map[,z_bathy])), length.out = length(plot_cols[[3]])), 0, seq(1e-32, max(na.omit(satmap_final[, sat_vars[i]])), length.out = length(plot_cols[[1]])))
        } else if(!any(sat_values %in% c("default", "quartile", "IQR"))) {
          satscale_vals <- sat_values
        }
        
        if(any(sat_breaks %in% c("default", "quartile", "IQR"))) {
          satscale_breaks <- c(seq(round(min(na.omit(bathy_map[,z_bathy])), 0), round(max(na.omit(bathy_map[,z_bathy])), 0), length.out = 4), 0, seq(round(min(na.omit(satmap_final[, sat_vars[i]])), scale_opts[1]), round(max(na.omit(satmap_final[, sat_vars[i]])), scale_opts[1]), length.out = 4))
        } else if(!any(sat_breaks %in% c("default", "quartile", "IQR"))) {
          satscale_breaks <- sat_breaks
        }
        
        if(any(sat_lims %in% "default") | any(sat_lims %in% "quartile") | any(sat_lims %in% "IQR")) {
          satscale_lims <- c(round(min(na.omit(bathy_map[,z_bathy])), 0), round(max(na.omit(satmap_final[, sat_vars[i]])), 0))
        } else if(!any(sat_lims %in% "default") | !any(sat_lims %in% "quartile") | !any(sat_lims %in% "IQR")) {
          satscale_lims <- sat_lims
        }
        satscale_labs <- round(satscale_breaks, scale_opts[2])
      }
      
      if(bathy_res!="none") {
        Layer_List[[paste0("E_Sat_SC_", sat_vars[i])]] <- sat_scales[[sat_vars[i]]] <- scale_fill_gradientn(colours=c(plot_cols[[3]], plot_cols[[1]]), values=scales::rescale(satscale_vals), breaks=satscale_breaks, name=leg_labs[1], na.value = sat_sub[[2]], limits=satscale_lims, labels=satscale_labs)
      } else if(bathy_res=="none") {
        Layer_List[[paste0("E_Sat_SC_", sat_vars[i])]] <- sat_scales[[sat_vars[i]]] <- scale_fill_gradientn(colours=plot_cols[[1]], values=scales::rescale(satscale_vals), breaks=satscale_breaks, name=leg_labs[1], na.value = sat_sub[[2]], limits=satscale_lims, labels=satscale_labs)
      }
      #END OF NEW COLOUR PALETTE CODE!
      
    }
  }
  
  #STAGE 3: Importing and processing the coastline/land files
  if(coast_res[1]!= "none") {
    print("Importing and processing coastlines...")
    
    if(coast_res[1]=="crude") {
      skeleton_link <- "/c/GSHHS_c_"
    } else if(coast_res[1]=="low") {
      skeleton_link <- "/l/GSHHS_l_"
    } else if(coast_res[1]=="medium") {
      skeleton_link <- "/i/GSHHS_i_"
    } else if(coast_res[1]=="high") {
      skeleton_link <- "/h/GSHHS_h_"
    } else if(coast_res[1]=="full") {
      skeleton_link <- "/f/GSHHS_f_"
    }
    
    #Importing and processing World coastlines (except SO)
    world_link <- paste0(core_dir, "/Mapping/coastlines/GSHHS/GSHHS_shp", skeleton_link, "L1.shp")
    world_data <- readOGR(world_link)
    #world_colines <- as.data.frame(tidy(world_data)) #After BROOM update 0.5.0 the default "tidy" now returns tibbles... and "fortify" is faster
    world_colines <- fortify(world_data)
    
    #Importing and processing SO coastlines (unless proj_final contains epsg:3411)
    if(coast_res[2]!="none") {
      
      if(coast_res[2]=="ifb") {
        SO_link <- paste0(core_dir, "/Mapping/coastlines/GSHHS/GSHHS_shp", skeleton_link, "L5.shp")
      } else if(coast_res[2]=="gl") {
        SO_link <- paste0(core_dir, "/Mapping/coastlines/GSHHS/GSHHS_shp", skeleton_link, "L6.shp")
      }
      
      SO_data <- readOGR(SO_link)
      #SO_colines <- as.data.frame(tidy(SO_data)) #After BROOM update 0.5.0 the default "tidy" now returns tibbles... and "fortify" is faster
      SO_colines <- fortify(SO_data)
      SO_colines[,"group"] <- paste(SO_colines[,"group"], "ant", sep="_")
      
      total_colines <- rbind(world_colines, SO_colines)
      
    } else if(coast_res[2]=="none") {
      total_colines <- world_colines
    }
    
    total_colines_coord.old <- SpatialPoints(total_colines[,1:2], proj4string = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs"))
    total_colines_coord.new <- invisible(suppressWarnings(try(spTransform(total_colines_coord.old, CRS(proj_final)), silent=TRUE)))
    
    if(inherits(total_colines_coord.new, "try-error")) {
      print("Failed to convert coastline coordinates using rGDAL... Trying with package SF!")
      
      total_colines_coord.new <- st_as_sf(total_colines_coord.old)
      total_colines_coord.new <- sf::st_transform(total_colines_coord.new, crs=proj_final)
      total_colines_coord.new <- st_coordinates(total_colines_coord.new)
      
      total_colines <- cbind(total_colines_coord.new, total_colines[,3:ncol(total_colines)])
      colnames(total_colines)[1:2] <- coord_vars
      
    } else if(!inherits(total_colines_coord.new, "try-error")) {
      
      total_colines <- as.data.frame(do.call(cbind, list(coordinates(total_colines_coord.new), total_colines[,3:ncol(total_colines)])))
      colnames(total_colines)[1:2] <- coord_vars
      
    }
    
    Layer_List[["H_Coast"]] <- coast_layer <- geom_polygon(data=total_colines, aes_string(group="group"), colour=plot_cols[[6]], fill=plot_cols[[5]])
    print("Coastlines successfully processed!")
  }
  
  #STAGE 4: Converting and applying the coordinate bounding box from the coord_sub argument
  #Generating graticules to determine bounding box for proj_final
  if(coast_res[1]!="none" & exists("total_colines_coord.old")) {
    
    coline_gratcoords <- st_as_sf(total_colines_coord.old)
    coline_gratcoords <- sf::st_transform(coline_gratcoords, crs=proj_final)
    #total_colines_coord.new <- st_coordinates(total_colines_coord.new)
    total_coline_graticules <- st_graticule(coline_gratcoords, crs=proj_final, datum=sf::st_crs(4326), ndiscr=1000, lon=seq(0, 360, 20), lat=seq(-90, 90, 1))
    
    #Detect the largest-area polygon and use it to generate plot background (for when bathy_res=="none")
    grat_poly <- st_polygonize(total_coline_graticules)
    poly_detect <- which.max(st_area(grat_poly))
    
    if(bathy_res=="none" | !exists("bathy_layer")) {
      Layer_List[["A_Panel_BG"]] <- panelbg_layer <- geom_sf(data=grat_poly[poly_detect,], inherit.aes = FALSE, colour="black", fill=plot_cols[[4]])
    }
    
    #Get extent of the bounding box and multiply this by the fractions in coord_sub
    #proj_bbox <- extent(grat_poly[poly_detect,]) #Another way to get extent
    proj_bbox <- as.numeric(st_bbox(grat_poly[poly_detect,]))
    
    if(!is.null(coord_sub)) {
      print("Applying coordinate bounding box from coord_sub...")
      
      if(grat[[1]]!="Distance") {
      Layer_List[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(xlim=c(proj_bbox[1]*coord_sub[1], proj_bbox[3]*coord_sub[2]), 
                                                              ylim=c(proj_bbox[2]*coord_sub[3], proj_bbox[4]*coord_sub[4]), 
                                                              expand=FALSE, crs=proj_final, datum=NA, default=FALSE)
      } else if(grat[[1]]=="Distance") {
        Layer_List[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(xlim=c(proj_bbox[1]*coord_sub[1], proj_bbox[3]*coord_sub[2]), 
                                                                ylim=c(proj_bbox[2]*coord_sub[3], proj_bbox[4]*coord_sub[4]),
                                                                expand=FALSE, crs=proj_final, datum=proj_final, default=FALSE)
      }
      
    } else if(is.null(coord_sub)) {
      
      if(grat[[1]]!="Distance") {
        Layer_List[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(expand=FALSE, crs=proj_final, datum=NA, default=FALSE)
      } else if(grat[[1]]=="Distance") {
        Layer_List[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(expand=FALSE, crs=proj_final, datum=proj_final, default=FALSE)
      }
      
    }
    
    #Generating graticules that will actually be displayed in the plot
    if(grat[[1]]!="none" & grat[[1]]=="WGS84" & !is.null(grat[[2]]) & !is.null(grat[[3]]) & !is.null(grat[[4]]) & !is.null(grat[[5]]) & !is.null(grat[[6]])) {
      print("Generating graticules using the proj_final coordinate reference system...")
      grat_final <- st_graticule(coline_gratcoords, crs=proj_final, datum=sf::st_crs(4326), ndiscr=1000, lon=grat[[2]], lat=grat[[3]])
      
      Layer_List[["I_Graticules"]] <- grat_layer <- geom_sf(data=grat_final, inherit.aes = FALSE, colour=grat[[4]], linetype=grat[[5]], size=grat[[6]])
      
    } else if(grat[[1]]!="none" & grat[[1]]=="Distance" & !is.null(grat[[4]]) & !is.null(grat[[5]]) & !is.null(grat[[6]])) {
      
      Dummy_GG <- ggplot(data=satmap_final, aes_string(x=coord_vars[1], y=coord_vars[2])) + Layer_List[["J_Coord_SF"]]
      x_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$x_range #This is for coord_sf only, see below for coord_cartesian!
      y_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$y_range
      #x_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$x.major_source #ggplot_build(Dummy_GG)$layout$panel_ranges[[1]]$x.major_source
      #y_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$y.major_source #ggplot_build(Dummy_GG)$layout$panel_ranges[[1]]$y.major_source
      #THE BELOW APPROACH WORKS ONLY FOR COORD CARTESIAN (when there is no geom_sf objects being plotted)
      #Layer_List[["I_Graticules"]] <- list(geom_hline(yintercept = y_inter, colour=grat[[4]], linetype=grat[[5]], size=grat[[6]]), 
      #                                     geom_vline(xintercept = x_inter, colour=grat[[4]], linetype=grat[[5]], size=grat[[6]]),
      #                                     scale_x_continuous(breaks=x_inter),
      #                                     scale_y_continuous(breaks=y_inter))
      Layer_List[["Z_Theme_DistGrats"]] <- list(scale_x_continuous(breaks=pretty(x_inter, n=grat[[7]])), scale_y_continuous(breaks=pretty(y_inter, n=grat[[7]])),
                                                 theme(legend.direction = "vertical", legend.position = "right",
                                                 panel.background = element_blank(),
                                                 #panel.background = element_rect(fill=plot_cols[[4]]),
                                                 panel.ontop = TRUE,
                                                 panel.grid.major=element_line(colour=grat[[4]], linetype=grat[[5]], size=grat[[6]])))
      
    } else if(grat[[1]]=="none") {
      
      Layer_List[["I_Graticules"]] <- geom_blank()
      
    }
    
  }
  
  #STAGE 5: Putting together the satellite plot data
  #Order the Layer_List elements in the correct order for plotting!
  print("Preparing satellite data for plotting...")
  #Layer_List <- Layer_List[order(names(Layer_List), decreasing=FALSE)]
  
  Layer_List_Base <- Layer_List[!grepl(paste0(sat_vars, collapse="|"), names(Layer_List))]
  
  Plotvar_List <- list()
  
  for(i in seq_along(sat_vars)) {
    Plotvar_List[[i]] <- c(Layer_List[grepl(sat_vars[i], names(Layer_List))], Layer_List_Base)
    Plotvar_List[[i]] <- Plotvar_List[[i]][order(names(Plotvar_List[[i]]), decreasing=FALSE)]
  }
  
  #Plotting satellite data
  Baseplot <- ggplot(data=satmap_final, aes_string(x=coord_vars[1], y=coord_vars[2])) + 
    guides(colour="legend", fill=guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
    theme(legend.direction = "vertical", legend.position = "right",
          panel.background = element_blank(),
          #panel.background = element_rect(fill=plot_cols[[4]]),
          panel.ontop = FALSE,
          panel.grid.major=element_blank())
  
  SatPlot_List <- list()
  
  for(i in seq_along(Plotvar_List)) {
    print(paste0("Compiling satellite plot for variable ", sat_vars[i], "..."))
    SatPlot_List[[sat_vars[i]]] <- Baseplot + Plotvar_List[[i]] + labs(title=sat_varlabs[i], x=x_lab, y=y_lab)
  }
  #Conditionally print the satellite plots
  if(print_plots=="sat"|print_plots=="all") {
    print(SatPlot_List)
  }
  
  #STAGE 6: Importing, processing and plotting sample data
  if(!is.na(point_data) & is.character(point_data) & !any(is.na(point_vars))) {
    print("Importing sample/point data...")
    pvar_vec <- c()
    
    if(!is.null(plot_aes[[2]])) {
    for(i in 1:length(plot_aes[[2]])) {
      pvar_vec[eval(parse(text=sub("^[^\"]*", "", plot_aes[[2]])))] <- eval(parse(text=sub("^[^\"]*", "", plot_aes[[2]])))
      
    }
      sample_data <- data.table::fread(point_data, select=unique(c(coord_vars, pvar_vec, point_vars)), data.table = FALSE)
    } else if(is.null(plot_aes[[2]])) {
      sample_data <- data.table::fread(point_data, select=unique(c(coord_vars, point_vars)), data.table = FALSE)
    }
    
    
    
    #Transforming the sample data into the proj_final CRS...
    print("Transforming points into the proj_final coordinate reference system...")
    
    points_coord.old <- SpatialPoints(sample_data[,coord_vars], proj4string = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs"))
    points_coord.new <- invisible(suppressWarnings(try(spTransform(points_coord.old, CRS(proj_final)), silent=TRUE)))
    
    if(inherits(points_coord.new, "try-error")) {
      print("Failed to convert point coordinates using rGDAL... Trying with package SF!")
      
      points_coord.new <- st_as_sf(points_coord.old)
      points_coord.new <- sf::st_transform(points_coord.new, crs=proj_final)
      points_coord.new <- st_coordinates(points_coord.new)
      
      points_total <- do.call(cbind, list(points_coord.new, sample_data[,coord_vars[1]], sample_data[,coord_vars[2]], sample_data[,!colnames(sample_data) %in% coord_vars]))
      colnames(points_total)[1:4] <- c(coord_vars, "xcoord_old", "ycoord_old")
      
    } else if(!inherits(points_coord.new, "try-error")) {
      
      points_total <- do.call(cbind, list(coordinates(points_coord.new), sample_data[,coord_vars[1]], sample_data[,coord_vars[2]], sample_data[,!colnames(sample_data) %in% coord_vars, drop=FALSE]))
      colnames(points_total)[1:4] <- c(coord_vars, "xcoord_old", "ycoord_old")
      
    }
    
    #Convert values which will use discrete scales (plot_aes[[2]]) into factors...
    if(!is.null(plot_aes[[2]])) {
      for(i in seq_along(pvar_vec)) {
        points_total[, pvar_vec[i]] <- factor(points_total[, pvar_vec[i]], levels=unique(points_total[, pvar_vec[i]])) 
      }
    }
    
    print("Point coordinates transformed successfully!")
    
    #Creating GGPlot point layers, mapping aes_string from plot_aes[[1]]...
    print("Generating map(s) of point data...")
    
    if(any(names(Layer_List_Base) %in% "A_Panel_BG")) {
      Point_List <- Layer_List_Base[!grepl("A_Panel_BG", names(Layer_List_Base))]
      Point_List[["A_Panel_BG"]] <- geom_sf(data=grat_poly[poly_detect,], inherit.aes = FALSE, colour="black", fill=plot_cols[[9]])
    } else if(!any(names(Layer_List_Base) %in% "A_Panel_BG") | any(names(Layer_List_Base) %in% "B_Bathy_L")) {
      Point_List <- Layer_List_Base[!grepl("B_Bathy_L", names(Layer_List_Base))]
      Point_List[["A_Panel_BG"]] <- geom_sf(data=grat_poly[poly_detect,], inherit.aes = FALSE, colour="black", fill=plot_cols[[9]])
    }
    
    if(pie_plot[1]==FALSE) {
    
    aes_list <- list()
    aes_aggrlist <- list()
    
    if(!is.null(plot_aes[[2]])) {
      
      for(i in seq_along(point_vars)) {
        aes_list[[point_vars[i]]] <- eval(parse(text=paste0("aes_string(", paste0(plot_aes[[1]], "=point_vars[i]", collapse =", "),  ", ", plot_aes[[2]], ")")))
        
        Point_List[[paste0("X_Point_L_", point_vars[i])]] <- geom_point(data=points_total, aes_list[[point_vars[i]]], colour="grey20", pch=scale_symbol[1], na.rm=TRUE)
        
      }
      Point_List[["Z_Fill_SC"]] <- scale_fill_manual(values=plot_cols[[2]])
      Point_List[["ZZ_Guides"]] <- guides(colour="legend", fill="legend")
      
    } else if(is.null(plot_aes[[2]])) {
      
      for(i in seq_along(point_vars)) {
        aes_list[[point_vars[i]]] <- eval(parse(text=paste0("aes_string(", paste0(plot_aes[[1]], "=point_vars[i]", collapse =", "), ")")))
      #REMEMBER NOT TO INCLUDE LOOP VALUES WITHIN SCALE AESTHETICS!!! THEY DONT GET EVALUATED UNTIL THE ACTUAL PLOTTING!
        Point_List[[paste0("X_Point_L_", point_vars[i])]] <- geom_point(data=points_total, aes_list[[point_vars[i]]], colour="grey20", pch=scale_symbol[1], na.rm=TRUE)
        
        #Setting up values for manipulating the fill scale
        quart_vals <- as.numeric(quantile(points_total[,point_vars[i]], na.rm=TRUE)[2:4])
        min_val <- min(na.omit(points_total[,point_vars[i]]))
        quart1 <- as.numeric(quantile(points_total[,point_vars[i]], na.rm=TRUE)[2])
        quart2 <- as.numeric(quantile(points_total[,point_vars[i]], na.rm=TRUE)[3])
        quart3 <- as.numeric(quantile(points_total[,point_vars[i]], na.rm=TRUE)[4])
        iqr_val <- quart3 - quart1
        max_val <- max(na.omit(points_total[,point_vars[i]]))
        
        #Setting up limits for fill scale
        if(any(sat_lims %in% "quartile")) {
          
          fillgrad_lims <- c(quart1, quart3)
          
        } else if(any(sat_lims %in% "IQR")) {
          
          fillgrad_lims <- c(quart1-iqr_val, quart3+iqr_val)
          if(scale_opts[4]==FALSE) {
            fillgrad_lims[fillgrad_lims<0] <- 0
          }
          
        } else if(any(sat_lims %in% "default")) {
          
          fillgrad_lims <- c(min_val, max_val)
          
        } else if(!any(sat_lims %in% c("default", "quartile", "IQR")) & is.numeric(sat_lims)) {
          
          fillgrad_lims <- sat_lims
        }
        
        #Setting up values for fill scale
        if(any(sat_values %in% "quartile")) {
          
          fillgrad_vals <- c(seq(quart1, quart3, length.out = length(plot_cols[[2]])-1), max(fillgrad_lims))
          
        } else if(any(sat_values %in% "IQR")) {
          
          fillgrad_vals <- c(seq(quart1-iqr_val, quart3, length.out = length(plot_cols[[2]])-2), quart3+iqr_val) #c(quart1-iqr_val, seq(quart1, quart3, length.out = length(plot_cols[[2]])-2), quart3+iqr_val) 
          
          if(scale_opts[4]==FALSE) {
            fillgrad_vals[fillgrad_vals<0] <- 0
          }
          
        } else if(any(sat_values %in% "default")) {
          
          fillgrad_vals <- seq(min(fillgrad_lims), max(fillgrad_lims), length.out=length(plot_cols[[2]]))
          
        } else if(!any(sat_values %in% c("default", "quartile", "IQR")) & is.numeric(sat_values)) {
          fillgrad_vals <- sat_values
        }
        
        #Setting up breaks for the fill scale
        if(any(sat_breaks %in% "quartile")) {
          
          fillgrad_breaks <- unique(c(min(fillgrad_lims), quart_vals, max(fillgrad_lims)))
          
        } else if(any(sat_breaks %in% "IQR")) {
          
          fillgrad_breaks <- unique(c(min(fillgrad_lims), quart1-iqr_val, quart_vals, quart3+iqr_val, max(fillgrad_lims)))
          
        } else if(any(sat_breaks %in% "default")) {
          
          fillgrad_breaks <- pretty(range(fillgrad_lims), n=break_num)
          
        } else if(!any(sat_breaks %in% c("default", "quartile", "IQR")) & is.numeric(sat_breaks)) {
          
          fillgrad_breaks <- sat_breaks
        }
        
        fillgrad_labs <- round(fillgrad_breaks, scale_opts[2])
        
        Point_List[[paste0("Z_Fill_SC_", point_vars[i])]] <- scale_fill_gradientn(colours=plot_cols[[2]], breaks=fillgrad_breaks, values=scales::rescale(fillgrad_vals), limits=fillgrad_lims, labels=as.character(fillgrad_labs), na.value=plot_cols[[10]]) #values=scales::rescale(fillgrad_vals) #limits=range(fillgrad_lims)
        }
      
      Point_List[["ZZ_Guides"]] <- guides(colour="legend", fill=guide_colorbar(frame.colour = "black", ticks.colour = "black"))
    }
    
    } else if(pie_plot[1]==TRUE) {
      
      if(pie_plot[2]>0) {
        
      for(i in seq_along(point_vars)) {
        points_total_scatterpie <- points_total
        radius_name <- paste0("radius_", point_vars[i])
        radint_name <- paste0("radint_", point_vars[i])
        points_total_scatterpie[,radius_name] <- cut(points_total_scatterpie[,point_vars[i]], unique(quantile(points_total_scatterpie[,point_vars[i]], na.rm=TRUE)), include.lowest = TRUE)
        points_total_scatterpie[,radint_name] <- as.integer(points_total_scatterpie[,radius_name])
        points_total_scatterpie[points_total_scatterpie[,radint_name]>1 & !is.na(points_total_scatterpie[,radint_name]), radint_name] <- na.omit(points_total_scatterpie[points_total_scatterpie[,radint_name]>1 & !is.na(points_total_scatterpie[,radint_name]),radint_name])*pie_plot[2]
        points_total_scatterpie[,radint_name] <- points_total_scatterpie[,radint_name]*(pie_plot[3]*proj_bbox[4])
        
        print(paste0("Removing ", as.character(nrow(yearly_summary[!complete.cases(yearly_summary),])), " rows with missing values for Scatter Pie Plot..."))
        points_total_scatterpie <- points_total_scatterpie[complete.cases(points_total_scatterpie),]
        points_total_scatterpie[,"group_scatpie"] <- factor(1:nrow(points_total_scatterpie))
        
          Point_List[[paste0("X_Point_L_", point_vars[i])]] <- list(geom_scatterpie(data=points_total_scatterpie, aes_string(x=coord_vars[1], y=coord_vars[2], group="group_scatpie", r=radint_name), cols=c(point_vars), sorted_by_radius = FALSE), #colour=NA, size=5 (controls OUTLINE thickness)
                                                                    geom_scatterpie_legend(points_total_scatterpie[,radint_name], x=-3e6, y=-3e6, n=max(as.integer(unique(points_total_scatterpie[,radius_name]))), labeller=function(x) x=as.character(sort(unique(points_total_scatterpie[,radius_name])))))
          
          #Override back to manual/discrete scale for the "fill" aesthetic
          Point_List[[paste0("Z_Fill_SC_", point_vars[i])]] <- scale_fill_manual(values=plot_cols[[2]], guide="legend", name="Variable")
          
      }
      } else if(pie_plot[2]<=0) {
        print("Equi-sized scatter-pie plots were selected. No size scaling will be applied!")
        
        points_total_scatterpie <- points_total
        radius_name <- "pieplot_radius"
        points_total_scatterpie[,radius_name] <- as.integer(1)
        radint_name <- "pieplot_radint"
        points_total_scatterpie[,radint_name] <- points_total_scatterpie[,radius_name]*(pie_plot[3]*proj_bbox[4])
        
        print(paste0("Removing ", as.character(nrow(points_total_scatterpie[!complete.cases(points_total_scatterpie),])), " rows with missing values for Scatter Pie Plot..."))
        points_total_scatterpie <- points_total_scatterpie[complete.cases(points_total_scatterpie),]
        points_total_scatterpie[,"group_scatpie"] <- factor(1:nrow(points_total_scatterpie))
        
        Point_List[["X_Point_L"]] <- geom_scatterpie(data=points_total_scatterpie, aes_string(x=coord_vars[1], y=coord_vars[2], group="group_scatpie", r=radint_name), cols=c(point_vars), sorted_by_radius = FALSE) #), #colour=NA, size=5 (controls OUTLINE thickness)
                                                                  #geom_scatterpie_legend(points_total_scatterpie[,radint_name], x=-3e6, y=-3e6, n=max(as.integer(unique(points_total_scatterpie[,radius_name]))), labeller=function(x) x=as.character(sort(unique(points_total_scatterpie[,radius_name])))))
        
        #Override back to manual/discrete scale for the "fill" aesthetic
        Point_List[["Z_Fill_SC"]] <- scale_fill_manual(values=plot_cols[[2]], guide="legend", name="Variable")
        
      }
      
      #Override the colour legend style to discrete values (for contours)
      Point_List[["ZZ_Guides"]] <- guides(colour="legend", fill="legend")
      
    }

    #Re-generating contours with new aesthetic mapping for sample plot
      if(!is.null(sat_contours) & is.numeric(sat_contours) & satcont_type[1]!="none") {
    for(i in seq_along(sat_vars)) {
          sat_scalevar <- sat_vars[i]
          Point_List[[paste0("F_Pointcont_L_", sat_vars[i])]] <- geom_contour(data=satmap_final, aes_string(z=sat_scalevar, colour="..level.."), breaks=sat_contours, na.rm = TRUE, linetype=as.numeric(satcont_type[2]), size=as.numeric(satcont_type[3]))
  
    }
        Point_List[["Y_PCont_SC"]] <- scale_colour_gradientn(colours=plot_cols[[7]], name=leg_labs[2], breaks=sat_contours)
      }
    
   
   #Point_List <- Point_List[order(names(Point_List), decreasing=FALSE)]
    #print(climplot_results[[1]] + geom_point(data=points_total, plot_aes[[1]], colour="black", fill="darkred") +
    #        scale_shape_manual(values=c(1:6,20:25)))

   #Generate SCALES (currently "fill" is only mappable to a discrete scale, "colour" is reserved for contours,
   #and what is left is size and alpha for continuous variables and shape for discrete vars - NOT GREAT!)
   #REMEMBER THAT THE COLOUR SCALE GETS CREATED BASED ON WHETHER OR NOT THE SAT_CONTOURS ARE PRESENT! LINE 799!
   #REMEMBER THAT THE FILL SCALE GETS CREATED BASED ON WHETHER OR NOT PLOT_AES[[2]] is.null! LINES 786, 794ish!
   
   Point_List[["Z_Alpha_SC"]] <- scale_alpha_continuous(range=scale_alpha)
   scale_aes <- list()
   
   if(pie_plot[1]==FALSE) {
        for(i in seq_along(point_vars)) {
          
     if(any(scale_size=="default")) {
       scale_size <- c(1, 3)
     }
       
       sizesc_vals <- round(as.numeric(quantile(points_total[,point_vars[i]], na.rm=TRUE)[2:4]),scale_opts[2])
       
       if(size_lims[1]=="default") {
         
         Point_List[[paste0("Z_Size_SC_", point_vars[i])]] <- scale_size_continuous(range=scale_size, breaks=sizesc_vals)
         
       } else if(size_lims[1]=="quart") {
         
         Point_List[[paste0("Z_Size_SC_", point_vars[i])]] <- scale_size_continuous(range=scale_size, breaks=sizesc_vals, limits=range(sizesc_vals, na.rm=TRUE))
         
         if(!is.null(plot_aes[[2]])) {
           
           scale_aes[[point_vars[i]]] <- eval(parse(text=paste0("aes_string(", plot_aes[[1]][plot_aes[[1]]!="size"], "=point_vars[i]", ", ", plot_aes[[2]], ")")))
           
         } else if(is.null(plot_aes[[2]])) {
           
           scale_aes[[point_vars[i]]] <- eval(parse(text=paste0("aes_string(", plot_aes[[1]][plot_aes[[1]]!="size"], "=point_vars[i]", ")")))
           
         }
         
         if(size_lims[2] %in% c(1:25)) {
           
           Point_List[[paste0("X_Size_L_", point_vars[i])]] <- geom_point(data=points_total[points_total[,point_vars[i]]<min(sizesc_vals),], scale_aes[[point_vars[i]]], colour="grey20", pch=as.numeric(size_lims[2]), size=scale_size[1], na.rm=TRUE)
           Point_List[[paste0("X_Size_H_", point_vars[i])]] <- geom_point(data=points_total[points_total[,point_vars[i]]>max(sizesc_vals),], scale_aes[[point_vars[i]]], colour="grey20", pch=as.numeric(size_lims[2]), size=scale_size[2], na.rm=TRUE)
           
         } else if(is.na(size_lims[2])) {
           
           Point_List[[paste0("X_Size_L_", point_vars[i])]] <- geom_point(data=points_total[points_total[,point_vars[i]]<min(sizesc_vals),], scale_aes[[point_vars[i]]], colour="grey20", pch=scale_symbol[1], size=scale_size[1], na.rm=TRUE)
           Point_List[[paste0("X_Size_H_", point_vars[i])]] <- geom_point(data=points_total[points_total[,point_vars[i]]>max(sizesc_vals),], scale_aes[[point_vars[i]]], colour="grey20", pch=scale_symbol[1], size=scale_size[2], na.rm=TRUE)
           
         }
       }
   }
     
   }

   #Point_List[["Z_Shape_SC"]] <- scale_shape_manual(values=scale_symbol)
    
   #Generate a list of basic layers (without any mention of sat_vars or point_vars)
   Point_List_Base <- Point_List[!grepl(paste0(paste0(sat_vars, collapse="|"), "|", paste0(point_vars, collapse="|"), collapse="|"), names(Point_List))]
    
   #Generate layer lists for every combination of sat_vars and point_vars...
   Pointvar_List <- list() 
   total_plotlabs <- c()
   
   if(pie_plot[1]==FALSE | any(pie_plot[1] %in% c(TRUE, FALSE) & pie_plot[2]>0)) {
     
     for(i in seq_along(sat_vars)) {
       for(j in seq_along(point_vars)) {
         Pointvar_List[[paste(sat_vars[i], "with", point_vars[j])]] <- c(Point_List[grepl(paste0(c(sat_vars[i], point_vars[j]), "$", collapse="|"), names(Point_List))], Point_List_Base)
         Pointvar_List[[paste(sat_vars[i], "with", point_vars[j])]] <- Pointvar_List[[paste(sat_vars[i], "with", point_vars[j])]][order(names(Pointvar_List[[paste(sat_vars[i], "with", point_vars[j])]]), decreasing=FALSE)]
         total_plotlabs[paste0(i, j)] <- paste(sat_varlabs[i], "with", point_vars[j])
       }
     }
     
   } else if(pie_plot[1]==TRUE & pie_plot[2]<=0) {
     
     for(i in seq_along(sat_vars)) {
       
       Pointvar_List[[paste(sat_vars[i], "(all variables)")]] <- c(Point_List[grepl(sat_vars[i], names(Point_List))], Point_List_Base)
       Pointvar_List[[paste(sat_vars[i], "(all variables)")]] <- Pointvar_List[[paste(sat_vars[i], "(all variables)")]][order(names(Pointvar_List[[paste(sat_vars[i], "(all variables)")]]), decreasing=FALSE)]
       total_plotlabs[i] <- paste(sat_varlabs[i], "(all variables)") 
     }
     
   }


   #Create the skeleton/base plot...
   BasePointPlot <- ggplot(data=points_total, aes_string(x=coord_vars[1], y=coord_vars[2])) + 
     theme(legend.direction = "vertical", legend.position = "right",
           panel.background = element_blank(),
           #panel.background = element_rect(fill=plot_cols[[4]]),
           panel.ontop = FALSE,
           panel.grid.major=element_blank())
   
   PointPlot_List <- list()
   
   for(i in seq_along(Pointvar_List)) {
    
       print(paste0("Compiling point plot for variables ", names(Pointvar_List)[i], "..."))
       PointPlot_List[[names(Pointvar_List)[i]]] <- BasePointPlot + Pointvar_List[[i]] + labs(title=total_plotlabs[i], x=x_lab, y=y_lab) #names(Pointvar_List)[i]

   }
   
   #Conditionally print the point/sample plots
   if(print_plots=="point"|print_plots=="all") {
     print(PointPlot_List)
   }
   
  } else if(is.na(point_data) | any(is.na(point_vars))) {
    print("No point/sample plots were drawn since point_data is NA!")
    PointPlot_List <- NULL
  }
  
  if(export_results=="all" | export_results=="plots") {
  Total_Plot_List <- list(SatPlot_List, PointPlot_List)
  pdf(paste(export_path, "/CLIM_Plots ", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin") , ".pdf", sep=""),
      width=width, height=height, pointsize = point_size)
  print(Total_Plot_List)
  dev.off()
  }
  
  return(list(SatPlot_List, PointPlot_List))
}



#FUNCTION: Extract the variable colnames that you need using the clim_sep helper function
clim_sep <- function(data, vars, data_type, labs=NA) {
  
  if(length(data)>1 | !is.list(data) | is.data.frame(data)) { data <- list(data) }
  if(length(data_type)>1 | !is.list(data_type)) { data_type <- list(data_type) }
  if(length(data)!=length(data_type)) { stop("The length of data and data_type arguments must be equal!") }
  
  res_list <- list()
  res_lablist <- list()
  name_vec <- c("mean", "sd", "anomaly")
  grep_vec <- c("Mean", "SD", "Anomaly")
  
  for(data_elem in seq_along(data)) {
    
    for(i in seq_along(grep_vec)) {
      for(j in seq_along(vars)) {
        
        listname <- paste0(data_type[[data_elem]], "_", name_vec[i], "_", vars[j])
        res_list[[listname]] <- grep(paste0(grep_vec[i], ".of.", vars[j]), colnames(data[[data_elem]]), value=TRUE)
        
      }
    }
  }
  
  if(!any(is.na(labs))) {
    
    if(length(labs)>1 | !is.list(labs)) { labs <- list(labs) }
    
    for(data_elem in seq_along(labs)) {
      for(i in seq_along(grep_vec)) {
        for(j in seq_along(vars)) {
          
          listname <- paste0(data_type[[data_elem]], "_", name_vec[i], "_", vars[j], "_labs")
          res_lablist[[listname]] <- grep(paste0(grep_vec[i], " of ", vars[j]), labs[[data_elem]], value=TRUE)
          
        }
      }
      
    }
  }
  
  return(list(sep_results=res_list, sep_labs=res_lablist))
}

