clim_region <- function(core_dir, poly_path, poly_type="shp_file", poly_list="all", poly_pat="\\.shp$", separ=",",
                        sat_data, sat_vars, sat_varlabs=NA, mean_col=NA, bar_varlabs="default", coord_vars=c("Longitude", "Latitude"),
                        check_inter=FALSE, proj_init="WGS84", proj_final, coast_res=c("low", "ifb"), coord_sub=NULL,
                        grat=list("none", seq(0, 360, 20), seq(0, 90, 10), "grey15", 1, 0.7, 5), plot_cols=rep(list("default"), 7),
                        plot_type="heat_rank", plot_labs=coord_vars, y_lab="Mean", plot_by="varib", print_plots=TRUE,
                        facet_plots="summarise", plot_opts=c(45, -0.2, 7), plot_extras="sd", plot_oob=FALSE, extra_trends=FALSE, export_plots="pdf",
                        export_path, height=10, width=10, point_size=12, dpi=500) {
  
  #Install and load the required libraries
  list.of.packages <- c("data.table", "RColorBrewer", "ggplot2", "ggmap", "raster", "rgeos", "sp", "sf", "rgdal", "broom", "dplyr", "reshape2")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  library(data.table)
  library(RColorBrewer)
  library(ggplot2)
  library(ggmap)
  library(raster)
  library(rgeos)
  library(sp)
  library(sf)
  library(rgdal)
  library(broom)
  library(dplyr)
  library(reshape2)
  
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
  
  #PREP STAGE: Error Checks
  plot_cols <- c(plot_cols, as.list(c(rep("default", 7-length(plot_cols)))))
  
  if(length(plot_cols[[2]])>1 | length(plot_cols[[3]])>1 | length(plot_cols[[4]])>1) {
    print("The 2nd to 4th elements of plot_cols may only be of length 1. Switching to default colours for offending elements!")
    
    which_len <- which(lengths(plot_cols)>1)
    which_len <- which_len[which_len %in% c(2:4)]
    
    print(paste0("Default colours were used for these plot_cols elements: ", paste(which_len, collapse=","), "!"))
    
    for(i in which_len) {
      plot_cols[[i]] <- "default"
    }
  }
  
  if(any(is.na(sat_varlabs)) & !any(is.character(sat_varlabs))) {
    print("Using variable names as graph titles since a vector of labels was not provided in sat_varlabs...")
    sat_varlabs <- sat_vars
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
  
  if(!any(facet_plots %in% c(TRUE, FALSE, "summarise"))) {
    print("Incorrect value provided for the facet_plots argument! Defaulting to TRUE...")
    facet_plots <- TRUE
  }
  
  if(!any(plot_type %in% c("point", "bar", "heat", "heat_rank"))) {
    print("Incorrect value provided for the plot_type argument! Defaulting to \"point\"...")
  }
  
  if(facet_plots!="summarise" & plot_type=="heat" | facet_plots!="summarise" & plot_type=="heat_rank") {
    print("Heatmaps are not supported unless the faceting function is set to \"summarise\"! Switching...")
    facet_plots <- "summarise"
  }
  
  # PREP STAGE: Sorting and assigning plot colours (indices are: 1 - polygon colours, 2 - missing value colour, 3 - coast polygon colour, 4 - coastline and polygon outline colour)
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
                      c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"))
  
  defcol_ids <- c("SIC", "SIC_heat", "SIC_anom", "MODIS", "ODV", "ODV_bathy", "Alter_bathy", "RYG")
  
  for(i in seq_along(defcol_ids)) {
    if(any(plot_cols==defcol_ids[i])) { plot_cols[[which(plot_cols==defcol_ids[i])]] <- defcol_list[[i]] }
  }
  
  def_colvals <- list(c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499", "#DE0000", "#E7972D", "#3BB826"), #"red", "orange", "green", "grey", "purple", "yellow", "white", "coral4", "deeppink", "slategray1", "lightblue", 
                      "transparent", "#AD9284", "black", #"#093C70"
                      #c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
                      c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"),
                      c("#DE0000", "#E7972D", "#3BB826", "black"),
                      c("black", "blue", "white"))
  
  if(any(plot_cols=="default")) {
    which_cols <- which(plot_cols=="default")
    
    for(i in which_cols) {
      plot_cols[[i]] <- def_colvals[[i]]
    }
  }
  
  
  #STAGE 1: Load satellite and polygon data
  #Load the satellite/grid data
  print("Loading and processing data...")
  if(is.object(sat_data)) {
    satmap <- sat_data[,c(coord_vars, sat_vars)]
  } else if(is.character(sat_data)) {
    satmap <- data.table::fread(sat_data, sep=separ, select=c(coord_vars, sat_vars), data.table = FALSE)
  }
  
  #Load the requested shapefiles from the folder based on the value of POLY_TYPE!
  if(poly_type=="shp_file") {
    
    shp_files <- polygon_list <- list()
    
    if(any(poly_list %in% "all")) {
      
      shp_dirs <- list.dirs(poly_path, full.names=TRUE, recursive = FALSE)
      shp_dirlabs <- list.dirs(poly_path, full.names=FALSE, recursive = FALSE)
      
      for(i in seq_along(shp_dirs)) {
        
        shp_files[[i]] <- list.files(shp_dirs[i], pattern=poly_pat)
        polygon_list[[i]] <- st_read(paste0(shp_dirs[i], "/", shp_files[[i]]), quiet = TRUE)
        polygon_list[[i]][,"name"] <- shp_dirlabs[i]
        polygon_list[[i]] <- polygon_list[[i]][,colnames(polygon_list[[i]]) %in% c("name", "geometry")]
      }
      
    } else if(!any(poly_list %in% "all")) {
      
      for(i in seq_along(poly_list)) {
        
        shp_files[[i]] <- list.files(paste0(poly_path, "/", poly_list[i]), pattern=poly_pat)
        #names(shp_files)[i] <- gsub("\\.shp$", "", names(shp_files)[i])
        
        polygon_list[[i]] <- st_read(paste0(poly_path, "/", poly_list[i], "/", shp_files[[i]]), quiet=TRUE)
        polygon_list[[i]][,"name"] <- poly_list[i]
        polygon_list[[i]] <- polygon_list[[i]][,colnames(polygon_list[[i]]) %in% c("name", "geometry")]
      }
      
    }
    
  } else if(!any(poly_type %in% "shp_file") & is.character(poly_type)) {
    
    satmap[,poly_type] <- polygon_list <- factor(satmap[,poly_type], levels=unique(satmap[,poly_type])) 
    #polygon_list[[i]] <- st_read(poly_type[i]) #If poly_type is to represent full file paths...
    
  } else if(poly_type!="shp_file" & !is.character(poly_type)) {
    
    stop("The poly_type argument is incorrect, and should either be a list of shapefile directories, pre-defined group column or be equal to \"all\"!")
  }
  
  #Check for intersecting polygons and filter out the areas that intersect
  polygon_vec <- do.call(rbind, polygon_list) #THE WRONG APPROACH, USE c() INSTEAD!
  poly_plainvec <- polygon_vec
  st_geometry(poly_plainvec) <- NULL
  polygon_vec[,"name"] <- factor(poly_plainvec[,"name"], levels=unique(poly_plainvec[,"name"]))
  
  if(check_inter==TRUE) {
    
    print("Detecting and listing intersecting polygons... This may take a while!")
    which_inter <- st_intersects(polygon_vec)
    which_inter <- lapply(which_inter, function(x) x[!x==x[1]]) #Remove the self-intersection detection
    inter_unl <- unique(unlist(which_inter))
    poly_check <- nrow(polygon_vec[!inter_unl,])
    
    print(paste("Removed a total of", length(inter_unl), "intersecting polygons..."))
    
    if(poly_check==0) {
      stop("All polygon shapefiles intersect with each other... Stopping function!")
    } else if(poly_check>1) {
      polygon_vec <- poly_check
    }
    
  }
  
  #The procedure below can remove overlapping areas of polygons BUT is VERY computationally expensive... avoid!
  #inter <- st_intersection(polygon_vec) %>% dplyr::filter(n.overlaps < 2)
  #polygon_vec <- do.call(c, polygon_list)
  #inter <- st_intersection(st_sf(polygon_vec)) %>% dplyr::filter(n.overlaps < 2)
  
  #STAGE 3: Find which points belong to which polygon (make sure the polygons do NOT intersect... or use intersection check!)
  #First, reproject point/sat data and polygon data to a common coordinate system --> preferably the one closest to their default, e.g. EPSG:4326!
  print("Segregating points according to polygon shapefiles or group column...")
  satmap_final <- st_as_sf(x=satmap, coords=coord_vars, crs=proj_init)
  satmap_final <- sf::st_transform(satmap_final, crs=proj_init)
  #satmap_final <- st_coordinates(satmap_final)
  #satmap_final <- cbind(satmap_final, satmap[,!colnames(satmap_final) %in% coord_vars])
  #colnames(satmap_final)[1:2] <- coord_vars
  polygon_vec <- sf::st_transform(polygon_vec, crs=proj_init)
  
  #Now, detect which points of satmap_final (sat/point data) belong to which feature/shapefile of polygon_vec!
  #as.data.frame(st_join(satmap_final, polygon_vec, join = st_intersects))[2] %>% setNames("name")
  which_poly <- st_join(satmap_final, polygon_vec, join = st_intersects)
  
  #STAGE 4: Reproject data for map and compile the required data frame
  #Sample/satellite Data
  which_poly <- st_transform(which_poly, crs=proj_final)
  which_poly_coords <- st_coordinates(which_poly)
  st_geometry(which_poly) <- NULL
  which_poly <- cbind(which_poly_coords, which_poly)
  
  which_poly[,"name"] <- factor(which_poly[,"name"], levels = c(levels(which_poly[,"name"]), NA), labels = c(levels(which_poly[,"name"]), "Out of bounds"), exclude = NULL)
  #which_poly[which(is.na(which_poly[["name"]])==TRUE), "name"] <- "Out of bounds" #DOES NOT WORK WITH FACTOR COLUMNS!
  colnames(which_poly)[which(colnames(which_poly) %in% c("X", "Y"))] <- coord_vars
  
  #Polygon Data
  polygon_vec <- sf::st_transform(polygon_vec, crs=proj_final)
  #polygon_vec <- st_cast(polygon_vec, "POINT")
  #polygon_coords <- st_coordinates(polygon_vec)
  #st_geometry(polygon_vec) <- NULL
  #polygon_vec <- cbind(polygon_coords, polygon_vec)
  
  #STAGE 5: Load and process coastlines
  #Create a list to store the dynamic ggplot2 layers...
  Plotlist <- list()
  
  #Import and process coastline polygons...
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
    Plotlist[["B_Coast_L"]] <- geom_polygon(data=total_colines, aes_string(x=coord_vars[1], y=coord_vars[2], group="group"), fill=plot_cols[[3]], colour=plot_cols[[4]])
    print("Coastlines successfully processed!")
  } else if(coast_res=="none") {
    Plotlist[["B_Coast_L"]] <- geom_blank()
    print("Coastlines successfully processed!")
  }
  
  #STAGE 6: Converting and applying the coordinate bounding box from the coord_sub argument
  #Generating graticules to determine bounding box for proj_final
  if(coast_res[1]!="none" & exists("total_colines_coord.old")) {
    print("Generating graticules and processing coordinate window...")
    coline_gratcoords <- st_as_sf(total_colines_coord.old)
    coline_gratcoords <- sf::st_transform(coline_gratcoords, crs=proj_final)
    #total_colines_coord.new <- st_coordinates(total_colines_coord.new)
    total_coline_graticules <- st_graticule(coline_gratcoords, crs=proj_final, datum=sf::st_crs(4326), ndiscr=1000, lon=seq(0, 360, 20), lat=seq(-90, 90, 1))
    
    #Detect the largest-area polygon and use it to generate plot background (for when bathy_res=="none")
    grat_poly <- st_polygonize(total_coline_graticules)
    poly_detect <- which.max(st_area(grat_poly))
    
    #Get extent of the bounding box and multiply this by the fractions in coord_sub
    #proj_bbox <- extent(grat_poly[poly_detect,]) #Another way to get extent
    proj_bbox <- as.numeric(st_bbox(grat_poly[poly_detect,]))
    
    if(!is.null(coord_sub)) {
      print("Applying coordinate bounding box from coord_sub...")
      
      if(grat[[1]]!="Distance") {
        Plotlist[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(xlim=c(proj_bbox[1]*coord_sub[1], proj_bbox[3]*coord_sub[2]), 
                                                              ylim=c(proj_bbox[2]*coord_sub[3], proj_bbox[4]*coord_sub[4]), 
                                                              expand=FALSE, crs=proj_final, datum=NA, default=FALSE)
      } else if(grat[[1]]=="Distance") {
        Plotlist[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(xlim=c(proj_bbox[1]*coord_sub[1], proj_bbox[3]*coord_sub[2]), 
                                                              ylim=c(proj_bbox[2]*coord_sub[3], proj_bbox[4]*coord_sub[4]),
                                                              expand=FALSE, crs=proj_final, datum=proj_final, default=FALSE)
      }
      
    } else if(is.null(coord_sub)) {
      
      if(grat[[1]]!="Distance") {
        Plotlist[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(expand=FALSE, crs=proj_final, datum=NA, default=FALSE)
      } else if(grat[[1]]=="Distance") {
        Plotlist[["J_Coord_SF"]] <- coord_sflayer <- coord_sf(expand=FALSE, crs=proj_final, datum=proj_final, default=FALSE)
      }
      
    }
    
    #Generating graticules that will actually be displayed in the plot
    if(grat[[1]]!="none" & grat[[1]]=="WGS84" & !is.null(grat[[2]]) & !is.null(grat[[3]]) & !is.null(grat[[4]]) & !is.null(grat[[5]]) & !is.null(grat[[6]])) {
      print("Generating graticules using the proj_final coordinate reference system...")
      grat_final <- st_graticule(coline_gratcoords, crs=proj_final, datum=sf::st_crs(4326), ndiscr=1000, lon=grat[[2]], lat=grat[[3]])
      
      Plotlist[["I_Graticules"]] <- grat_layer <- geom_sf(data=grat_final, inherit.aes = FALSE, colour=grat[[4]], linetype=grat[[5]], size=grat[[6]])
      
    } else if(grat[[1]]!="none" & grat[[1]]=="Distance" & !is.null(grat[[4]]) & !is.null(grat[[5]]) & !is.null(grat[[6]])) {
      
      Dummy_GG <- ggplot(data=which_poly, aes_string(x=coord_vars[1], y=coord_vars[2])) + Plotlist[["J_Coord_SF"]]
      x_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$x_range #This is for coord_sf only, see below for coord_cartesian!
      y_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$y_range
      #x_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$x.major_source #ggplot_build(Dummy_GG)$layout$panel_ranges[[1]]$x.major_source
      #y_inter <- ggplot_build(Dummy_GG)$layout$panel_params[[1]]$y.major_source #ggplot_build(Dummy_GG)$layout$panel_ranges[[1]]$y.major_source
      #THE BELOW APPROACH WORKS ONLY FOR COORD CARTESIAN (when there is no geom_sf objects being plotted)
      #Layer_List[["I_Graticules"]] <- list(geom_hline(yintercept = y_inter, colour=grat[[4]], linetype=grat[[5]], size=grat[[6]]), 
      #                                     geom_vline(xintercept = x_inter, colour=grat[[4]], linetype=grat[[5]], size=grat[[6]]),
      #                                     scale_x_continuous(breaks=x_inter),
      #                                     scale_y_continuous(breaks=y_inter))
      Plotlist[["Z_Theme_DistGrats"]] <- list(scale_x_continuous(breaks=pretty(x_inter, n=grat[[7]])), scale_y_continuous(breaks=pretty(y_inter, n=grat[[7]])),
                                              theme(legend.direction = "vertical", legend.position = "right",
                                                    panel.background = element_blank(),
                                                    #panel.background = element_rect(fill=plot_cols[[4]]),
                                                    panel.ontop = TRUE,
                                                    panel.grid.major=element_line(colour=grat[[4]], linetype=grat[[5]], size=grat[[6]])))
      
    } else if(grat[[1]]=="none") {
      
      Plotlist[["I_Graticules"]] <- geom_blank()
      
    }
    
  }
  
  #STAGE 7: Plot the point/satellite data on a map by assigned polygon
  print("Plotting regional map with points assigned by polygon shapefile...")
  Plotlist <- Plotlist[order(names(Plotlist), decreasing = FALSE)]
  
  if(any(bar_varlabs %in% "default")) {
    colour_sc <- scale_colour_manual(values=c(plot_cols[[1]][1:nrow(unique(which_poly[,"name", drop=FALSE]))-1], plot_cols[[2]]))
    fill_sc_base <- scale_fill_manual(values=c(plot_cols[[1]][1:nrow(unique(which_poly[,"name", drop=FALSE]))-1], plot_cols[[2]]))
  } else if(!any(bar_varlabs %in% "default")) {
    #NOTE: The -1 in the colour subset is to get rid of the "Out of bounds" value that is present in which_poly!!!
    colour_sc <- scale_colour_manual(values=c(plot_cols[[1]][1:nrow(unique(which_poly[,"name", drop=FALSE]))-1], plot_cols[[2]]), labels=bar_varlabs[which(unique(poly_plainvec[,"name"]) %in% as.character(unique(which_poly[,"name"])))])
    fill_sc_base <- scale_fill_manual(values=c(plot_cols[[1]][1:nrow(unique(which_poly[,"name", drop=FALSE]))-1], plot_cols[[2]]), labels=bar_varlabs[which(unique(poly_plainvec[,"name"]) %in% as.character(unique(which_poly[,"name"])))])
  }
  
  
  Base_Plot <- ggplot(data=which_poly) + 
    geom_point(aes_string(x=coord_vars[1], y=coord_vars[2], colour="name"), pch=19) +
    geom_sf(data=polygon_vec, colour=plot_cols[[4]], fill="transparent") +
    colour_sc +
    #coord_sf(xlim = c(-3e6, 3e6), ylim = c(-3e6, 3e6), expand=FALSE, crs=proj_final, datum=NA) +
    guides(colour="legend") +
    labs(x=plot_labs[1], y=plot_labs[2], title="Points detected across polygons", colour="Polygon")
  
  Region_Plain_Plot <- ggplot(data=which_poly) +
    geom_sf(data=polygon_vec, aes_string(fill="name"), colour=plot_cols[[4]]) +
    fill_sc_base +
    guides(fill="legend") +
    labs(x=plot_labs[1], y=plot_labs[2], title="Region/Polygon Showcase", fill="Polygon")
  
  #Create a list to hold all plots
  Plot_List <- list()
  Plot_List[["Region_Plot"]] <- Base_Plot + Plotlist
  Plot_List[["Basic_Region_Showcase"]] <- Region_Plain_Plot + Plotlist
  
  if(any(print_plots %in% "map")) {
    print(Plot_List[["Region_Plot"]])
    print(Plot_List[["Region_Plot_Basic"]])
  }
  
  #STAGE 8: Calculating and plotting summary statistics for points within each polygon
  print("Calculating and plotting summary statistics...")
  summary_stats <- list()
  
  for(i in which(colnames(which_poly) %in% sat_vars)) {
    summary_stats[[colnames(which_poly)[i]]] <- tapply(which_poly[,colnames(which_poly)[i]], which_poly[,"name"], function(x) c(Mean=mean(x, na.rm=TRUE), StDev=sd(x, na.rm=TRUE), Min=quantile(x, na.rm=TRUE)[1], Q1=quantile(x, na.rm=TRUE)[2], Q2=median(x, na.rm=TRUE), Q3=quantile(x, na.rm=TRUE)[4], Max=quantile(x, na.rm=TRUE)[5], Sample_Size=length(!is.na(x))))
    summary_stats[[colnames(which_poly)[i]]] <- do.call(rbind, c(summary_stats[[colnames(which_poly)[i]]], make.row.names=FALSE))
    summary_stats[[colnames(which_poly)[i]]] <- data.frame(Region=rownames(summary_stats[[colnames(which_poly)[i]]]), summary_stats[[colnames(which_poly)[i]]])
  }
  
  
  summary_stats <- mapply(cbind, summary_stats, "Timevar"=names(summary_stats), SIMPLIFY=F)
  summary_stats <- do.call(rbind, c(summary_stats, make.row.names=FALSE))
  summary_stats <- summary_finalres <- summary_stats[!summary_stats[,"Region"] %in% "make.row.names",]
  
  #CALCULATING ADDITIONAL TRENDS PER REGION (rather than PER PIXEL, which is SKEWED)! GET BACK to this for future updates of mean_col!
  if(isTRUE(extra_trends) & !is.na(mean_col)) {
    print("Calculating additional trends per summarised region (rather than per pixel)...")
   #summary_trendlist <- split(summary_stats, f = summary_stats[,"Region"])
    summary_trends <- summary_stats
    summary_trends <- summary_trends[summary_trends[,"Timevar"] != mean_col,]
    summary_trends <- summary_trends[order(summary_trends[,"Region"]),]
    summary_trends[,"Region"] <- factor(summary_trends[,"Region"], levels=unique(summary_trends[,"Region"]))
    summary_trends[,"TempID"] <- 1:nrow(summary_trends[summary_trends[,"Region"]==levels(summary_trends[,"Region"])[1],])
    region_lst <- unique(levels(summary_trends[,"Region"]))
    summary_lmlist <- lapply(levels(summary_trends[,"Region"]), function(x) summary(lm(Mean ~ TempID, data=summary_trends[summary_trends[,"Region"]==x,])))
    summary_trends <- lapply(summary_lmlist, function(x) {data.frame(R2=x$r.squared, adj_R2=x$adj.r.squared, Slope=x$coefficients[2,1], Slope_SE=x$coefficients[2,2], t_val=x$coefficients[2,3], p_val=x$coefficients[2,4])})
    summary_trends <- do.call(rbind, summary_trends)
    summary_trends <- cbind(Region=region_lst, summary_trends)
  }
  
  if(plot_oob==FALSE) {
    summary_stats <- summary_stats[!summary_stats[,"Region"] %in% "Out of bounds",]
  }
  
  #This line is used to determine which bar_varlabs to use in cases where points were NOT detected in every polygon, but only some!
  varlab_indvec <- which(unique(poly_plainvec[,"name"]) %in% unique(as.character(summary_stats[,"Region"])))
  
  if(!any(bar_varlabs %in% "default") & plot_oob==TRUE) {
    summary_stats[,"Bar_Labs"] <- rep(c(bar_varlabs[varlab_indvec], "OOB"), length(sat_vars))
  } else if(!any(bar_varlabs %in% "default") & plot_oob==FALSE) {
    summary_stats[,"Bar_Labs"] <- rep(bar_varlabs[varlab_indvec], length(sat_vars))
  } else if(any(bar_varlabs %in% "default")) {
    summary_stats[,"Bar_Labs"] <- summary_stats[,"Region"]
  }
  
  summary_stats <- summary_stats[order(summary_stats[,"Region"]),] #Necessary to order summary_stats to correctly use the RANKING functionality!
  summary_stats[,"Region"] <- factor(summary_stats[,"Region"], levels=unique(summary_stats[,"Region"]))
  
  #For cases where NOT ALL POINT (e.g. HBI) VARIABLES ARE DETECTABLE IN ALL REGIONS!
  summary_stats[is.nan(summary_stats[,"Mean"]), "Mean"] <- 0
  summary_stats[is.na(summary_stats[,"Mean"]), "Mean"] <- 0
  if(plot_type!="none") {
  if(plot_type=="heat_rank") {
    
    if(is.na(mean_col)) {
      rank_vec <- rep("oob", nrow(summary_stats))
      
      for(i in unique(summary_stats[,"Region"])) {
        
        summ_sub <- summary_stats[summary_stats[,"Region"] %in% i,]
        ref_low <- ref_high <- summ_sub[1, "Mean"]
        
        for(j in which(summary_stats[,"Region"] %in% i)) {
          
          ref_value <- summary_stats[j, "Mean"]
          rank_vec[j] <- ifelse(ref_value > ref_high, "rhigh", ifelse(ref_value < ref_low, "rlow", "oob"))
          
          if(ref_value > ref_high) {ref_high <- ref_value}
          if(ref_value < ref_low) {ref_low <- ref_value}
          
        }
      }
      
    } else if(!is.na(mean_col)) {
      
      summary_nomean <- summary_stats[!summary_stats[,"Timevar"] %in% mean_col,]
      summary_meancol <- summary_stats[summary_stats[,"Timevar"] %in% mean_col,]
      summary_meancol[,"Record_Values"] <- "oob"
      
      rank_vec <- rep("oob", nrow(summary_nomean))
      
      for(i in unique(summary_nomean[,"Region"])) {
        
        summ_sub <- summary_nomean[summary_nomean[,"Region"] %in% i,]
        ref_low <- ref_high <- summ_sub[1, "Mean"]
        
        for(j in which(summary_nomean[,"Region"] %in% i)) {
          
          ref_value <- summary_nomean[j, "Mean"]
          rank_vec[j] <- ifelse(ref_value > ref_high, "rhigh", ifelse(ref_value < ref_low, "rlow", "oob"))
          
          if(ref_value > ref_high) {ref_high <- ref_value}
          if(ref_value < ref_low) {ref_low <- ref_value}
          
        }
      }
    }
    
      summary_nomean[,"Record_Values"] <- rank_vec
      summary_stats <- rbind(summary_nomean, summary_meancol)
      summary_stats <- summary_stats[order(summary_stats[,"Region"], summary_stats[,"Timevar"]),]
      summary_stats[,"Record_Values"] <- factor(summary_stats[,"Record_Values"], levels=c("oob", "rlow", "rhigh"))
      
      heatrank_layer <- geom_text(aes(label=round(Mean, 1), colour=Record_Values), show.legend = FALSE)
      
  } else if(plot_type=="heat") {
    heatrank_layer <- geom_text(aes(label=round(Mean, 1)), show.legend = FALSE)
  }
  
  #Plotting the summarised data
  #Plot_Data <- melt(summary_stats, id.vars=c(x_var, names(cluster_data)), measure.vars=c(sat_vars))
  #Plot_Data <- as.data.frame(melt(Plot_Data, id.vars=c(x_var, "value", "variable")))
  
  if(plot_by=="polygon") {
    if(facet_plots==TRUE & plot_type!="heat" | facet_plots==TRUE & plot_type!="heat_rank") {
      
      #plot_sub <- reshape2::melt(summary_stats, measure.vars="Timevar")
      plot_sub <- summary_stats
      
      if(plot_type=="point") {
        main_layer <- geom_point(aes_string(x="Timevar", y="Mean"), colour=plot_cols[[6]][4], pch=21, fill=plot_cols[[5]][1])
      } else if(plot_type=="bar") {
        main_layer <- geom_bar(stat="identity", aes_string(x="Timevar", y="Mean"), colour=plot_cols[[6]][4], fill=plot_cols[[5]][1], width=0.6)
      }
      
      Plot_List[["Facet Plot (by polygon)"]] <- ggplot(data=plot_sub) +
        main_layer +
        scale_x_discrete(labels=sat_varlabs) +
        labs(y=y_lab, title="Facet Plot (by polygon)") +
        facet_wrap(~Bar_Labs, scales="free_y") +
        coord_cartesian(expand=TRUE) +
        theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
              strip.background = element_rect(fill="grey95", colour="grey40"),
              axis.text.y = element_text(colour="black"),
              axis.title.x=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
              panel.background=element_rect(fill=NA, colour="grey50"),
              axis.line = element_line(colour="black"))
      
      if(any(plot_extras %in% "sd")) {
        
        Plot_List[["Facet Plot (by polygon)"]] <- Plot_List[["Facet Plot (by polygon)"]] + geom_errorbar(aes(x=Timevar, ymin=Mean-StDev, ymax=Mean+StDev), width=0.2, show.legend = TRUE)
        
      }
      
      if(any(plot_extras %in% "qrt") & plot_type=="bar") {
        
        Plot_List[["Facet Plot (by polygon)"]] <- Plot_List[["Facet Plot (by polygon)"]] + list(geom_errorbar(aes(x=Timevar, ymin=Q1.25., ymax=Q1.25.), colour=plot_cols[[6]][1], size=1.2, width=0.6, show.legend = TRUE),
                                                                                                geom_errorbar(aes(x=Timevar, ymin=Q2, ymax=Q2), colour=plot_cols[[6]][2], size=1.2, width=0.6, show.legend = TRUE),
                                                                                                geom_errorbar(aes(x=Timevar, ymin=Q3.75., ymax=Q3.75.), colour=plot_cols[[6]][3], size=1.2, width=0.6, show.legend = TRUE))
        
      }
      
      if(any(plot_extras %in% "n") & plot_type=="bar") {
        
        Plot_List[["Facet Plot (by polygon)"]] <- Plot_List[["Facet Plot (by polygon)"]] + geom_text(aes(x=Timevar, y=Mean, label=paste0("n = ", Sample_Size)), colour="black", vjust=plot_opts[2], size=3.5)
        
      }
      
    } else if(facet_plots==FALSE & plot_type!="heat" | facet_plots==FALSE & plot_type!="heat_rank") {
      
      for(i in as.character(unique(summary_stats[,"Region"]))) {
        
        plot_sub <- summary_stats[summary_stats[,"Region"] %in% i,]
        pltname <- paste0("Summary for ", i)
        
        if(plot_type=="bar") {
          main_layer <- geom_bar(stat="identity", aes_string(x="Timevar", y="Mean"), colour=plot_cols[[6]][4], fill=plot_cols[[5]][1], width=0.6) 
        } else if(plot_type=="point") {
          main_layer <- geom_point(aes_string(x="Timevar", y="Mean"), colour=plot_cols[[6]][4], pch=21, fill=plot_cols[[5]][1]) 
        }
        
        Plot_List[[pltname]] <- ggplot(data=plot_sub) + 
          main_layer +
          scale_x_discrete(labels=sat_varlabs) +
          labs(y=y_lab, title=pltname) +
          theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
                axis.text.y = element_text(colour="black"),
                axis.title.x=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
                panel.background=element_rect(fill=NA, colour="grey50"),
                axis.line = element_line(colour="black"))
        
        if(any(plot_extras %in% "sd")) {
          
          Plot_List[[pltname]] <- Plot_List[[pltname]] + geom_errorbar(aes(x=Timevar, ymin=Mean-StDev, ymax=Mean+StDev), width=0.2, show.legend = TRUE)
          
        }
        
        if(any(plot_extras %in% "qrt") & plot_type=="bar") {
          
          Plot_List[[pltname]] <- Plot_List[[pltname]] + list(geom_errorbar(aes(x=Timevar, ymin=Q1.25., ymax=Q1.25.), colour=plot_cols[[6]][1], size=1.2, width=0.6, show.legend = TRUE),
                                                              geom_errorbar(aes(x=Timevar, ymin=Q2, ymax=Q2), colour=plot_cols[[6]][2], size=1.2, width=0.6, show.legend = TRUE),
                                                              geom_errorbar(aes(x=Timevar, ymin=Q3.75., ymax=Q3.75.), colour=plot_cols[[6]][3], size=1.2, width=0.6, show.legend = TRUE))
          
        }
        
        if(any(plot_extras %in% "n") & plot_type=="bar") {
          
          Plot_List[[pltname]] <- Plot_List[[pltname]] + geom_text(aes(x=Timevar, y=Mean, label=paste0("n = ", Sample_Size)), colour="black", vjust=plot_opts[2], size=3.5)
          
        }
        
      }
      
    } else if(facet_plots=="summarise") {
      
      sum_cols <- scales::seq_gradient_pal(plot_cols[[5]])(seq(0,1,length.out=length(sat_vars)))
      
      if(plot_type=="point" | plot_type=="bar") {
        
        Plot_List[["Total summary (by polygon)"]] <- ggplot(data=summary_stats, aes_string(x="Region", y="Mean")) +
          geom_point(aes_string(fill="Timevar"), pch=21, colour="black") +
          scale_x_discrete(labels=sat_varlabs) +
          scale_fill_manual(name="Region", values=plot_cols[[1]]) +
          labs(y=y_lab, title="Total summary (by polygon)") +
          #coord_cartesian(ylim=c(75, 100)) +
          theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
                axis.text.y = element_text(colour="black"),
                axis.title.x=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
                panel.background=element_rect(fill=NA, colour="grey50"),
                axis.line = element_line(colour="black"))
        
      } else if(plot_type=="heat" | plot_type=="heat_rank") {
        
        Plot_List[["Total summary (by polygon)"]] <- ggplot(data=summary_stats, aes_string(x="Region", y="Timevar")) +
          geom_tile(aes_string(fill="Mean"), colour="black") +
          heatrank_layer +
          scale_y_discrete(labels=sat_varlabs) +
          scale_x_discrete(labels=unique(summary_stats[,"Bar_Labs"])) +
          scale_fill_gradientn(name="Mean", colours=plot_cols[[5]]) +
          scale_colour_manual(values=plot_cols[[7]]) +
          labs(title="Total summary (by polygon)") +
          coord_equal(expand=FALSE) +
          guides(fill=guide_colorbar(frame.colour = "black", ticks.colour = "black"), title=y_lab) +
          #coord_cartesian(ylim=c(75, 100)) +
          theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
                axis.text.y = element_text(colour="black"),
                axis.title=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
                panel.background=element_rect(fill=NA, colour="grey50"),
                axis.line = element_line(colour="black"))
      }
      
    }
    
    
  } else if(plot_by=="varib") {
    if(facet_plots==TRUE & plot_type!="heat" | facet_plots==TRUE & plot_type!="heat_rank") {
      
      #plot_sub <- reshape2::melt(summary_stats, measure.vars="Timevar")
      plot_sub <- summary_stats
      
      if(plot_type=="point") {
        main_layer <- geom_point(aes_string(x="Region", y="Mean"), colour=plot_cols[[6]][4], pch=21, fill=plot_cols[[5]][1])
      } else if(plot_type=="bar") {
        main_layer <- geom_bar(stat="identity", aes_string(x="Region", y="Mean"), colour=plot_cols[[6]][4], fill=plot_cols[[5]][1], width=0.6)
      }
      
      levels(plot_sub[,"Timevar"]) <- sat_varlabs #ENSURES CORRECT LABELS BEING PRINTED ON FACET STRIPS!
      
      Plot_List[["Facet Plot (by variable)"]] <- ggplot(data=plot_sub) +
        main_layer +
        scale_x_discrete(labels=unique(plot_sub[,"Bar_Labs"])) +
        labs(y=y_lab, title="Facet Plot (by variable)") +
        facet_wrap(~Timevar, scales="free_y") +
        coord_cartesian(expand=TRUE) +
        theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
              strip.background = element_rect(fill="grey95", colour="grey40"),
              axis.text.y = element_text(colour="black"),
              axis.title.x=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
              panel.background=element_rect(fill=NA, colour="grey50"),
              axis.line = element_line(colour="black"))
      
      if(any(plot_extras %in% "sd")) {
        
        Plot_List[["Facet Plot (by variable)"]] <- Plot_List[["Facet Plot (by variable)"]] + geom_errorbar(aes(x=Region, ymin=Mean-StDev, ymax=Mean+StDev), width=0.2, show.legend = TRUE)
        
      }
      
      if(any(plot_extras %in% "qrt") & plot_type=="bar") {
        
        Plot_List[["Facet Plot (by variable)"]] <- Plot_List[["Facet Plot (by variable)"]] + list(geom_errorbar(aes(x=Region, ymin=Q1.25., ymax=Q1.25.), colour=plot_cols[[6]][1], size=1.2, width=0.6, show.legend = TRUE),
                                                                                                  geom_errorbar(aes(x=Region, ymin=Q2, ymax=Q2), colour=plot_cols[[6]][2], size=1.2, width=0.6, show.legend = TRUE),
                                                                                                  geom_errorbar(aes(x=Region, ymin=Q3.75., ymax=Q3.75.), colour=plot_cols[[6]][3], size=1.2, width=0.6, show.legend = TRUE))
        
      }
      
      if(any(plot_extras %in% "n") & plot_type=="bar") {
        
        Plot_List[["Facet Plot (by variable)"]] <- Plot_List[["Facet Plot (by variable)"]] + geom_text(aes(x=Region, y=Mean, label=paste0("n = ", Sample_Size)), colour="black", vjust=plot_opts[2], size=3.5)
        
      }
      
    } else if(facet_plots==FALSE & plot_type!="heat" | facet_plots==FALSE & plot_type!="heat_rank") {
      
      for(i in seq_along(sat_vars)) {
        
        plot_sub <- summary_stats[summary_stats[,"Timevar"] %in% sat_vars[i],]
        pltname <- paste0("Summary for ", sat_varlabs[i])
        
        if(plot_type=="bar") {
          main_layer <- geom_bar(stat="identity", aes_string(x="Region", y="Mean"), colour=plot_cols[[6]][4], fill=plot_cols[[5]][1], width=0.6) 
        } else if(plot_type=="point") {
          main_layer <- geom_point(aes_string(x="Region", y="Mean"), colour=plot_cols[[6]][4], pch=21, fill=plot_cols[[5]][1]) 
        }
        
        Plot_List[[pltname]] <- ggplot(data=plot_sub) + 
          main_layer +
          scale_x_discrete(labels=plot_sub[,"Bar_Labs"]) +
          labs(y=y_lab, title=pltname) +
          theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
                axis.text.y = element_text(colour="black"),
                axis.title.x=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
                panel.background=element_rect(fill=NA, colour="grey50"),
                axis.line = element_line(colour="black"))
        
        
        if(any(plot_extras %in% "sd")) {
          
          Plot_List[[pltname]] <- Plot_List[[pltname]] + geom_errorbar(aes(x=Region, ymin=Mean-StDev, ymax=Mean+StDev), width=0.2, show.legend = TRUE)
          
        }
        
        if(any(plot_extras %in% "qrt") & plot_type=="bar") {
          
          Plot_List[[pltname]] <- Plot_List[[pltname]] + list(geom_errorbar(aes(x=Region, ymin=Q1.25., ymax=Q1.25.), colour=plot_cols[[6]][1], size=1.2, width=0.6, show.legend = TRUE),
                                                              geom_errorbar(aes(x=Region, ymin=Q2, ymax=Q2), colour=plot_cols[[6]][2], size=1.2, width=0.6, show.legend = TRUE),
                                                              geom_errorbar(aes(x=Region, ymin=Q3.75., ymax=Q3.75.), colour=plot_cols[[6]][3], size=1.2, width=0.6, show.legend = TRUE))
          
        }
        
        if(any(plot_extras %in% "n") & plot_type=="bar") {
          
          Plot_List[[pltname]] <- Plot_List[[pltname]] + geom_text(aes(x=Region, y=Mean, label=paste0("n = ", Sample_Size)), colour="black", vjust=plot_opts[2], size=3.5)
          
        }
        
      }
      
    } else if(facet_plots=="summarise") {
      
      if(plot_type=="point" | plot_type=="bar") {
        
        Plot_List[["Total summary (by variable)"]] <- ggplot(data=summary_stats, aes_string(x="Timevar", y="Mean")) +
          geom_point(aes_string(fill="Bar_Labs"), pch=21, colour="black") +
          scale_x_discrete(labels=sat_varlabs) +
          scale_fill_manual(name="Region", values=plot_cols[[1]]) +
          labs(y=y_lab, title="Total summary (by variable)") +
          #coord_cartesian(ylim=c(75, 100)) +
          theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
                axis.text.y = element_text(colour="black"),
                axis.title.x=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
                panel.background=element_rect(fill=NA, colour="grey50"),
                axis.line = element_line(colour="black"))
        
      } else if(plot_type=="heat" | plot_type=="heat_rank") {
        
        Plot_List[["Total summary (by variable)"]] <- ggplot(data=summary_stats, aes_string(x="Timevar", y="Region")) +
          geom_tile(aes_string(fill="Mean"), colour="black") +
          heatrank_layer +
          scale_x_discrete(labels=sat_varlabs) +
          scale_y_discrete(labels=unique(summary_stats[,"Bar_Labs"])) +
          scale_fill_gradientn(name="Mean", colours=plot_cols[[5]]) +
          scale_colour_manual(values=plot_cols[[7]]) +
          labs(title="Total summary (by variable)") +
          coord_equal(expand=FALSE) +
          guides(fill=guide_colorbar(frame.colour = "black", ticks.colour = "black"), title=y_lab) +
          #coord_cartesian(ylim=c(75, 100)) +
          theme(axis.text.x=element_text(angle=plot_opts[1], vjust=0.75, colour="black", size=plot_opts[3]),
                axis.text.y = element_text(colour="black"),
                axis.title=element_blank(),
                panel.grid.minor=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_line(colour="grey65",linetype="dashed"),
                panel.background=element_rect(fill=NA, colour="grey50"),
                axis.line = element_line(colour="black"))
        
      }
    }
    
  }
  }
  
  if(print_plots==TRUE) {
    print(Plot_List)
  }
  
  if(export_plots=="pdf" & is.character(export_path)) {
    print("Exporting plots...")
    pdf(paste(export_path, "/CLIM_Regional_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin") , ".pdf", sep=""),
        width = width, height = height, pointsize = point_size)
    print(Plot_List)
    dev.off()
    
  } else if(export_plots=="png" & is.character(export_path)) {
    
    for (i in names(Plot_List)) {
      png(filename=paste("CLIM_Regional_",i,".png",sep=""),
          type="cairo", units="in",
          width=width, height=height,
          pointsize=point_size,
          res=dpi)
      print(Plot_List[[i]])
      dev.off()
    }
    
  }
  print("Exporting regional summary statistics as a .csv file...")
  write.csv(summary_finalres, paste0(export_path, "/Regional_Data_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin"), ".csv"))
  
  if(isTRUE(extra_trends) & !is.na(mean_col)) {
    print("Exporting additional trend statistics (by region) as a .csv file...")
    write.csv(summary_trends, paste0(export_path, "/Regional_AddTrends_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin"), ".csv"))
  }
  
  print("Processing complete!")
  
  return(list(regional_data=summary_finalres, regional_plots=Plot_List))
}
