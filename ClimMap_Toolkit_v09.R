#CLIMAP TOOLKIT v0.9 (Nov 2019)
#Author e-mail: denizcan.koseoglu@gmail.com (Deniz Koseoglu)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Converting dates within filenames from yyyy-mm-dd to yyyy-day_of_year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_ndate <- function(file_path) {

namechg_vec <- list.files(file_path)
name_numvec <- sub(".*([[:digit:]]{8}).*", "\\1", namechg_vec)

name_numvec <- sapply(name_numvec, function(x) as.POSIXlt(x, format="%Y%m%d"))
name_numyear <- name_numvec[grep(".year$", names(name_numvec))]
name_numdoy <- name_numvec[grep(".yday$", names(name_numvec))]

name_numrepl <- paste0(1900 + as.numeric(paste(name_numyear)), sapply(as.numeric(paste(name_numdoy)) + 1, function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x)))))

new_fnames <- sapply(1:length(namechg_vec), function(x) gsub("[[:digit:]]{8}", name_numrepl[x], namechg_vec[x]))

menu_yn <- menu(c("Yes", "No"), title="Are you sure you want to rename the files? This action is permanent!")

if(menu_yn==TRUE) {
  file.rename(list.files(file_path, full.names = TRUE), paste0(file_path, new_fnames))
  print("Files renamed!")
} else if(menu_yn==FALSE) {
  print("Aborting...")
}
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Converting .SHPs manually drawn in Ocean Data View (ODV) to a usable format...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#NOT YET INCLUDED (WIP)!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Estimate phytoplankton carbon, i.e. C(phyto), from bbp(443) 
#using the method of Behrenfeld et al. (2005) or Graff et al. (2016), labelled BH05 and GF16, respectively.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_cphyto <- function(bbp_data, algo="BH05", slp=NA, incpt=0, bkgrd=NA, coord_vars=c("Longitude", "Latitude"), ex_path=getwd()) { 
  
  #Install and load the required libraries 
  list.of.packages <- c("data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  invisible(lapply(c(list.of.packages, "parallel"), require, character.only = TRUE))
  
  library(data.table)
  
  if(is.character(bbp_data)) {
    
    main_data <- fread(bbp_data, data.table=FALSE)
    print(paste0("BBP data successfully loaded from: ", bbp_data, "!"))
    
  } else if(is.object(bbp_data)) {
    
    main_data <- as.data.frame(bbp_data)
    print("BBP data successfully loaded from the R environment!")
  }
  
  if(algo=="BH05" & all(is.na(c(slp, bkgrd)))) {
    
    slope_val <- 13000
    intercept_val <- 0
    background_val <- 0.00035
    
  } else if(algo=="GF16" & all(is.na(c(slp, bkgrd)))) {
    
    slope_val <- 12128
    intercept_val <- 0.59
    background_val <- 0
    
  } else if(!any(algo %in% c("BH05", "GF16")) & !any(is.na(c(slp, bkgrd))) & all(is.numeric(c(slp, bkgrd)))) {
    
    slope_val <- slp
    intercept_val <- incpt
    background_val <- bkgrd
    algo <- "Custom"
    
  } else if(!any(algo %in% c("BH05", "GF16")) & !any(is.na(slp, bkgrd))) {
    
    stop("If no algorithm to use is specified (BH05 or GF16), please provide the slope, background value (and, optionally, intercept) of the bbp-Chla relationship!")
  }
  
  #bckup_data <- main_data
  print("Calculating C(phyto) from bbp...")
  main_data[main_data<0.00035 & !is.na(main_data)] <- 0.00036
  main_data[,!colnames(main_data) %in% coord_vars] <- slope_val * (main_data[,!colnames(main_data) %in% coord_vars] - background_val) + intercept_val
  print("Done!")
  
  print("Removing C(phyto) values below zero...")
  main_data[,!colnames(main_data) %in% coord_vars][main_data[, !colnames(main_data) %in% coord_vars] < 0] <- NA
  
  colnames(main_data) <- gsub("bbp_443_giop", "cphyto", colnames(main_data))
  
  if(!is.na(ex_path) & is.character(ex_path)) {
    print(paste0("Exporting data as a .CSV file to: ", ex_path, "..."))
    fwrite(x=main_data, file=paste0(ex_path, "/", algo, "_Cphyto_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv"), na=NA)
  }
  print("Processing complete!")
  return(main_data)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate day length (in hours) given latitude and day of year (1-365)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_day <- function(lat_data, lat_var="Latitude", long_var="Longitude", day_rng, ex_path=NA) {
  
  #Install and load the required libraries 
  list.of.packages <- c("data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  invisible(lapply(c(list.of.packages, "parallel"), require, character.only = TRUE))
  
  library(data.table)
  if(is.character(lat_data)) {
    main_data <- fread(lat_data, data.table=FALSE)
    print("Data successfully loaded from file!")
  } else if(is.object(lat_data)) {
    main_data <- lat_data
    print("Data successfully loaded from the R environment!")
  }
  
  #Calculate day length (Kirk et al., 1994, page 35; see Oregon University Ocean Productivity website)
  print("Calculating day length...")
  coords <- main_data[,c(long_var, lat_var)]
  latdummy <- main_data[,!colnames(main_data) %in% c(long_var, lat_var)]
  latdummy[,1:ncol(latdummy)] <- coords[,lat_var]/180*pi
  daydummy <- do.call(rbind, replicate(nrow(latdummy), day_rng/365*2*pi, simplify = FALSE))
  
  rm(main_data)
  
  daydummy <- (0.39637 - 22.9133*cos(daydummy) + 4.02543*sin(daydummy) - 0.38720*cos(2*daydummy) + 0.05200*sin(2*daydummy))*pi/180
  r_res <- -tan(latdummy)*tan(daydummy)
  
  rm(latdummy)
  rm(daydummy)
  
  r_res[r_res>1] <- 0
  #if(!all(abs(r_res)<1, na.rm = TRUE)) {stop("Results don't fall within the allowed range (-1, 1)! Aborting...")}
  r_res[abs(r_res)<1] <- 24*acos(r_res[abs(r_res)<1])/pi
  r_res[r_res<=-1] <- 24
  r_res <- cbind.data.frame(coords, r_res)
  print("Day length calculation complete!")
  
  if(!is.na(ex_path) & is.character(ex_path)) {
    print(paste0("Exporting data as a .CSV file to: ", ex_path, "..."))
    fwrite(r_res, paste0(ex_path, "/DayLength_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv"), na=NA)
  }
  return(r_res)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculating division rate of phytoplankton (u, day^-1) using Behnrenfeld et al., 2005 and Behrenfeld et al., 2017 algorithms 
#(with derivations from CAFE model, Silsbe et al., 2016 and Behrenfeld et al., 2016)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_divrate <- function(chla=NA, cphyto, kd490, ipar, par, sst, zeu=NA, dayl, mld, aph, q_val=2, method=c("BH05", "BH17"), chl_c="sat",
                         coord_vars=c("Longitude", "Latitude"), cons_par=TRUE, div_nums=c(64,1), ex_path=getwd(), res="depth_only") {
  #Install and load the required libraries 
  list.of.packages <- c("data.table", "pracma")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  invisible(lapply(c(list.of.packages, "parallel"), require, character.only = TRUE))
  
  library(data.table)
  library(pracma)
  
  #Load data
  input_list <- list(chla=chla, cphyto=cphyto, kd490=kd490, ipar=ipar, par=par, sst=sst, zeu=zeu, dayl=dayl, mld=mld, aph=aph)
  data_list <- list()
  
  if(any(sapply(input_list, is.character))) {
    for(i in which(sapply(input_list, is.character))) {
      data_list[[names(input_list)[i]]] <- fread(input_list[[i]], data.table=FALSE)
    }
    print(paste0("Data loaded from files..."))
  }
  if(any(sapply(input_list, is.object))) {
    for(i in which(sapply(input_list, is.object))) {
      data_list[[names(input_list)[i]]] <- input_list[[i]]
    }
    print("Data loaded from the R environment...")
  }
  
  if(any(is.na(input_list))) {
    for(i in which(sapply(input_list, is.na))) {
      data_list[[names(input_list)[i]]] <- input_list[[i]]
    }
  }
  
  rm(input_list)
  
  #Run checks
  if(length(unique(lapply(data_list, FUN=dim)))!=1) {
    stop("Dimensions of input variables are not equal! Stopping...")
  }
  
  if(length(unique(lapply(data_list, function(x) x[,coord_vars])))!=1) {
    stop("Coordinates are not identical for all variables! Stopping...")
  }
  
  #if(mthread>detectCores()) {
  #  print("More cores selected for multi-threading than are available! Setting to maximum available CPU cores...")
  #  mthread <- detectCores()
  #}
  
  if(!dir.exists(ex_path)) {
    stop("Export path does not exist! Please provide a correct path...")
  }
  
  #Remove coordinates from all data
  coord_df <- cphyto[,coord_vars]
  for(i in seq_along(data_list)) {
    data_list[[i]] <- data_list[[i]][,!colnames(data_list[[i]]) %in% coord_vars]
  }
  
  chla <- data_list[[which(names(data_list)=="chla")]]
  cphyto <- data_list[[which(names(data_list)=="cphyto")]]
  kd490 <- data_list[[which(names(data_list)=="kd490")]]
  ipar <- data_list[[which(names(data_list)=="ipar")]]
  par <- data_list[[which(names(data_list)=="par")]]
  sst <- data_list[[which(names(data_list)=="sst")]]
  zeu <- data_list[[which(names(data_list)=="zeu")]]
  dayl <- data_list[[which(names(data_list)=="dayl")]]
  mld <- data_list[[which(names(data_list)=="mld")]]
  aph <- data_list[[which(names(data_list)=="aph")]]
  rm(data_list)
  
  #Create a list to store results
  reslist <- list()
  
  #Calculate variables common to both BH05 and BH17 models
  kd_par <- 0.0665+0.874*kd490-0.00121/kd490
  colnames(kd_par) <- gsub("Kd_490", "kd_par", colnames(kd_par))
  
  i_ml <- par/dayl*exp(-kd_par*(mld/2))
  colnames(i_ml) <- gsub("kd_par", "i_ml", colnames(kd_par))
  
  if(all(is.na(zeu))) {
    print("Euphotic zone depth (Zeu) not provided! Estimating as per Behrenfeld et al. (2016)...")
    #Alternative from Boss and Behrenfeld, 2010: zeu <- log(0.415/0.98*par)*(zeu/log(0.01))
    zeu <- abs(log(0.415/par)/kd_par) #LOG FUNCTION IS NATURAL LOG BY DEFAULT IN R!!! Use log10() or log2() otherwise!
    zeu[dayl==0] <- 1 #Zeu assumed to be 1m during polar night!
    colnames(zeu) <- gsub("kd_par", "zeu", colnames(kd_par))
  } else if(!all(is.na(zeu))) {
    zeu <- zeu[,!colnames(zeu) %in% coord_vars]
  }
  
  #Calculate Chl/C ratio either from satellite data (MODIS Chla or converted APH/BBP ratio-based) or using photoacclimation model of Behrenfeld et al., 2016
  if(chl_c=="sat") {
    chlc_ratio <- chla/cphyto
    colnames(chlc_ratio) <- gsub("chlor_a", "chlc_ratio", colnames(chlc_ratio))
  } else if(chl_c=="aph_ratio" & !all(is.na(aph))) {
    aph <- aph[,!colnames(aph) %in% coord_vars]
    chlc_ratio <- (aph/0.05582)/cphyto
    colnames(chlc_ratio) <- gsub("aph_443_giop", "chlc_ratio", colnames(chlc_ratio))
  } else if(chl_c=="BH16") {
    Ek <- kd_par
    sub1 <- mld<=zeu & !is.na(mld) & !is.na(zeu)
    sub2 <- mld>zeu & !is.na(mld) & !is.na(zeu)
    Ek[sub1] <- 19*exp(0.038*(par[sub1]/dayl[sub1])^0.45/kd_par[sub1])*((1+exp(-0.15*(par[sub1]/dayl[sub1])))/(1+exp(-3*i_ml[sub1])))
    Ek[sub2] <- 19*exp(0.038*(par[sub2]/dayl[sub2])^0.45/kd_par[sub2])*1
    colnames(Ek) <- gsub("kd_par", "chlc_ratio", colnames(Ek))
    chlc_ratio <- (Ek*0.8)^-1
  }
  
  #Add Chla/C ratio to output and export list
  reslist[["Chl_C_ratio"]] <- as.data.frame(cbind(coord_df, chlc_ratio))
  
  #Calculate variables and phytoplankton division rate specific to either model
  if(any(method %in% "BH05")) {
    print("Calculating phytoplankton division rate using the BH05 model...")
    divrate_bh05 <- 2*(chlc_ratio/(0.022+(0.045-0.022)*exp(-3*i_ml)))*(1-exp(-3*i_ml))
    colnames(divrate_bh05) <- paste0("divrate_bh05.",seq(1:ncol(divrate_bh05)))           #TEMPORARY
    reslist[["DivRate_BH05"]] <- as.data.frame(cbind(coord_df, divrate_bh05))
  } 
  
  if(any(method %in% "BH17")) {
    print("Calculating ML phytoplankton division rate using the BH17 model...")
    
    #Calculate Ek (dependent on balance between MLD and Zeu)
    if(chl_c!="BH16") {
      Ek <- kd_par
      sub1 <- mld<=zeu & !is.na(mld) & !is.na(zeu)
      sub2 <- mld>zeu & !is.na(mld) & !is.na(zeu)
      Ek[sub1] <- 19*exp(0.038*(par[sub1]/dayl[sub1])^0.45/kd_par[sub1])*((1+exp(-0.15*(par[sub1]/dayl[sub1])))/(1+exp(-3*i_ml[sub1])))
      Ek[sub2] <- 19*exp(0.038*(par[sub2]/dayl[sub2])^0.45/kd_par[sub2])*1
      colnames(Ek) <- gsub("kd_par", "Ek", colnames(Ek))
    }
    
    Ek <- 0.0036*Ek #Converting Ek from micromol photons m^-2 s^-1 to mol photons m^-2 hr^-1, use 0.0864 to convert to mol photons m^-2 d^-1 (Silsbe et al., 2016)
    
    pc_max <- (4.6*q_val^(sst/10-20/10))*chlc_ratio
    ac <- pc_max/Ek
    
    divrate_bh17 <- as.data.frame(matrix(0, nrow=nrow(cphyto), ncol=ncol(cphyto)))
    colnames(divrate_bh17) <- paste0("divrate_bh17.",seq(1:ncol(divrate_bh17))) 
    #colnames(divrate_bh17) <- gsub("chlor_a", "divrate_bh17", colnames(chla))
    
    if(cons_par==TRUE) { ipar <- pi*(par/dayl)/2} else ipar <- ipar*60*60 #If iPAR integral through the day must equal daily integrated MODIS PAR, set cons_par to TRUE, else MODIS iPAR will be used and assumed to represent PAR at solar noon
    
    
    if(res=="time_depth") {
      
      #METHOD 1: NON-VECTORISED, AUTOMATED (Integral2 from PRACMA), SINGLE-THREADED APPROACH (SLOWER!)
      
      int_func <- function(t,z) {pc_max[i,j]*tanh((ac[i,j]*((ipar[i,j])*sin(pi/dayl[i,j]*t)*exp(-kd_par[i,j]*z)))/pc_max[i,j])}
      
      for(i in 1:nrow(divrate_bh17)) {
        
        svMisc::progress(i, max.value=nrow(divrate_bh17), progress.bar = FALSE)
        Sys.sleep(1/nrow(divrate_bh17))
        if(i==nrow(divrate_bh17)) cat("Done!\n")
        
        for(j in 1:ncol(divrate_bh17)) {
          #divrate_bh17[i,j] <- tryCatch(adaptIntegrate(int_func, lowerLimit = c(0,0), upperLimit = c(dayl[i,j], mld[i,j]))$integral, error=function(err) NA)
          #int2(int_func, a=c(0,0), b=c(24,500))
          divrate_bh17[i,j] <- tryCatch(integral2(int_func, xmin=0, xmax=dayl[i,j], ymin=0, ymax=mld[i,j])$Q, error=function(err) NA)
          #divrate_bh17[i,j] <- tryCatch(integrate(function(z) {pc_max[i,j]*tanh((ac[i,j]*par[i,j]*exp(-kd_par[i,j]*z))/pc_max[i,j])}, lower = 0, upper = mld[i,j])[["value"]]/mld[i,j], error=function(err) NA)
        }
      }
      divrate_bh17 <- divrate_bh17/mld
      
      #METHOD 2: NON-VECTORIZED, MANUAL, SINGLE-THREADED APPROACH! This is only an approximation, using 1m and 1hr increments!
      #time_ref <- Sys.time()
      
      #int_func <- function(x) sum(pc_max[i,j]*tanh((ac[i,j]*((ipar[i,j])*sin(pi/dayl[i,j]*day_seq)*exp(-kd_par[i,j]*x)))/pc_max[i,j]))
      
      #for(i in 1:nrow(divrate_bh17)) {
      
      #  svMisc::progress(i, max.value=nrow(divrate_bh17), progress.bar = FALSE)
      #  Sys.sleep(1/nrow(divrate_bh17))
      #  if(i==nrow(divrate_bh17)) cat("Done!\n")
      
      #  for (j in 1:ncol(divrate_bh17)) {
      #    day_seq <- seq(0, dayl[i,j], dayl[i,j]/div_nums[1]) #dayl[i,j]/div_nums[1]
      #    mld_seq <- seq(0, mld[i,j], div_nums[2])
      #    divrate_bh17[i,j] <- sum(sapply(mld_seq, int_func))
      #  }
      #}
      #divrate_bh17 <- divrate_bh17/mld
      #divrate_bh17 <- divrate_bh17*(dayl/div_nums[1])
      #print(Sys.time()-time_ref)
      
    } else if(res=="depth_only") {
      
      #METHOD 3: NOT TIME-RESOLVED (ONLY DEPTH RESOLVED) SINGLE INTEGRATION
      
      int_func <- function(z) {pc_max[i,j]*tanh((ac[i,j]*((par[i,j]/dayl[i,j])*exp(-kd_par[i,j]*z)))/pc_max[i,j])}
      
      for(i in 1:nrow(divrate_bh17)) {
        
        svMisc::progress(i, max.value=nrow(divrate_bh17), progress.bar = FALSE)
        Sys.sleep(1/nrow(divrate_bh17))
        if(i==nrow(divrate_bh17)) cat("Done!\n")
        
        for(j in 1:ncol(divrate_bh17)) {
          divrate_bh17[i,j] <- tryCatch(integrate(int_func, lower=0, upper=mld[i,j])$value, error=function(err) NA)
          #divrate_bh17[i,j] <- tryCatch(integrate(function(z) {pc_max[i,j]*tanh((ac[i,j]*par[i,j]*exp(-kd_par[i,j]*z))/pc_max[i,j])}, lower = 0, upper = mld[i,j])[["value"]]/mld[i,j], error=function(err) NA)
        }
      }
      divrate_bh17 <- divrate_bh17*dayl/mld
    }
    
    reslist[[paste0("DivRate_BH17_",res)]] <- as.data.frame(cbind(coord_df, divrate_bh17))
  }
  
  print(paste0("Exporting data to: ", ex_path, "..."))
  sapply(1:length(reslist), function(x) fwrite(as.data.frame(reslist[[x]]), paste0(ex_path, "/", names(reslist)[x], "_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv"), na=NA))
  print("Processing complete!")
  
  return(reslist)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Download NSIDC Sea Ice Concentration, MODIS AQUA, and CEDA (terrestrial) data from FTP servers!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_download <- function(repository="NSIDC", hemisphere="north", frequency="monthly", data_type="Chla", grid_res="9km",
                          year, month, res_path, usrname=NA, pword=NA, suppr_msgs=TRUE, opt_down=TRUE, down_mode="wb",
                          dcores=detectCores(), max_retries=6, shared_folder=TRUE) {

#Install and load the required libraries 
list.of.packages <- c("XML", "rvest", "RCurl", "curl", "purrr", "devtools", "svMisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])) install.packages(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])

if(any(new.packages %in% "threddscrawler")) devtools::install_github("BigelowLab/threddscrawler")
if(any(new.packages %in% "obpgcrawler")) devtools::install_github("BigelowLab/obpgcrawler")

invisible(lapply(c(list.of.packages, "parallel"), require, character.only = TRUE))

library(XML)
library(rvest)
library(RCurl)
library(threddscrawler)
library(obpgcrawler)
library(purrr)
library(curl)
library(parallel)
library(devtools)
library(svMisc)

if(dcores > detectCores()) {
  stop(paste0("The CPU is equipped with ", detectCores(), " processors! Please adjust the maximum value of dcores accordingly."))
}

if(any(!month %in% c("all","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) & !is.numeric(month)||any(!month %in% c(1:12)) & !is.character(month)) { #& !is.numeric(month)
  stop("Incorrect month values provided! Please use any of: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, or corresponding numerical values (1 to 12).")
}

if(month[1]=="all") {
  month_vec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
} else if(month[1]!="all") {
  month_vec <- month
}

if(any(repository %in% c("MODIS_A", "SeaWifs")) & !any(data_type %in% c("Chla", "SST", "NSST", "PAR", "iPAR", "PIC", "POC", "NFLH", "Zeu", "KD490", "BBP_GIOP",
                                                                        "BBP_s_GIOP", "Adg_GIOP", "Aph_GIOP", "BBP_GSM", "BBP_QAA"))) {
  
  if(repository=="SeaWifs" & data_type=="SST") {"SeaWifs does not have SST data! Aborting..."}
  
  stop("MODIS Aqua (and SeaWifs) only supports Chla, SST, NSST, iPAR, PAR, FLH, PIC, POC, BBP_GIOP, BBP_s_GIOP, Adg_GIOP, Aph_GIOP, 
       BBP_GSM, BBP_QAA, KD490, or Zeu as data types...")
  
} else if(repository=="NSIDC" & !any(data_type %in% c("SIC","SIE_polyline", "SIE_polygon", "SIE_median"))) {
  
  stop("NSIDC only supports SIC, SIE_polyline, SIE_polygon, or SIE_median as data types...")
  
} else if(repository=="CEDA_MO" & !any(data_type %in% c("groundfrost", "hurs", "psl", "pv", "rainfall", "sfcWind", "sun", "tas", "tasmax", "tasmin"))) {
  
  stop("CEDA_MO only supports groundfrost, hurs, psl, pv, rainfall, sfcWind, sun, tas, tasmax, or tasmin as data types...")
  
} else if(repository=="TClim" & !any(data_type %in% c("aet", "def", "swe", "q", "soil", "PDSI", "pet", "ppt", "srad", "tmax", "tmin", "vap", "vpd", "ws"))) {
  
  stop("TClim only supports aet, def, swe, q, soil, PDSI, pet, ppt, srad, tmax, tmin, vap, vpd, or ws as data types...")
  
} else if(repository=="CRU4" & !any(data_type %in% c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet"))) {
  
  stop("CRU4 only supports cld, dtr, frs, pet, pre, tmn, tmp, tmx, vap, or wet as data types...")
  
} else if(repository=="IC_CCO" & !any(data_type %in% c("Chla", "IOP", "RRS", "KD490"))) {
  
  stop("IC_CCO (v4) only suports Chla, IOP, RRS, or KD490 as data types...")
}

if(repository=="CEDA_MO" & grid_res!="1km") {
  print("Note that CEDA_MO is currently only available at 1km resolution!")
} else if(any(repository %in% c("MODIS_A", "SeaWifs")) & grid_res=="1km") {
  print("Note that only CEDA_MO is available at 1km resolution... Defaulting to 9km for MODIS!")
  grid_res <- "9km"
} else if(repository=="OC_CCI" & grid_res!="4km") {
  print("Note that IC_CCO is currently only available at 4km resolution!")
} 

if(repository=="SeaWifs" & grid_res!="9km") {
  print("SeaWifs is only available at a 9km resolution! Defaulting...")
  grid_res <- "9km"
}

if(any(repository %in% c("TClim", "CRU4", "CEDA_MO", "NSIDC"))) {print("Note that grid_res will be ignored for all repositories but MODIS and SeaWifs...")}  

if(repository=="TClim" & !any(c("monthly", "mon-30y") %in% frequency)) {
  print("TClim only supports monthly and mon-30y as data frequencies! Defaulting to monthly...")
  frequency <- "monthly"
}  

if(!any(repository %in% c("CEDA_MO", "TClim", "CRU4")) & any(frequency %in% c("mon-20y", "mon-30y"))) {
  print("Only CEDA_MO, TClim, and CRU4 support mon-20y and mon-30y as frequency values! Dafaulting to monthly frequency...")
  frequency <- "monthly"
}

if(any(repository %in% c("CEDA_MO", "CRU4", "OC_CCI")) & opt_down==TRUE) {
  print("CEDA Archive and OC_CCI downloads currently cannot be optimised! Switching to looped downloads...")
  opt_down <- FALSE
}

if(any(repository %in% c("CEDA_MO", "CRU4")) & any(is.na(c(usrname, pword)))) {
  stop("CEDA Archive requires user credentials. Please provide a username and password!")
}

if(repository=="OC_CCI") {
  print("Setting username and password for the OC_CCI (v4) dataset...")
  usrname <- "oc-cci-data"
  pword <- "ELaiWai8ae"
}

url_vec <- c()

dir_list_handle <- new_handle(ftp_use_epsv=FALSE, dirlistonly=TRUE, crlf=TRUE,
                              ssl_verifypeer=FALSE, ftp_response_timeout=30)
#dir.create(cache_dir, showWarnings=TRUE)

# Wrapping the memory fetcher (for dir listings)
s_curl_fetch_memory <- safely(curl_fetch_memory)
retry_cfm <- function(url, handle) {
  
  i <- 0
  repeat {
    i <- i + 1
    res <- s_curl_fetch_memory(url, handle=handle)
    if (!is.null(res$result)) return(res$result)
    if (i==max_retries) { stop("Too many retries... server may be under load. Alternatively, check that your URLs are correct!") }
  }
  
}

if(repository=="NSIDC") {
  res_dir <- paste(res_path, "/Data/", repository, sep="")
  cache_dir <- paste(res_dir,"/gsod_cache", sep="")
  
  if(frequency=="daily" & data_type!="SIE_median") {
    if(frequency=="daily" & data_type!="SIC") {
      stop("A daily frequency value is only supported with NSIDC when data_type is SIC or SIE_median! Please use a monthly frequency for SIE_polygon or SIE_polyline.")
    }}
  
  month_df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), Month_Num=c(1:12), Month_ID=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), Month_Day=c("001", "032", "061", "092", "122", "153", "183", "214", "245", "275", "306", "336"), stringsAsFactors = FALSE)
  
  if(data_type=="SIC") {
    url <- c("ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_V3/")
  } else if(data_type=="SIE_polyline"|data_type=="SIE_polygon"|data_type=="SIE_median") {
    url <- c("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/")
  } #else if(!any(data_type %in% c("SIC","SIE_polyline", "SIE_polygon", "SIE_median"))) {
    #stop("Incorrect data_type argument selected! For NSIDC data, acceptable values are: SIC, SIE_polyline, SIE_polygon or SIE_median!")
  #}
  
  

if(hemisphere=="north") {
  url <- paste(url, "north/", sep="")
} else if(hemisphere=="south") {
  url <- paste(url, "south/", sep="")
}

if(frequency=="daily") {
  url <- paste(url, "daily/", sep="")
} else if(frequency=="monthly") {
  url <- paste(url, "monthly/", sep="")
}

  if(data_type=="SIC") {
    if(!isTRUE(opt_down)) {
for(i in seq_along(year)) {
  print(paste("Downloading SIC files for year ", i, " of ", max(seq_along(year)), " (", year[i], "). This could take a while!", sep=""))
  
  if(frequency=="daily") {
  url_vec[i] <- paste(url,year[i],"/", sep="")
  } else if(frequency=="monthly") {
  url_vec[i] <- url
  }
  
  tmp <- retry_cfm(url_vec[i], handle=dir_list_handle)
  con <- rawConnection(tmp$content)
  filenames <- readLines(con)
  close(con)
 
    for(j in seq_along(month_vec)) {
      if(is.character(month_vec)) {
      month_ind <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec[j]])
      } else if(is.numeric(month_vec)) {
      month_ind <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec[j]])
      }
      fils_month <- grep(paste(year[i], month_ind, sep=""), filenames, value=TRUE)
      for(file_m in fils_month) {
          download.file(paste(url_vec[i], file_m, sep=""), paste(res_dir, "/Data/", data_type, "/", file_m, sep=""), quiet=suppr_msgs, mode=down_mode)
        }
    }
}
    } else if(isTRUE(opt_down)) {
      month_ind <- c()
      #url_list <- c()
      filenames <- list()
      url_list <- list()
      
      for(j in seq_along(month_vec)) {
        if(is.character(month_vec)) {
          month_ind[j] <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec[j]])
        } else if(is.numeric(month_vec)) {
          month_ind[j] <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec[j]])
        }
      }
      
      for(i in seq_along(year)) {
        print(paste("Preparing a list of SIC files for year ", i, " of ", max(seq_along(year)), " (", year[i], ")", sep=""))
        
      if(frequency=="daily") {
        url_vec[i] <- paste(url,year[i],"/", sep="")
      } else if(frequency=="monthly") {
        url_vec[i] <- url
      }
        
        tmp <- retry_cfm(url_vec[i], handle=dir_list_handle)
        con <- rawConnection(tmp$content)
        filenames[[as.character(year[i])]] <- readLines(con)
        close(con)
        filenames[[as.character(year[i])]] <- grep(paste(year[i], month_ind, sep="", collapse="|"), filenames[[as.character(year[i])]], value=TRUE)
        url_list[[as.character(year[i])]] <- paste(url_vec[i], filenames[[as.character(year[i])]], sep="")
      }
      
      filenames <- Reduce(c, filenames)
      url_list <- Reduce(c, url_list)
      
      #Final download directory for NSIDC data (nh_monthly, etc.)
      if(hemisphere=="north") {
        final_dest <- paste0("nh_", frequency, "/")
      } else if(hemisphere=="south") {
        final_dest <- paste0("sh_", frequency, "/")
      }
      
      require(parallel)
      cl <- makeCluster(dcores)
      print(paste("Downloading specified SIC files from NSIDC servers to: ", paste(res_dir, "/Data/", data_type, "/", final_dest, sep=""), ". This could take a while!", sep=""))
      
      if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", final_dest, filenames, sep=""), .scheduling="dynamic")))
      } else if(!isTRUE(suppr_msgs)) {
        clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", final_dest, filenames, sep=""), .scheduling="dynamic")
      }
    }
  } else if(data_type=="SIE_polyline"|data_type=="SIE_polygon") {
    
    filenames <- list()
    url_list <- list()
    
    for(i in seq_along(month)) {
      if(is.character(month_vec)) {
        month_val <- as.character(month_df$Month[month_df[,c("Month")] %in% month_vec[i]])
        month_ind <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec[i]])
      } else if(is.numeric(month_vec)) {
        month_val <- as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec[i]])
        month_ind <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec[i]])
      }
      url_vec[i] <- paste(url,"shapefiles/shp_extent/", month_ind, "_", month_val, "/", sep="")
      
      tmp <- retry_cfm(url_vec[i], handle=dir_list_handle)
      con <- rawConnection(tmp$content)
      filenames[[month_vec[i]]] <- readLines(con)
      close(con)
      
      if(data_type=="SIE_polyline") {
        filenames[[month_vec[i]]] <- grep("extent_", filenames[[month_vec[i]]], value=TRUE) #possible to use OR as well e.g. paste("one", "two", "three", collapse="|")
        filenames[[month_vec[i]]] <- grep(paste(as.character(year), collapse="|"), filenames[[month_vec[i]]], value=TRUE)
        filenames[[month_vec[i]]] <- grep("polyline", filenames[[month_vec[i]]], value=TRUE)
        
        url_list[[month_vec[i]]] <- paste(url_vec[i], filenames[[month_vec[i]]], sep="") # For use with package "parallel"
        
      } else if(data_type=="SIE_polygon") {
        filenames[[month_vec[i]]] <- grep("extent_", filenames[[month_vec[i]]], value=TRUE)
        filenames[[month_vec[i]]] <- grep(paste(as.character(year), collapse="|"), filenames[[month_vec[i]]], value=TRUE)
        filenames[[month_vec[i]]] <- grep("polygon", filenames[[month_vec[i]]], value=TRUE)
        
        url_list[[month_vec[i]]] <- paste(url_vec[i], filenames[[month_vec[i]]], sep="")
        
      }
    }
    
      #for(j in seq_along(year)) {
        #if(data_type=="SIE_polyline") {
        #filenames <- lapply(filenames, function(x) {grep(paste("extent.*", year[j], ".*polyline", sep=""), x, value=TRUE)}) #possible to use OR as well e.g. paste("one", "two", "three", collapse="|")
        #} else if(data_type=="SIE_polygon") {
        #  filenames <- lapply(filenames, function(x) {grep(paste("extent.*", year[j], ".*polygon", sep=""), x, value=TRUE)})
        #}
    
        #for(j in seq_along(year)) {
        #if(data_type=="SIE_polyline") {
        #filenames <- lapply(filenames, function(x) {grep("extent_", x, value=TRUE)}) #possible to use OR as well e.g. paste("one", "two", "three", collapse="|")
        #filenames <- lapply(filenames, function(x) {grep(year[j], x, value=TRUE)})
        #filenames <- lapply(filenames, function(x) {grep("polyline", x, value=TRUE)})
        #} else if(data_type=="SIE_polygon") {
        #  filenames <- lapply(filenames, function(x) {grep("extent_", x, value=TRUE)})
        #  filenames <- lapply(filenames, function(x) {grep(year[j], x, value=TRUE)})
        #  filenames <- lapply(filenames, function(x) {grep("polygon", x, value=TRUE)})
        #}    
    if(!isTRUE(opt_down)) {
      
      print(paste("Downloading specified SIE files from NSIDC servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
      
        for(j in seq_along(filenames)) {
          for(k in filenames[[j]]) {
            download.file(paste(url_vec[j], k, sep=""), paste(res_dir, "/Data/", data_type,"/", k, sep=""), quiet=suppr_msgs, mode=down_mode)
          }
        }
    } else if(isTRUE(opt_down)) {
      filenames <- Reduce(c, filenames)
      url_list <- Reduce(c, url_list)
      
      require(parallel)
      cl <- makeCluster(dcores)
      print(paste("Downloading specified SIE files from NSIDC servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
      
      if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", filenames, sep=""), .scheduling = "dynamic")))
      } else if(!isTRUE(suppr_msgs)) {
        clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", filenames, sep=""), .scheduling = "dynamic")
      }
    }
  } else if(data_type=="SIE_median") {
    if(!is.null(year)) {
      warning("The year argument was ignored for data_type SIE_median since this represents year-averaged SIE values from NSIDC!")
    }
    
    month_val <- c()
    month_ind <- c()
    month_seq <- list()
    
    for(i in seq_along(month_vec)) {

      if(is.character(month_vec)) {
        if(frequency=="monthly") {
        month_val[i] <- paste("_", as.character(month_df$Month[month_df[,c("Month")] %in% month_vec[i]]), "_", sep="")
        month_ind[i] <- paste("_", as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec[i]]), "_", sep="")
        } else if(frequency=="daily") {
          month_val[i] <- paste("_", as.character(month_df$Month[month_df[,c("Month")] %in% month_vec[i]]), "_", sep="")
          start_day <- as.character(month_df$Month_Day[month_df[,c("Month")] %in% month_vec[i]])
          start_day_num <- as.numeric(start_day)
          
          if(month_val[i]!="Dec") {
          end_day_num <- as.numeric(month_df$Month_Day[which(month_df[,c("Month")] %in% month_vec[i])+1])
          } else if(month_val[i]=="Dec") {
            end_day_num <- as.numeric(366)
          }
          
          end_day_val <- end_day_num-1
          end_day <- as.character(end_day_val)
          
          if(nchar(end_day)==1) {
            end_day <- paste0("00", end_day)
          } else if(nchar(end_day)==2) {
            end_day <- paste0("0", end_day)
          }
          
          day_seq <- as.character(c(start_day_num:end_day_val))
          
          day_seq <-  sapply(day_seq, function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
          
          month_seq[[i]] <- paste("_", day_seq, "_", sep="")
        }
      } else if(is.numeric(month_vec)) {
        if(frequency=="monthly") {
        month_val[i] <- paste("_", as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec[i]]), "_", sep="")
        month_ind[i] <- paste("_", as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec[i]]), "_", sep="")
        } else if(frequency=="daily") {
          month_val[i] <- paste("_", as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec[i]]), "_", sep="")
          start_day <- as.character(month_df$Month_Day[month_df[,c("Month_Num")] %in% month_vec[i]])
          start_day_num <- as.numeric(start_day)
          
          if(month_val[i]!="Dec") {
            end_day_num <- as.numeric(month_df$Month_Day[which(month_df[,c("Month_Num")] %in% month_vec[i])+1])
          } else if(month_val[i]=="Dec") {
            end_day_num <- as.numeric(366)
          }
          
          end_day_val <- end_day_num-1
          end_day <- as.character(end_day_val)
          
          if(nchar(end_day)==1) {
            end_day <- paste0("00", end_day)
          } else if(nchar(end_day)==2) {
            end_day <- paste0("0", end_day)
          }
          
          day_seq <- as.character(c(start_day_num:end_day_val))
          
          day_seq <-  sapply(day_seq, function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
          
          month_seq[[i]] <- paste("_", day_seq, "_", sep="")
        }
      }}
      
    
      if(frequency=="daily") {
      url_vec <- paste(url, "shapefiles/dayofyear_median/", sep="")
      month_ind <- Reduce(c, month_seq)
      } else if(frequency=="monthly") {
      url_vec <- paste(url,"shapefiles/shp_median/", sep="")
      }
      
      tmp <- retry_cfm(url_vec, handle=dir_list_handle)
      con <- rawConnection(tmp$content)
      filenames <- readLines(con)
      close(con)
      
      filenames <- grep(paste(month_ind, collapse="|"), filenames, value=TRUE)
      
      if(!isTRUE(opt_down)) {
      for(filename in seq_along(filenames)) {
        print(paste("Downloading SIE (median) file ", filename, " of ", length(filenames), " from the NSIDC servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
        download.file(paste(url_vec, filenames[filename], sep=""), paste(res_dir, "/Data/", data_type,"/", filenames[filename], sep=""), quiet=suppr_msgs, mode=down_mode)
      }
      } else if(isTRUE(opt_down)) {
        
        url_list <- c()
        url_list <- paste(url_vec, filenames, sep="")
        
        require(parallel)
        cl <- makeCluster(dcores)
        print(paste("Downloading specified SIE (median) files from NSIDC servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
        
        if(isTRUE(suppr_msgs)) {
        invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", filenames, sep=""), .scheduling = "dynamic")))
        } else if(!isTRUE(suppr_msgs)) {
          clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", filenames, sep=""), .scheduling = "dynamic")
        }
        
      }
  }
} else if(any(repository %in% c("MODIS_A", "SeaWifs"))) {

  if(shared_folder==TRUE) {res_dir <- paste0(res_path, "/Data/SeaWifs_MODISA")} else if(shared_folder==FALSE) {res_dir <- paste0(res_path, "/Data/", repository)}
  cache_dir <- paste(res_dir,"/gsod_cache", sep="")
  
  month_df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), Month_Num=c(1:12), Month_ID=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), stringsAsFactors = FALSE)
  
  if(frequency=="daily") {
    url_time <- "Daily/"
    time_varib <- "DAY_"
  } else if(frequency=="monthly") {
    url_time <- "Monthly/"
    time_varib <- "MO_"
  }
  
  if(data_type=="Chla") {
    id_dtype <- paste0(time_varib, "CHL_chlor_a")
    id_dir <- "chlor_a"
  } else if(data_type=="SST") {
    id_dtype <- paste0(time_varib, "SST_sst")
    id_dir <- "sst"
  } else if(data_type=="NSST") {
    id_dtype <- paste0(time_varib, "NSST_sst")
    id_dir <- "sst"
  } else if(data_type=="PAR") {
    id_dtype <- paste0(time_varib, "PAR_par")
    id_dir <- "par"
  } else if(data_type=="iPAR") {
    id_dtype <- paste0(time_varib, "FLH_ipar")
    id_dir <- "ipar"
  } else if(data_type=="PIC") {
    id_dtype <- paste0(time_varib, "PIC_pic")
    id_dir <- "pic"
  } else if(data_type=="POC") {
    id_dtype <- paste0(time_varib, "POC_poc")
    id_dir <- "poc"
  } else if(data_type=="NFLH") {
    id_dtype <- paste0(time_varib, "FLH_nflh")
    id_dir <- "nflh"
  } else if(data_type=="KD490") {
    id_dtype <- paste0(time_varib, "KD490_Kd_490")
    id_dir <- "Kd_490"
  } else if(data_type=="Zeu") {
    id_dtype <- paste0(time_varib, "ZLEE_Zeu_lee")
    id_dir <- "Zeu_lee"
  } else if(data_type=="BBP_GIOP") {
    id_dtype <- paste0(time_varib, "IOP_bbp_443_giop")
    id_dir <- "bbp_443_giop"
  } else if(data_type=="BBP_GSM") {
    id_dtype <- paste0(time_varib, "IOP_bbp_443_gsm")
    id_dir <- "bbp_443_gsm"
  } else if(data_type=="BBP_QAA") {
    id_dtype <- paste0(time_varib, "IOP_bbp_443_qaa")
    id_dir <- "bbp_443_qaa"
  } else if(data_type=="BBP_s_GIOP") {
    id_dtype <- paste0(time_varib, "IOP_bbp_s_giop")
    id_dir <- "bbp_s_giop"
  } else if(data_type=="Adg_GIOP") {
    id_dtype <- paste0(time_varib, "IOP_adg_443_giop")
    id_dir <- "adg_443_giop"
  } else if(data_type=="Aph_GIOP") {
    id_dtype <- paste0(time_varib, "IOP_aph_443_giop")
    id_dir <- "aph_443_giop"
  }
  
  grid_resvar <- paste0("_", grid_res)

  id_var <- paste0(id_dtype, grid_resvar)
  
  greplargs <- list(pattern=id_var, fixed=TRUE)
  
  days_first <- get_monthdays(c(year), when="first")
  days_last <- get_monthdays(c(year), when="last")
  

  if(is.character(month_vec)) {
    month_val <- as.character(month_df$Month[month_df[,c("Month")] %in% month_vec])
    month_ind <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec])
  } else if(is.numeric(month_vec)) {
    month_val <- as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec])
    month_ind <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec])
  }
  
  if(repository=="SeaWifs") {
    pasvar <- "SeaWiFS"
  } else if(repository=="MODIS_A") {
    pasvar <- "MODIS-Aqua"
  }
    base_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/", pasvar, "/Mapped/", url_time, grid_res, "/", id_dir)
    day_rlist <- list()
    day_charlist <- list()
    
    if(frequency=="daily") {
      
      year_cat <- httr::GET(base_url)
      if (httr::status_code(year_cat) == 200) {
        year_list <- base_url %>% xml2::read_html(httr::content(year_cat, type = "text/html", 
                                                                as = "text", encoding = "UTF-8")) %>% rvest::html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
          rvest::html_table()
      } else {
        stop("Could not retrieve year list from SeaWifs_MODISA data HTML! Aborting.")
      }
      
        year_list <- gsub("/","", year_list[[1]][,colnames(year_list[[1]])])
        year_list <- year_list[year_list %in% year]
        
        days_first <- days_first[names(days_first) %in% year_list]
        days_last <- days_last[names(days_last) %in% year_list]
      
    } else if(frequency=="monthly") {

      yr_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/", pasvar, "/Mapped/Daily/", grid_res, "/", id_dir)
      
      year_cat <- httr::GET(yr_url)
      
      if (httr::status_code(year_cat) == 200) {
        year_list <- yr_url %>% xml2::read_html(httr::content(year_cat, type = "text/html", 
                                                                as = "text", encoding = "UTF-8")) %>% rvest::html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
          rvest::html_table()
      } else {
        stop("Could not retrieve year list from SeaWifs_MODISA data HTML! Aborting.")
      }
      
      year_list <- gsub("/","", year_list[[1]][,colnames(year_list[[1]])])
      year_list <- year_list[year_list %in% year]
      
      days_first <- days_first[names(days_first) %in% year_list]
      days_last <- days_last[names(days_last) %in% year_list]
      
      file_cat <- httr::GET(base_url)
      if (httr::status_code(file_cat) == 200) {
        file_lst <- base_url %>% xml2::read_html(httr::content(year_cat, type = "text/html", 
                                                                as = "text", encoding = "UTF-8")) %>% rvest::html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
          rvest::html_table()
      } else {
        stop("Could not retrieve year list from SeaWifs_MODISA data HTML! Aborting.")
      }
      
      file_lst <- file_lst[[1]][,colnames(file_lst[[1]])[1]]
    }
    
    dirquery_year <- list()
    year_url <- c()
    
    for(i in seq_along(year_list)) {
      print(paste("Assembling list of files for available year ", i, " of ", length(year_list), " (", year_list[i], ")", sep=""))
      
      #day_range[[as.character(year[i])]] <- c(days_first[[i]][month_ind[1]]:days_last[[i]][month_ind[length(month_ind)]])
      #day_range <- c(days_first[[i]][month_ind[1]]:days_last[[i]][month_ind[length(month_ind)]])
      
      if(frequency=="daily") {
        day_range <- unlist(sapply(seq_along(month_ind), function(x) days_first[[i]][month_ind[x]]:days_last[[i]][month_ind[x]]))
        
        day_rlist[[as.character(year_list[i])]] <- day_range
        day_charlist[[as.character(year_list[i])]] <-  sapply(day_rlist[[as.character(year_list[i])]], function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
        
        year_url[i] <- paste(base_url, "/", year_list[i], "/", sep="")
        
        dirquery_year[[as.character(year_list[i])]] <- dirquery <- query_direct(alt_uri = year_url[i])$Filename
        
        if(repository=="SeaWifs") {
          start_lett <- "^S"
        } else if(repository=="MODIS_A") {
          start_lett <- "^A"
        }
        grep_patt <- paste(start_lett, year_list[i], day_charlist[[as.character(year_list[i])]], ".*", id_dtype, sep="")
        grep_patt <- paste(grep_patt, collapse="|", sep="")
        
        dirquery_year[[as.character(year_list[i])]] <- dirquery[grep(grep_patt, dirquery, value=FALSE)]
      
      } else if(frequency=="monthly") {
        day_range <- unlist(sapply(seq_along(month_ind), function(x) c(days_first[[i]][month_ind[x]], days_last[[i]][month_ind[x]])))
        
        day_rlist[[as.character(year_list[i])]] <- day_range
        #day_charlist[[as.character(year_list[i])]] <-  sapply(day_rlist[[as.character(year_list[i])]], function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
        
        if(repository=="SeaWifs") {
          start_lett <- "^S"
        } else if(repository=="MODIS_A") {
          start_lett <- "^A"
        }
        grep_patt <- paste(start_lett, year_list[i], day_charlist[[as.character(year_list[i])]], ".*", id_dtype, sep="")
        grep_patt <- paste(grep_patt, collapse="|", sep="")
        
        dirquery_year[[as.character(year_list[i])]] <- file_lst[grep(grep_patt, file_lst, value=FALSE)]
        }
      
    }
    
    down_url <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/"
    dirquery_unlist <- unlist(dirquery_year)
    url_complete <- paste0(down_url, dirquery_unlist)
    
    if(!isTRUE(opt_down)) {
      print(paste("Downloading specified ", data_type, " files from SeaWifs_MODISA servers to: ", paste0(res_dir, "/Data/", data_type, "/", frequency, "/"), ". This could take a while!", sep=""))
      for(j in seq_along(url_complete)) {
        
        svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
        Sys.sleep(1/length(url_complete))
        if(j==length(url_complete)) cat("Done!\n")
        
          download.file(url_complete[j], paste0(res_dir, "/Data/", data_type,"/", frequency, "/", dirquery_unlist[j]), quiet=suppr_msgs, mode=down_mode)
        }
      
    } else if(isTRUE(opt_down)) {
      
    require(parallel)
    cl <- makeCluster(dcores)
    print(paste("Downloading specified ", data_type, " files from SeaWifs_MODISA servers to: ", paste0(res_dir, "/Data/", data_type, "/"), ". This could take a while!", sep=""))
    
    if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste0(res_dir, "/Data/", data_type, "/", frequency, "/", dirquery_unlist), .scheduling="dynamic")))
    } else if(!isTRUE(suppr_msgs)) {
      clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste0(res_dir, "/Data/", data_type, "/", frequency, "/", dirquery_unlist), .scheduling="dynamic")
    }
    }
  
} else if(repository=="CEDA_MO") {
  
  res_dir <- paste(res_path, "/Data/", repository, sep="")
  cache_dir <- paste(res_dir,"/gsod_cache", sep="")
  
  month_df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), Month_Num=c(1:12), Month_ID=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), stringsAsFactors = FALSE)
  
  id_dir <- paste0(data_type, "/")
  
  if(frequency=="daily") {
    url_time <- "day/"
    
  } else if(frequency=="monthly") {
    url_time <- "mon/"
    
  } else if(any(frequency %in% c("mon-20y", "mon-30y"))) {
    url_time <- paste0(frequency, "/")
  }
  
  if(is.character(month_vec)) {
    month_val <- as.character(month_df$Month[month_df[,c("Month")] %in% month_vec])
    month_ind <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec])
  } else if(is.numeric(month_vec)) {
    month_val <- as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec])
    month_ind <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec])
  }
  
  print("Obtaining FTP catalog for CEDA_MO...")
  ftp_handle <- new_handle(username=usrname, password=pword, dirlistonly=FALSE, failonerror=FALSE)
  ftp_con <- curl(paste0("ftp://ftp.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.0.0.0/1km/", id_dir, url_time, "v20181126/"), handle=ftp_handle)
  ftp_tbl <- read.table(ftp_con, stringsAsFactors=FALSE, fill=TRUE)
  ftp_filelist <- ftp_tbl[,9]
  #close(ftp_con)
  
  print("Deriving final list of files to download...")
  if(frequency=="daily") {
    final_filelist <- paste0(sapply(year, function(x) paste0(x, month_ind)), collapse="|")
    
    final_filelist <- grep(final_filelist, ftp_filelist, value=TRUE)
    
  } else if(frequency=="monthly") {
    
    final_filelist <- grep(paste0(year, "01-", collapse="|"), ftp_filelist, value=TRUE)
    
  } else if(any(frequency %in% c("mon-20y", "mon-30y"))) {
    
    final_filelist <- ftp_filelist
  }
  
  print("Compiling URLs...")
  url_complete <- paste0("ftp://ftp.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.0.0.0/1km/", id_dir, url_time, "v20181126/", final_filelist)
  
  if(!isTRUE(opt_down)) {
    print(paste("Downloading specified ", data_type, " files from CEDA THREDDS servers to: ", paste(res_dir, "/Data/", data_type, "/", url_time, sep=""), ". This could take a while!", sep=""))
    for(j in seq_along(url_complete)) {
      
      svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
      Sys.sleep(1/length(url_complete))
      if(j==length(url_complete)) cat("Done!\n")
      
      curl::curl_download(url_complete[j], paste(res_dir, "/Data/", data_type, "/", url_time, final_filelist[j], sep=""), quiet=suppr_msgs, mode=down_mode, handle=ftp_handle)
    }
    
  } else if(isTRUE(opt_down)) {
    
    require(parallel)
    cl <- makeCluster(dcores)
    print(paste("Downloading specified ", data_type, " files from CEDA THREDDS servers to: ", paste(res_dir, "/Data/", data_type, "/", url_time, sep=""), ". This could take a while!", sep=""))
    
    if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, curl_download, MoreArgs=c(mode=down_mode, handle=ftp_handle), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", url_time, final_filelist, sep=""), .scheduling="dynamic")))
    } else if(!isTRUE(suppr_msgs)) {
      clusterMap(cl, curl_download, MoreArgs=c(mode=down_mode, handle=ftp_handle), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", url_time, final_filelist, sep=""), .scheduling="dynamic")
    }
  }
  
} else if(repository=="TClim") {
  
  res_dir <- paste(res_path, "/Data/", repository, sep="")
  cache_dir <- paste(res_dir,"/gsod_cache", sep="")
  
  month_df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), Month_Num=c(1:12), Month_ID=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), stringsAsFactors = FALSE)
  
  id_dir <- data_type
  
  if(frequency=="monthly") {
    url_time <- "data/"
    url_exp <- "monthly/"
    
  } else if(frequency=="mon-30y") {
    url_time <- "summaries/"
    url_exp <- "mon-30y/"
  }
  
  print("Obtaining THREDDS catalog for TClim...")
  thredds_con <- get_catalog(paste0("http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/", url_time, "catalog.xml"))
  thredds_filelist <- thredds_con$get_datasets()

  print("Deriving final list of files to download...")
  if(frequency=="monthly") {
    
    final_filelist <- grep(paste0(id_dir, "_", year, collapse="|"), names(thredds_filelist), value=TRUE)
    
  } else if(frequency=="mon-30y") {
    
    final_filelist <- grep(paste0(min(year),max(year), "_", id_dir, collapse="|"), names(thredds_filelist), value=TRUE)
  }
  
  print("Compiling URLs...")
  url_complete <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/", url_time, final_filelist)
  
  if(!isTRUE(opt_down)) {
    print(paste("Downloading specified ", data_type, " files from TClim THREDDS servers to: ", paste(res_dir, "/Data/", data_type, "/", url_exp, sep=""), ". This could take a while!", sep=""))
    for(j in seq_along(url_complete)) {
      
      svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
      Sys.sleep(1/length(url_complete))
      if(j==length(url_complete)) cat("Done!\n")
      
      download.file(url_complete[j], paste(res_dir, "/Data/", data_type, "/", url_exp, final_filelist[j], sep=""), quiet=suppr_msgs, mode=down_mode)
    }
    
  } else if(isTRUE(opt_down)) {
    
    require(parallel)
    cl <- makeCluster(dcores)
    print(paste("Downloading specified ", data_type, " files from TClim THREDDS servers to: ", paste(res_dir, "/Data/", data_type, "/", url_exp, sep=""), ". This could take a while!", sep=""))
    
    if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", url_exp, final_filelist, sep=""), .scheduling="dynamic")))
    } else if(!isTRUE(suppr_msgs)) {
      clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", url_exp, final_filelist, sep=""), .scheduling="dynamic")
    }
  }
  
} else if(repository=="CRU4") {
  
  res_dir <- paste(res_path, "/Data/", repository, sep="")
  cache_dir <- paste(res_dir,"/gsod_cache", sep="")
  
  id_dir <- paste0(data_type, "/")
  
  if(frequency!="monthly") {
    print("CRU4 only supports monthly data! Switching frequency to monthly...")
    frequency <- "monthly"
  }
  
  if(frequency=="monthly") {
    url_exp <- "monthly/"
  }

  print("Obtaining FTP catalog for CEDA CRU4...")
  ftp_handle <- new_handle(username=usrname, password=pword, dirlistonly=FALSE, failonerror=FALSE)
  ftp_con <- curl(paste0("ftp://ftp.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.03/data/", id_dir), handle=ftp_handle)
  ftp_tbl <- read.table(ftp_con, stringsAsFactors=FALSE, fill=TRUE)
  ftp_filelist <- ftp_tbl[,9]
  
  print("Deriving final list of files to download...")
  final_filelist <- grep("\\.nc$", ftp_filelist, value=TRUE)
  
  print("Compiling URLs...")
  url_complete <- paste0("ftp://ftp.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.03/data/", id_dir, final_filelist)
  
  if(!isTRUE(opt_down)) {
    print(paste("Downloading specified ", data_type, " files from CEDA THREDDS servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
    for(j in seq_along(url_complete)) {
      
      svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
      Sys.sleep(1/length(url_complete))
      if(j==length(url_complete)) cat("Done!\n")
      
      curl::curl_download(url_complete[j], paste(res_dir, "/Data/", data_type, "/", final_filelist[j], sep=""), quiet=suppr_msgs, mode=down_mode, handle=ftp_handle)
    }
    
  } else if(isTRUE(opt_down)) {
    
    require(parallel)
    cl <- makeCluster(dcores)
    print(paste("Downloading specified ", data_type, " files from CEDA THREDDS servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
    
    if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, curl_download, MoreArgs=c(mode=down_mode, handle=ftp_handle), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", final_filelist, sep=""), .scheduling="dynamic")))
    } else if(!isTRUE(suppr_msgs)) {
      clusterMap(cl, curl_download, MoreArgs=c(mode=down_mode, handle=ftp_handle), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", final_filelist, sep=""), .scheduling="dynamic")
    }
  }
  
} else if(repository=="OC_CCI") {
  
  res_dir <- paste(res_path, "/Data/", repository, sep="")
  cache_dir <- paste(res_dir,"/gsod_cache", sep="")
  
  month_df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), Month_Num=c(1:12), Month_ID=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), stringsAsFactors = FALSE)
  
  if(frequency=="daily") {
    url_time <- "daily/"
  } else if(frequency=="monthly") {
    url_time <- "monthly/"
  }
  
  if(data_type=="Chla") {
    id_dir <- "chlor_a/"
  } else if(data_type=="IOP") {
    id_dir <- "iop/"
  } else if(data_type=="RRS") {
    id_dir <- "rrs/"
  } else if(data_type=="KD490") {
    id_dir <- "kd/"
  }
  
  if(is.character(month_vec)) {
    month_val <- as.character(month_df$Month[month_df[,c("Month")] %in% month_vec])
    month_ind <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec])
  } else if(is.numeric(month_vec)) {
    month_val <- as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec])
    month_ind <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec])
  }
  
  base_urlist <- paste0("ftp://oceancolour.org/occci-v4.0/geographic/netcdf/", url_time, id_dir, year, "/")
  
  print("Obtaining FTP catalog for OC_CCI (v4)...")
  ftp_handle <- new_handle(username=usrname, password=pword, dirlistonly=FALSE, failonerror=FALSE)
  
  final_filelist <- list()
  
  for(i in seq_along(base_urlist)) {
    print(paste0("Compiling file list and URLs for year ", year[i], "..."))
    ftp_con <- curl(base_urlist[i], handle=ftp_handle)
    ftp_tbl <- read.table(ftp_con, stringsAsFactors=FALSE, fill=TRUE)
    final_filelist[[i]] <- grep(paste0("-", year[i], month_ind, collapse="|"), paste0(base_urlist[i], ftp_tbl[,9]), value=TRUE)
  }
  
  url_complete <- unlist(final_filelist)
  
  if(!isTRUE(opt_down)) {
    print(paste("Downloading specified ", data_type, " files from OC_CCI FTP servers to: ", paste(res_dir, "/Data/", data_type, "/", url_time, sep=""), ". This could take a while!", sep=""))
    for(j in seq_along(url_complete)) {
      
      svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
      Sys.sleep(1/length(url_complete))
      if(j==length(url_complete)) cat("Done!\n")
      
      curl::curl_download(url_complete[j], paste(res_dir, "/Data/", data_type, "/", url_time, basename(url_complete[j]), sep=""), quiet=suppr_msgs, mode=down_mode, handle=ftp_handle)
    }
    
  } else if(isTRUE(opt_down)) {
    
    require(parallel)
    cl <- makeCluster(dcores)
    print(paste("Downloading specified ", data_type, " files from OC_CCI FTP servers to: ", paste(res_dir, "/Data/", data_type, "/", url_time, sep=""), ". This could take a while!", sep=""))
    
    if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, curl_download, MoreArgs=c(mode=down_mode, handle=ftp_handle), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", url_time, basename(url_complete), sep=""), .scheduling="dynamic")))
    } else if(!isTRUE(suppr_msgs)) {
      clusterMap(cl, curl_download, MoreArgs=c(mode=down_mode, handle=ftp_handle), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", url_time, basename(url_complete), sep=""), .scheduling="dynamic")
    }
  }
  
}

print("Download complete!")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTIONS to check available RAM on the system
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
showMemoryUse <- function(sort="size", decreasing=FALSE, limit) {
  
  objectList <- ls(parent.frame())
  
  #oneKB <- 1024
  oneMB <- 1048576
  #oneGB <- 1073741824
  
  memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))
  
  memListing <- round(memoryUse/oneMB,5)
  
  memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)
  
  if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
  else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
  
  if(!missing(limit)) memListing <- memListing[1:limit,]
  
  memSum <- as.numeric(sum(memListing[,"memorySize"]))
  
  #print(memListing, row.names=FALSE)
  return(memSum)
}

get_free_ram <- function(){
  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- as.numeric(gsub("\r", "", x, fixed = TRUE))
    
    oneKB <- 1024
    #oneMB <- 1048576
    #oneGB <- 1073741824
    
    x <- round(x/oneKB,5)
    
    as.numeric(x)
  } else if(Sys.info()[["sysname"]] != "Windows") {
    #stop("RAM check currently only supported on Windows!")
    #memory.limit() - showMemoryUse()
    x <- sum(gc()[,6])
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTIONS to check odd vs even numbers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Merge and export gridded (e.g. daily NSIDC, MODIS_A, CRU4 etc.) into collection .csv files (except Melt Season data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
multMerge <- function(mypath, use_dt=TRUE, patt="\\.csv$", which.files="all", by_cols=FALSE, which.cols=NULL) {
  
  #Install and load the required libraries 
  list.of.packages <- c("data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  invisible(lapply(c(list.of.packages, "parallel"), require, character.only = TRUE))
  
  library(data.table)
  
  filenames = list.files(path = mypath, pattern = patt, full.names = TRUE)
  if(which.files=="all") {which_files <- 1:length(filenames) } else if(is.numeric(which.files)) { which_files <- which.files }
  
  if(!is.null(which.cols) & use_dt==FALSE) {stop("Only the Data Table package supports column selection! Adjust the use_dt argument...")}
  
  if(isTRUE(use_dt)) {
    library(data.table)
    datalist = lapply(filenames[which_files], 
                      function(x){fread(file = x,   
                                        header = TRUE,
                                        stringsAsFactors = FALSE,
                                        data.table = FALSE,
                                        select=which.cols)})
  } else if(!isTRUE(use_dt)) {
    datalist = lapply(filenames[which_files], 
                      function(x){read.csv(file = x,   #MIGHT WANT TO SWITCH TO fread INSTEAD OF read.csv in the future!
                                           header = TRUE,
                                           stringsAsFactors = FALSE)})
  }
  
  if(by_cols==FALSE) {
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
  } else if(is.character(by_cols)) {
    Reduce(function(x,y) {merge(x, y, by=by_cols, all = TRUE)}, datalist)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Extract and summarise/export netcdf or hdf data (e.g. NSIDC, MODIS_A, CRU4 etc.) into .csv files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_summary <- function(remove_miss="all", frequency="daily", repository="NSIDC", hemisphere="north", data_type="Chla", nc_path, year_rng="all", month_rng="all", day_rng=NULL, var_names="default", 
                         coord_subset=NULL, subset_order=c("year", "month", ""), summary_func="total", export_path=NULL, big_data, good_names=TRUE, calc_anom=FALSE, calc_sum=FALSE, n_limit=0,
                         mode="summary") {
  
  #Install and load the required libraries
  list.of.packages <- c("purrr", "ncdf4", "raster", "scatterpie", "maps", "mapdata", "lubridate", "ggmap", "rgdal", "sp", "sf", "chron",
                        "lattice", "svMisc", "zoo", "data.table", "bigmemory", "bigalgebra")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])) install.packages(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])
  
  if(any(new.packages %in% "threddscrawler")) devtools::install_github("BigelowLab/threddscrawler")
  if(any(new.packages %in% "obpgcrawler")) devtools::install_github("BigelowLab/obpgcrawler")
  
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  library(purrr)
  #library(processNC)
  library(ncdf4)
  library(raster)
  library(scatterpie)
  library(maps)
  library(mapdata)
  library(lubridate)
  library(tidyverse)
  library(ggmap)
  library(RNetCDF)
  library(rgdal)
  library(sp)
  library(sf)
  library(chron)
  library(lattice)
  library(svMisc)
  library(zoo)
  library(threddscrawler)
  library(obpgcrawler)
  library(data.table)
  library(bigmemory)
  library(bigalgebra)
  library(ff)
  library(ffbase)
  #library(WGCNA)
  
  
  #if(!any(dnames %in% consec_calcs[1]) & !is.null(consec_calcs) & consec_calcs!="none") {
  #  stop("The variable chosen for consecutive occurrence calculations (consec_calcs) must be one of extracted variables!")
  #}
  
  if(frequency=="daily" & any(repository %in% c("NASA_Melt", "CEDA_MO", "TClim", "CRU4"))) {
    print("Please note that only monthly frequency is supported for CEDA_MO, TClim, and CRU4 data. NASA_Melt is only yearly. The frequency argument will be ignored.")
  }
  
  if(frequency=="mon-30y" & !any(repository %in% c("CEDA_MO", "TClim"))) { stop("Only CEDA_MO and TClim repositories support the mon-30y frequency value!") }
  if(frequency=="mon-30y" & length(year)>2) { stop("Year range must be a vector of length 2 (earliest year, latest year) when frequency is mon-30y!") }
  
  if(any(!month_rng %in% c("all","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) & !is.numeric(month_rng)||any(!month_rng %in% c(1:12)) & !is.character(month_rng)) { #& !is.numeric(month)
    stop("Incorrect month values provided! Please use any of: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, or corresponding numerical values (1 to 12).")
  }
  
  if(repository=="NASA_Melt" & summary_func=="yearly_monthly") {
    print("Monthly summaries cannot be calculated from the NASA Cryosphere ice melt or CEDA_MO datasets! Changing summary_func to yearly...")
    summary_func <- "yearly"
  }
  
  if(is.numeric(day_rng) & frequency!="daily") {
    stop("Selecting a day_rng is not supported unless daily data resolution is used! Stopping...")
  }
  
  if(!any(subset_order %in% "day") & is.numeric(day_rng)) {
    stop("Selecting a day range (day_rng) is only supported when subset_order is daily! Stopping...")
  }
  
  if(is.numeric(day_rng) & any(c("NASA_Melt", "CEDA_MO") %in% repository)) {
    stop("Selecting a day_rng is currently only supported for the SeaWifs_MODISA and NSIDC datasets! Stopping...")
  }
  
  if(is.numeric(day_rng) & summary_func!="total") {
    print("Only the total summary function is supported when day_rng is in use! Changing summary_func to total...")
    summary_func <- "total"
  }
  
  #Creating reference data.frame containing month definitions
  month_df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), Month_Num=c(1:12), Month_ID=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), Month_Day=c("001", "032", "061", "092", "122", "153", "183", "214", "245", "275", "306", "336"), stringsAsFactors = FALSE)
  
  #Creating a reference data.frame for Monthly Mean NSIDC SIC values spanning 1981-2010 (from Sea Ice Index)
  #sic_df <- data.frame()
  
  if(month_rng[1]=="all") {
    month_vec <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  } else if(month_rng[1]!="all") {
    month_vec <- month_rng
  }
  
  #Additional helping functions
  is.even <- function(x) x %% 2 == 0
  is.odd <- function(x) x %% 2 != 0
  
  get_free_ram <- function(){
    if(Sys.info()[["sysname"]] == "Windows"){
      x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
      x <- x[grepl("FreePhysicalMemory", x)]
      x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
      x <- as.numeric(gsub("\r", "", x, fixed = TRUE))
      
      oneKB <- 1024
      #oneMB <- 1048576
      #oneGB <- 1073741824
      
      x <- round(x/oneKB,5)
      
      as.numeric(x)
    } else {
      stop("RAM check currently only supported on Windows!")
      #memory.limit() - showMemoryUse()
    }
  }
  
  #Listing and importing the files
  #The HYCOM_MLD repository files are HDF4 SDS, not NCDF4!
  if(repository!="HYCOM_MLD") {
    nc_files <- list.files(nc_path, pattern="\\.nc$", full.names=TRUE)
  } else if(repository=="HYCOM_MLD") {
    nc_files <- list.files(nc_path, pattern="\\.hdf$", full.names=TRUE)
  }
  
  
  if(!any(repository %in% c("NASA_Melt", "CEDA_MO", "TClim", "CRU4"))) {
    
    month_val <- c()
    month_ind <- c()
    listvar1 <- c()
    listvar2 <- c()
    varlist1 <- list()
    varlist2 <- list()
    import_vec <- c()
    
    for(i in seq_along(month_vec)) {
      
      if(is.numeric(month_vec)) {
        month_val[i] <- as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec[i]])
        month_ind[i] <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec[i]])
      } else if(is.character(month_vec)) {
        month_val[i] <- as.character(month_df$Month[month_df[,c("Month")] %in% month_vec[i]])
        month_ind[i] <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec[i]])
      }}
    
    
    if(any(subset_order %in% c("day"))) {
      
      mdays_list <- list()
      days_first <- get_monthdays(year_rng, when="first")
      days_last <- get_monthdays(year_rng, when="last")
      
      for(i in seq_along(year_rng)) {
        day_range <- unlist(sapply(seq_along(month_ind), function(x) days_first[[i]][month_ind[x]]:days_last[[i]][month_ind[x]]))
        mdays_list[[as.character(year_rng[i])]] <- day_range
        mdays_list[[as.character(year_rng[i])]] <-  sapply(mdays_list[[as.character(year_rng[i])]], function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
      }
    }
    
    for(i in seq_along(month_ind)) {
      for(j in seq_along(year_rng)) {
        
        if(all(subset_order[1:2]==c("year", "month"))) {
          listvar1[j] <- year_rng[j]
          listvar2[i] <- month_ind[i]
          sepvar <- subset_order[3]
          import_vec[paste(i,j, sep="+")] <- paste0(listvar1[j], sepvar, listvar2[i])
        } else if(all(subset_order[1:2]==c("month", "year"))) {
          listvar1[i] <- month_ind[i]
          listvar2[j] <- year_rng[j]
          sepvar <- subset_order[3]
          import_vec[paste(i,j, sep="+")] <- paste0(listvar1[i], sepvar, listvar2[j])
        } else if(all(subset_order[1:2]==c("year","day"))) {
          listvar1[j] <- year_rng[j]
          varlist2[[j]] <- mdays_list[[j]]
          sepvar <- subset_order[3]
          import_vec[j] <- paste0(listvar1[j], sepvar, varlist2[[j]], collapse="|")
        } else if(all(subset_order[1:2]==c("day","year"))) {
          varlist1[[j]] <- mdays_list[[j]]
          listvar2[j] <- year_rng[j]
          sepvar <- subset_order[3]
          import_vec[j] <- paste0(varlist1[[j]], sepvar, listvar2[j], collapse="|")
        }
      }
    }
    
  } else if(repository=="NASA_Melt") {
    
    month_ind <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    import_vec <- paste0(year_rng)
    
  } else if(any(c("CEDA_MO", "TClim", "CRU4") %in% repository)) {
    
    month_val <- c()
    month_ind <- c()
    
    for(i in seq_along(month_vec)) {
      
      if(is.numeric(month_vec)) {
        month_val[i] <- as.character(month_df$Month[month_df[,c("Month_Num")] %in% month_vec[i]])
        month_ind[i] <- as.character(month_df$Month_ID[month_df[,c("Month_Num")] %in% month_vec[i]])
      } else if(is.character(month_vec)) {
        month_val[i] <- as.character(month_df$Month[month_df[,c("Month")] %in% month_vec[i]])
        month_ind[i] <- as.character(month_df$Month_ID[month_df[,c("Month")] %in% month_vec[i]])
      }}
    
    import_vec <- paste0(year_rng)
  }
  
  if(is.numeric(day_rng)) {
    day_importvec <- sapply(day_rng, function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
    day_importvec <- paste0("[[:digit:]]{4}", day_importvec)
    
    import_dayunlist <- unlist(strsplit(import_vec, "|", fixed=TRUE))
    
    import_vec <- paste(import_dayunlist[grep(paste(day_importvec, collapse="|"), import_dayunlist)], collapse="|")
  }
  
  if(length(unlist(strsplit(import_vec, "|", fixed=TRUE)))>=2000) {
    print("File vector too long! Splitting regex call to grep into several chunks...")
    grep_chunks <- ggplot2::cut_interval(1:length(unlist(strsplit(import_vec, "|", fixed=TRUE))), length=1000, labels=FALSE)
    
    nc_greplist <- list()
    import_unlisted <- unlist(strsplit(import_vec, "|", fixed=TRUE))
    
    for(grep_chunk in unique(grep_chunks)) {
      grep_whichchunk <- which(grep_chunks %in% grep_chunk)
      
      nc_greplist[[paste0("Grep Chunk (", min(grep_whichchunk), "-", max(grep_whichchunk), ")")]] <- nc_files[grep(paste0(import_unlisted[grep_whichchunk], collapse="|"), nc_files)]
    }
    
    nc_files <- unique(unlist(nc_greplist))
    
  } else if(length(unlist(strsplit(import_vec, "|", fixed=TRUE)))<2000) {
    nc_files <- nc_files[grep(paste0(import_vec, collapse="|"), nc_files)]
    #nc_files <- nc_files[grep(paste0(import_vec, collapse="|"), nc_files, perl=subset_order[4])]
  }
  
  if(repository=="CRU4") { nc_files <- list.files(nc_path, pattern="\\.nc$", full.names = TRUE) }
  
  #Auxillary RAM threshold variable for big_data
  if(length(big_data)>1 & is.character(big_data[1]) & big_data[2]!="force") {
    big_thres <- as.numeric(big_data[2])
  } else if(length(big_data)==1 & big_data[1]!=FALSE) {
    big_thres <- 0
  }
  
  
  #For the NSIDC repository
  if(repository=="NSIDC") {
    
    if(!is.null(data_type) & data_type!="SIC") {
      print("Please note that only the SIC data_type is supported for NSIDC! Data_type argument will be ignored.")
    }
    
    #Defining variables to extract from netCDF files
    #if(frequency=="daily" & var_names[1]=="monthly_default") {
    #stop("The frequency is incompatible with chosen value of var_names! Please change either to match.")
    #}
    
    #if(frequency=="monthly" & var_names[1]=="daily_default") {
    #  stop("The frequency is incompatible with chosen value of var_names! Please change either to match.")
    #}
    
    if(any(var_names %in% "default") & frequency=="daily") {
      dnames <- c("seaice_conc_cdr", "stdev_of_seaice_conc_cdr")
    } else if(any(var_names %in% "default") & frequency=="monthly") {
      dnames <- c("seaice_conc_monthly_cdr","stdev_of_seaice_conc_monthly_cdr")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    
    #This code creates a coordinate set to test the other time slices against!
    print("Obtaining reference coordinate grid from the first imported netCDF file...")
    nc_ex <- nc_open(nc_files[1])
    
    xgrid <- ncvar_get(nc_ex, "xgrid")
    nxgrid <- dim(xgrid)
    
    ygrid <- ncvar_get(nc_ex, "ygrid")
    nygrid <- dim(ygrid)
    
    proj_info <- list()
    proj_info[["srid"]] <- ncatt_get(nc_ex, "projection", "srid")
    proj_info[["parent_grid_cell_row_subset_end"]] <- ncatt_get(nc_ex, "projection", "parent_grid_cell_row_subset_end")
    proj_info[["parent_grid_cell_column_subset_end"]] <- ncatt_get(nc_ex, "projection", "parent_grid_cell_column_subset_end")
    proj_info[["proj4text"]] <- ncatt_get(nc_ex, "projection", "proj4text")
    
    #xgrid_km <- xgrid/1000
    #ygrid_km <- ygrid/1000
    #totalgrid_km <- expand.grid(xcoord = xgrid_km, ycoord = ygrid_km, stringsAsFactors = FALSE)
    totalgrid_m <- expand.grid(xcoord = xgrid, ycoord = ygrid, stringsAsFactors = FALSE)
    #totalgrid_m <- totalgrid_km*1000
    
    if(hemisphere=="north") {
      cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(paste(proj_info$proj4text$value,"+init=epsg:3411")))
      cord.new <- spTransform(cord.dec, sp::CRS("+init=epsg:4326 +proj=longlat"))
    } else if(hemisphere=="south") {
      cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(paste(proj_info$proj4text$value,"+init=epsg:3031")))
      cord.new <- spTransform(cord.dec, sp::CRS("+init=epsg:4326 +proj=longlat"))
    }
    
    coordinates_test <- coordinates_full <- cbind(coordinates(cord.dec), coordinates(cord.new))
    colnames(coordinates_test) <- colnames(coordinates_full) <- c("xcoord", "ycoord", "Longitude", "Latitude")
    
    colname_bckp <- c("xcoord", "ycoord", "Longitude", "Latitude")
    #colname_vec <- character(length(nc_files)*length(dnames))
    colname_vec <- c()
    
    xcori <- match(c("xcoord"), colname_bckp)
    ycori <- match(c("ycoord"), colname_bckp)
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    nc_close(nc_ex)
    
    
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    #data_total <- as.data.frame(coordinates_test)
    #colnames(data_total) <- NULL
    
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    
    print(paste("Processing netCDF files..."))
    cat("/n")
    
    #This code runs the loop for all files, testing the coordinate data of each against coordinates_test!
    for(i in seq_along(nc_files)) { 
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")
      
      nc_ex <- nc_open(nc_files[i])
      
      lon <- ncvar_get(nc_ex, "latitude")
      nlon <- dim(lon)
      
      lat <- ncvar_get(nc_ex, "longitude")
      nlat <- dim(lat)
      
      xgrid <- ncvar_get(nc_ex, "xgrid")
      nxgrid <- dim(xgrid)
      
      ygrid <- ncvar_get(nc_ex, "ygrid")
      nygrid <- dim(ygrid)
      
      proj_info <- list()
      proj_info[["srid"]] <- ncatt_get(nc_ex, "projection", "srid")
      proj_info[["parent_grid_cell_row_subset_end"]] <- ncatt_get(nc_ex, "projection", "parent_grid_cell_row_subset_end")
      proj_info[["parent_grid_cell_column_subset_end"]] <- ncatt_get(nc_ex, "projection", "parent_grid_cell_column_subset_end")
      proj_info[["proj4text"]] <- ncatt_get(nc_ex, "projection", "proj4text")
      #nproj <- dim(proj_info)
      
      time_cov <- ncvar_get(nc_ex, "time")
      #ntime <- dim(time_cov)
      time_units <- ncatt_get(nc_ex, "time", "units")
      
      dlist <- list()
      dlname <- list()
      dunits <- list()
      dfillval <- list()
      dmissval <- list()
      dmissmean <- list()
      
      for(j in seq_along(dnames)) {
        listname <- paste(dnames[j])
        dlist[[listname]] <- ncvar_get(nc_ex, dnames[j])
        
        if(listname=="seaice_conc_cdr" | listname=="seaice_conc_monthly_cdr" | listname=="stdev_of_seaice_conc_monthly_cdr" | listname=="stdev_of_seaice_conc_cdr") {
          dlist[[listname]] <- dlist[[listname]]*100
        }
        
        dlname[[listname]] <- ncatt_get(nc_ex, dnames[j], "long_name")
        dunits[[listname]] <- ncatt_get(nc_ex, dnames[j], "units")
        dfillval[[listname]] <- ncatt_get(nc_ex, dnames[j], "_FillValue")
        dmissval[[listname]] <- ncatt_get(nc_ex, dnames[j], "missing_value")
        if(dmissval[[listname]]$value==0 & dmissval[[j]]$hasatt==FALSE) {
          dmissval[[listname]] <- ncatt_get(nc_ex, dnames[j], "flag_values")
          dmissmean[[listname]] <- ncatt_get(nc_ex, dnames[j], "flag_meanings")
        }
      }
      
      #At this point the .nc file can be closed as it is no longer needed!
      nc_close(nc_ex)
      
      #Removing missing data. However, the below might be too complex for sustained use - think about simplifying!
      dlist_vec <- list()
      
      for(k in seq_along(dlist)) {
        
        if(remove_miss=="all") {
          dlist_vec[[k]] <- as.vector(dlist[[k]])
          dlist_vec[[k]][round(dlist_vec[[k]]) %in% dmissval[[k]]$value] <- NA
          dlist_vec[[k]][round(dlist_vec[[k]]) %in% dfillval[[k]]$value] <- NA
        } else if(remove_miss=="keep_coastal") {
          dlist_vec[[k]] <- as.vector(dlist[[k]])
          if(names(dlist)[k]=="seaice_conc_cdr") {
            dfillstr <- strsplit(dmissmean[[k]]$value, " ")
            remval <- dmissval[[k]]$value[!dfillstr[[k]] %in% c("coastal", "lakes")]
            zeroval <- dmissval[[k]]$value[!dmissval[[k]]$value %in% remval]
            dlist_vec[[k]][round(dlist_vec[[k]]) %in% remval] <- NA
            dlist_vec[[k]][round(dlist_vec[[k]]) %in% dfillval[[k]]$value] <- NA
          } else if(names(dlist)[i]!="seaice_conc_cdr") {
            dlist_vec[[k]][round(dlist_vec[[k]]) %in% dmissval[[k]]$value] <- NA
            dlist_vec[[k]][round(dlist_vec[[k]]) %in% dfillval[[k]]$value] <- NA
          }
        }
      }
      
      #Converting time variable to a sensible format (if it is in the "days since" format!)
      if(grepl("days since", time_units$value)==TRUE & is.numeric(time_cov)) {
        tustr <- strsplit(time_units$value, " ")
        tdstr <- strsplit(unlist(tustr)[3], "-")
        tmonth <- as.integer(unlist(tdstr)[2])
        tday <- as.integer(unlist(tdstr)[3])
        tyear <- as.integer(unlist(tdstr)[1])
        tstamp <- as.Date(chron(time_cov,origin=c(tmonth, tday, tyear)))
      }
      
      #xgrid_km <- xgrid/1000
      #ygrid_km <- ygrid/1000
      totalgrid_m <- expand.grid(xcoord = xgrid, ycoord = ygrid, stringsAsFactors = FALSE)
      #totalgrid_km <- expand.grid(xcoord = xgrid/1000, ycoord = ygrid/1000, stringsAsFactors = FALSE)
      #totalgrid_m <- totalgrid_km*1000
      
      #One way to convert coordinates using the RGDAL package (lifesaver!)
      if(hemisphere=="north") {
        cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(paste(proj_info$proj4text$value,"+init=epsg:3411")))
        cord.new <- spTransform(cord.dec, sp::CRS("+init=epsg:4326 +proj=longlat"))
      } else if(hemisphere=="south") {
        cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(paste(proj_info$proj4text$value,"+init=epsg:3031")))
        cord.new <- spTransform(cord.dec, sp::CRS("+init=epsg:4326 +proj=longlat"))
      }
      coordinates_total <- cbind(coordinates(cord.dec), coordinates(cord.new))
      colnames(coordinates_total) <- c("xcoord", "ycoord", "Longitude", "Latitude")
      #colnames(coordinates_total) <- NULL
      
      #Subsetting coordinates in the looping dataset
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !coordinates_total[,latin] < coord_subset[1] & !coordinates_total[,latin]>coord_subset[2] & !coordinates_total[,lonin]<coord_subset[3] & !coordinates_total[,lonin]>coord_subset[4]
        coordinates_total <- coordinates_total[coord_grep,]
        dlist_vec <- lapply(dlist_vec, function(x) {x[coord_grep]})
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_test)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(as.character(i), dnames)] <- paste(dnames, as.character(tstamp), sep=";") 
        #if(i==1) {
        #  colname_vec[i:(i+(length(dnames)-1))] <- paste(dnames, as.character(tstamp), sep=";") 
        #} else if(i!=1 & is.even(i)) {
        #  colname_vec[(i+(length(dnames)-1)):(i+(length(dnames)))] <- paste(dnames, as.character(tstamp), sep=";")
        #} else if(i!=1 & is.odd(i)) {
        #  colname_vec[(i+(length(dnames)-1)):(i+(length(dnames)))] <- paste(dnames, as.character(tstamp), sep=";")
        #}
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
    }
    
  } else if(any(repository %in% c("SeaWifs", "MODIS_A", "SeaWifs_MODISA"))) {
    
    #if(frequency=="monthly") {
    #  print("Please note that monthly frequency is currently NOT supported for MODIS data. The frequency argument will be ignored.")
    #}
    
    if(any(var_names %in% "default") & data_type=="Chla") {
      dnames <- c("chlor_a")
    } else if(any(var_names %in% "default") & data_type=="PAR") {
      dnames <- c("par")
    } else if(any(var_names %in% "default") & data_type=="iPAR") {
      dnames <- c("ipar")
    } else if(any(var_names %in% "default") & data_type=="SST") {
      dnames <- c("sst")
    } else if(any(var_names %in% "default") & data_type=="NSST") {
      dnames <- c("sst")
    } else if(any(var_names %in% "default") & data_type=="PIC") {
      dnames <- c("pic")
    } else if(any(var_names %in% "default") & data_type=="POC") {
      dnames <- c("poc")
    } else if(any(var_names %in% "default") & data_type=="NFLH") {
      dnames <- c("nflh")
    } else if(any(var_names %in% "default") & data_type=="KD490") {
      dnames <- c("Kd_490")
    } else if(any(var_names %in% "default") & data_type=="Zeu") {
      dnames <- c("Zeu_lee")
    } else if(any(var_names %in% "default") & data_type=="BBP_GIOP") {
      dnames <- c("bbp_443_giop")
    } else if(any(var_names %in% "default") & data_type=="BBP_GSM") {
      dnames <- c("bbp_443_gsm")
    } else if(any(var_names %in% "default") & data_type=="BBP_QAA") {
      dnames <- c("bbp_443_qaa")
    } else if(any(var_names %in% "default") & data_type=="BBP_s_GIOP") {
      dnames <- c("bbp_s_giop")
    } else if(any(var_names %in% "default") & data_type=="Adg_GIOP") {
      dnames <- c("adg_443_giop")
    } else if(any(var_names %in% "default") & data_type=="Aph_GIOP") {
      dnames <- c("aph_443_giop")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    print("Obtaining reference coordinate grid from the first imported netCDF file...")
    nc_ex <- nc_open(nc_files[1])
    
    lon <- ncvar_get(nc_ex, "lon")
    nxgrid <- dim(lon)
    
    lat <- ncvar_get(nc_ex, "lat")
    nygrid <- dim(lat)
    
    proj_info <- ncatt_get(nc_ex, 0, "map_projection")$value
    
    totalgrid <- expand.grid(Longitude = lon, Latitude = lat, stringsAsFactors = FALSE)
    
    coordinates_test <- coordinates_full <- coordinates(totalgrid)
    
    colname_bckp <- c("Longitude", "Latitude")
    #colname_vec <- character(length(nc_files)*length(dnames))
    colname_vec <- c()
    
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    nc_close(nc_ex)
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    #data_total <- as.data.frame(coordinates_test)
    #colnames(data_total) <- NULL
    
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    
    print(paste("Processing netCDF files..."))
    cat("/n")
    
    for(i in seq_along(nc_files)) {
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")
      
      nc_ex <- nc_open(nc_files[i])
      
      lon <- ncvar_get(nc_ex, "lon")
      nxgrid <- dim(lon)
      
      lat <- ncvar_get(nc_ex, "lat")
      nygrid <- dim(lat)
      
      proj_info <- ncatt_get(nc_ex, 0, "map_projection")$value
      
      totalgrid <- expand.grid(Longitude = lon, Latitude = lat, stringsAsFactors = FALSE)
      
      coordinates_total <- coordinates(totalgrid)
      
      time_cov <- ncatt_get(nc_ex, 0, "time_coverage_start")
      tstamp <- as.Date(time_cov[["value"]])
      
      dlist <- list()
      dlname <- list()
      dunits <- list()
      dfillval <- list()
      dmissval <- list()
      dmissmean <- list()
      
      for(j in seq_along(dnames)) {
        listname <- paste(dnames[j])
        dlist[[listname]] <- ncvar_get(nc_ex, dnames[j])
        
        dlname[[listname]] <- ncatt_get(nc_ex, dnames[j], "long_name")
        dunits[[listname]] <- ncatt_get(nc_ex, dnames[j], "units")
        dfillval[[listname]] <- ncatt_get(nc_ex, dnames[j], "_FillValue")
      }
      
      #At this point the .nc file can be closed as it is no longer needed!
      nc_close(nc_ex)
      
      #Removing missing data. However, the below might be too complex for sustained use - think about simplifying!
      dlist_vec <- list()
      
      for(k in seq_along(dlist)) {
        
        if(remove_miss=="all") {
          dlist_vec[[k]] <- as.vector(dlist[[k]])
          dlist_vec[[k]][round(dlist_vec[[k]]) %in% dfillval[[k]]$value] <- NA
        } else if(remove_miss=="keep_coastal") {
          print("MODIS data does not support the keep_coastal option for missing values! Argument will be ignored, and all missing values removed.")
          dlist_vec[[k]] <- as.vector(dlist[[k]])
          dlist_vec[[k]][round(dlist_vec[[k]]) %in% dfillval[[k]]$value] <- NA
        }
      }
      
      #Subsetting coordinates within the loop
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !coordinates_total[,latin] < coord_subset[1] & !coordinates_total[,latin]>coord_subset[2] & !coordinates_total[,lonin]<coord_subset[3] & !coordinates_total[,lonin]>coord_subset[4]
        coordinates_total <- coordinates_total[coord_grep,]
        dlist_vec <- lapply(dlist_vec, function(x) {x[coord_grep]})
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(as.character(i), dnames)] <- paste(dnames, as.character(tstamp), sep=";")
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
    }
    
  } else if(repository=="NASA_Melt") {
    
    if(!is.null(data_type) & data_type!="MF") {
      print("Please note that only the MF (Melt-Freeze) data_type is supported for NASA_Melt! Data_type argument will be ignored.")
    }
    
    print("Note that the NASA melt-freeze data is only available at yearly resolution! The frequency argument will be ignored.")
    
    if(any(var_names %in% "default")) {
      dnames <- c("Earlymelt", "Melt", "Earlyfreeze", "Freeze")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    print("Obtaining reference coordinate grid from the first imported netCDF file...")
    nc_ex <- nc_open(nc_files[1])
    
    #xgrid <- ncvar_get(nc_ex, "x") #The dimensions in the file are quite useless, setting own coordinates using the NSIDC SIC grid...
    xgrid <- c(-3837500, -3812500, -3787500, -3762500, -3737500, -3712500, -3687500, -3662500, -3637500, -3612500, -3587500, -3562500, 
               -3537500, -3512500, -3487500, -3462500, -3437500, -3412500, -3387500, -3362500, -3337500, -3312500, -3287500, -3262500, 
               -3237500, -3212500, -3187500, -3162500, -3137500, -3112500, -3087500, -3062500, -3037500, -3012500, -2987500, -2962500, 
               -2937500, -2912500, -2887500, -2862500, -2837500, -2812500, -2787500, -2762500, -2737500, -2712500, -2687500, -2662500, 
               -2637500, -2612500, -2587500, -2562500, -2537500, -2512500, -2487500, -2462500, -2437500, -2412500, -2387500, -2362500, 
               -2337500, -2312500, -2287500, -2262500, -2237500, -2212500, -2187500, -2162500, -2137500, -2112500, -2087500, -2062500, 
               -2037500, -2012500, -1987500, -1962500, -1937500, -1912500, -1887500, -1862500, -1837500, -1812500, -1787500, -1762500, 
               -1737500, -1712500, -1687500, -1662500, -1637500, -1612500, -1587500, -1562500, -1537500, -1512500, -1487500, -1462500, 
               -1437500, -1412500, -1387500, -1362500, -1337500, -1312500, -1287500, -1262500, -1237500, -1212500, -1187500, -1162500, 
               -1137500, -1112500, -1087500, -1062500, -1037500, -1012500, -987500, -962500, -937500, -912500, -887500, -862500, -837500, 
               -812500, -787500, -762500, -737500, -712500, -687500, -662500, -637500, -612500, -587500, -562500, -537500, -512500, -487500, 
               -462500, -437500, -412500, -387500, -362500, -337500, -312500, -287500, -262500, -237500, -212500, -187500, -162500, -137500, 
               -112500, -87500, -62500, -37500, -12500, 12500, 37500, 62500, 87500, 112500, 137500, 162500, 187500, 212500, 237500, 262500, 
               287500, 312500, 337500, 362500, 387500, 412500, 437500, 462500, 487500, 512500, 537500, 562500, 587500, 612500, 637500, 662500, 
               687500, 712500, 737500, 762500, 787500, 812500, 837500, 862500, 887500, 912500, 937500, 962500, 987500, 1012500, 1037500, 1062500, 
               1087500, 1112500, 1137500, 1162500, 1187500, 1212500, 1237500, 1262500, 1287500, 1312500, 1337500, 1362500, 1387500, 1412500, 
               1437500, 1462500, 1487500, 1512500, 1537500, 1562500, 1587500, 1612500, 1637500, 1662500, 1687500, 1712500, 1737500, 1762500, 
               1787500, 1812500, 1837500, 1862500, 1887500, 1912500, 1937500, 1962500, 1987500, 2012500, 2037500, 2062500, 2087500, 2112500, 
               2137500, 2162500, 2187500, 2212500, 2237500, 2262500, 2287500, 2312500, 2337500, 2362500, 2387500, 2412500, 2437500, 2462500, 
               2487500, 2512500, 2537500, 2562500, 2587500, 2612500, 2637500, 2662500, 2687500, 2712500, 2737500, 2762500, 2787500, 2812500, 
               2837500, 2862500, 2887500, 2912500, 2937500, 2962500, 2987500, 3012500, 3037500, 3062500, 3087500, 3112500, 3137500, 3162500, 
               3187500, 3212500, 3237500, 3262500, 3287500, 3312500, 3337500, 3362500, 3387500, 3412500, 3437500, 3462500, 3487500, 3512500, 
               3537500, 3562500, 3587500, 3612500, 3637500, 3662500, 3687500, 3712500, 3737500)
    
    #ygrid <- ncvar_get(nc_ex, "y")
    ygrid <- c(5837500, 5812500, 5787500, 5762500, 5737500, 5712500, 5687500, 5662500, 5637500, 5612500, 5587500, 5562500, 5537500, 5512500, 
               5487500, 5462500, 5437500, 5412500, 5387500, 5362500, 5337500, 5312500, 5287500, 5262500, 5237500, 5212500, 5187500, 5162500, 
               5137500, 5112500, 5087500, 5062500, 5037500, 5012500, 4987500, 4962500, 4937500, 4912500, 4887500, 4862500, 4837500, 4812500, 
               4787500, 4762500, 4737500, 4712500, 4687500, 4662500, 4637500, 4612500, 4587500, 4562500, 4537500, 4512500, 4487500, 4462500, 
               4437500, 4412500, 4387500, 4362500, 4337500, 4312500, 4287500, 4262500, 4237500, 4212500, 4187500, 4162500, 4137500, 4112500, 
               4087500, 4062500, 4037500, 4012500, 3987500, 3962500, 3937500, 3912500, 3887500, 3862500, 3837500, 3812500, 3787500, 3762500, 
               3737500, 3712500, 3687500, 3662500, 3637500, 3612500, 3587500, 3562500, 3537500, 3512500, 3487500, 3462500, 3437500, 3412500, 
               3387500, 3362500, 3337500, 3312500, 3287500, 3262500, 3237500, 3212500, 3187500, 3162500, 3137500, 3112500, 3087500, 3062500, 
               3037500, 3012500, 2987500, 2962500, 2937500, 2912500, 2887500, 2862500, 2837500, 2812500, 2787500, 2762500, 2737500, 2712500, 
               2687500, 2662500, 2637500, 2612500, 2587500, 2562500, 2537500, 2512500, 2487500, 2462500, 2437500, 2412500, 2387500, 2362500, 
               2337500, 2312500, 2287500, 2262500, 2237500, 2212500, 2187500, 2162500, 2137500, 2112500, 2087500, 2062500, 2037500, 2012500, 
               1987500, 1962500, 1937500, 1912500, 1887500, 1862500, 1837500, 1812500, 1787500, 1762500, 1737500, 1712500, 1687500, 1662500, 
               1637500, 1612500, 1587500, 1562500, 1537500, 1512500, 1487500, 1462500, 1437500, 1412500, 1387500, 1362500, 1337500, 1312500, 
               1287500, 1262500, 1237500, 1212500, 1187500, 1162500, 1137500, 1112500, 1087500, 1062500, 1037500, 1012500, 987500, 962500, 
               937500, 912500, 887500, 862500, 837500, 812500, 787500, 762500, 737500, 712500, 687500, 662500, 637500, 612500, 587500, 562500, 
               537500, 512500, 487500, 462500, 437500, 412500, 387500, 362500, 337500, 312500, 287500, 262500, 237500, 212500, 187500, 162500, 
               137500, 112500, 87500, 62500, 37500, 12500, -12500, -37500, -62500, -87500, -112500, -137500, -162500, -187500, -212500, 
               -237500, -262500, -287500, -312500, -337500, -362500, -387500, -412500, -437500, -462500, -487500, -512500, -537500, -562500, 
               -587500, -612500, -637500, -662500, -687500, -712500, -737500, -762500, -787500, -812500, -837500, -862500, -887500, -912500, 
               -937500, -962500, -987500, -1012500, -1037500, -1062500, -1087500, -1112500, -1137500, -1162500, -1187500, -1212500, -1237500, 
               -1262500, -1287500, -1312500, -1337500, -1362500, -1387500, -1412500, -1437500, -1462500, -1487500, -1512500, -1537500, -1562500, 
               -1587500, -1612500, -1637500, -1662500, -1687500, -1712500, -1737500, -1762500, -1787500, -1812500, -1837500, -1862500, -1887500, 
               -1912500, -1937500, -1962500, -1987500, -2012500, -2037500, -2062500, -2087500, -2112500, -2137500, -2162500, -2187500, -2212500, 
               -2237500, -2262500, -2287500, -2312500, -2337500, -2362500, -2387500, -2412500, -2437500, -2462500, -2487500, -2512500, -2537500, 
               -2562500, -2587500, -2612500, -2637500, -2662500, -2687500, -2712500, -2737500, -2762500, -2787500, -2812500, -2837500, -2862500, 
               -2887500, -2912500, -2937500, -2962500, -2987500, -3012500, -3037500, -3062500, -3087500, -3112500, -3137500, -3162500, -3187500, 
               -3212500, -3237500, -3262500, -3287500, -3312500, -3337500, -3362500, -3387500, -3412500, -3437500, -3462500, -3487500, -3512500, 
               -3537500, -3562500, -3587500, -3612500, -3637500, -3662500, -3687500, -3712500, -3737500, -3762500, -3787500, -3812500, -3837500, 
               -3862500, -3887500, -3912500, -3937500, -3962500, -3987500, -4012500, -4037500, -4062500, -4087500, -4112500, -4137500, -4162500, 
               -4187500, -4212500, -4237500, -4262500, -4287500, -4312500, -4337500, -4362500, -4387500, -4412500, -4437500, -4462500, -4487500, 
               -4512500, -4537500, -4562500, -4587500, -4612500, -4637500, -4662500, -4687500, -4712500, -4737500, -4762500, -4787500, -4812500, 
               -4837500, -4862500, -4887500, -4912500, -4937500, -4962500, -4987500, -5012500, -5037500, -5062500, -5087500, -5112500, -5137500, 
               -5162500, -5187500, -5212500, -5237500, -5262500, -5287500, -5312500, -5337500)
    
    #The projection for this data is NSIDC North Polar Stereographic (EPSG 3411); see Markus et al., 2009 for details...
    proj_info <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs +init=epsg:3411"
    
    totalgrid_m <- expand.grid(xcoord = xgrid, ycoord = ygrid, stringsAsFactors = FALSE)
    
    #No choice for EPSG 3031 South Polar projection since this data is only available for the Arctic Ocean; only EPSG 3411 needed here!
    cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(proj_info))
    cord.new <- spTransform(cord.dec, sp::CRS("+init=epsg:4326 +proj=longlat"))
    coordinates_test <- coordinates_full <- cbind(coordinates(cord.dec), coordinates(cord.new))
    colnames(coordinates_test) <- coordinates_full <- c("xcoord", "ycoord", "Longitude", "Latitude")
    
    colname_bckp <- c("xcoord", "ycoord", "Longitude", "Latitude")
    colname_vec <- c()
    
    xcori <- match(c("xcoord"), colname_bckp)
    ycori <- match(c("ycoord"), colname_bckp)
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    nc_close(nc_ex)
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    
    print(paste("Processing netCDF files..."))
    cat("/n")
    
    #This code runs the loop for all files, testing the coordinate data of each against coordinates_test!
    for(i in seq_along(nc_files)) { 
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")
      
      nc_ex <- nc_open(nc_files[i])
      
      #These are not needed, use the old coordinates_test instead
      #xgrid <- ncvar_get(nc_ex, "xgrid")
      #ygrid <- ncvar_get(nc_ex, "ygrid")
      
      proj_info <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs +init=epsg:3411"
      
      #Getting the time variable (in this case, year only is available)
      tstamp <- ncatt_get(nc_ex, 0, "Year")[["value"]]
      
      dlist <- list()
      dlname <- list()
      dunits <- list()
      drange <- list() #For determining the values OUTSIDE of the permitted range of the dataset
      dfillval <- list()
      
      for(j in seq_along(dnames)) {
        listname <- paste(dnames[j])
        dlist[[listname]] <- ncvar_get(nc_ex, dnames[j])
        
        dlname[[listname]] <- ncatt_get(nc_ex, dnames[j], "long_name")
        dunits[[listname]] <- ncatt_get(nc_ex, dnames[j], "units")
        drange[[listname]] <- as.numeric(unlist(strsplit(ncatt_get(nc_ex, dnames[j], "Valid_Range")[["value"]], "-")))
        dfillval[[listname]] <- unique(as.vector(dlist[[listname]][!dlist[[listname]] %in% c(drange[[listname]][1]:drange[[listname]][2])]))
      }
      
      #At this point the .nc file can be closed as it is no longer needed!
      nc_close(nc_ex)
      
      #Removing missing data.
      dlist_vec <- list()
      
      for(k in seq_along(dlist)) {
        dlist_vec[[k]] <- as.vector(dlist[[k]])
        dlist_vec[[k]][round(dlist_vec[[k]]) %in% dfillval[[k]]] <- NA
      }
      
      #Expanding and converting coordinates
      totalgrid_m <- expand.grid(xcoord = xgrid, ycoord = ygrid, stringsAsFactors = FALSE)
      
      cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(proj_info))
      cord.new <- spTransform(cord.dec, sp::CRS("+init=epsg:4326 +proj=longlat"))
      coordinates_total <- cbind(coordinates(cord.dec), coordinates(cord.new))
      colnames(coordinates_total) <- c("xcoord", "ycoord", "Longitude", "Latitude")
      
      
      #Subsetting coordinates in the looping dataset
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !coordinates_total[,latin] < coord_subset[1] & !coordinates_total[,latin]>coord_subset[2] & !coordinates_total[,lonin]<coord_subset[3] & !coordinates_total[,lonin]>coord_subset[4]
        coordinates_total <- coordinates_total[coord_grep,]
        dlist_vec <- lapply(dlist_vec, function(x) {x[coord_grep]})
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_test)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(as.character(i), dnames)] <- paste(dnames, as.character(tstamp), sep=";")
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
    }
    
  } else if(repository=="CEDA_MO") {
    
    if(frequency=="mon-30y" & length(nc_files)>1 & mode=="summary") { stop("Only one file at a time can be analysed when frequency is mon-30y!") }
    
    if(!any(c("groundfrost", "hurs", "psl", "pv", "rainfall", "sfcWind", "sun", "tas", "tasmax", "tasmin") %in% data_type)) {
      stop("Incorrect data type selected for CEDA_MO! Use one of: groundfrost, hurs, psl, pv, rainfall, sfcWind,
           sun, tas, tasmax, or tasmin.")
    }
    
    if(any(var_names %in% "default") & data_type=="groundfrost") {
      dnames <- c("groundfrost")
    } else if(any(var_names %in% "default") & data_type=="hurs") {
      dnames <- c("hurs")
    } else if(any(var_names %in% "default") & data_type=="psl") {
      dnames <- c("psl")
    } else if(any(var_names %in% "default") & data_type=="pv") {
      dnames <- c("pv")
    } else if(any(var_names %in% "default") & data_type=="rainfall") {
      dnames <- c("rainfall")
    } else if(any(var_names %in% "default") & data_type=="sfcWind") {
      dnames <- c("sfcWind")
    } else if(any(var_names %in% "default") & data_type=="sun") {
      dnames <- c("sun")
    } else if(any(var_names %in% "default") & data_type=="tas") {
      dnames <- c("tas")
    } else if(any(var_names %in% "default") & data_type=="tasmax") {
      dnames <- c("tasmax")
    } else if(any(var_names %in% "default") & data_type=="tasmin") {
      dnames <- c("tasmin")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    print("Obtaining reference coordinate grid from the first imported netCDF file...")
    nc_ex <- nc_open(nc_files[1])
    
    #Retrieving coordinate dimensions (in meters!)
    lon <- ncvar_get(nc_ex, "projection_x_coordinate") #"longitude"
    nxgrid <- dim(lon)
    
    lat <- ncvar_get(nc_ex, "projection_y_coordinate") #"latitude"
    nygrid <- dim(lat)
    
    #Getting map projection info - lon_0 is false! It is actually -2
    #proj_info <- c()
    #proj_info["id"] <- ncatt_get(nc_ex, "transverse_mercator", "grid_mapping_name")[["value"]]
    #proj_info["longitude_of_prime_meridian"] <- ncatt_get(nc_ex, "transverse_mercator", "longitude_of_prime_meridian")[["value"]]
    #proj_info["semi_major_axis"] <- ncatt_get(nc_ex, "transverse_mercator", "semi_major_axis")[["value"]]
    #proj_info["semi_minor_axis"] <- ncatt_get(nc_ex, "transverse_mercator", "semi_minor_axis")[["value"]]
    #proj_info["longitude_of_central_meridian"] <- ncatt_get(nc_ex, "transverse_mercator", "longitude_of_central_meridian")[["value"]]
    #proj_info["latitude_of_projection_origin"] <- ncatt_get(nc_ex, "transverse_mercator", "latitude_of_projection_origin")[["value"]]
    #proj_info["false_easting"] <- ncatt_get(nc_ex, "transverse_mercator", "false_easting")[["value"]]
    #proj_info["false_northing"] <- ncatt_get(nc_ex, "transverse_mercator", "false_northing")[["value"]]
    #proj_info["scale_factor_at_central_meridian"] <- ncatt_get(nc_ex, "transverse_mercator", "scale_factor_at_central_meridian")[["value"]]
    #proj_info["units"] <- ncatt_get(nc_ex, "projection_y_coordinate", "units")[["value"]]
    #proj_info["x_bounds"] <- ncvar_get(nc_ex, "projection_x_coordinate_bnds")
    #proj_info["y_bounds"] <- ncvar_get(nc_ex, "projection_y_coordinate_bnds")
    
    proj_info <- "+proj=tmerc +lon_0=-2 +a=6377563.396 +b=6356256.909 +lat_0=49 +x_0=4e+05 +y_0=-1e+05 +k_0=0.9996012717 +units=m +no_defs"
    
    coord_dims <- c(min(ncvar_get(nc_ex, "longitude")), max(ncvar_get(nc_ex, "longitude")), 
                    min(ncvar_get(nc_ex, "latitude")), max(ncvar_get(nc_ex, "latitude")))
    
    fill_val <- ncatt_get(nc_ex, dnames, "_FillValue")[["value"]]
    
    #totalgrid_m <- expand.grid(xcoord = lon, ycoord = lat, stringsAsFactors = FALSE)
    
    #cord.dec <- SpatialPoints(totalgrid_m, proj4string = sp::CRS(proj_info))
    #cord.new <- spTransform(cord.dec, sp::CRS(st_crs(4326)[[2]]))
    
    #Alternative with SF package (takes too long...)
    #cord.new <- st_as_sf(cord.dec)
    #cord.new <- sf::st_transform(cord.new, crs=st_crs(4326)[[2]])
    #cord.new <- st_coordinates(cord.new)
    
    #coordinates_total <- cbind(coordinates(cord.dec), coordinates(cord.new))
    #colnames(coordinates_total) <- c("xcoord", "ycoord", "Longitude", "Latitude")
    
    nc_close(nc_ex)
    
    #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
    test_rast <- suppressWarnings(stack(nc_files[1]))
    crs(test_rast) <- "+proj=tmerc +lon_0=-2 +a=6377563.396 +b=6356256.909 +lat_0=49 +x_0=4e+05 +y_0=-1e+05 +k_0=0.9996012717 +units=m +no_defs"
    test_rast <- projectRaster(test_rast, crs = crs(st_crs(4326)[[2]]))
    
    xyz_rast <- rasterToPoints(test_rast)
    xyz_rast[xyz_rast==fill_val] <- NA
    colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
    
    coordinates_test <- coordinates_full <- xyz_rast[,c("Longitude", "Latitude")]
    
    colname_bckp <- c("Longitude", "Latitude")
    #colname_vec <- character(length(nc_files)*length(dnames))
    colname_vec <- c()
    
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    rm(test_rast)
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    #data_total <- as.data.frame(coordinates_test)
    #colnames(data_total) <- NULL
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    print(paste("Processing NetCDF files..."))
    cat("/n")
    
    for(i in seq_along(nc_files)) {
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")

      #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
      test_rast <- suppressWarnings(stack(nc_files[i]))
      crs(test_rast) <- "+proj=tmerc +lon_0=-2 +a=6377563.396 +b=6356256.909 +lat_0=49 +x_0=4e+05 +y_0=-1e+05 +k_0=0.9996012717 +units=m +no_defs"
      test_rast <- projectRaster(test_rast, crs = crs(st_crs(4326)[[2]]))
      
      xyz_rast <- rasterToPoints(test_rast)
      
      rm(test_rast)
      
      xyz_rast[xyz_rast==fill_val] <- NA
      colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
      
      
      
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !xyz_rast[,latin] < coord_subset[1] & !xyz_rast[,latin]>coord_subset[2] & !xyz_rast[,lonin]<coord_subset[3] & !xyz_rast[,lonin]>coord_subset[4]
        xyz_rast <- xyz_rast[coord_grep,]
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      coordinates_total <- xyz_rast[,c("Longitude", "Latitude")]
      
      dlist <- as.data.frame(xyz_rast[,!colnames(xyz_rast) %in% c("Longitude", "Latitude")])
      dlist_vec <- as.list(dlist[,as.numeric(month_ind), drop=FALSE])
      
      if(frequency=="monthly") {
        tstamp <- paste(import_vec[i], month_ind, sep="-")
      } else if(frequency=="mon-30y") {
        tstamp <- paste(min(year_rng),max(year_rng), month_ind, sep="-")
      }
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(dnames, as.character(tstamp), sep=";")] <- paste(dnames, as.character(tstamp), sep=";")
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
      
    }
    
  } else if(repository=="TClim") {
      
    if(frequency=="mon-30y" & length(nc_files)>1 & mode=="summary") { stop("Only one file at a time can be analysed when frequency is mon-30y!") }
    
    if(!any(c("aet", "def", "swe", "q", "soil", "PDSI", "pet", "ppt", "srad", "tmax", "tmin", "vap", "vpd", "ws") %in% data_type)) {
      stop("Incorrect data type selected for TClim! Use one of: aet, def, swe, q, soil, PDSI, pet, ppt, srad, tmax, tmin, vap, vpd, or ws.")
    }
    
    if(any(var_names %in% "default") & data_type=="aet") {
      dnames <- c("aet")
    } else if(any(var_names %in% "default") & data_type=="def") {
      dnames <- c("def")
    } else if(any(var_names %in% "default") & data_type=="swe") {
      dnames <- c("swe")
    } else if(any(var_names %in% "default") & data_type=="q") {
      dnames <- c("q")
    } else if(any(var_names %in% "default") & data_type=="soil") {
      dnames <- c("soil")
    } else if(any(var_names %in% "default") & data_type=="PDSI") {
      dnames <- c("PDSI")
    } else if(any(var_names %in% "default") & data_type=="pet") {
      dnames <- c("pet")
    } else if(any(var_names %in% "default") & data_type=="ppt") {
      dnames <- c("ppt")
    } else if(any(var_names %in% "default") & data_type=="srad") {
      dnames <- c("srad")
    } else if(any(var_names %in% "default") & data_type=="tmax") {
      dnames <- c("tmax")
    } else if(any(var_names %in% "default") & data_type=="tmin") {
      dnames <- c("tmin")
    } else if(any(var_names %in% "default") & data_type=="vap") {
      dnames <- c("vap")
    } else if(any(var_names %in% "default") & data_type=="vpd") {
      dnames <- c("vpd")
    } else if(any(var_names %in% "default") & data_type=="ws") {
      dnames <- c("ws")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    print("Obtaining reference coordinate grid from the first imported netCDF file...")
    nc_ex <- nc_open(nc_files[1])
    
    proj_info <- st_crs(4326)[[2]]
    
    fill_val <- ncatt_get(nc_ex, dnames, "_FillValue")[["value"]]
    
    rm(nc_ex)
    
    #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
    test_rast <- suppressWarnings(brick(nc_files[1]))
    test_rast <- test_rast[[as.numeric(month_ind)]]
    crs(test_rast) <- proj_info
    #test_rast <- raster::aggregate(test_rast, factor = 4, fun=mean, na.rm=TRUE) #Aggregates surrounding cells, whereas projectRaster (below) reproduces the raster with less cells!
    test_rast <- projectRaster(test_rast, res=res(test_rast)[1]*3, crs=proj_info)
    
    xyz_rast <- rasterToPoints(test_rast)
    rm(test_rast)
    
    xyz_rast[xyz_rast==fill_val] <- NA
    colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
    
    coordinates_test <- xyz_rast[,c("Longitude", "Latitude")]
    rm(xyz_rast)
    coordinates_full <- c()
    
    colname_bckp <- c("Longitude", "Latitude")
    #colname_vec <- character(length(nc_files)*length(dnames))
    colname_vec <- c()
    
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    #data_total <- as.data.frame(coordinates_test)
    #colnames(data_total) <- NULL
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    print(paste("Processing NetCDF files..."))
    cat("/n")
    
    for(i in seq_along(nc_files)) {
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")
      
      #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
      test_rast <- suppressWarnings(stack(nc_files[i]))
      test_rast <- test_rast[[as.numeric(month_ind)]]
      crs(test_rast) <- proj_info
      test_rast <- projectRaster(test_rast, res=res(test_rast)[1]*3, crs=proj_info)
      
      xyz_rast <- rasterToPoints(test_rast)
      rm(test_rast)
      
      xyz_rast[xyz_rast==fill_val] <- NA
      colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
      
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !xyz_rast[,latin] < coord_subset[1] & !xyz_rast[,latin]>coord_subset[2] & !xyz_rast[,lonin]<coord_subset[3] & !xyz_rast[,lonin]>coord_subset[4]
        xyz_rast <- xyz_rast[coord_grep,]
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      coordinates_total <- xyz_rast[,c("Longitude", "Latitude")]
      
      xyz_rast <- as.data.frame(xyz_rast[,!colnames(xyz_rast) %in% c("Longitude", "Latitude")])
      dlist_vec <- xyz_rast
      rm(xyz_rast)
      dlist_vec <- as.list(dlist_vec)
      
      if(frequency=="monthly") {
        tstamp <- paste(import_vec[i], month_ind, sep="-")
      } else if(frequency=="mon-30y") {
        tstamp <- paste(min(year_rng),max(year_rng), month_ind, sep="-")
      }
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(dnames, as.character(tstamp), sep=";")] <- paste(dnames, as.character(tstamp), sep=";")
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
      
    }
    
  } else if(repository=="CRU4") {
      
    if(!any(c("cld", "dtr", "frs", "pet", "pre", "tmn", "tmp", "tmx", "vap", "wet") %in% data_type)) {
      stop("Incorrect data type selected for CRU 4.03! Use one of: cld, dtr, frs, pet, pre, tmn, tmp, tmx, vap, or wet.")
    }
    
    if(any(var_names %in% "default") & data_type=="cld") {
      dnames <- c("cld")
    } else if(any(var_names %in% "default") & data_type=="dtr") {
      dnames <- c("dtr")
    } else if(any(var_names %in% "default") & data_type=="frs") {
      dnames <- c("frs")
    } else if(any(var_names %in% "default") & data_type=="pet") {
      dnames <- c("pet")
    } else if(any(var_names %in% "default") & data_type=="pre") {
      dnames <- c("pre")
    } else if(any(var_names %in% "default") & data_type=="tmn") {
      dnames <- c("tmn")
    } else if(any(var_names %in% "default") & data_type=="tmp") {
      dnames <- c("tmp")
    } else if(any(var_names %in% "default") & data_type=="tmx") {
      dnames <- c("tmx")
    } else if(any(var_names %in% "default") & data_type=="vap") {
      dnames <- c("vap")
    } else if(any(var_names %in% "default") & data_type=="wet") {
      dnames <- c("wet")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    print("Obtaining reference coordinate grid from the first imported netCDF file...")
    nc_ex <- nc_open(nc_files[1])
    
    proj_info <- st_crs(4326)[[2]]
    
    fill_val <- ncatt_get(nc_ex, dnames, "_FillValue")[["value"]]
    
    rm(nc_ex)
    
    #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
    test_rast <- suppressWarnings(brick(nc_files[1]))
    test_rast <- test_rast[[grep(paste0(sprintf('%s.%s',expand.grid(year_rng, month_ind)[,1], expand.grid(year_rng, month_ind)[,2]), collapse="|"), names(test_rast))]]
    #crs(test_rast) <- proj_info
    #test_rast <- raster::aggregate(test_rast, factor = 4, fun=mean, na.rm=TRUE) #Aggregates surrounding cells, whereas projectRaster (below) reproduces the raster with less cells!
    #test_rast <- projectRaster(test_rast, res=res(test_rast)[1]*3, crs=proj_info)
    
    xyz_rast <- rasterToPoints(test_rast)
    rm(test_rast)
    
    xyz_rast[xyz_rast==fill_val] <- NA
    colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
    
    coordinates_test <- coordinates_full <- xyz_rast[,c("Longitude", "Latitude")]
    rm(xyz_rast)
    
    colname_bckp <- c("Longitude", "Latitude")
    #colname_vec <- character(length(nc_files)*length(dnames))
    colname_vec <- c()
    
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    #data_total <- as.data.frame(coordinates_test)
    #colnames(data_total) <- NULL
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    print(paste("Processing NetCDF files..."))
    cat("/n")
    
    for(i in seq_along(nc_files)) {
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")
      
      #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
      test_rast <- suppressWarnings(stack(nc_files[i]))
      test_rast <- test_rast[[grep(paste0(sprintf('%s.%s',expand.grid(year_rng, month_ind)[,1], expand.grid(year_rng, month_ind)[,2]), collapse="|"), names(test_rast))]]
      #crs(test_rast) <- proj_info
      #test_rast <- raster::aggregate(test_rast, factor = 4, fun=mean, na.rm=TRUE) #Aggregates surrounding cells, whereas projectRaster (below) reproduces the raster with less cells!
      #test_rast <- projectRaster(test_rast, res=res(test_rast)[1]*3, crs=proj_info)
      
      xyz_rast <- rasterToPoints(test_rast)
      rm(test_rast)
      
      xyz_rast[xyz_rast==fill_val] <- NA
      colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
      
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !xyz_rast[,latin] < coord_subset[1] & !xyz_rast[,latin]>coord_subset[2] & !xyz_rast[,lonin]<coord_subset[3] & !xyz_rast[,lonin]>coord_subset[4]
        xyz_rast <- xyz_rast[coord_grep,]
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      coordinates_total <- xyz_rast[,c("Longitude", "Latitude")]
      
      xyz_rast <- as.data.frame(xyz_rast[,!colnames(xyz_rast) %in% c("Longitude", "Latitude")])
      dlist_vec <- xyz_rast
      rm(xyz_rast)
      dlist_vec <- as.list(dlist_vec)
      
      
      tstamp <- paste0(sprintf('%s-%s',expand.grid(year_rng, month_ind)[,1], expand.grid(year_rng, month_ind)[,2]))
      
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(dnames, as.character(tstamp), sep=";")] <- paste(dnames, as.character(tstamp), sep=";")
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
      
    }
  } else if(repository=="HYCOM_MLD") {
      
    if(!any(c("mld") %in% data_type)) {
      stop("Incorrect data type selected for HYCOM_MLD! Use one of: mld.")
    }
    
    if(any(var_names %in% "default") & data_type=="mld") {
      dnames <- c("mld")
    } else if(!any(var_names %in% "default")) {
      dnames <- var_names
    }
    
    print("Obtaining reference coordinate grid from the first imported HDF file...")
    proj_info <- st_crs(4326)[[2]]
    fill_val <- -9999.0
    
    #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
    test_rast <- suppressWarnings(raster(nc_files[1]))
    extent(test_rast) <- c(-180, 180, -90, 90)
    crs(test_rast) <- proj_info
    
    #For tstamp
    rastname <- names(test_rast)
    
    xyz_rast <- rasterToPoints(test_rast)
    rm(test_rast)
    
    xyz_rast[xyz_rast==fill_val] <- NA
    colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
    
    coordinates_test <- coordinates_full <- xyz_rast[,c("Longitude", "Latitude")]
    rm(xyz_rast)
    
    colname_bckp <- c("Longitude", "Latitude")
    #colname_vec <- character(length(nc_files)*length(dnames))
    colname_vec <- c()
    
    lonin <- match(c("Longitude"), colname_bckp)
    latin <- match(c("Latitude"), colname_bckp)
    
    if(!is.null(coord_subset) & is.numeric(coord_subset)) {
      
      coordinates_test <- coordinates_test[!coordinates_test[,latin] < coord_subset[1] & !coordinates_test[,latin]>coord_subset[2] & !coordinates_test[,lonin]<coord_subset[3] & !coordinates_test[,lonin]>coord_subset[4],]
      
    } else if(!is.null(coord_subset) & is.character(coord_subset)) {
      print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
    }
    
    data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0])
    #data_total <- as.data.frame(coordinates_test)
    #colnames(data_total) <- NULL
    
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
    
    print(paste("Processing NetCDF files..."))
    cat("/n")
    
    for(i in seq_along(nc_files)) {
      
      svMisc::progress(i, max.value=length(nc_files), progress.bar = FALSE)
      Sys.sleep(1/length(nc_files))
      if(i==length(nc_files)) cat("Done!\n")
      
      #Rasterising and passing the proj4 string! (raster can interpret this automatically normally, but not the semi_minor_axis attribute IN THIS CASE!)
      test_rast <- suppressWarnings(raster(nc_files[i]))
      extent(test_rast) <- c(-180, 180, -90, 90)
      crs(test_rast) <- proj_info
      
      xyz_rast <- rasterToPoints(test_rast)
      rm(test_rast)
      
      xyz_rast[xyz_rast==fill_val] <- NA
      colnames(xyz_rast)[1:2] <- c("Longitude", "Latitude")
      
      if(!is.null(coord_subset) & is.numeric(coord_subset)) {
        coord_grep <- !xyz_rast[,latin] < coord_subset[1] & !xyz_rast[,latin]>coord_subset[2] & !xyz_rast[,lonin]<coord_subset[3] & !xyz_rast[,lonin]>coord_subset[4]
        xyz_rast <- xyz_rast[coord_grep,]
        
      } else if(!is.null(coord_subset) & is.character(coord_subset)) {
        print("SUBSETTING BY POLYGONS STILL NOT IMPLEMENTED!")
      }
      
      coordinates_total <- xyz_rast[,c("Longitude", "Latitude")]
      
      xyz_rast <- as.data.frame(xyz_rast[,!colnames(xyz_rast) %in% c("Longitude", "Latitude")])
      dlist_vec <- xyz_rast
      rm(xyz_rast)
      dlist_vec <- as.list(dlist_vec)
      
      if(frequency=="daily") {
        tstamp <- paste0(gsub("\\D{3}.([0-9]{4}).*", "\\1", rastname), "-day", gsub(".*(\\d{3}).*", "\\1", rastname))
      } else if(frequency=="monthly") {
        yearsub <- gsub("\\D{3}.([0-9]{4}).*", "\\1", rastname)
        mon_daysub <- gsub(".*(\\d{3}).*", "\\1", rastname)
        monthdays <- get_monthdays(as.numeric(yearsub), when="first")[[1]]
        tstamp <- paste0(yearsub, "-mon", names(monthdays)[grep(mon_daysub, monthdays)])
      }
      
      
      
      if(identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        
        dlist_vec <- do.call(cbind.data.frame, dlist_vec)
        data_total <- cbind.data.frame(data_total, dlist_vec, stringsAsFactors=FALSE)
        colnames(data_total) <- NULL
        
        colname_vec[paste(dnames, as.character(tstamp), sep=";")] <- paste(dnames, as.character(tstamp), sep=";")
        
        if(get_free_ram() < big_thres | big_data[2]=="force" | i==length(nc_files)) {
          if(i==length(nc_files)) {print("Exporting data file from RAM onto disk... Please hold.")}
          
          data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), append=TRUE, sep=",", dec=".", quote=FALSE, row.names=FALSE, col.names=FALSE, na=NA, showProgress = FALSE)
          rm(data_total)
          data_total <- data.frame(coordinates_test[1:nrow(coordinates_test),0]) 
          invisible(gc())
        }
        
        
      } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
        stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
      }
      
    }
    
    }
  
  #Summary statistics calculations
  #Calculating summary statistics (mean, sd, sum, anomaly) of EACH variable extracted from netCDF files
  #The above are calculated either for the whole chosen period, or ADDITIONALLY for each year/month within that period separately
  #gc()
  
  data_barebones <- as.data.frame(coordinates_total)
  data_monthly_summary <- data_barebones
  
  # 1). Create lists and vectors to store the output of each summary statistic (mean, sd) for each variable and summarised time period
  
  data_ln <- nrow(coordinates_test)
  chunk_vec <- ggplot2::cut_interval(1:data_ln, length=as.numeric(big_data[3]), labels=FALSE)
  
  if(length(unique(chunk_vec))==1) {
    print("The chosen chunk size is larger than the dataset. Attempting to load the entire file at once.")
  } else if(length(unique(chunk_vec))>1) {
    print(paste("Breaking the data into a total of", length(unique(chunk_vec)), "partitions for analysis..."))
  }
  
  #vec_ex <- numeric(length=data_ln)
  #chunk_list <- list() #vector("list", length=5) 
  #chunk_df <- as.data.frame(matrix(nrow=data_ln, ncol=0))
  chunk_list <- monthly_list <- list()
  
  #Depending on "mode" argument ("extract" or "summary"), the data is extracted or additionally processed, respectively.
  if(mode=="summary") {
    
    for(chunk in unique(chunk_vec)) {
      
      data_total <- fread(file=paste0(big_data[1], "/data_total.csv"), sep=",", dec=".", header = FALSE, select=which(chunk_vec==chunk), nThread=as.numeric(big_data[4]), data.table=FALSE, check.names = FALSE)
      colnames(data_total) <- NULL
      #rownames(data_total) <- colname_vec
      
      for(i in seq_along(dnames)) {
        print(paste0("Summarising data for partition number ", chunk, " and variable ", dnames[i], "..."))
        
        if(summary_func=="total" | summary_func=="yearly" | summary_func=="yearly_monthly") {
          
          if(is.numeric(day_rng) & !any(is.null(day_rng))) {
            slist_name <- paste(dnames[i], " (", min(year_rng), "-",max(year_rng), " days", day_rng[1], "-", day_rng[length(day_rng)], ")", sep="")
          } else if(any(is.null(day_rng)) & !is.numeric(day_rng)) {
            slist_name <- paste(dnames[i], " (", min(year_rng), "-",max(year_rng), " ", month_ind[1], "-", month_ind[length(month_ind)], ")", sep="")
          }
          
          chunk_grep <- grep(paste0("^", dnames[i]), colname_vec)
          chunk_list[[paste0("Mean of ", slist_name)]][which(chunk_vec==chunk)] <- total_recmean <- apply(data_total[chunk_grep,], 2, mean, na.rm=TRUE)
          chunk_list[[paste0("SD of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, sd, na.rm=TRUE)
          if(calc_sum==TRUE) {
            chunk_list[[paste0("Sum of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, sum, na.rm=TRUE)
          }
          
          if(n_limit>0 & is.numeric(n_limit)) {
            chunk_list[[paste0("SSize of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, function(x){ length(which(!is.na(x))) })
          }
          
        }
        
        if(summary_func=="yearly"| summary_func=="yearly_monthly") {
          
          if(is.numeric(day_rng)) {
            stop("Yearly and yearly_monthly summary functions are not supported with day ranges (day_rng)! Stopping...")
          }
          
          for(j in seq_along(year_rng)) {
            print(paste0("Summarising data for year ", j, " of ", length(year_rng), " (", year_rng[j], ")"))
            
            slist_name <- paste(dnames[i], " (", month_ind[1], "-", month_ind[length(month_ind)], " ", year_rng[j], ")", sep="")
            chunk_grep <- grep(paste("^", dnames[i], ";", year_rng[j], sep=""), colname_vec)
            chunk_list[[paste0("Mean of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, mean, na.rm=TRUE)
            chunk_list[[paste0("SD of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, sd, na.rm=TRUE)
            
            if(calc_sum==TRUE) {
              chunk_list[[paste0("Sum of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, sum, na.rm=TRUE)
            }
            
            if(calc_anom==TRUE) {
              chunk_list[[paste0("Anomaly of ", slist_name)]][which(chunk_vec==chunk)] <- chunk_list[[paste0("Mean of ", slist_name)]][which(chunk_vec==chunk)] - total_recmean
            }
            
            if(summary_func=="yearly_monthly") { 
              for(k in seq_along(month_rng)) {
                print(paste0("Calculating monthly values for ", month_val[k], "..."))
                
                slist_name <- paste(dnames[i], " (", year_rng[j], "-", month_ind[k], ")", sep="")
                chunk_grep <- grep(paste("^", dnames[i], ";", year_rng[j], "-", month_ind[k], sep=""), colname_vec)
                
                if(frequency=="monthly") {
                  
                  monthly_list[[paste0("Mean of ", slist_name)]][which(chunk_vec==chunk)] <- as.vector(data_total[chunk_grep,])
                  
                } else if(frequency=="daily") {
                  
                  monthly_list[[paste0("Mean of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, mean, na.rm=TRUE)
                  monthly_list[[paste0("SD of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, sd, na.rm=TRUE)
                  
                  if(calc_sum==TRUE) {
                    monthly_list[[paste0("Sum of ", slist_name)]][which(chunk_vec==chunk)] <- apply(data_total[chunk_grep,], 2, sum, na.rm=TRUE)
                  }
                  
                }
                
                if(calc_anom==TRUE) {
                  monthly_yearly_mean <- as.numeric(monthly_list[[paste0("Mean of ", slist_name)]][which(chunk_vec==chunk)])
                  anom_slist_name <- paste(dnames[i], " (", month_ind[k], " ", min(year_rng), "-",max(year_rng), ")", sep="")
                  anom_chunk_grep <- grep(paste("^", dnames[i], ";", year_rng, "-", month_ind[k], collapse="|", sep=""), colname_vec)
                  monthly_list[[paste0("Mean of ", anom_slist_name)]][which(chunk_vec==chunk)] <- month_total_recmean <- apply(data_total[anom_chunk_grep,], 2, mean, na.rm=TRUE)
                  monthly_list[[paste0("Anomaly of ", slist_name)]][which(chunk_vec==chunk)] <- monthly_yearly_mean - month_total_recmean
                }
                
              }
            }
          }
        }
      }
    }
  } else if(mode=="extract") {
    
    print("Extracting data without further calculations...")
    for(chunk in unique(chunk_vec)) {
      
      print(paste0("Working on partition number " , chunk, "..."))
      
      chunk_list[[chunk]] <- t(fread(file=paste0(big_data[1], "/data_total.csv"), sep=",", dec=".", header = FALSE, select=which(chunk_vec==chunk), nThread=as.numeric(big_data[4]), data.table=FALSE, check.names = FALSE))
      rownames(chunk_list[[chunk]]) <- NULL
      colnames(chunk_list[[chunk]]) <- colname_vec
      
    }
    chunk_list <- do.call(rbind, chunk_list)
  }
  
  print("Compiling processed data into a single .CSV file for export...")
  
  #Convert all the vectors to matrices (NOT data frames!) and cbind to the raw coordinate data...
  if(mode=="summary") { data_barebones <- cbind(data_barebones, simplify2array(chunk_list)) } else if(mode=="extract") { data_barebones <- cbind(data_barebones, chunk_list) }
  
  if(mode=="summary" & n_limit>0 & is.numeric(n_limit)) {
    print(paste0("Detected ", length(which(data_barebones[,grep("SSize of", colnames(data_barebones))] < n_limit)), 
                 " summarised samples with a sample size < ", n_limit, ". Excluding from analysis..."))
    
    data_barebones[which(data_barebones[,grep("SSize of", colnames(data_barebones))] < n_limit),
                   grep(paste0(min(year_rng), "-",max(year_rng)), colnames(data_barebones))] <- NA
    
  }
  
  if(mode=="summary" & summary_func=="yearly_monthly") {
    print("Compiling monthly data...")
    data_monthly_summary <- cbind(data_monthly_summary, simplify2array(monthly_list))
  }
  
  if(good_names==TRUE) {
    print("Converting column names to be syntactically correct, and exporting pretty names separately...")
    
    pretty_names_yearly <- colnames(data_barebones)
    colnames(data_barebones) <- make.names(colnames(data_barebones))
    
    if(mode=="summary" & summary_func=="yearly_monthly") {
      
      pretty_names_monthly <- colnames(data_monthly_summary)
      colnames(data_monthly_summary) <- make.names(colnames(data_monthly_summary))
    }
    
  } else if(good_names==FALSE) {
    print("Column names were NOT changed to their syntactically correct equivalents (not recommended)...")
    pretty_names_yearly <- NULL
    
    if(mode=="summary" & summary_func=="yearly_monthly") {
      pretty_names_monthly <- NULL
    }
  }
  
  if(!is.null(export_path) & is.character(export_path)) {
    print("Exporting summarised data as .CSV files...")
    
    if(summary_func=="total" | summary_func=="yearly") {
      data.table::fwrite(data_barebones, paste(export_path, "/", repository, "_", frequency, "_Total_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv", sep=""), na=NA, showProgress = FALSE)
      
    } else if(mode=="summary" & summary_func=="yearly_monthly") {
      data.table::fwrite(data_barebones, paste(export_path, "/", repository, "_", frequency, "_Total_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv", sep=""), na=NA, showProgress = FALSE)
      data.table::fwrite(data_monthly_summary, paste(export_path, "/", repository, "_", frequency, "_Monthly_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv", sep=""), na=NA, showProgress = FALSE)
    }
  }
  
  print("Processing complete!")
  
  if(summary_func=="total" | summary_func=="yearly") {
    if(repository != "NASA_Melt") {
      return(list(yearly_summary=data_barebones, projection_info=proj_info, clim_pretty_names=pretty_names_yearly, coords_total=coordinates_full[,c("Longitude", "Latitude")]))
    } else if(repository == "NASA_Melt") {
      return(list(yearly_summary=data_barebones, projection_info=proj_info, clim_pretty_names=pretty_names_yearly))
    }
    
  } else if(mode=="summary" & summary_func=="yearly_monthly") {
    if(repository != "NASA_Melt") {
      return(list(yearly_summary=data_barebones, monthly_summary=data_monthly_summary, projection_info=proj_info, clim_pretty_names=pretty_names_yearly, clim_pretty_names_monthly=pretty_names_monthly, coords_total=coordinates_full[,c("Longitude", "Latitude")]))
    } else if(repository == "NASA_Melt") {
      return(list(yearly_summary=data_barebones, monthly_summary=data_monthly_summary, projection_info=proj_info, clim_pretty_names=pretty_names_yearly, clim_pretty_names_monthly=pretty_names_monthly))
    }
    
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Visualise/plot maps of data obtained via clim_summary, as well as sample locations!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    pdf(paste(export_path, "/CLIM_Plots ", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".pdf", sep=""),
        width=width, height=height, pointsize = point_size)
    print(Total_Plot_List)
    dev.off()
  }
  
  return(list(SatPlot_List, PointPlot_List))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Extract the variable colnames that you need using this helper function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Summarise climate parameters obtained from clim_summary using polygon shapefiles!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        
        summary_stats[,"Record_Values"] <- rank_vec
        
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
        summary_nomean[,"Record_Values"] <- rank_vec
        summary_stats <- rbind(summary_nomean, summary_meancol)
      }
      
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: find out parameter values (e.g. SIC, chlorophyll, PAR) at selected coordinates (e.g. sample locations)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_locate <- function(sat_data, point_data, ex_method="bilinear", coord_vars=c("Longitude", "Latitude"), sat_vars=sat_data[,!colnames(sat_data) %in% coord_vars],
                        sat_varlabs=NA, separ=",", proj_init="WGS84", proj_orig, rast_res=c(NA, NA, 3), export_path=getwd()) {
  
  #Install and load the required libraries
  list.of.packages <- c("data.table", "raster", "rgeos", "sp", "sf", "rgdal", "broom", "dplyr")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  library(data.table)
  library(raster)
  library(rgeos)
  library(sp)
  library(sf)
  library(rgdal)
  library(broom)
  library(dplyr)
  
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
  
  if(any(proj_ids %in% proj_orig)) {
    
    proj_orig <- proj_mapped[[proj_orig]]
    
  } else if(!any(proj_ids %in% proj_orig) & !is.character(proj_orig)) {
    print("Projection was not provided in the correct format! Switching to the default: Orthographic North Polar...")
    proj_orig <- proj_defs[1]
  }
  
  #PREP STAGE: Error Checks
  if(any(is.na(sat_varlabs)) | length(sat_varlabs)!=length(sat_vars)) {
    print("Satellite variable labels were not provided is the correct format! Using sat_vars as labels...")
    sat_varlabs <- sat_vars
  }
  
  #Load satellite data
  print("Loading and processing satellite data...")
  
  if(is.object(sat_data)) {
    satmap <- sat_data[,c(coord_vars, sat_vars)]
  } else if(is.character(sat_data)) {
    satmap <- data.table::fread(sat_data, sep=separ, select=c(coord_vars, sat_vars), data.table = FALSE)
  }
  
  #Rasterize satellite data
  print("Projecting and rasterising satellite data...")
  
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
    
    satmap_origproj[,coord_vars] <- round(satmap_origproj[,coord_vars], 5) #FOR SOME REASON THIS PREVENTS SOME RASTERISATION FAILURES... WHY?
    
    if(!is.na(rast_res[2])) {
      satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_origproj), crs=proj_orig, res=rast_res[2], digits=rast_res[3]), silent=TRUE))
    } else if(is.na(rast_res[2])) {
      satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_origproj), crs=proj_orig, digits=rast_res[3]), silent=TRUE))
    }
  }
  
  if(inherits(satmap_rast, "try-error")) {
    stop("Failed to rasterize the satellite data using the initial projection (proj_init), most likely due to an irregular grid. Cancelling execution...")
    
  } else if (!inherits(satmap_rast, "try-error")) {
    
    if(!is.na(rast_res[2])) {
      satmap_final <- projectRaster(satmap_rast, crs=proj_orig, res=rast_res[2]) #Points not finite warning does NOT pose a problem for the data!
    } else if(is.na(rast_res[2])) {
      satmap_final <- projectRaster(satmap_rast, crs=proj_orig)
    }
    
    satmap_final <- as.data.frame(satmap_final, xy=TRUE)
    colnames(satmap_final)[which(colnames(satmap_final) %in% c("x", "y"))] <- coord_vars
    print("Conversion to raster successful!")
    
  }
  
  #Load point data
  print("Loading point data...")
  
  if(is.object(point_data)) {
    pointmap <- point_data[,c(coord_vars)]
  } else if(is.character(point_data)) {
    pointmap <- data.table::fread(point_data, sep=separ, data.table = FALSE)
    #pointmap <- data.table::fread(point_data, sep=separ, select=coord_vars, data.table = FALSE)
  }
  
  #Convert point data to SF object, assign projection (proj_init then proj_orig), and extract coordinates...
  print("Extracting satellite data for each provided coordinate pair...")
  points_sf <- st_as_sf(pointmap, coords=coord_vars)
  points_sf <- st_sf(points_sf, crs=proj_init)
  points_sf <- st_transform(points_sf, crs=proj_orig)
  point_coordinates <-st_coordinates(points_sf)
  
  #Use extracted coordinates to obtain satellite data values for each sample location
  extracted_satvars <- raster::extract(satmap_rast, point_coordinates, method=ex_method, fun=mean, na.rm=TRUE)
  colnames(extracted_satvars) <- sat_varlabs
  
  sat_results_final <- cbind(pointmap, extracted_satvars)
  
  #Export results
  if(!is.na(export_path) & is.character(export_path)) {
    print("Exporting the results as a .CSV file...")
    write.csv(sat_results_final, paste0(export_path, "/ClimLocate_Results_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec") , ".csv"))
  }
  
  print("Processing complete!")
  return(sat_results_final)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Adding Inner and Outer Melt Season Length data to clim_summary output for NASA_Melt! (Stroeve et al., 2014)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_melt <- function(data) {
  
  varibvec <- sapply(c("Earlymelt", "Melt", "Earlyfreeze", "Freeze"), function(x) any(grepl(x, colnames(data))))
  
  if(!all(varibvec)) {
    stop("Melt and freeze cycle variables incomplete! Cannot calculate melt season length... Cancelling procedure.")
  }
  
  varibnames <- names(varibvec[varibvec == TRUE])
  varibvec <- varibnames[1]
  
  data[,gsub(paste0(varibvec,".."), "Inmelt..", colnames(data)[grep(paste0("Mean.of.", varibvec,".."), colnames(data))])] <- data[,grep("Mean.of.Earlyfreeze", colnames(data))] - data[,grep("Mean.of.Melt", colnames(data))]
  data[,gsub(paste0(varibvec,".."), "Outmelt..", colnames(data)[grep(paste0("Mean.of.", varibvec,".."), colnames(data))])] <- data[,grep("Mean.of.Freeze", colnames(data))] - data[,grep("Mean.of.Earlymelt", colnames(data))]
  
  return(data)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Adding least-squares trends to interannual data! (see NSIDC Sea Ice Index, version 3 documentation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_trend <- function(data, trend_vars, trend_unit="default", add_stats = TRUE, coord_vars = c("Longitude", "Latitude"), time_span) {
  
  list.of.packages <- c("data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(data.table)
  
  meltsum_linreg <- list()
  
  system.time(
    for(i in trend_vars) {
      print(paste0("Calculating least squares regression trends for variable ", i, " (", match(i, trend_vars), " of ", length(trend_vars), ")..."))
      
      data_linreg <- data[,grep(paste0("Mean.of.", i), colnames(data))]
      data_linreg <- data_linreg[,-grep(paste0(time_span[1],".", time_span[2]), colnames(data_linreg))]
      data_linreg <- cbind(data[,colnames(data) %in% coord_vars], data_linreg)
      
      data_linreg <- data_lincoords <- data_linreg[rowSums(is.na(data_linreg[,3:ncol(data_linreg)])) != ncol(data_linreg[,3:ncol(data_linreg)]), ]
      #data_lincoords <- data_linreg[,colnames(data_linreg) %in% coord_vars]
      data_linreg <- data_linreg[,!colnames(data_linreg) %in% coord_vars]
      data_linreg[data_linreg == 0] <- NA
      
      #FASTER WAY to get ONLY THE SLOPE in one line!
      if(!isTRUE(add_stats)) {
        meltsum_linreg[[i]] <- sapply(1:nrow(data_linreg), function(x) tryCatch(coef(lm(as.numeric(data_linreg[x,]) ~ seq_along(data_linreg[x,])))[[2]], error=function(err) NA))
        
        trendname <- paste0("Trend.in.", i,"..", trend_unit)
        
        row_vals <- c(as.numeric(rownames(data_linreg)))
        
        if(trend_unit=="%") {
          data[row_vals, trendname] <- meltsum_linreg[[i]] / apply(data_linreg, 1, mean, na.rm=TRUE)
        } else if(trend_unit=="% decade") {
          data[row_vals, trendname] <- (meltsum_linreg[[i]]*10) / apply(data_linreg, 1, mean, na.rm=TRUE)
        } else if(trend_unit=="decade") {
          data[row_vals, trendname] <- (meltsum_linreg[[i]]*10)
        } else if(trend_unit=="default") {
          data[row_vals, trendname] <- meltsum_linreg[[i]]
        }
      } else if(isTRUE(add_stats)) {
        
        #SLOWER WAY to get slope, standard error, and p-value of the fit... optionally, R^2 could be added in the future!
        lm_list <- lapply(1:nrow(data_linreg), function(x) tryCatch(summary(lm(as.numeric(data_linreg[x,]) ~ seq_along(data_linreg[x,]))), error=function(err) NA))
        meltsum_linreg[[i]] <- t(sapply(lm_list, function(x) tryCatch(c(slope=x$coefficients[2,1],
                                                                        stnd_err=x$coefficients[2,2],
                                                                        pval=x$coefficients[2,4]), error=function(err) c(slope=NA,
                                                                                                                         stnd_err=NA,
                                                                                                                         pval=NA))))
        #meltsum_linreg[[i]] <- as.data.frame(meltsum_linreg[[i]])
        meltsum_linreg[[i]][is.nan(meltsum_linreg[[i]])] <- NA
        
        trendname <- paste0("Trend.in.", i,"..", trend_unit)
        SDtrendname <- paste0("SD.of.", trendname)
        pvalname <- paste0("Pval.of.", trendname)
        
        data[,c(trendname, SDtrendname, pvalname)] <- NA
        
        row_vals <- c(as.numeric(rownames(data_linreg)))
        
        if(trend_unit=="%") {
          data[row_vals, trendname] <- meltsum_linreg[[i]][,"slope"] / apply(data_linreg, 1, mean, na.rm=TRUE)
          data[row_vals, SDtrendname] <- meltsum_linreg[[i]][,"stnd_err"] / apply(data_linreg, 1, mean, na.rm=TRUE)
        } else if(trend_unit=="% decade") {
          data[row_vals, trendname] <- (meltsum_linreg[[i]][,"slope"]*10) / apply(data_linreg, 1, mean, na.rm=TRUE)
          data[row_vals, SDtrendname] <- (meltsum_linreg[[i]][,"stnd_err"]*10) / apply(data_linreg, 1, mean, na.rm=TRUE)
        } else if(trend_unit=="decade") {
          data[row_vals, trendname] <- (meltsum_linreg[[i]][,"slope"]*10)
          data[row_vals, SDtrendname] <- (meltsum_linreg[[i]][,"stnd_err"]*10)
        } else if(trend_unit=="default") {
          data[row_vals, trendname] <- meltsum_linreg[[i]][,"slope"]
          data[row_vals, SDtrendname] <- meltsum_linreg[[i]][,"stnd_err"]
        }
        
        data[row_vals, pvalname] <- meltsum_linreg[[i]][,"pval"]
      }
    }
  )
  
  return(data)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculating relative differences for DAILY clim_summary output data. 
#Useful for bloom tracking with chla data! (McKibben et al., 2012), division rates (Behrenfeld et al., 2005, 2017), or phytoplankton biomass change rate (Behrenfeld et al., 2017)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_btrack <- function(data, sat_vars, coord_vars=c("Longitude", "Latitude"), 
                        run_window = 8, mean_fun=c("arithm", "arithm"), var_lab="default", method="mckibben",
                        times=c("days", NA), year_val=2009, monthly_aggr=TRUE, export_path=NA, extras=TRUE, smoothing=NA) {
  
  list.of.packages <- c("data.table","EnvStats", "gtools", "dplyr", "svMisc", "obpgcrawler", "threddscrawler")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])) install.packages(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])
  
  if(any(new.packages %in% "threddscrawler")) devtools::install_github("BigelowLab/threddscrawler")
  if(any(new.packages %in% "obpgcrawler")) devtools::install_github("BigelowLab/obpgcrawler")
  
  library(data.table)
  library(EnvStats)
  library(gtools)
  library(dplyr)
  library(svMisc)
  library(obpgcrawler)
  library(threddscrawler)
  
  if(is.character(data)) {
    init_data <- fread(file=data, data.table=FALSE)
    print("Data successfully read from file...")
  } else if(is.object(data)) {
    init_data <- as.data.frame(data)
    print("Data successfully read from an R environment object...")
  }
  
  
  print("Processing data to obtain record of relative differences...")
  print("Processing...")
  print("Processing...")
  
  track_df <- init_data[,sat_vars, drop=FALSE]
  
  
  #Checking if column names comply with the "digits{1,3}.digits{1.3}." condition... If not, change them accordingly.
  if(length(grep(".[[:digit:]]{1,3}.[[:digit:]]{1,3}\\.$", colnames(track_df)))==0) {
    print("Adjusting column names...") 
    colnames(track_df) <- paste0(colnames(track_df), ".", times[1], ".", times[2:length(times)], ".", times[2:length(times)], ".")
    colnames(init_data) <- c(coord_vars, colnames(track_df))
  }
  
  if(method!="mckibben" & all(is.na(times[2:length(times)]))) {
    stop("A vector of time points is required when method is different from mckibben! Stopping...")
  }
  
  if(!any(times[1] %in% c("day", "days")) & monthly_aggr==TRUE) {
    stop("Only daily timeseries can be aggregated by month! Stopping...")
  }
  
  track_vec <- track_df[1,]
  #if(any(method %in% c("bh_r", "bh_dudt"))) {
  #  timetrack_vec <- times[2:length(times)]
  #}
  if(any(method %in% c("bh_r", "bh_dudt"))) {
  tstep <- unique(diff(as.numeric(times[2:length(times)])))
  
  if(length(tstep)!=1) {
    stop("Could not calculate time step! Stopping...")
  }
  }
  
  if(mean_fun[1]=="geo") {
    ref_vec <- running(as.numeric(track_vec), fun=EnvStats::geoMean, na.rm=TRUE, width=run_window)
    
    #if(any(method %in% c("bh_r", "bh_dudt"))) {
    #  ref_timevec <- running(as.numeric(timetrack_vec), fun=EnvStats::geoMean, na.rm=TRUE, width=run_window)
    #}
    
  } else if(mean_fun[1]=="arithm") {
    ref_vec <- running(as.numeric(track_vec), fun=mean, na.rm=TRUE, width=run_window)
    
    #if(any(method %in% c("bh_r", "bh_dudt"))) {
    #  ref_timevec <- running(as.numeric(timetrack_vec), fun=mean, na.rm=TRUE, width=run_window)
    #}
  }
  
  ref_lagvec <- dplyr::lead(ref_vec, run_window)
  #if(any(method %in% c("bh_r", "bh_dudt"))) {
  #  ref_timelagvec <- dplyr::lead(ref_timevec, run_window)
  #}
  
  if(method=="mckibben") {
    ref_resvec <- ((ref_lagvec-ref_vec)/ref_vec)*100
  } else if(method=="bh_r") {
    #ref_resvec <- log(ref_lagvec/ref_vec)/(ref_timelagvec-ref_timevec)
    ref_resvec <- log(ref_lagvec/ref_vec)/tstep
  } else if(method=="bh_dudt") {
    #ref_resvec <- (ref_lagvec-ref_vec)/(ref_timelagvec-ref_timevec)
    ref_resvec <- (ref_lagvec-ref_vec)/tstep
  }
  
  coord_cols <- init_data[colnames(init_data) %in% coord_vars]
  name_trvec <- colnames(init_data[,sat_vars])[!colnames(init_data[,sat_vars]) %in% coord_vars]
  name_vec <- gsub(paste0(".*", times[1], ".[[:digit:]]{1,3}.([[:digit:]]{1,3}).$"), "\\1", name_trvec)
  
  last_digits <- name_vec[as.numeric(gsub(".*:([[:digit:]]{1,3})", "\\1", dplyr::lead(names(ref_resvec), run_window)))]
  last_digits <- last_digits[!is.na(last_digits)]
  coverage_vec <- paste0(name_vec[as.numeric(gsub("([[:digit:]]{1,3}).*", "\\1", names(ref_resvec)))], ":", name_vec[as.numeric(gsub(".*:([[:digit:]]{1,3})", "\\1", dplyr::lead(names(ref_resvec), run_window)))])
  
  del_indices <- grep("NA", coverage_vec)
  
  coverage_vec <- coverage_vec[!coverage_vec %in% grep("NA", coverage_vec, value=TRUE)]
  ref_resvec <- ref_resvec[-del_indices]
  
  if(any(var_lab %in% "default")) {
    var_labname <- unique(gsub("Mean\\.of\\.(.*)\\.{2}.*", "\\1", colnames(init_data)[grep("Mean\\.of\\.", colnames(init_data))]))
  } else if(!any(var_lab %in% "default") & !is.na(var_lab)) {
    var_labname <- as.character(var_lab)
  } else if(is.na(var_lab)) {
    var_labname <- "ArbitraryVar"
  }
  
  if(length(var_labname)==0) {var_labname <- "ArbitraryVar"}
  
  names(ref_resvec) <- fnames_track <- paste0(var_labname, "_track_", times[1], ".", coverage_vec)
  
  bloomtrack_list <- list()
  
  bloomtrack_list[[1]] <- ref_resvec
  
  for(i in 2:nrow(track_df)) { #nrow(track_df)
    
    svMisc::progress(i, max.value=nrow(track_df), progress.bar = FALSE)
    Sys.sleep(1/nrow(track_df))
    if(i==nrow(track_df)) cat("Done!\n")
    
    track_vec <- track_df[i,]
    
    if(mean_fun[1]=="geo") {
      ref_vec <- running(as.numeric(track_vec), fun=EnvStats::geoMean, na.rm=TRUE, width=run_window)
    } else if(mean_fun[1]=="arithm") {
      ref_vec <- running(as.numeric(track_vec), fun=mean, na.rm=TRUE, width=run_window)
    }
    
    ref_lagvec <- dplyr::lead(ref_vec, run_window)

    if(method=="mckibben") {
      ref_resvec <- ((ref_lagvec-ref_vec)/ref_vec)*100
    } else if(method=="bh_r") {
      ref_resvec <- log(ref_lagvec/ref_vec)/tstep
    } else if(method=="bh_dudt") {
      ref_resvec <- (ref_lagvec-ref_vec)/tstep
    }
    
    ref_resvec <- bloomtrack_list[[i]] <- ref_resvec[-del_indices]
  }
  
  bloomtrack_final <- as.data.frame(do.call(rbind, bloomtrack_list))
  
  #Calculating number of positive/negative values, sum, max, mean values with and without additional LOESS (lowess) smoothing...
  if(extras==TRUE) {
    print("Calculating additional parameters...")
    extra_df <- data.frame(matrix(nrow=nrow(bloomtrack_final), ncol=0))
    #nametemp <- paste0(var_labname, ".", times[1], ".", min(name_vec),".", max(name_vec), "_") 
    nametemp <- paste0(var_labname, ".", times[1], "_")
    
    extra_df[,paste0(nametemp,"RAW_Pos_MAX")] <- apply(bloomtrack_final, 1, function (x) max(x[x > 0], na.rm=TRUE))
    extra_df[,paste0(nametemp,"RAW_Pos_MEAN")] <- apply(bloomtrack_final, 1, function (x) mean(x[x > 0], na.rm=TRUE)) #rowMeans()
    extra_df[,paste0(nametemp,"RAW_Pos_MEDIAN")] <- apply(bloomtrack_final, 1, function (x) median(x[x > 0], na.rm=TRUE))
    extra_df[,paste0(nametemp,"RAW_Neg_MIN")] <- apply(bloomtrack_final, 1, function (x) min(x[x <= 0], na.rm=TRUE))
    extra_df[,paste0(nametemp,"RAW_Neg_MEAN")] <- apply(bloomtrack_final, 1, function (x) mean(x[x <= 0], na.rm=TRUE)) #rowMeans()
    extra_df[,paste0(nametemp,"RAW_Neg_MEDIAN")] <- apply(bloomtrack_final, 1, function (x) median(x[x <= 0], na.rm=TRUE))
    extra_df[,paste0(nametemp,"RAW_Pos_DUR")] <- rowSums(bloomtrack_final > 0, na.rm = TRUE)
    extra_df[,paste0(nametemp,"RAW_Neg_DUR")] <- rowSums(bloomtrack_final <= 0, na.rm = TRUE)
    extra_df[,paste0(nametemp, "RAW_SSize")] <- rowSums(!is.na(bloomtrack_final))
    extra_df[,paste0(nametemp,"RAW_Pos_Dur%")] <- rowMeans(bloomtrack_final > 0, na.rm = TRUE)
    extra_df[,paste0(nametemp,"RAW_Neg_Dur%")] <- rowMeans(bloomtrack_final <= 0, na.rm = TRUE)
    
    if(is.numeric(smoothing) & !is.na(smoothing)) {
      print("Appying LOESS smoothing...")
      #Without outlier detection (but FASTER)!
      #bloomtrack_smooth <- do.call(rbind, apply(bloomtrack_final, 1, function(d) t(lowess(y=d, x=1:length(d), f=smoothing)["y"])))
      #bloomtrack_smooth <- do.call(rbind, bloomtrack_smooth)
      #bloomtrack_smooth[is.nan(bloomtrack_smooth)] <- NA
      #colnames(bloomtrack_smooth) <- paste0(colnames(bloomtrack_final), "_LOESS")
      
      #With outlier detection/non-parametric algorithm (but SLOWER)!
      bloomtrack_smooth <- do.call(rbind, apply(bloomtrack_final, 1, function(d) tryCatch(loess.smooth(y=d, x=1:length(d), span=smoothing, family="symmetric", evaluation=length(d))[["y"]],
                                                                               error=function(err) NA)))
      bloomtrack_smooth[is.nan(bloomtrack_smooth)] <- NA
      colnames(bloomtrack_smooth) <- paste0(colnames(bloomtrack_final), "_LOESS")
      
      extra_df[,paste0(nametemp,"LOESS_Pos_MAX")] <- apply(bloomtrack_smooth, 1, function (x) max(x[x > 0], na.rm=TRUE))
      extra_df[,paste0(nametemp,"LOESS_Pos_MEAN")] <- apply(bloomtrack_smooth, 1, function (x) mean(x[x > 0], na.rm=TRUE)) #rowMeans()
      extra_df[,paste0(nametemp,"LOESS_Pos_MEDIAN")] <- apply(bloomtrack_smooth, 1, function (x) median(x[x > 0], na.rm=TRUE))
      extra_df[,paste0(nametemp,"LOESS_Neg_MIN")] <- apply(bloomtrack_smooth, 1, function (x) min(x[x <= 0], na.rm=TRUE))
      extra_df[,paste0(nametemp,"LOESS_Neg_MEAN")] <- apply(bloomtrack_smooth, 1, function (x) mean(x[x <= 0], na.rm=TRUE)) #rowMeans()
      extra_df[,paste0(nametemp,"LOESS_Neg_MEDIAN")] <- apply(bloomtrack_smooth, 1, function (x) median(x[x <= 0], na.rm=TRUE))
      extra_df[,paste0(nametemp,"LOESS_Pos_DUR")] <- rowSums(bloomtrack_smooth > 0, na.rm = TRUE)
      extra_df[,paste0(nametemp,"LOESS_Neg_DUR")] <- rowSums(bloomtrack_smooth <= 0, na.rm = TRUE)
      extra_df[,paste0(nametemp, "LOESS_SSize")] <- rowSums(!is.na(bloomtrack_smooth))
      extra_df[,paste0(nametemp,"LOESS_Pos_Dur%")] <- rowMeans(bloomtrack_smooth > 0, na.rm = TRUE)
      extra_df[,paste0(nametemp,"LOESS_Neg_Dur%")] <- rowMeans(bloomtrack_smooth <= 0, na.rm = TRUE)
    }
    
    SSize <- apply(bloomtrack_final, 1, function(x){ length(which(!is.na(x))) })
    extra_df[which(SSize==0),] <- NA
    extra_df[extra_df==-Inf] <- NA
    extra_df[extra_df==Inf] <- NA
    extra_df <- as.matrix(extra_df)
    extra_df[is.nan(extra_df)] <- NA
    extra_df <- as.data.frame(extra_df)
  }
  
  #Calculating monthly aggregates
  if(monthly_aggr==TRUE) {
    print("Aggregating the daily tracking/relative difference data to monthly values...")
    
    days_first <- get_monthdays(as.numeric(year_val), when="first")
    days_last <- get_monthdays(as.numeric(year_val), when="last")
    
    month_ids <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month_ind <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    month_num <- 1:12
    month_df <- cbind(month_ids, month_ind, month_num)
    
    day_range <- list()
    day_range <- lapply(seq_along(month_ind), function(x) days_first[[as.character(year_val)]][month_ind[x]]:days_last[[as.character(year_val)]][month_ind[x]])
    names(day_range) <- month_ids
    
    colnames_lastdays <- as.numeric(gsub(".*\\.[[:digit:]]{1,3}\\:([[:digit:]]{1,3}$)", "\\1", colnames(bloomtrack_final)))
    
    month_list <- list()
    
    for(i in month_ids) {
      month_list[[i]] <- which(colnames_lastdays %in% day_range[[i]])
    }
    
    month_list <- month_list[lapply(month_list,length)>0]
    
    for(i in seq_along(month_list)) {
      if(mean_fun[2]=="arithm") {
        bloomtrack_final[,paste0(names(month_list)[i], "_", var_labname, "_track")] <- apply(bloomtrack_final[,month_list[[i]]], 1, mean, na.rm = TRUE)
      } else if(mean_fun[2]=="geo") {
        bloomtrack_final[,paste0(names(month_list)[i], "_", var_labname, "_track")] <- apply(bloomtrack_final[,month_list[[i]]], 1, EnvStats::geoMean, na.rm = TRUE)
      }
    }
    print("Aggregation complete!")
  }
  
  bloomtrack_final <- as.data.frame(do.call(cbind, list(coord_cols, bloomtrack_final, extra_df)))
  
  if(is.character(export_path) & !is.na(export_path)) {
    print(paste0("Exporting results as a .CSV file to: ", export_path, "..."))
    data.table::fwrite(x=bloomtrack_final, file=paste0(export_path, "/", var_labname, "_RelDiffTracking_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv"), na=NA)
    print(paste0("Data successfully written to file: ", paste0(var_labname, "_RelDiffTracking_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv!")))
  } else if(!is.character(export_path) | is.na(export_path)) {
    print("The results will not be written to a .CSV file as export_path was not provided!")
  }
  
  print("Processing complete!")
  
  return(bloomtrack_final)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Calculating various bloom-related variables from Chla and other data! 
# Following Hopkins et al., 2015 for spring bloom detection and using Ardyna et al. (2014) for additional detection and comparison with fall blooms!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clim_bloom <- function(coord_vars=c("Longitude", "Latitude"), data, consec_num=2, thres_percent=0.05,
                       thres_num=2, grep_patt=".*days[[:digit:]]{1,3}.([[:digit:]]{1,3}).$", varlab="chlor_a",
                       export_path, bloom_dur=FALSE, smoothing=NA, smooth_max=NA) { # bloom_num=FALSE
  
  list.of.packages <- c("data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(data.table)
  
  if(is.character(data)) {
    data <- fread(data, data.table=FALSE)
    print(paste0("Data loaded from file: ", data, "..."))
  } else if(is.object(data)) {
    print("Data loaded from the R environment...")
  }
  
  print("Separating coordinate, variable, and time values...")
  coord_cols <- res_base <- data[,coord_vars]
  data <- data[,!colnames(data) %in% coord_vars]
  timevec <- as.numeric(gsub(grep_patt, "\\1", colnames(data)))
  colname_bckup <- colnames(data)
  smooth_lab <- "RAW"
  
  if(!is.na(smoothing) & is.numeric(smoothing)) {
    print("Applying LOESS smoothing to data...")
    data <- as.data.frame(do.call(rbind, apply(data, 1, function(d) tryCatch(loess.smooth(y=d, x=1:length(d), span=smoothing, family="symmetric", evaluation=length(d))[["y"]],
                                                                             error=function(err) NA))))
    colnames(data) <- colname_bckup
    smooth_lab <- paste0("LOESS_", smoothing)
  }
  
  #if(!is.numeric(smoothing) & bloom_num==TRUE) {
  #  stop("The detection of blooms (spring, fall) as per Ardyna et al. (2014) can only be carried out after LOESS smoothing! Stopping...")
  
  #} else if(is.numeric(smoothing) & bloom_num==TRUE) {
  #  print("Deriving the number of Gaussian peaks that best describe the data (0, 1 or 2)...")
  
  #  data_nls <- cbind(as.data.frame(t(data)), timevec=timevec)
  #  colnames(data_nls) <- c(paste(varlab, colnames(data_nls)[1:(ncol(data_nls)-1)], sep="_"), "timevec")
  
  #  print("Creating model formulae...")
  #  mod1_f <- paste0(colnames(data_nls),"~", "0*timevec + C")
  
  #  mod2_f <- paste0(colnames(data_nls),"~", "C1 * exp(-(timevec-mean1)^2/(2 * sigma1^2))")
  
  #  mod3_f <- paste0(colnames(data_nls), "~", "C1 * exp(-(timevec-mean1)^2/(2 * sigma1^2)) + C2 * exp(-(timevec-mean2)^2/(2 * sigma2^2))")
  
  #  nls_bloom <- function(mod1, mod2, mod3) {
  #    nls_mod1 <- nls(as.formula(mod1), data=data_nls, start=list(C=0), control=list(iter=nrow(data_nls)))
  #    nls_mod2 <- nls(as.formula(mod2), data=data_nls, start=list(C1=1.2,sigma1=1,mean1=100), control=list(maxiter=189000))
  #    nls_mod3 <- nls(as.formula(mod3), data=data_nls, start=list(C1=1.2, C2=0.8, sigma1=2, sigma2=1, mean1=100, mean2=240), control=list(maxiter=189000))
  #    #nls_test <- lapply(mod1_f[1:3], function(x) nls(x, data=data_nls, start=list(C=0), control=list(iter=nrow(data_nls)), algorithm="port"))
  #  }
  
  #} else if(bloom_num==FALSE) {
  #  blnum_vec <- rep(1, ncol(data))
  #}
  
  print(paste0("Calculating maximum ", varlab, " values and their occurrence throughout the year..."))
  coord_cols[,paste0("Max_", varlab, "_", smooth_lab)] <- apply(data, 1, function(x) ifelse(!all(is.na(x)), max(x, na.rm=TRUE), NA))
  coord_cols[,paste0("Day_of_Max_", varlab, "_", smooth_lab)] <- as.numeric(unlist(apply(data, 1, function(x) ifelse(!all(is.na(x)), gsub(grep_patt, "\\1", colnames(data)[which(x==max(x, na.rm=TRUE))]), NA))))
  
  ind_max_chla <- as.numeric(unlist(apply(data, 1, function(x) ifelse(!all(is.na(x)), which(x==max(x, na.rm=TRUE)), NA))))
  
  if(is.numeric(smooth_max)) {
    print(paste0("Calculating smooth maxima with a tolerance value of ", smooth_max, "..."))
    rle_res <- apply(data, 1, function(x) { rle(!all(is.na(x)) & x>=max(x)-(max(x, na.rm = TRUE)*smooth_max) & x<max(x)+(max(x, na.rm = TRUE)*smooth_max))}) #ifelse(!all(is.na(x)), rep(NA, length(x))
    rle_res <- sapply(1:length(rle_res), function(x) {vec <- rep(as.vector(rle_res[[x]][["values"]]), rle_res[[x]][["lengths"]])
    vec <- with(rle(vec), rep(lengths == max(lengths[values]) & values, lengths))
    #names(vec) <- colnames(data)
    vec <- which(vec==TRUE)
    }) 
    coord_cols[,paste0("SmoothMax_", varlab, "_", smooth_lab)] <- rle_max <- sapply(1:length(rle_res), function(x) if(is.element(ind_max_chla[x], rle_res[[x]])) {mean(as.numeric(data[x,as.numeric(rle_res[[x]])]), na.rm=TRUE)} else {NA})
    ind_max_chla <- unlist(sapply(1:length(rle_max), function(x) if(!any(is.na(c(as.numeric(data[x, as.numeric(rle_res[[x]])]), rle_max[x])))) rle_res[[x]][which(abs(as.numeric(data[x, as.numeric(rle_res[[x]])]) - rle_max[x])==min(abs(as.numeric(data[x, as.numeric(rle_res[[x]])]) - rle_max[x]), na.rm=TRUE))[1]] else NA))
    coord_cols[,paste0("Day_of_SmoothMax_", varlab, "_", smooth_lab)] <- as.numeric(sapply(ind_max_chla, function(x) if(!is.na(x)) gsub(grep_patt, "\\1", colnames(data)[x]) else NA))
  }
  #vec <- which(vec==TRUE & is.element(ind_max_chla[x], vec))
  
  print("Compiling processed data...")
  coord_cols[coord_cols==-Inf] <- NA
  print("Done!")
  
  #Determine the day of year when the dataset value FIRST exceeds the threshold BEFORE and AFTER dataset maximum, 
  #after a number of CONSECUTIVE points BELOW said threshold (in this case, 2 points)
  #Bloom START and END days
  #Find indices of maximum values for each row in dataset
  if(bloom_dur==TRUE) {
    
    thres_befmax <- c()
    bloom_start <- c()
    bloom_startind <- c()
    thres_afmax <- c()
    bloom_end <- c()
    bloom_endind <- c()
    
    print(paste0("Calculating bloom start and end dates for variable ", varlab, "..."))
    
    for(i in seq_along(ind_max_chla)) {
      svMisc::progress(i, max.value=length(ind_max_chla), progress.bar = FALSE)
      Sys.sleep(1/length(ind_max_chla))
      if(i==length(ind_max_chla)) cat("Done!\n")
      
      if(thres_num==1) {
        thres_befmax[i] <-ifelse(!is.na(ind_max_chla[i]), min(data[i,], na.rm=TRUE)+(abs((max(data[i,], na.rm=TRUE)-min(data[i,], na.rm=TRUE)))*thres_percent), NA)
      } else if(thres_num!=1) {
        thres_befmax[i] <-ifelse(!is.na(ind_max_chla[i]), min(data[i,1:ind_max_chla[i]], na.rm=TRUE)+(abs((max(data[i,1:ind_max_chla[i]], na.rm=TRUE)-min(data[i,1:ind_max_chla[i]], na.rm=TRUE)))*thres_percent), NA)
      }
      
      
      if(is.na(thres_befmax[i])) {
        bloom_start[i] <- bloom_startind[i] <- NA
      } else if(!is.na(thres_befmax[i])) {
        
        row_vec <- reclass <- data[i,1:ind_max_chla[i]]
        reclass[row_vec<thres_befmax[i]] <- 1
        reclass[row_vec>=thres_befmax[i]] <- 0
        runs <- rle(reclass)
        
        runs <- as.numeric(rep(runs[["values"]], runs[["lengths"]]))
        names(runs) <- names(row_vec)
        runs <- rle(runs)
        bloom_startind[i] <- if(length(which(names(row_vec) %in% tail(names(runs[["lengths"]])[which(runs[["values"]]==1 & runs[["lengths"]] >=consec_num)], n=1)))==0) NA else which(names(row_vec) %in% tail(names(runs[["lengths"]])[which(runs[["values"]]==1 & runs[["lengths"]] >=consec_num)], n=1))
        
        #bloom_startind[i] <- which(colnames(row_vec) %in% tail(names(runs[["values"]])[runs[["values"]]==1 & runs[["lengths"]] >=consec_num], n=1))
        
        bloom_colname <- colnames(row_vec)[bloom_startind[i]]
        
        if(length(bloom_colname)==0) {
          bloom_start[i] <- NA
        } else if(length(bloom_colname)==1) {
          bloom_start[i] <- gsub(grep_patt, "\\1", bloom_colname)
        }
      }
      
      if(thres_num==1) {
        thres_afmax[i] <- thres_befmax[i]
      } else if(thres_num!=1) {
        thres_afmax[i] <-ifelse(!is.na(ind_max_chla[i]), min(data[i,ind_max_chla[i]:ncol(data)], na.rm=TRUE)+(abs((max(data[i,ind_max_chla[i]:ncol(data)], na.rm=TRUE)-min(data[i,ind_max_chla[i]:ncol(data)], na.rm=TRUE)))*thres_percent), NA)
      }
      
      if(is.na(thres_afmax[i])) {
        bloom_end[i] <- bloom_endind[i] <- NA
      } else if(!is.na(thres_afmax[i])) {
        row_vec <- reclass <- data[i,ind_max_chla[i]:ncol(data)]
        reclass[row_vec<thres_afmax[i]] <- 1
        reclass[row_vec>=thres_afmax[i]] <- 0
        runs <- rle(reclass)
        
        runs <- as.numeric(rep(runs[["values"]], runs[["lengths"]]))
        names(runs) <- names(row_vec)
        runs <- rle(runs)
        bloom_endind[i] <- if(length(which(names(row_vec) %in% head(names(runs[["lengths"]])[which(runs[["values"]]==1 & runs[["lengths"]] >=consec_num)-1], n=1)))==0) NA else which(names(row_vec) %in% head(names(runs[["lengths"]])[which(runs[["values"]]==1 & runs[["lengths"]] >=consec_num)-1], n=1))
        #bloom_endind[i] <- which(colnames(row_vec) %in% head(names(runs[["values"]])[runs[["values"]]==1 & runs[["lengths"]] >=consec_num], n=1))
        
        bloom_colname <- colnames(row_vec)[bloom_endind[i]]
        
        if(length(bloom_colname)==0) {
          bloom_end[i] <- NA
        } else if(length(bloom_colname)==1) {
          bloom_end[i] <- gsub(grep_patt, "\\1", bloom_colname)
        }
        
        bloom_endind[i] <- if(length(which(colnames(data) %in% bloom_colname))==0) NA else which(colnames(data) %in% bloom_colname)
      }
      
    }
    
    print("Aggregating final data...")
    rm(runs)
    rm(reclass)
    rm(row_vec)
    bloom_start <- as.numeric(bloom_start)
    bloom_end <- as.numeric(bloom_end)
    bloom_duration <- bloom_end-bloom_start
    bloom_intensity <- sapply(1:nrow(data), function(x) if(!any(is.na(c(bloom_startind[x], bloom_endind[x])))) mean(as.numeric(data[x, bloom_startind[x]:bloom_endind[x]]), na.rm=TRUE) else NA)
    bloom_magnitude <- sapply(1:nrow(data), function(x) if(!any(is.na(c(bloom_startind[x], bloom_endind[x])))) sum(as.numeric(data[x, bloom_startind[x]:bloom_endind[x]]), na.rm=TRUE) else NA)
    bloom_contrib <- bloom_magnitude/rowSums(data, na.rm=TRUE)*100
    
    coord_cols[,paste0(varlab, "_Bloom_Start_", smooth_lab)] <- bloom_start
    coord_cols[,paste0(varlab, "_Bloom_End_", smooth_lab)] <- bloom_end
    coord_cols[,paste0(varlab, "_Bloom_Duration_", smooth_lab)] <- bloom_duration
    coord_cols[,paste0(varlab, "_Bloom_Intensity_", smooth_lab)] <- bloom_intensity
    coord_cols[,paste0(varlab, "_Bloom_Magnitude_", smooth_lab)] <- bloom_magnitude
    coord_cols[,paste0(varlab, "_Bloom_Contribution_", smooth_lab)] <- bloom_contrib
    
  }
  
  if(is.character(export_path)) {
    print("Exporting data as a .CSV file...")
    fwrite(coord_cols, paste0(export_path,"/", varlab, "_Bloom_Derived_Variables_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv"))
    print("Done!")
  }
  return(coord_cols)
}
