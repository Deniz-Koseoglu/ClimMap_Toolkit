#trace(utils:::unpackPkgZip, edit=TRUE) # If there is an "Unable to move temporary installation" error, run this and change the value on line 140 (Sys.sleep time) from 0.5 to 2.5
#install.packages("XML")
#install.packages("rvest")
#install.packages("RCurl")
#install.packages("curl")
#install.packages("devtools")
#install.packages("purrr")
#devtools::install_github("BigelowLab/threddscrawler")
#devtools::install_github("BigelowLab/obpgcrawler")
# "parallel" is a base package!

#KNOWN BUGS: 1) When using the NASA Melt dataset in clim_summary, coordinates_full does not get returned due to "incorrect number of dimensions"!

clim_download <- function(repository="NSIDC", hemisphere="north", frequency="monthly", data_type="Chla", grid_res="9km",
                          year, month, res_path, suppr_msgs=TRUE, opt_down=TRUE, obpg_access=FALSE, down_mode="wb",
                          dcores=detectCores(), max_retries=6) {

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
  res_dir <- paste(res_path, "/NSIDC", sep="")
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
  } else if(!any(data_type %in% c("SIC","SIE_polyline", "SIE_polygon", "SIE_median"))) {
    stop("Incorrect data_type argument selected! For NSIDC data, acceptable values are: SIC, SIE_polyline, SIE_polygon or SIE_median!")
  }
  
  

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
      
      require(parallel)
      cl <- makeCluster(dcores)
      print(paste("Downloading specified SIC files from NSIDC servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
      
      if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", filenames, sep=""), .scheduling="dynamic")))
      } else if(!isTRUE(suppr_msgs)) {
        clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_list, destfile=paste(res_dir, "/Data/", data_type, "/", filenames, sep=""), .scheduling="dynamic")
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
} else if(repository=="MODIS_A_Mapped" | repository=="MODIS_A_L3SMI") {
  
  if(data_type %in% c("SIC", "SIE_polyline", "SIE_polygon", "SIE_median")) {
    stop("MODIS Aqua does not support SIC or SIE data, which are only available from NSIDC! Please use Chla, SST, NSST, iPAR or PAR instead.")
  }
  
  res_dir <- paste(res_path, "/", repository, sep="")
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
  
  if(repository=="MODIS_A_L3SMI") {
    
  if(frequency=="monthly") {
    stop("Monthly frequency is NOT supported for MODIS_A_L3SMI data! Please use MODIS_A_Mapped instead, or switch to daily frequency.")
  }
    
  platform <- "MODISA"
  top <- "https://oceandata.sci.gsfc.nasa.gov/opendap/catalog.xml"
  product <- "L3SMI"
  base_url <- "https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/L3SMI"
  
  obpg_vec <- list()
  day_rlist <- list()
  file_vec <- list()
  
  #Connecting to the page with direct data access for each year and getting a list of available years
  year_cat <- threddscrawler::get_catalog("https://oceandata.sci.gsfc.nasa.gov/opendap/MODISA/L3SMI/catalog.xml")
  year_cats <- year_cat$get_catalogs()
  suppressWarnings(year_list <- names(year_cats))
  year_list <- year_list[year_list %in% year]
  
  days_first <- days_first[names(days_first) %in% year_list]
  days_last <- days_last[names(days_last) %in% year_list]
  
  for(i in seq_along(year_list)) {
    
    #day_range[[as.character(year[i])]] <- c(days_first[[i]][month_ind[1]]:days_last[[i]][month_ind[length(month_ind)]])
    #day_range <- c(days_first[[i]][month_ind[1]]:days_last[[i]][month_ind[length(month_ind)]])
    day_range <- unlist(sapply(seq_along(month_ind), function(x) days_first[[i]][month_ind[x]]:days_last[[i]][month_ind[x]]))
    
    day_rlist[[as.character(year_list[i])]] <- day_range
    
    if(obpg_access==TRUE) {
    print(paste("Assembling list of files for available year ", i, " of ", length(year_list), " (", year_list[i], ")", sep=""))
      
    obpg_vec[[as.character(year_list[i])]] <- obpg_query(top=top, platform = platform, product = product,
                                                  year=year[i], day=day_range, what="all",
                                                  greplargs=greplargs)
    } else if(obpg_access==FALSE) {}
  }

      #day_rlist <- day_rlist[names(day_rlist) %in% year_list]
      day_list <- list()
      url_list <- list()
      data_cats <- list()
      
      for(i in seq_along(year_list)) {
      ann_cat <- year_cats[year_list[i]]
      suppressWarnings(day_cat <- ann_cat[[year_list[i]]]$GET())
      day_cat <- day_cat$get_catalogs()
      suppressWarnings(day_vec <- names(day_cat))
      day_list[[as.character(year_list[i])]] <- day_lvec <- day_vec[as.numeric(day_vec) %in% day_rlist[[i]]]
      #data_cats[[as.character(year_list[i])]] <- sapply(day_lvec, function(x) day_cat[[x]]$GET())
      
      url_list[[as.character(year_list[i])]] <- url_templist <- paste(base_url, "/", year_list[i], "/", day_list[[as.character(year_list[i])]], "/", sep="")
      }
      
      
      url_total <- unlist(url_list)
      
      print(paste("Adding required MODIS L3SMI files to download list..."))
      
      for(i in seq_along(url_total)) {
        
        svMisc::progress(i, max.value=length(url_total), progress.bar = FALSE)
        Sys.sleep(1/length(url_total))
        if(i==length(url_total)) cat("Done!\n")
        
        file_vec_ex <- query_direct(alt_uri = url_total[i])$Filename
        file_vec[[i]] <- file_vec_ex[grep(id_var, file_vec_ex)]
      }
      
      file_vec <- unlist(Filter(length, file_vec))
      down_url <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/"
      url_complete <- paste0(down_url, file_vec)
      
      if(!isTRUE(opt_down)) {
        print(paste("Downloading specified ", data_type, " files from MODIS_A servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
        for(j in seq_along(url_complete)) {
          
          svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
          Sys.sleep(1/length(url_complete))
          if(j==length(url_complete)) cat("Done!\n")
          
          download.file(url_complete[j], paste(res_dir, "/Data/", data_type,"/", url_complete[j], sep=""), quiet=suppr_msgs, mode=down_mode)
        }
        
      } else if(isTRUE(opt_down)) {
        
        require(parallel)
        cl <- makeCluster(dcores)
        print(paste("Downloading specified ", data_type, " files from MODIS_A servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
        
        if(isTRUE(suppr_msgs)) {
          invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", file_vec, sep=""), .scheduling="dynamic")))
        } else if(!isTRUE(suppr_msgs)) {
          clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", file_vec, sep=""), .scheduling="dynamic")
        }
      }
      
  } else if(repository=="MODIS_A_Mapped") {
    
    base_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/", url_time, grid_res, "/", id_dir)
    day_rlist <- list()
    day_charlist <- list()
    
    if(frequency=="daily") {
      
      year_cat <- httr::GET(base_url)
      if (httr::status_code(year_cat) == 200) {
        year_list <- base_url %>% xml2::read_html(httr::content(year_cat, type = "text/html", 
                                                                as = "text", encoding = "UTF-8")) %>% rvest::html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
          rvest::html_table()
      } else {
        stop("Could not retrieve year list from MODIS data HTML! Aborting.")
      }
      
        year_list <- gsub("/","", year_list[[1]][,colnames(year_list[[1]])])
        year_list <- year_list[year_list %in% year]
        
        days_first <- days_first[names(days_first) %in% year_list]
        days_last <- days_last[names(days_last) %in% year_list]
      
    } else if(frequency=="monthly") {
      yr_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Daily/", grid_res, "/", id_dir)
      year_cat <- httr::GET(yr_url)
      
      if (httr::status_code(year_cat) == 200) {
        year_list <- yr_url %>% xml2::read_html(httr::content(year_cat, type = "text/html", 
                                                                as = "text", encoding = "UTF-8")) %>% rvest::html_nodes(xpath = "//*[@id=\"content\"]/table") %>% 
          rvest::html_table()
      } else {
        stop("Could not retrieve year list from MODIS data HTML! Aborting.")
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
        stop("Could not retrieve year list from MODIS data HTML! Aborting.")
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
        
        grep_patt <- paste("^A", year_list[i], day_charlist[[as.character(year_list[i])]], ".*", id_dtype, sep="")
        grep_patt <- paste(grep_patt, collapse="|", sep="")
        
        dirquery_year[[as.character(year_list[i])]] <- dirquery[grep(grep_patt, dirquery, value=FALSE)]
      
      } else if(frequency=="monthly") {
        day_range <- unlist(sapply(seq_along(month_ind), function(x) c(days_first[[i]][month_ind[x]], days_last[[i]][month_ind[x]])))
        
        day_rlist[[as.character(year_list[i])]] <- day_range
        #day_charlist[[as.character(year_list[i])]] <-  sapply(day_rlist[[as.character(year_list[i])]], function(x) ifelse(nchar(x)==1, paste0("00", x), ifelse(nchar(x)==2, paste0("0", x), paste(x))))
        
        grep_patt <- paste("^A", year_list[i], day_charlist[[as.character(year_list[i])]], ".*", id_dtype, sep="")
        grep_patt <- paste(grep_patt, collapse="|", sep="")
        
        dirquery_year[[as.character(year_list[i])]] <- file_lst[grep(grep_patt, file_lst, value=FALSE)]
        }
      
    }
    
    down_url <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/"
    dirquery_unlist <- unlist(dirquery_year)
    url_complete <- paste0(down_url, dirquery_unlist)
    
    if(!isTRUE(opt_down)) {
      print(paste("Downloading specified ", data_type, " files from MODIS_A servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
      for(j in seq_along(url_complete)) {
        
        svMisc::progress(j, max.value=length(url_complete), progress.bar = FALSE)
        Sys.sleep(1/length(url_complete))
        if(j==length(url_complete)) cat("Done!\n")
        
          download.file(url_complete[j], paste(res_dir, "/Data/", data_type,"/", dirquery_unlist[j], sep=""), quiet=suppr_msgs, mode=down_mode)
        }
      
    } else if(isTRUE(opt_down)) {
      
    require(parallel)
    cl <- makeCluster(dcores)
    print(paste("Downloading specified ", data_type, " files from MODIS_A servers to: ", paste(res_dir, "/Data/", data_type, "/", sep=""), ". This could take a while!", sep=""))
    
    if(isTRUE(suppr_msgs)) {
      invisible(capture.output(clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", dirquery_unlist, sep=""), .scheduling="dynamic")))
    } else if(!isTRUE(suppr_msgs)) {
      clusterMap(cl, download.file, MoreArgs=c(mode=down_mode), url=url_complete, destfile=paste(res_dir, "/Data/", data_type, "/", dirquery_unlist, sep=""), .scheduling="dynamic")
    }
    }
  }
}
print("Download complete!")
}

#FUNCTIONS to check available RAM on the system
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

#Functions to check odd vs even numbers
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

#FUNCTION to convert km to lat/long for EPSG:3411 NSIDC Stereograpghic North Polar (x's have to be given in km)
mapxy <- function(X, Y, sgn = 1, slat = 70,  re = 6378.273, e2 = 0.006693883,
                  degrees = TRUE){
  pi = 3.141592654
  E <-  sqrt(e2)
  SL <-  slat*pi/180.
  delta <- 0
  
  rho <- sqrt(X^2 + Y^2)
  CM <- cos(SL)/sqrt(1 - e2*(sin(SL)^2))
  T <- tan((pi/4)-(SL/(2)))/((1 - E*sin(SL))/(1 + E*sin(SL)))^(E/2)
  
  if (abs(slat - 90) < 1e-5) {
    T <- rho*sqrt((1 + E)^(1 + E)*(1 - E)^(1 - E))/2/re
  } else {
    T <- rho*T/(re*CM)
  }
  
  chi <- (pi/2) - 2*atan(T)
  aLat <- (chi + ((e2/2) + (5*e2^2/24) + (e2^3/12))*sin(2*chi) +
             ((7*e2^2/48)+(29*e2^3/240))*sin(4*chi) +
             (7*e2^3/120)*sin(6*chi))
  aLat <- sgn*aLat;
  aLong <- atan2(sgn*X, -sgn*Y)
  aLong <- sgn*aLong
  
  if (any(rho <= 0.1)) {
    temp <- which(rho <= 0.1)
    aLat[temp] <- (pi/2)*sgn
    aLong[temp] <- 0
  }
  
  #Calculated longitude is rotated pi/4 clockwise from NSIDC polar stereographic grid
  aLong <- aLong - pi/4;
  aLong[aLong < -pi] <- 2*pi + aLong[aLong < -pi]
  
  if (degrees == TRUE) {
    aLat <- (180/pi)*aLat
    aLong <- (180/pi)*aLong - delta
  }
  
  coords <- c(aLong, aLat)
  
  return(coords)
}


#MAPXY usage example
#res <- list()
#system.time(
#  for(i in 1:nrow(map_test)) {
#    res[[i]] <- mapxy(X=map_test[i,1], Y=map_test[i,2])
#  }
#)

# FUNCTION clim_summary: LOADING .NC (netCDF) files, extracting and summarising parameters
#devtools::install_github("RS-eco/processNC")
#install.packages("ncdf4")
#install.packages("raster")
#install.packages("scatterpie")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("lubridate")
#install.packages("tidyverse")
#install.packages("ggmap")
#install.packages("RNetCDF")
#install.packages("rgdal")
#install.packages("chron")
#install.packages("lattice")
#install.packages("svMisc")
#install.packages("zoo")
#install.packages("ff")
#install.packages("ffbase")
#install.packages("bigmemory")
#install.packages("bigalgebra")
#install.packages("sf")

## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite() # For specific packages, use biocLite("packagename")
#biocLite(c("GO.db", "impute", "preprocessCore"))
#install.packages("stringi")
#install.packages("broom")
#install.packages("WGCNA")


clim_summary <- function(remove_miss="all", frequency="daily", repository="NSIDC", hemisphere="north", data_type="Chla", nc_path, year_rng="all", month_rng="all", day_rng=NULL, var_names="default", 
                         coord_subset=NULL, subset_order=c("year", "month", ""), summary_func="total", export_path=NULL, big_data=FALSE, good_names=TRUE, calc_anom=TRUE) {

#Install and load the required libraries
list.of.packages <- c("purrr", "processNC", "ncdf4", "raster", "scatterpie", "maps", "mapdata", "lubridate", "ggmap", "rgdal", "sp", "chron",
                      "lattice", "svMisc", "zoo", "data.table", "bigmemory", "bigalgebra")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])) install.packages(new.packages[!new.packages %in% c("threddscrawler", "obpgcrawler")])

if(any(new.packages %in% "threddscrawler")) devtools::install_github("BigelowLab/threddscrawler")
if(any(new.packages %in% "obpgcrawler")) devtools::install_github("BigelowLab/obpgcrawler")

invisible(lapply(list.of.packages, require, character.only = TRUE))
  
  
library(purrr)
library(processNC)
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

if(calc_anom==TRUE & !is.character(big_data[1])) {
  print("Anomaly calculations are NOT supported when big_data routines are turned off... Only Means and SD values will be returned!")
  calc_anom <- FALSE
}
  
if(any(!month_rng %in% c("all","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) & !is.numeric(month_rng)||any(!month_rng %in% c(1:12)) & !is.character(month_rng)) { #& !is.numeric(month)
  stop("Incorrect month values provided! Please use any of: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, or corresponding numerical values (1 to 12).")
}
  
if(repository=="NASA_Melt" & summary_func=="yearly_monthly") {
  print("Monthly summaries cannot be calculated from the NASA Cryosphere ice melt dataset! Changing summary_func to yearly...")
  summary_func <- "yearly"
}

if(is.numeric(day_rng) & frequency!="daily") {
  stop("Selecting a day_rng is not supported unless daily data resolution is used! Stopping...")
}

if(!any(subset_order %in% "day") & is.numeric(day_rng)) {
  stop("Selecting a day range (day_rng) is only supported when subset_order is daily! Stopping...")
}

if(is.numeric(day_rng) & repository=="NASA_Melt") {
  stop("Selecting a day_rng is currently only supported for the MODIS_A and NSIDC datasets! Stopping...")
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
nc_files <- list.files(nc_path, pattern="\\.nc$", full.names=TRUE)

if(repository!="NASA_Melt") {
  
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

if(is.character(big_data[1])) {
  data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
}

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
  if(is.character(big_data[1])) {
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
    
  } else if(!is.character(big_data[1]) | big_data==FALSE) {
    data_total[,paste(dnames, as.character(tstamp), sep=";")] <- dlist_vec
  }
  
} else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
  stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
}
}

} else if(repository=="MODIS_A") {
  
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
  
  if(is.character(big_data[1])) {
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
  }
  
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
      if(is.character(big_data[1])) {
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
        
      } else if(!is.character(big_data[1]) | big_data==FALSE) {
        data_total[,paste(dnames, as.character(tstamp), sep=";")] <- dlist_vec
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
  
  if(is.character(big_data[1])) {
    data.table::fwrite(as.data.frame(t(data_total)), file=paste(big_data[1], "data_total.csv", sep="/"), dec=".", sep=",", quote=FALSE, col.names = FALSE, row.names = FALSE, na=NA, showProgress = FALSE)
  }
  
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
      if(is.character(big_data[1])) {
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
        
      } else if(!is.character(big_data[1]) | big_data==FALSE) {
        data_total[,paste(dnames, as.character(tstamp), sep=";")] <- dlist_vec
      }
      
    } else if(!identical(coordinates_total[,1:ncol(coordinates_total)], coordinates_test[,1:ncol(coordinates_total)])) {
      stop("Not all imported netCDF files represent an identical coordinate grid! Stopping function execution.")
    }
  }
  
}

  #Summary statistics calculations
  #Calculating summary statistics (mean, sd, RSD) of EACH variable extracted from netCDF files
  #The above are calculated either for the whole chosen period, or ADDITIONALLY for each year/month within that period separately
  #gc()
  
  data_barebones <- as.data.frame(coordinates_total)
  data_monthly_summary <- data_barebones
  
  if(is.character(big_data[1])) {
    
    if(!is.null(big_data[3]) & big_data[3]>0) {
      
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
      
      print("Compiling processed data into a single .CSV file for export...")
      
      #Conditionally calculate the average anomaly for all selected years throughout year_rng! DEPRECATED SINCE TOO SMALL VALUES (UNDERSTANDABLY)!
      #if(calc_anom==TRUE) {
      #  
      #  if(summary_func=="yearly" | summary_func=="yearly_monthly") {
      #   
      #    for(i in seq_along(dnames)) {
      #      anom_slist <- paste(dnames[i], " (", month_ind[1], "-", month_ind[length(month_ind)], " ", min(year_rng), "-",max(year_rng), ")", sep="")
      #      anom_grep <- paste0("Anomaly of ", dnames[i], " (", month_ind[1], "-", month_ind[length(month_ind)], " ", year_rng, ")") #NOT ACTUALLY GREP but EXACT MATCHING
      #      #anom_grep <- grep("Anomaly of", names(chunk_list))
      #      
      #      chunk_list[[paste0("Anomaly of ", anom_slist)]] <- colMeans(do.call(rbind, chunk_list[names(chunk_list) %in% anom_grep]), na.rm=TRUE) #apply(do.call(rbind, chunk_list[names(chunk_list) %in% anom_grep]), 2, mean, na.rm=TRUE) 
      #    }
      #  }
      #  
      #  if(summary_func=="yearly_monthly") {
      #    
      #    anom_greplist <- suppressWarnings(levels(interaction(year_rng, month_ind, sep="-")))
      #    
      #    for(i in seq_along(dnames)) {
      #      
      #      anom_slist <- paste(dnames[i], " (", month_ind[1], "-", month_ind[length(month_ind)], " ", min(year_rng), "-",max(year_rng), ")", sep="")
      #      anom_grep <- paste0("Anomaly of ", dnames[i]," (", anom_greplist, ")")
      #      
      #      monthly_list[[paste0("Anomaly of ", anom_slist)]] <- colMeans(do.call(rbind, monthly_list[names(monthly_list) %in% anom_grep]), na.rm=TRUE) #apply(do.call(rbind, chunk_list[names(monthly_list) %in% anom_grep]), 2, mean, na.rm=TRUE) 
      #    }
      #  }
      #}
      
      #Convert all the vectors to matrices (NOT data frames!) and cbind to the raw coordinate data...
      data_barebones <- cbind(data_barebones, simplify2array(chunk_list))
      
      if(summary_func=="yearly_monthly") {
        print("Compiling monthly data...")
        data_monthly_summary <- cbind(data_monthly_summary, simplify2array(monthly_list))
      }
      
    } else if(is.null(big_data[3]) | big_data[3]==0) {
      
      print("Chunk-wise analysis of big_data not selected. Attempting import of the processed dataset as a big.matrix...")
      
      #data_msub <- as.ffdf(fread(file=paste0(big_data, "/data_total.csv"), sep=",", dec=".", header = FALSE, stringsAsFactors = TRUE))
      data_total <- read.big.matrix(paste0(big_data[1], "/data_total.csv"), header=FALSE, has.row.names = FALSE, type="double")
      #data_total <- as.big.matrix(t(data_total[]))
      options(bigmemory.allow.dimnames = TRUE)
      rownames(data_total) <- colname_vec
    }
  } else if(!is.character(big_data[1]) | big_data==FALSE) {
    print("Big data analysis not selected. Proceeding with the full dataset in RAM...")
    data_total <- as.data.frame(t(data_total))
    colnames(data_total) <- NULL
  }
  
  if(!is.character(big_data[1]) | big_data[1]==FALSE | is.null(big_data[3]) | big_data[3]==0) {
    
    for(i in seq_along(dnames)) {
      if(summary_func=="total" | summary_func=="yearly" | summary_func=="yearly_monthly") {
        
        print(paste0("Processing summary statistics (NOT chunk-wise) for variable ", i, " of ", length(dnames), " (", dnames[i],")"))
        
        #total_subset <- data_total[,grep(paste0("^", dnames[i]),colnames(data_total))]
        if(!any(is.null(day_rng)) & is.numeric(day_rng)) {
          data_barebones[,paste("Mean of ", dnames[i], " (", min(year_rng), "-",max(year_rng), " days", day_rng[1], "-", day_rng[length(day_rng)], ")", sep="")] <- apply(data_total[grep(paste0("^", dnames[i]),rownames(data_total)),], 2, mean, na.rm=TRUE)
          data_barebones[,paste("SD of ", dnames[i], " (", min(year_rng), "-",max(year_rng), " days", day_rng[1], "-", day_rng[length(day_rng)], ")", sep="")] <- apply(data_total[grep(paste0("^", dnames[i]),rownames(data_total)),], 2, sd, na.rm=TRUE)
        } else if(any(is.null(day_rng)) & !is.numeric(day_rng)) {
          data_barebones[,paste("Mean of ", dnames[i], " (", min(year_rng), "-",max(year_rng), " ", month_ind[1], "-", month_ind[length(month_ind)], ")", sep="")] <- apply(data_total[grep(paste0("^", dnames[i]),rownames(data_total)),], 2, mean, na.rm=TRUE)
          data_barebones[,paste("SD of ", dnames[i], " (", min(year_rng), "-",max(year_rng), " ", month_ind[1], "-", month_ind[length(month_ind)], ")", sep="")] <- apply(data_total[grep(paste0("^", dnames[i]),rownames(data_total)),], 2, sd, na.rm=TRUE)
        }
        
        
      }
      
      if(summary_func=="yearly"| summary_func=="yearly_monthly") {
        
        if(is.numeric(day_rng)) {
          stop("Yearly and yearly_monthly summary functions are not supported with day ranges (day_rng)! Stopping...")
        }
        
        for(j in seq_along(year_rng)) {
          #progr_seq <- i*j
          #progress(progr_seq, max.value=length(dnames)*length(year_rng), progress.bar = TRUE)
          #Sys.sleep(0.01)
          #if(progr_seq==length(dnames)*length(year_rng)) cat("Done!\n")
          
          print(paste0("Summarising data for year ", j, " of ", length(year_rng), " (", year_rng[j], ")"))
          
          #data_subset <- data_total[,grep(paste("^", dnames[i], ",", year_rng[j], sep=""),colnames(data_total))]
          #data_barebones <- transform(data_barebones, Mean=apply(data_subset, 1, mean, na.rm=TRUE), SD=apply(data_subset, 1, sd, na.rm = TRUE)) #Alters colnames badly!
          #colnames(data_barebones)[colnames(data_barebones) %in% c("Mean", "SD")] <- c(paste("Mean of ", dnames[i], " (", year_rng[j], ")", sep=""), paste("SD of ", dnames[i], " (", year_rng[j], ")", sep=""))
          data_barebones[,paste("Mean of ", dnames[i], " (",  month_ind[1], "-", month_ind[length(month_ind)], " ", year_rng[j], ")", sep="")] <- apply(data_total[grep(paste("^", dnames[i], ";", year_rng[j], sep=""),rownames(data_total)),], 2, mean, na.rm=TRUE)
          data_barebones[,paste("SD of ", dnames[i], " (", month_ind[1], "-", month_ind[length(month_ind)], " ", year_rng[j], ")", sep="")] <- apply(data_total[grep(paste("^", dnames[i], ";", year_rng[j], sep=""),rownames(data_total)),], 2, sd, na.rm=TRUE)
          
          if(summary_func=="yearly_monthly") {
            for(k in seq_along(month_vec)) {
              print(paste0("Calculating monthly values for ", month_val[k], "..."))
              
              #data_monthly_subset <- data_total[,grep(paste("^", dnames[i], ",", year_rng[j], "-", month_ind[k], sep=""),colnames(data_total))]
              
              if(frequency=="monthly") {
                data_monthly_summary[,paste("Mean of ", dnames[i], " (", year_rng[j], "-", month_ind[k], ")", sep="")] <- as.vector(data_total[grep(paste("^", dnames[i], ",", year_rng[j], "-", month_ind[k], sep=""),rownames(data_total)),])
              } else if(frequency=="daily") {
                data_monthly_summary[,paste("Mean of ", dnames[i], " (", year_rng[j], "-", month_ind[k], ")", sep="")] <- apply(data_total[grep(paste("^", dnames[i], ";", year_rng[j], "-", month_ind[k], sep=""),rownames(data_total)),], 2, mean, na.rm=TRUE)
                data_monthly_summary[,paste("SD of ", dnames[i], " (", year_rng[j], "-", month_ind[k],  ")", sep="")] <- apply(data_total[grep(paste("^", dnames[i], ";", year_rng[j], "-", month_ind[k], sep=""),rownames(data_total)),], 2, sd, na.rm=TRUE)
              }
            }
          }
        }
      }
    }
  }

  if(good_names==TRUE) {
    print("Converting column names to be syntactically correct, and exporting pretty names separately...")
    
    pretty_names_yearly <- colnames(data_barebones)
    colnames(data_barebones) <- make.names(colnames(data_barebones))
    
    if(summary_func=="yearly_monthly") {
      
      pretty_names_monthly <- colnames(data_monthly_summary)
      colnames(data_monthly_summary) <- make.names(colnames(data_monthly_summary))
    }
    
  } else if(good_names==FALSE) {
    print("Column names were NOT changed to their syntactically correct equivalents (not recommended)...")
    pretty_names_yearly <- NULL
    
    if(summary_func=="yearly_monthly") {
      pretty_names_monthly <- NULL
    }
  }
  
if(!is.null(export_path) & is.character(export_path)) {
  print("Exporting summarised data as .CSV files...")
  
  if(summary_func=="total" | summary_func=="yearly") {
    data.table::fwrite(data_barebones, paste(export_path, "/", repository, "_", frequency, "_Total_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".csv", sep=""), na=NA, showProgress = FALSE)
    #print(paste(export_path, "/", repository, "_", frequency, "_Total_", time_var, ".csv", sep=""))
  } else if(summary_func=="yearly_monthly") {
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
  
} else if(summary_func=="yearly_monthly") {
  if(repository != "NASA_Melt") {
    return(list(yearly_summary=data_barebones, monthly_summary=data_monthly_summary, projection_info=proj_info, clim_pretty_names=pretty_names_yearly, clim_pretty_names_monthly=pretty_names_monthly, coords_total=coordinates_full[,c("Longitude", "Latitude")]))
  } else if(repository == "NASA_Melt") {
    return(list(yearly_summary=data_barebones, monthly_summary=data_monthly_summary, projection_info=proj_info, clim_pretty_names=pretty_names_yearly, clim_pretty_names_monthly=pretty_names_monthly))
  }

}
}


#Calculating the melt onset, freeze-up and duration of the melt season (WORK CURRENTLY ON HOLD!)
#1) Subset dataset by year (same as for calculating yearly averages)
#if(!is.null(consec_calcs) & consec_calcs!="none") {

#  con_vars <- consec_calcs[1]
#  con_thrs <- consec_calcs[2]
#  con_durs <- consec_calcs[3]

#  if(!is.na(consec_calcs[4]) & consec_calcs[4]=="all") {
#    data_consec <- data_full
#  } else if(is.na(consec_calcs[4]) | concec_calcs[4]=="subset") {
#    data_consec <- data_total
#  }

#  data_bare <- data_consec[,colnames(data_consec) %in% c("xcoord", "ycoord", "Longitude", "Latitude")]

#  for(i in seq_along(con_vars)) {
#  for(j in seq_along(year_rng)) {
#    print(paste0("Summarising data for year ", j, " of ", length(year_rng), " (", year_rng[j], ")"))
#    data_cons <- data_consec[,grep(paste("^", con_vars[i], ",", year_rng[j], sep=""),colnames(data_consec))]
#    con_cols <- strsplit(colnames(data_cons), ",")
#    con_cols <- sapply(col_cols, "[", 2)
#    colnames(data_cons) <- con_cols

# Test function to calculate the day of melt onset (WORK CURRENTLY ON HOLD!)
#xx_res <- c()
#data_submatfull <- as.matrix(data_subtest)
#data_submat <- data_submatfull[10117:10160,]

#system.time(
#for(i in 1:nrow(data_submat)) {
#  progress(i, max.value=nrow(data_submat), progress.bar = FALSE)
#  Sys.sleep(1/nrow(data_submat))
#  if(i==nrow(data_submat)) cat("Done!\n")

#  matrow_name <- as.character(i)
#  res_roll <- c()
#  res_roll[c("1", "2", "3")] <- c(NA,NA,NA)
#res_roll <- rollapply(data_submat[i,], width=4, align="left", function(x) max(x)) #Creates the same vector regardless of preceding values - fast but NOT suitable!
#  for(j in 1:ncol(data_submat)) {
#    if(j>=4 & j<=(ncol(data_submat)-5)) {
#      resname <- as.character(j)
#      if(all(na.omit(data_submat[i, c((j-3):j)] > 15))) {
#        res_roll[resname] <- max(data_submat[i, j:(j+5)])
#        xx_res[matrow_name] <- which(res_roll < 15)[1]
#      } else if(all(na.omit(data_submat[i, c((j-3):j)] <= 15))) {xx_res[matrow_name] <- NA}
#    } else if(j<4 & j > (ncol(data_submat)-5)) {xx_res[matrow_name] <- NA}
#  }
#})

#data_barebones[,paste("Mean of ", dnames[i], " (", month_rng[1], "-", month_rng[length(month_rng)], " ", min(year_rng), "-",max(year_rng), ")", sep="")] <- apply(total_subset, 1, mean, na.rm=TRUE)
#data_barebones[,paste("SD of ", dnames[i], " (", month_rng[1], "-", month_rng[length(month_rng)], " ", min(year_rng), "-",max(year_rng), ")", sep="")] <- apply(total_subset, 1, sd, na.rm=TRUE)

#2) For each such subset, determine the first (i.e. min) position of consecutive SpSIC values above a set threshold (e.g. 15%?)
#3) Extract the column name of said position for each row, strsplit it to obtain the date, and paste it into a NEW "barebones"-type dataframe!
#4) Using a reference data frame and get_monthdays() function, convert the dates into day-of-the-year integers and paste these into a new column

#strsplit(colnames(data_total)[grep(dnames[1], colnames(data_total))], ",")

#  }

#  }
#}