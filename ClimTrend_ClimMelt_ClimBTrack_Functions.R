#FUNCTION 1: Adding Inner and Outer Melt Season Length data to clim_summary output for NASA_Melt! (Stroeve et al., 2014)
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

#FUNCTION 2: Adding least-squares trends to interannual data! (see NSIDC Sea Ice Index, version 3 documentation)
clim_trend <- function(data, trend_vars, trend_unit, add_stats = TRUE, coord_vars = c("Longitude", "Latitude"), time_span) {
  
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


#FUNCTION 3: Calculating relative differences for DAILY clim_summary output data. Useful for bloom tracking with e.g. Chla data! (McKibben et al., 2012)
#install.packages("EnvStats")
clim_btrack <- function(data, sat_vars, coord_vars=c("xcoord", "ycoord", "Longitude", "Latitude"), 
                        run_window = 8, mean_fun=c("arithm", "arithm"), var_lab="default", 
                        year_val=2009, monthly_aggr=TRUE, export_path=NA) {
  
  list.of.packages <- c("data.table","EnvStats", "gtools", "dplyr", "svMisc")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(data.table)
  library(EnvStats)
  library(gtools)
  library(dplyr)
  library(svMisc)
  
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
  print("Processing...")
  print("Processing...")
  print("Processing...")
  print("Processing...")
  
  track_df <- init_data[,sat_vars, drop=FALSE]
  track_vec <- track_df[1,]
  
  if(mean_fun[1]=="geo") {
    ref_vec <- running(as.numeric(track_vec), fun=EnvStats::geoMean, na.rm=TRUE, width=run_window)
  } else if(mean_fun[1]=="arithm") {
    ref_vec <- running(as.numeric(track_vec), fun=mean, na.rm=TRUE, width=run_window)
  }
  
  ref_lagvec <- dplyr::lead(ref_vec, run_window)
  ref_resvec <- ((ref_lagvec-ref_vec)/ref_vec)*100
  
  coord_cols <- init_data[colnames(init_data) %in% coord_vars]
  name_trvec <- colnames(init_data)[!colnames(init_data) %in% coord_vars]
  name_vec <- gsub(".*days[[:digit:]]{1,3}.([[:digit:]]{1,3}).$", "\\1", name_trvec)
  
  last_digits <- name_vec[as.numeric(gsub(".*:([[:digit:]]{1,3})", "\\1", dplyr::lead(names(ref_resvec), run_window)))]
  last_digits <- last_digits[!is.na(last_digits)]
  coverage_vec <- paste0(name_vec[as.numeric(gsub("([[:digit:]]{1,3}).*", "\\1", names(ref_resvec)))], ":", name_vec[as.numeric(gsub(".*:([[:digit:]]{1,3})", "\\1", dplyr::lead(names(ref_resvec), run_window)))])
  
  del_indices <- grep("NA", coverage_vec)
  
  coverage_vec <- coverage_vec[!coverage_vec %in% grep("NA", coverage_vec, value=TRUE)]
  ref_resvec <- ref_resvec[-del_indices]
  
  if(any(var_lab %in% "default") | is.na(var_lab)) {
    var_labname <- unique(gsub("Mean\\.of\\.(.*)\\.{2}.*", "\\1", colnames(init_data)[grep("Mean\\.of\\.", colnames(init_data))]))
  } else if(!any(var_lab %in% "default") & !is.na(var_lab)) {
    var_labname <- as.character(var_lab)
  }
  
  names(ref_resvec) <- fnames_track <- paste0(var_labname, "_track_days.", coverage_vec)
  
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
    ref_resvec <- ((ref_lagvec-ref_vec)/ref_vec)*100
    
    ref_resvec <- bloomtrack_list[[i]] <- ref_resvec[-del_indices]
    #names(ref_resvec) <- fnames_track
  }
  
  bloomtrack_final <- as.data.frame(do.call(rbind, bloomtrack_list))
  #colnames(bloomtrack_final) <- fnames_track
  
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
    
    bloomtrack_final <- as.data.frame(cbind(coord_cols, bloomtrack_final))
    print("Aggregation complete!")
  }
  
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
