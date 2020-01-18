library(ncdf4)
library(gridExtra)
library(tidyverse)
library(lubridate)


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

analysis_location <- "C:\\Users\\mooret\\Desktop\\flare_feeagh\\fc_run1/"
setwd(analysis_location)

files <- tibble(file = list.files())
files <- files %>% 
  filter(!str_detect(file,"EDI.nc"),
         !str_detect(file,".png"),
         !str_detect(file,".pdf"),
         !str_detect(file,"FOR_H"),
         !str_detect(file,"configure_FLARE.R")
         )

### Load data ##

dates_analyzed <- c("H_2019_08_27","H_2019_09_03") #c("H_2018_09_01","H_2018_10_01","H_2018_11_01","H_2018_12_01","H_2019_01_01",
                    # "H_2019_02_01","H_2019_03_01","H_2019_04_01","H_2019_05_01","H_2019_06_01",
                    # "H_2019_07_01","H_2019_08_01","H_2019_09_01","H_2019_10_01","H_2019_11_01")

n_dates_analyzed <- length(dates_analyzed)

curr_files <- files %>% 
  filter(str_detect(file,dates_analyzed[1]))

curr_file <- curr_files %>% 
  filter(str_detect(file,"PROCESS_UNCERT"))
nc_file <- paste0(analysis_location,curr_file)
nc <- nc_open(nc_file)
check_dims <- ncvar_get(nc,'temp')
forecasted <- ncvar_get(nc,'forecasted')
forecast_index <- which(forecasted == 1)[1]
nc_close(nc)

temp_all <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
temp_process <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
temp_driver <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
init_short <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
init_long <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
temp_ds <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
temp_no_uncert <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
temp_param_uncert <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))
temp_driver_inflow <- array(NA, dim = c(n_dates_analyzed,dim(check_dims)))

for(k in 1:n_dates_analyzed){
  
  curr_files <- files %>% 
    filter(str_detect(file,dates_analyzed[k]))
  
  #curr_file <- curr_files %>% 
  #  filter(str_detect(file,"ALL_UNCERT"))
  #nc_file <- paste0(analysis_location,curr_file)
  #nc <- nc_open(nc_file)
  #temp_all[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  #forecasted <- ncvar_get(nc,'forecasted')
  #forecast_index <- which(forecasted == 1)[1]
  #t <- ncvar_get(nc, "time")
  #t <- as_datetime(as.numeric(t))
  #nc_close(nc)
  #print(t[forecast_index] - days(1))
  
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"PROCESS_UNCERT"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  temp_process[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  nc_close(nc)
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"NOAA_DRIVER_UNCERT"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  temp_driver[k, , ,] <- ncvar_get(nc,'temp')
  nc_close(nc)
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"INIT_UNCERT_WDATA"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  init_short[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  nc_close(nc)
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"INIT_UNCERT_WODATA"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  init_long[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  nc_close(nc)
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"DS_DRIVER_UNCERT"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  temp_ds[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  nc_close(nc)
  
  #curr_file <- curr_files %>% 
  #  filter(str_detect(file,"NO_UNCERT"))
  #nc_file <- paste0(analysis_location,curr_file)
  #nc <- nc_open(nc_file)
  #temp_no_uncert[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  #nc_close(nc)
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"PARAM_UNCERT"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  temp_param_uncert[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  nc_close(nc)
  
  curr_file <- curr_files %>% 
    filter(str_detect(file,"INFLOW_UNCERT"))
  nc_file <- paste0(analysis_location,curr_file)
  nc <- nc_open(nc_file)
  temp_driver_inflow[k, , ,] <- ncvar_get(nc,'temp')[,1:dim(check_dims)[2],]
  nc_close(nc)
  
}

#Create arrays

var_all <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_process <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_driver <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_init <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_init_long <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_ds <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_no_uncert <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_param <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_inflow <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))

var_process_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_driver_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_init_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_ds_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_no_uncert_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_param_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_inflow_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))

var_init_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_process_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_driver_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_ds_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_no_uncert_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_param_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))
var_inflow_long_prop <- array(NA,dim=c(n_dates_analyzed + 1, 28,16))

#Process data

for(k in 1:n_dates_analyzed){
  index = 0
  for(i in forecast_index:length(forecasted)){
    index <- index + 1
    for(j in 1:28){
      #var_all[k,j,index] <- var(temp_all[k,i,,j])
      var_process[k,j,index] <- var(temp_process[k,i,,j])
      var_driver[k,j,index] <- var(temp_driver[k,i,,j])
      var_init[k,j,index] <- var(init_short[k,i,,j])  
      var_init_long[k,j,index] <- var(init_long[k,i,,j])
      var_ds[k,j,index] <- var(temp_ds[k,i,,j])  
      #var_no_uncert[k,j,index] <- var(temp_no_uncert[k,i,,j])
      var_param[k,j,index] <- var(temp_param_uncert[k,i,,j])
      var_inflow[k,j,index]  <- var(temp_driver_inflow[k,i,,j])
      
      var_process_prop[k,j,index] <- var_process[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index] + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_driver_prop[k,j,index] <- var_driver[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_init_prop[k,j,index] <- var_init[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  +  var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_ds_prop[k,j,index] <- var_ds[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_param_prop[k,j,index] <- var_param[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index] + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_inflow_prop[k,j,index] <- var_inflow[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index] + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      
      var_process_long_prop[k,j,index] <- var_process[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_driver_long_prop[k,j,index] <- var_driver[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_init_long_prop[k,j,index] <- var_init_long[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_ds_long_prop[k,j,index] <- var_ds[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_param_long_prop[k,j,index] <- var_param[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
      var_inflow_long_prop[k,j,index] <- var_inflow[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])      
    }
  }
}

k = n_dates_analyzed + 1
index = 0
for(i in forecast_index:length(forecasted)){
  index <- index + 1
  for(j in 1:28){
    #var_all[k,j,index] <- mean(var_all[1:n_dates_analyzed,j,index])
    var_process[k,j,index] <- mean(var_process[1:n_dates_analyzed,j,index])
    var_driver[k,j,index] <- mean(var_driver[1:n_dates_analyzed,j,index])
    var_init[k,j,index] <- mean(var_init[1:n_dates_analyzed,j,index]) 
    var_init_long[k,j,index] <- mean(var_init_long[1:n_dates_analyzed,j,index])
    var_ds[k,j,index] <- mean(var_ds[1:n_dates_analyzed,j,index]) 
    #var_no_uncert[k,j,index] <- mean(var_no_uncert[1:n_dates_analyzed,j,index])
    var_param[k,j,index] <- mean(var_param[1:n_dates_analyzed,j,index])
    var_inflow[k,j,index] <- mean(var_inflow[1:n_dates_analyzed,j,index])
    
    var_process_prop[k,j,index] <- var_process[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_driver_prop[k,j,index] <- var_driver[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_init_prop[k,j,index] <- var_init[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_ds_prop[k,j,index] <- var_ds[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index] + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_param_prop[k,j,index] <- var_param[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_inflow_prop[k,j,index] <- var_inflow[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    
    
    var_process_long_prop[k,j,index] <- var_process[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index] + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_driver_long_prop[k,j,index] <- var_driver[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_init_long_prop[k,j,index] <- var_init_long[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_ds_long_prop[k,j,index] <- var_ds[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_param_long_prop[k,j,index] <- var_param[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    var_inflow_long_prop[k,j,index] <- var_inflow[k,j,index]/(var_process[k,j,index] + var_driver[k,j,index] + var_init_long[k,j,index]  + var_ds[k,j,index] + var_param[k,j,index] + var_inflow[k,j,index])
    
  }
}

#plot_names <- c("August", "October", "December", "Combined")
include_var <- TRUE

for(k in 1:(n_dates_analyzed + 1)){
  
  #Create polygon plot
  #ids <- factor(c("process","parameter",'driver: meteorology','driver: meteorology downscaling', "initial conditions (IC)"))
  ids <- factor(c("process","parameter",'driver: meteorology','driver: meteorology\ndownscaling', "inflow", "initial conditions"))
  
  depth_index = 1
  surface_nogap_top1 = rep(1,length(var_ds_prop[k,depth_index,]))
  surface_nogap_bottom1 = (var_ds_prop[k,depth_index,] + var_init_prop[k,depth_index,] + var_driver_prop[k,depth_index,] + var_param_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  surface_nogap_top2 = surface_nogap_bottom1
  surface_nogap_bottom2 = (var_ds_prop[k,depth_index,] + var_init_prop[k,depth_index,] + var_driver_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  surface_nogap_top3 = surface_nogap_bottom2
  surface_nogap_bottom3 = (var_ds_prop[k,depth_index,] + var_init_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  surface_nogap_top4 = surface_nogap_bottom3
  surface_nogap_bottom4 = (var_init_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  surface_nogap_top5 = surface_nogap_bottom4
  surface_nogap_bottom5 = (var_init_prop[k,depth_index,])
  surface_nogap_top6 = surface_nogap_bottom5
  surface_nogap_bottom6 = rep(0,length(var_init_prop[k,depth_index,]))
  
  surface_nogap_positions <- data.frame(
    id = rep(ids,each = length(c(seq(1,16,1),rev(seq(1,16,1))))),
    x = c(rep(c(seq(1,16,1),rev(seq(1,16,1))),times = 6)),
    y = c(surface_nogap_top1,rev(surface_nogap_bottom1),surface_nogap_top2, rev(surface_nogap_bottom2),surface_nogap_top3,rev(surface_nogap_bottom3),surface_nogap_top4,rev(surface_nogap_bottom4),surface_nogap_top5,rev(surface_nogap_bottom5),surface_nogap_top6,rev(surface_nogap_bottom6)))
  
  surface_nogap_total_unc <- data.frame(x =seq(1,16,1),y= var_ds[k,depth_index,] + var_init[k,depth_index,] + var_driver[k,depth_index,] + var_process[k,depth_index,] + var_param[k,depth_index,] + var_inflow[k,depth_index,])
  ymax <- max(surface_nogap_total_unc$y)
  ####
  surface_gap_top1 = rep(1,length(var_ds_long_prop[k,depth_index,]))
  surface_gap_bottom1 = (var_ds_long_prop[k,depth_index,] + var_init_long_prop[k,depth_index,] + var_driver_long_prop[k,depth_index,] + var_param_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  surface_gap_top2 = surface_gap_bottom1
  surface_gap_bottom2 = (var_ds_long_prop[k,depth_index,] + var_init_long_prop[k,depth_index,] + var_driver_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  surface_gap_top3 = surface_gap_bottom2
  surface_gap_bottom3 = (var_ds_long_prop[k,depth_index,] + var_init_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  surface_gap_top4 = surface_gap_bottom3
  surface_gap_bottom4 = (var_init_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  surface_gap_top5 = surface_gap_bottom4
  surface_gap_bottom5 = (var_init_long_prop[k,depth_index,])
  surface_gap_top6 = surface_gap_bottom5
  surface_gap_bottom6 = rep(0,length(var_init_long_prop[k,depth_index,]))
  
  surface_gap_positions <- data.frame(
    id = rep(ids,each = length(c(seq(1,16,1),rev(seq(1,16,1))))),
    x = c(rep(c(seq(1,16,1),rev(seq(1,16,1))),times = 6)),
    y = c(surface_gap_top1,rev(surface_gap_bottom1),surface_gap_top2, rev(surface_gap_bottom2),surface_gap_top3,rev(surface_gap_bottom3),surface_gap_top4,rev(surface_gap_bottom4),surface_gap_top5,rev(surface_gap_bottom5), surface_gap_top6,rev(surface_gap_bottom6)))
  
  surface_gap_total_unc <- data.frame(x =seq(1,16,1),y= var_ds[k,depth_index,] + var_init_long[k,depth_index,] + var_driver[k,depth_index,] + var_process[k,depth_index,] + var_param[k,depth_index,])
  ymax <- max(surface_gap_total_unc$y)
  
  ####
  depth_index = 8
  
  depth_nogap_top1 = rep(1,length(var_ds_prop[k,depth_index,]))
  depth_nogap_bottom1 = (var_ds_prop[k,depth_index,] + var_init_prop[k,depth_index,] + var_driver_prop[k,depth_index,] + var_param_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  depth_nogap_top2 = depth_nogap_bottom1
  depth_nogap_bottom2 = (var_ds_prop[k,depth_index,] + var_init_prop[k,depth_index,] + var_driver_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  depth_nogap_top3 = depth_nogap_bottom2
  depth_nogap_bottom3 = (var_ds_prop[k,depth_index,] + var_init_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  depth_nogap_top4 = depth_nogap_bottom3
  depth_nogap_bottom4 = (var_init_prop[k,depth_index,] + var_inflow_prop[k,depth_index,])
  depth_nogap_top5 = depth_nogap_bottom4
  depth_nogap_bottom5 = (var_init_prop[k,depth_index,])
  depth_nogap_top6 = depth_nogap_bottom5
  depth_nogap_bottom6 = rep(0,length(var_init_prop[k,depth_index,]))
  depth_nogap_positions <- data.frame(
    id = rep(ids,each = length(c(seq(1,16,1),rev(seq(1,16,1))))),
    x = c(rep(c(seq(1,16,1),rev(seq(1,16,1))),times = 6)),
    y = c(depth_nogap_top1,rev(depth_nogap_bottom1),depth_nogap_top2, rev(depth_nogap_bottom2),depth_nogap_top3,rev(depth_nogap_bottom3),depth_nogap_top4,rev(depth_nogap_bottom4),depth_nogap_top5,rev(depth_nogap_bottom5),depth_nogap_top6,rev(depth_nogap_bottom6)))
  
  depth_nogap_total_unc <- data.frame(x =seq(1,16,1),y= var_ds[k,depth_index,] + var_init[k,depth_index,] + var_driver[k,depth_index,] + var_process[k,depth_index,] + var_param[k,depth_index,] )
  ymax <- max(depth_nogap_total_unc$y)
  ####
  
  depth_gap_top1 = rep(1,length(var_ds_long_prop[k,depth_index,]))
  depth_gap_bottom1 = (var_ds_long_prop[k,depth_index,] + var_init_long_prop[k,depth_index,] + var_driver_long_prop[k,depth_index,] + var_param_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  depth_gap_top2 = depth_gap_bottom1
  depth_gap_bottom2 = (var_ds_long_prop[k,depth_index,] + var_init_long_prop[k,depth_index,] + var_driver_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  depth_gap_top3 = depth_gap_bottom2
  depth_gap_bottom3 = (var_ds_long_prop[k,depth_index,] + var_init_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  depth_gap_top4 = depth_gap_bottom3
  depth_gap_bottom4 = (var_init_long_prop[k,depth_index,] + var_inflow_long_prop[k,depth_index,])
  depth_gap_top5 = depth_gap_bottom4
  depth_gap_bottom5 = (var_init_long_prop[k,depth_index,])
  depth_gap_top6 = depth_gap_bottom5
  depth_gap_bottom6 = rep(0,length(var_init_long_prop[k,depth_index,]))
  
  depth_gap_positions <- data.frame(
    id = rep(ids,each = length(c(seq(1,16,1),rev(seq(1,16,1))))),
    x = c(rep(c(seq(1,16,1),rev(seq(1,16,1))),times = 6)),
    y = c(depth_gap_top1,rev(depth_gap_bottom1),depth_gap_top2, rev(depth_gap_bottom2),depth_gap_top3,rev(depth_gap_bottom3),depth_gap_top4,rev(depth_gap_bottom4),depth_gap_top5,rev(depth_gap_bottom5),depth_gap_top6,rev(depth_gap_bottom6)))
  
  depth_gap_total_unc <- data.frame(x =seq(1,16,1),y= var_ds[k,depth_index,] + var_init_long[k,depth_index,] + var_driver[k,depth_index,] + var_process[k,depth_index,] + var_param[k,depth_index,])
  ymax <- max(depth_gap_total_unc$y)
  color_plot <- tribble(
    ~id, ~color,
    "initial conditions" , "green",
    "meteorology downscaling","red",
    "meteorology","purple",
    "process","blue"
  )
  
  
  surface_nogap_positions$id <- factor(surface_nogap_positions$id, levels = c("process","parameter",'driver: meteorology','driver: meteorology\ndownscaling', "inflow", "initial conditions"))
  surface_gap_positions$id <- factor(surface_gap_positions$id, levels = c("process","parameter",'driver: meteorology','driver: meteorology\ndownscaling', "inflow", "initial conditions"))
  depth_nogap_positions$id <- factor(depth_nogap_positions$id, levels = c("process","parameter",'driver: meteorology','driver: meteorology\ndownscaling', "inflow", "initial conditions"))
  depth_gap_positions$id <- factor(depth_gap_positions$id, levels = c("process","parameter",'driver: meteorology','driver: meteorology\ndownscaling', "inflow", "initial conditions"))
  
  ymax <- max(depth_gap_total_unc$y,depth_nogap_total_unc$y,surface_gap_total_unc$y,surface_nogap_total_unc$y)
  ### PLots
  p1 <- ggplot() + 
    geom_polygon(data = surface_nogap_positions, aes(x=x, y=y, fill = id)) + 
    scale_fill_manual(values = c("green", "pink","limegreen", "darkgreen", "black","deepskyblue2"), guides(reverse = TRUE)) +
    labs(title ="Forecast initialized on day with data", x = " ", y = "Proportion of total variance", tag = "a") +
    scale_x_continuous(limits = c(1,16), breaks = seq(1,16,1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1.0), expand = c(0, 0),sec.axis = sec_axis(~.*ymax)) +
    theme(legend.position = 'bottom',plot.title = element_text(size=10)) +
    guides(fill=guide_legend(title="Uncertanity source:")) +
    #geom_vline(xintercept=seq(1,16,1), color = 'gray', alpha = 0.3) +
    #geom_hline(yintercept=c(0,0.25,0.5,0.75,1.0), color = 'gray', alpha = 0.3) +
    annotate("text", x = 8, y = 0.95, label = "0.1 m depth",color = 'black') + 
    #annotate("text", x = 2, y = 0.95, label = "(a)",color = 'black') + 
    geom_line(data = surface_nogap_total_unc, aes(x = x , y = y/ymax), color = "orange") #+
  #annotate("text", x = 14.0, y = 0.78, label = "total variance",color = 'black',angle = 57) 
  
  p2 <- ggplot() + 
    geom_polygon(data = surface_gap_positions, aes(x=x, y=y, fill = id)) + 
    scale_fill_manual(values = c("green", "pink","limegreen", "darkgreen", "black","deepskyblue2"), guides(reverse = TRUE)) +
    labs(title ="Forecast initialized 1 week after last data", x = " ", y = " ", tag = "b") +
    scale_x_continuous(limits = c(1,16), breaks = seq(1,16,1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1.0), expand = c(0, 0),sec.axis = sec_axis(~.*ymax, name = "Total variance")) +
    theme(legend.position = 'bottom',plot.title = element_text(size=10)) +
    guides(fill=guide_legend(title="Uncertanity Source")) +
    #geom_vline(xintercept=seq(1,16,1), color = 'gray', alpha = 0.3) +
    #geom_hline(yintercept=c(0,0.25,0.5,0.75,1.0), color = 'gray', alpha = 0.3) +
    annotate("text", x = 8, y = 0.95, label = "0.1 m depth",color = 'black') + 
    #annotate("text", x = 2, y = 0.95, label = "(b)",color = 'black') + 
    geom_line(data = surface_gap_total_unc, aes(x = x , y = y/ymax), color = "orange")
  
  #if(k == 4){
  #  toc_figure <- p2
  #  toc_data = depth_nogap_positions
  #}
  
  
  p3 <- ggplot() + 
    geom_polygon(data = depth_nogap_positions, aes(x=x, y=y, fill = id)) + 
    scale_fill_manual(values = c("green", "pink","limegreen", "darkgreen", "black","deepskyblue2"), guides(reverse = TRUE)) +
    labs(title ="Forecast initialized on day with data", x = "Day in future", y = "Proportion of total variance", tag = "c") +
    scale_x_continuous(limits = c(1,16), breaks = seq(1,16,1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1.0), expand = c(0, 0),sec.axis = sec_axis(~.*ymax)) +
    theme(legend.position = 'bottom',plot.title = element_text(size=10)) +
    guides(fill=guide_legend(title="Uncertanity Source")) +
    #geom_vline(xintercept=seq(1,16,1), color = 'gray', alpha = 0.3) +
    #geom_hline(yintercept=c(0,0.25,0.5,0.75,1.0), color = 'gray', alpha = 0.3) +
    annotate("text", x = 8, y = 0.95, label = "8 m depth",color = 'black') + 
    #annotate("text", x = 2, y = 0.95, label = "(c)",color = 'black') + 
    geom_line(data = depth_nogap_total_unc, aes(x = x , y = y/ymax), color = "orange")
  

  p4 <- ggplot() + 
    geom_polygon(data = depth_gap_positions, aes(x=x, y=y, fill = id)) + 
    scale_fill_manual(values = c("green", "pink","limegreen", "darkgreen", "black","deepskyblue2"), guides(reverse = TRUE)) +
    labs(title ="Forecast initialized 1 week after last data", x = "Day in future", y = " ", tag = "d") +
    scale_x_continuous(limits = c(1,16), breaks = seq(1,16,1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1.0), expand = c(0, 0),sec.axis = sec_axis(~.*ymax, name = "Total variance")) +
    theme(legend.position = 'bottom',plot.title = element_text(size=10)) +
    guides(fill=guide_legend(title="Uncertainty Source")) +
    #geom_vline(xintercept=seq(1,16,1), color = 'gray', alpha = 0.3) +
    #geom_hline(yintercept=c(0,0.25,0.5,0.75,1.0), color = 'gray', alpha = 0.3) +
    annotate("text", x = 8, y = 0.95, label = "8 m depth",color = 'black') + 
    #annotate("text", x = 2, y = 0.95, label = "(d)",color = 'black') + 
    geom_line(data = depth_gap_total_unc, aes(x = x , y = y/ymax), color = "orange")
  
  mylegend <- g_legend(p1)
  
  prow <- grid.arrange(arrangeGrob(p1 + theme(legend.position = "none"),
                                   p2 + theme(legend.position = "none"),
                                   p3 + theme(legend.position = "none"),
                                   p4 + theme(legend.position = "none"),
                                   nrow = 2,
                                   ncol = 2, 
                                   widths = c(12,12)),
                       mylegend,
                       nrow = 2,
                       heights = c(10,1),
                       top = "Relative contribution to forecast uncertainty")
  
  if(k <= n_dates_analyzed){
  ggsave(paste0("C:\\Users\\mooret\\Desktop\\flare_feeagh\\FLARE_figures\\FCR_glm/Figure_6_",dates_analyzed[k],".pdf"),plot = prow, device='pdf',dpi = 300, height = 6, width = 9)
  }else{
    ggsave("C:\\Users\\mooret\\Desktop\\flare_feeagh\\FLARE_figures\\FCR_glm/Figure_6_combined.pdf",plot = prow, device='pdf',dpi = 300, height = 6, width = 9)
    
  }
}

toc_figure <- ggplot() + 
  geom_polygon(data = depth_nogap_positions, aes(x=x, y=y, fill = id)) + 
  scale_fill_manual(values = c("green", "pink","limegreen", "darkgreen", "black","deepskyblue2"), guides(reverse = TRUE)) +
  labs(title ="Proportion of water temperature forecast uncertainty", x = "Day in future", y = "Proportion") +
  scale_x_continuous(limits = c(1,16), breaks = seq(1,16,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1.0), expand = c(0, 0),sec.axis = sec_axis(~.*ymax, name = "Total variance")) +
  #theme(legend.position = 'right',plot.title = element_text(size=8), text = element_text(size=7), legend.key.size = unit(0.1, "in"),
  #      legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,0,-10)) +
  guides(fill=guide_legend(title="", ncol = 1, byrow = TRUE)) +
  #geom_vline(xintercept=seq(1,16,1), color = 'gray', alpha = 0.3) +
  #geom_hline(yintercept=c(0,0.25,0.5,0.75,1.0), color = 'gray', alpha = 0.3) +
  #annotate("text", x = 8, y = 0.95, label = "8 m depth",color = 'black') + 
  #annotate("text", x = 2, y = 0.95, label = "(c)",color = 'black') + 
  geom_line(data = depth_nogap_total_unc, aes(x = x , y = y/ymax), color = "orange")

ggsave(paste0("C:\\Users\\mooret\\Desktop\\flare_feeagh\\FLARE_figures\\FCR_glm/Figure_6_TOC.pdf"),plot = toc_figure, device='pdf',dpi = 300, height = 1.9685, width = 5.11811)


