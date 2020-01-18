#missing noaa files 
#20181221gep_all_00z.csv to 20181229gep_all_00z.csv

do_step_1 <- FALSE
do_step_2 <- TRUE
do_step_3 <- TRUE
###Step 0 ####################
#Load required elements

if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")

library(mvtnorm)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)
library(imputeTS)
library(tidyverse)
library(tools)

data_location <<- "C:\\Users\\mooret\\Desktop\\flare_feeagh\\fcr_data"
code_folder <<- "C:\\Users\\mooret\\Desktop\\flare_feeagh\\FLARE_run/"
forecast_location <<- "C:\\Users\\mooret\\Desktop\\flare_feeagh\\fcr_run2/"
execute_location <<- "C:\\Users\\mooret\\Desktop\\flare_feeagh\\FLARE_run\\glm\\windows/"

file.copy(paste0(code_folder, 'configure_FLARE.R'), paste0(forecast_location, 'configure_FLARE.R'), overwrite = T)

source(paste0(forecast_location,"/","configure_FLARE.R"))
source(paste0(code_folder, "/", "Rscripts/run_flare.R"))
source(paste0(code_folder, "/", "Rscripts/run_EnKF.R"))
source(paste0(code_folder, "/", "Rscripts/plot_forecast.R"))

spin_up_days <- 0

start_day <- as_date("2019-01-01")
forecast_start<- as_date("2019-04-30")
start_time_local <-"07:00:00"
holder1 <- start_day
holder2 <- forecast_start
while(forecast_start < as_date("2019-11-01")){
  start_day <- forecast_start
  forecast_start <- forecast_start + days(1)
  if(forecast_start < as_date("2019-11-01")){
    holder1 <- c(holder1, start_day)
    holder2 <- c(holder2, forecast_start)
  }
}

forecast_days_vector <- rep(16, length(holder1))
#missing_forecasts <- seq(as_date("2018-12-21"), as_date("2018-12-29"), 1)

forecast_days_vector[1] <- 0
#forecast_days_vector[holder2 %in% missing_forecasts] <- 0

forecasting_timings <- data.frame(holder1,holder2,forecast_days_vector)

start_time_local <- "07:00:00"
partition_dates <- as_date(c("2019-05-01","2019-05-14","2019-06-01","2019-06-14","2019-07-01","2019-07-14","2019-08-01","2019-08-14","2019-09-01","2019-09-14","2019-10-01","2019-10-14"))
# partition_dates <- as_date(c("2019-08-27","2019-09-03"))
partition_restart_files <- rep(NA, length(partition_dates))

sim_name_list <- c('FLARE1_ALL_UNCERT',
                   'FLARE1_INIT_UNCERT_WDATA',
                   'FLARE1_INIT_UNCERT_WODATA',
                   'FLARE1_PROCESS_UNCERT',
                   'FLARE1_NOAA_DRIVER_UNCERT',
                   'FLARE1_DS_DRIVER_UNCERT',
                   'FLARE1_PARAM_UNCERT',
                   'FLARE1_INFLOW_UNCERT')

uncert_mode_list <- c(1,
                      5,
                      6,
                      3,
                      4,
                      8,
                      7,
                      10)

###Step 1 ####################

#RUN FROM JANUARY 1, 2018 to AUGUS
if(do_step_1){
  
  sim_name <- 'FLARE2_SPIN_UP'
  start_day_local <-  as_date(forecasting_timings[1,1])
  
  forecast_start_day_local <- as_date(forecasting_timings[1,2])
  
  hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))
  
  forecast_days <- forecasting_timings[1,3]
  restart_file <- NA
  forecast_sss_on <- TRUE
  
  out <- run_flare(start_day_local,
                   start_time_local,
                   forecast_start_day_local,
                   sim_name = sim_name,
                   hist_days = hist_days,
                   forecast_days = forecast_days,
                   spin_up_days = spin_up_days,
                   restart_file = restart_file,
                   code_folder = code_folder,
                   forecast_location = forecast_location,
                   execute_location = execute_location,
                   push_to_git = push_to_git,
                   pull_from_git = pull_from_git,
                   data_location = data_location,
                   n_enkf_members = n_enkf_members,
                   n_ds_members = n_ds_members,
                   include_wq = include_wq,
                   use_ctd = use_ctd,
                   uncert_mode = uncert_mode,
                   cov_matrix = cov_matrix,
                   downscaling_coeff = downscaling_coeff,
                   GLMversion = GLMversion,
                   DOWNSCALE_MET = DOWNSCALE_MET,
                   FLAREversion = FLAREversion,
                   met_ds_obs_start = met_ds_obs_start,
                   met_ds_obs_end = met_ds_obs_end,
                   modeled_depths = modeled_depths,
                   forecast_sss_on = forecast_sss_on)
  
  
  plot_forecast(pdf_file_name = unlist(out)[2],
                output_file = unlist(out)[1],
                include_wq = include_wq,
                forecast_days = forecast_days,
                code_folder = code_folder,
                save_location = forecast_location,
                data_location = data_location,
                plot_summaries = TRUE,
                push_to_git = push_to_git,
                pull_from_git = pull_from_git,
                use_ctd = use_ctd,
                modeled_depths = modeled_depths)
  
  step1_out <- unlist(out)[1]
  save(step1_out, file = paste0(forecast_location,"/step1_out.Rdata"))
}

### Step 2 ####

part_ind <- which(forecasting_timings$holder1 %in% partition_dates)
fils <- list.files(forecast_location, pattern = c('*.nc'))
fils <- fils[grep('FOR', fils)]
fil_ind <- substr(fils,25,34)

if(do_step_2){
  
  load(file = "C:\\Users\\mooret\\Desktop\\flare_feeagh\\fc_run1//step1_out.Rdata") #paste0(forecast_location,"/step1_out.Rdata"))
  # restart_file <- step1_out
  restart_file = "C:\\Users\\mooret\\Desktop\\flare_feeagh\\fc_run1/FLARE2_SPIN_UP_H_2019_01_01_2019_04_30_F_0_1142020_14_53.nc"
  sim_name <- 'FLARE1_FOR'
  
  for(i in 2:nrow(forecasting_timings)){  
    
    source(paste0(forecast_location,"/","configure_FLARE.R"))
    
    hist_days <- as.numeric(difftime(as_date(forecasting_timings[i,2]),as_date(forecasting_timings[i,1])))
    start_day_local <- forecasting_timings[i,1]
    forecast_start_day_local <- forecasting_timings[i,2]
    forecast_days <- forecasting_timings[i,3]
    print(forecasting_timings)
    forecast_sss_on <- FALSE
    use_ctd <- FALSE
    
    out <- run_flare(start_day_local,
                     start_time_local,
                     forecast_start_day_local,
                     sim_name = sim_name,
                     hist_days = hist_days,
                     forecast_days = forecast_days,
                     spin_up_days = spin_up_days,
                     restart_file = restart_file,
                     code_folder = code_folder,
                     forecast_location = forecast_location,
                     execute_location = execute_location,
                     push_to_git = push_to_git,
                     pull_from_git = pull_from_git,
                     data_location = data_location,
                     n_enkf_members = n_enkf_members,
                     n_ds_members = n_ds_members,
                     include_wq = include_wq,
                     use_ctd = use_ctd,
                     uncert_mode = uncert_mode,
                     cov_matrix = cov_matrix,
                     downscaling_coeff = downscaling_coeff,
                     GLMversion = GLMversion,
                     DOWNSCALE_MET = DOWNSCALE_MET,
                     FLAREversion = FLAREversion,
                     met_ds_obs_start = met_ds_obs_start,
                     met_ds_obs_end = met_ds_obs_end,
                     modeled_depths = modeled_depths,
                     forecast_sss_on = forecast_sss_on)
    
    restart_file <- unlist(out)[1]
    # rest_ind <- which(fil_ind == gsub('-', '_', forecasting_timings[i,2]))
    # restart_file <- NA

    if(start_day_local %in% partition_dates){
      index <- which(partition_dates == start_day_local)
      partition_restart_files[index] <- restart_file
    }
    
    plot_forecast(pdf_file_name = unlist(out)[2],
                  output_file = unlist(out)[1],
                  include_wq = include_wq,
                  forecast_days = forecast_days,
                  code_folder = code_folder,
                  save_location = forecast_location,
                  data_location = data_location,
                  plot_summaries = TRUE,
                  push_to_git = push_to_git,
                  pull_from_git = pull_from_git,
                  use_ctd = use_ctd,
                  modeled_depths = modeled_depths)
  }
  
  save(partition_restart_files, partition_dates, file = paste0(forecast_location,"/partition_restart_files.Rdata"))
}

#Step 3 

if(do_step_3){
  load(paste0(forecast_location,"/partition_restart_files.Rdata"))
  source(paste0(forecast_location,"/","configure_FLARE.R"))
  
  for(i in 1:length(partition_dates)){
    # for(i in 1:1){
    
    hist_days <- 6
    start_day_local <- partition_dates[i]
    forecast_start_day_local <- partition_dates[i] + days(hist_days)
    forecast_days <- 16
    forecast_sss_on <- FALSE
    use_ctd <- FALSE
    
    restart_file <- partition_restart_files[i]

    for(ii in 2:length(sim_name_list)){
      
      source(paste0(forecast_location,"/","configure_FLARE.R"))
      
      sim_name <- sim_name_list[ii]
      
      out1 <- run_flare(start_day_local,
                        start_time_local,
                        forecast_start_day_local,
                        sim_name = sim_name,
                        hist_days = hist_days,
                        forecast_days = forecast_days,
                        spin_up_days = spin_up_days,
                        restart_file = restart_file,
                        code_folder = code_folder,
                        forecast_location = forecast_location,
                        execute_location = execute_location,
                        push_to_git = push_to_git,
                        pull_from_git = pull_from_git,
                        data_location = data_location,
                        n_enkf_members = n_enkf_members,
                        n_ds_members = n_ds_members,
                        include_wq = include_wq,
                        use_ctd = use_ctd,
                        uncert_mode = uncert_mode_list[ii],
                        cov_matrix = cov_matrix,
                        downscaling_coeff = downscaling_coeff,
                        GLMversion = GLMversion,
                        DOWNSCALE_MET = DOWNSCALE_MET,
                        FLAREversion = FLAREversion,
                        met_ds_obs_start = met_ds_obs_start,
                        met_ds_obs_end = met_ds_obs_end,
                        modeled_depths = modeled_depths,
                        forecast_sss_on = forecast_sss_on)
      
      plot_forecast(pdf_file_name = unlist(out1)[2],
                    output_file = unlist(out1)[1],
                    include_wq = include_wq,
                    forecast_days = forecast_days,
                    code_folder = code_folder,
                    save_location = forecast_location,
                    data_location = data_location,
                    plot_summaries = TRUE,
                    push_to_git = push_to_git,
                    pull_from_git = pull_from_git,
                    use_ctd = use_ctd,
                    modeled_depths = modeled_depths)
      
    }
  }
}
