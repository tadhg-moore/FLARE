# --------------------------------------
# purpose: save coefficients from linear debiasing and temporal downscaling
# Creator: Laura Puckett, December 20 2018
# contact: plaura1@vt.edu
# --------------------------------------

## setup

fit_downscaling_parameters <- function(observations,
                                       for.file.path,
                                       working_directory,
                                       VarNames,
                                       VarNamesStates,
                                       replaceObsNames,
                                       PLOT,
                                       local_tzone,
                                       VarInfo,
                                       first_obs_date,
                                       last_obs_date){
  
  # process and read in saved forecast data
  process_saved_forecasts(data.path = for.file.path,
                          working_directory,
                          local_tzone) # geneartes flux.forecasts and state.forecasts dataframes
  NOAA.flux <- readRDS(paste(working_directory,"/NOAA.flux.forecasts", sep = ""))
  NOAA.state <- readRDS(paste(working_directory,"/NOAA.state.forecasts", sep = ""))
  NOAA.data <- inner_join(NOAA.flux, NOAA.state, by = c("forecast.date","ensembles"))
  NOAA_input_tz <- attributes(NOAA.data$forecast.date)$tzone
  
  forecasts <- prep_for(NOAA.data, input_tz = NOAA_input_tz, local_tzone, weather_uncertainty = FALSE) %>%
    dplyr::group_by(NOAA.member, date(timestamp))  %>%
    dplyr::mutate(n = n()) %>%
    # force NA for days without 4 NOAA entries (because having less than 4 entries would introduce error in daily comparisons)
    dplyr::mutate_at(vars(VarInfo$VarNames), funs(ifelse(n == 4, ., NA))) %>%
    ungroup() %>%
    dplyr::select(-"date(timestamp)", -n) %>%
    filter(timestamp >= first_obs_date & timestamp <= last_obs_date)
  
  # -----------------------------------
  # 3. aggregate forecasts and observations to daily resolution and join datasets
  # -----------------------------------
  
  daily.forecast <- aggregate_to_daily(data = forecasts)
  
  daily.obs <- aggregate_to_daily(data = observations)
  # might eventually alter this so days with at least a certain percentage of data remain in dataset (instead of becoming NA if a single minute of data is missing)
  
  joined.data.daily <- inner_join(daily.forecast, daily.obs, by = "date", suffix = c(".for",".obs")) %>%
    ungroup() %>%
    filter_all(all_vars(is.na(.) == FALSE))
  
  rm(NOAA.flux, NOAA.state, NOAA.data, daily.obs)
  
  # -----------------------------------
  # 4. save linearly debias coefficients and do linear debiasing at daily resolution
  # -----------------------------------
  out <- get_daily_debias_coeff(joined.data = joined.data.daily, VarInfo = VarInfo, PLOT, working_directory)
  debiased.coefficients <-  out[[1]]
  debiased.covar <-  out[[2]]
  
   debiased <- daily_debias_from_coeff(daily.forecast, debiased.coefficients, VarInfo)
  
  # -----------------------------------
  # 5.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  redistributed <- daily_to_6hr(forecasts, daily.forecast, debiased, VarNames = VarInfo$VarNames)
  
  # -----------------------------------
  # 5.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------
  
  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed) 
  VarNamesStates <- VarInfo %>%
    filter(VarType == "State")
  VarNamesStates <- VarNamesStates$VarNames
  states.ds.hrly <- spline_to_hourly(df = redistributed,
                                    VarNamesStates = VarNamesStates)
  # if filtering out incomplete days, that would need to happen here
  
  VarNames_6hr <- VarInfo %>%
    filter(ds_res == "6hr")
  VarNames_6hr <- VarNames_6hr$VarNames
  
  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  nonSW.flux.hrly <- redistributed %>%
    select(timestamp, NOAA.member, VarNames_6hr) %>%
    repeat_6hr_to_hrly()
  
  
  ## downscale shortwave to hourly
  ShortWave.ds <- ShortWave_to_hrly(debiased, time0 = NA, lat = lake_latitude, lon = 360 - lake_longitude, local_tzone)
  
  # -----------------------------------
  # 6. join debiased forecasts of different variables into one dataframe
  # -----------------------------------
  
  joined.ds <- full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    full_join(nonSW.flux.hrly, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    filter(timestamp >= min(forecasts$timestamp) & timestamp <= max(forecasts$timestamp))
  
  # -----------------------------------
  # 7. prepare dataframes of hourly observational data for comparison with forecasts
  # -----------------------------------
  
  # get hourly dataframe of observations
  hrly.obs <- aggregate_obs_to_hrly(observations)
  
  # -----------------------------------
  # 8. join hourly observations and hourly debiased forecasts
  # -----------------------------------
  
  joined.hrly.obs.and.ds <- inner_join(hrly.obs,joined.ds, by = "timestamp", suffix = c(".obs",".ds")) %>%
    mutate(Rain.ds = ifelse(Rain.ds<0 , 0, Rain.ds),
           ShortWave.ds = ifelse(ShortWave.ds<0, 0, ShortWave.ds),
           LongWave.ds = ifelse(LongWave.ds<0, 0, LongWave.ds),
           RelHum.ds = ifelse(RelHum.ds<0, 0, RelHum.ds),
           RelHum.ds = ifelse(RelHum.ds>100, 100, RelHum.ds))
  
  # -----------------------------------
  # 9. Calculate and save coefficients from hourly downscaling (R2 and standard deviation of residuals)
  # -----------------------------------
  
  model <- lm(joined.hrly.obs.and.ds$AirTemp.obs ~ joined.hrly.obs.and.ds$AirTemp.ds)
  debiased.coefficients[5,1] <- sd(residuals(model))
  debiased.coefficients[6,1] <- summary(model)$r.squared
  
  model <- lm(joined.hrly.obs.and.ds$WindSpeed.obs ~ joined.hrly.obs.and.ds$WindSpeed.ds)
  debiased.coefficients[5,2] <- sd(residuals(model))
  debiased.coefficients[6,2] <- summary(model)$r.squared
  
  model <- lm(joined.hrly.obs.and.ds$RelHum.obs ~ joined.hrly.obs.and.ds$RelHum.ds)
  debiased.coefficients[5,3] <- sd(residuals(model))
  debiased.coefficients[6,3] <- summary(model)$r.squared
  
  model <- lm(joined.hrly.obs.and.ds$ShortWave.obs ~ joined.hrly.obs.and.ds$ShortWave.ds)
  debiased.coefficients[5,4] <- sd(residuals(model))
  debiased.coefficients[6,4] <- summary(model)$r.squared
  
  model <- lm(joined.hrly.obs.and.ds$LongWave.obs ~ joined.hrly.obs.and.ds$LongWave.ds)
  debiased.coefficients[5,5] <- sd(residuals(model))
  debiased.coefficients[6,5] <- summary(model)$r.squared
  save(debiased.coefficients,debiased.covar, file = paste(working_directory,"/debiased.coefficients.RData", sep = ""))
  
  print(debiased.coefficients)
  # -----------------------------------
  # 10. Visual check (comparing observations and downscaled forecast ensemble mean)
  # -----------------------------------
  if(PLOT == TRUE){
    # pdf(paste0(working_directory, '/met_ds_forecast_average.pdf'), width = 16, height = 9)
    ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      geom_line(aes(y = AirTemp.obs, color = "observations"))+
      geom_line(aes(y = AirTemp.ds, color = "downscaled forecast average", group = NOAA.member))
    
    ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      geom_line(aes(y = WindSpeed.obs, color = "observations"))+
      geom_line(aes(y = WindSpeed.ds, color = "downscaled forecast average", group = NOAA.member))
    
    ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      geom_line(aes(y = RelHum.obs, color = "observations"))+
      geom_line(aes(y = RelHum.ds, color = "downscaled forecast average", group = NOAA.member))
    
    ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      geom_line(aes(y = ShortWave.obs, color = "observations"))+
      geom_line(aes(y = ShortWave.ds, color = "downscaled forecast average", group = NOAA.member))
    
    ggplot(data = joined.hrly.obs.and.ds[1:100000,], aes(x = timestamp)) +
      geom_line(aes(y = LongWave.obs, color = "observations"))+
      geom_line(aes(y = LongWave.ds, color = "downscaled forecast average", group = NOAA.member))
    ggplot(data = joined.hrly.obs.and.ds[1:100000,], aes(x = timestamp)) +
      geom_line(aes(y = Rain.obs, color = "observations"))+
      geom_line(aes(y = Rain.ds, color = "downscaled forecast average", group = NOAA.member))
    # dev.off()
  }
}

