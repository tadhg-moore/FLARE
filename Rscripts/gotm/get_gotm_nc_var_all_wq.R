#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
get_gotm_nc_var_all_wq <- function(ncFile,working_dir, z_out,vars){
  
  glm_nc <- nc_open(paste0(working_dir,ncFile))
  
  elev <- abs(ncvar_get(glm_nc, "z")[,12])
  elev[length(elev)] <- 0
  elevs_re <- elev
  
  # tim = ncvar_get(glm_nc, "time")
  # tunits = ncatt_get(glm_nc, "time")
  # lnam = tunits$long_name
  # tustr <- strsplit(tunits$units, " ")
  # step = tustr[[1]][1]
  # tdstr <- strsplit(unlist(tustr)[3], "-")
  # tmonth <- as.integer(unlist(tdstr)[2])
  # tday <- as.integer(unlist(tdstr)[3])
  # tyear <- as.integer(unlist(tdstr)[1])
  # thstr <- strsplit(unlist(tustr)[4], ":")
  # thour <- as.integer(unlist(thstr)[1])
  # tmin <- as.integer(unlist(thstr)[2])
  # tmin <- formatC(tmin, width = 2, format = "d", flag = "0")
  # tsec <- as.integer(unlist(thstr)[3])
  # sec <- formatC(tsec, width = 2, format = "d", flag = "0")
  # 
  # 
  # origin = as.POSIXct(paste0(tyear, "-", tmonth, 
  #                            "-", tday, ' ', thour, ':', tmin, ':', tsec), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # time = as.POSIXct(tim, origin = origin, tz = "UTC")
  # print(time[12])
  
  # elev_surf = get_surface_height(paste0(working_dir,ncFile))
  # max_i <- tallest_layer[length(tallest_layer)]
  # elev <- elev[1:max_i, length(tallest_layer)]
  # num_step <- length(tallest_layer)
  # num_dep <- length(z_out)
  # temp_out <- rep(NA,num_dep)
  # tme = num_step
  # elevs_out <- elev_surf[tme, 2] - z_out
  # elevs = elev
  # num_z <- max_i
  # layer_mids <- c(elevs[1]/2, elevs[1:num_z-1] + diff(elevs)/2)
  # elevs_re <- c(0, layer_mids, tail(elevs, 1))
  
  # snow <- ncvar_get(glm_nc, "hsnow")[2]
  # ice_white <- ncvar_get(glm_nc, "hwice")[2] 
  # ice_blue <- ncvar_get(glm_nc, "hice")[2] 
  # avg_surf_temp <- ncvar_get(glm_nc, "avg_surf_temp")[2] 
  
  
  output <- array(NA,dim=c(length(z_out),length(vars)))
  for(v in 1:length(vars)){
    temp <- ncvar_get(glm_nc, vars[v])[,12]
    
    temps_re <- temp
    if(length(which(is.na(c(temps_re)))) == 0){
      output[,v] <- approx(x = elevs_re, y = temps_re, xout = z_out)$y
    }
    
  }
  
  # mixing_restart_variables <- c("dep_mx","prev_thick", "g_prime_two_layer", "energy_avail_max", "mass_epi", 
  #                               "old_slope", "time_end_shear", "time_start_shear", "time_count_end_shear", "time_count_sim", 
  #                               "half_seiche_period", "thermocline_height", "f0", "fsum", "u_f", "u0", "u_avg")
  # mixing_vars <- rep(NA, length(mixing_restart_variables))
  #   for(v in 1:length(mixing_restart_variables)){
  #     mixing_vars[v] <- ncvar_get(glm_nc, mixing_restart_variables[v])[2] 
  #   }
  
  
  nc_close(glm_nc)
  return(list(output = output)#,
              # surface_height = elev_surf[length(tallest_layer), 2],
              # snow_wice_bice = c(snow, ice_white, ice_blue),
              # avg_surf_temp = avg_surf_temp)#,
         # mixing_vars = mixing_vars)
  )
}
