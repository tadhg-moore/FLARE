set_met_cols_yaml <- function(obs_met_outfile, working_directory){
  df <- read_delim(file.path(working_directory, obs_met_outfile), delim = '\t')
  
  yaml = file.path(working_directory, 'gotm_test.yaml')
  
  ######
  #u10
  input_yaml(file = yaml, label = 'u10', key = 'column', value = (which(colnames(df) == "u10")-1))
  input_yaml(file = yaml, label = 'u10', key = 'scale_factor', value = 1)
  #v10
  input_yaml(file = yaml, label = 'v10', key = 'column', value = (which(colnames(df) == "v10")-1))
  input_yaml(file = yaml, label = 'v10', key = 'scale_factor', value = 1)
  #airp
  input_yaml(file = yaml, label = 'airp', key = 'column', value = (which(colnames(df) == "airp" )-1))
  input_yaml(file = yaml, label = 'airp', key = 'scale_factor', value = 1)
  #airt
  input_yaml(file = yaml, label = 'airt', key = 'column', value = (which(colnames(df) == "airt")-1))
  input_yaml(file = yaml, label = 'airt', key = 'scale_factor', value = 1)
  #cloud
  input_yaml(file = yaml, label = 'cloud', key = 'column', value = (which(colnames(df) == "cloud" )-1))
  input_yaml(file = yaml, label = 'cloud', key = 'scale_factor', value = 1)
  #swr
  input_yaml(file = yaml, label = 'swr', key = 'column', value = (which(colnames(df) == "swr")-1))
  input_yaml(file = yaml, label = 'swr', key = 'scale_factor', value = 1)
  #precip
  input_yaml(file = yaml, label = 'precip', key = 'column', value = (which(colnames(df) == "precip")-1))
  input_yaml(file = yaml, label = 'precip', key = 'scale_factor', value = 1)
  
  if("relh" %in% colnames(df)){
    #hum
    input_yaml(file = yaml, label = 'hum', key = 'column', value = (which(colnames(df) == "relh")-1))
    input_yaml(file = yaml, label = 'hum', key = 'type', value = 1) #1=relative humidity (%), 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)
    input_yaml(file = yaml, label = 'hum', key = 'scale_factor', value = 1)
  }else if("dewt" %in% colnames(df)){
    #hum
    input_yaml(file = yaml, label = 'hum', key = 'file', value = met_outfile)
    input_yaml(file = yaml, label = 'hum', key = 'column', value = (which(colnames(df) == "Dewpoint_Temperature_celsius")-1))
    input_yaml(file = yaml, label = 'hum', key = 'type', value = 3) #1=relative humidity (%), 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)
    input_yaml(file = yaml, label = 'hum', key = 'scale_factor', value = 1)
  }
  
  
}
