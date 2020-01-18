update_gotm_met_file_var <- function(working_directory, met_file){
  
  yaml = file.path(working_directory, 'gotm.yaml')
  
  ######
  #u10
  input_yaml(file = yaml, label = 'u10', key = 'file', value = met_file)
  #v10
  input_yaml(file = yaml, label = 'v10', key = 'file', value = met_file)
  #airp
  input_yaml(file = yaml, label = 'airp', key = 'file', value = met_file)
  #airt
  input_yaml(file = yaml, label = 'airt', key = 'file', value = met_file)
  #cloud
  input_yaml(file = yaml, label = 'cloud', key = 'file', value = met_file)
  #swr
  input_yaml(file = yaml, label = 'swr', key = 'file', value = met_file)
  #precip
  input_yaml(file = yaml, label = 'precip', key = 'file', value = met_file)
    #hum
    input_yaml(file = yaml, label = 'hum', key = 'file', value = met_file)
}