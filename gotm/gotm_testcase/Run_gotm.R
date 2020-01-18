setwd("C:\\Users\\mooret\\Desktop\\flare_feeagh\\FLARE_gotm\\gotm\\gotm_testcase")

library(gotmtools)
library(ggpubr)

dir.create('output', showWarnings = F)

yaml = 'gotm.yaml'
out = 'output.nc'
start = '2019-01-01 05:00:00'
stop = '2020-01-01 00:00:00'

update_gotm_met_file_var('.', met_file = 'gotm_met_all.dat')

input_yaml(yaml, 'time', 'start', start)
input_yaml(yaml, 'time', 'stop', stop)

# Switch to hourly output
input_yaml(yaml, 'output', 'time_unit', 'hour')
input_yaml(yaml, 'output', 'time_step', 1)
input_yaml(yaml, 'output', 'time_method', 'point')


obs.file = 'temp_1hr_2018-2020.dat'
obs <- load_obs(obs.file = obs.file, header = T)
head(obs, 14)
init_prof(obs.file, start, 'init_t_prof.dat')

run_gotm()

p1 <- plot_wtemp(out, size = 3)

mod_wtemp <- get_vari(ncdf = out, var = 'temp') # Extract potential temp

z <- get_vari(out, 'z') # Extract depths corresponding to temp


### But is our model any good?
# We need to compare this with actual observed data

long_heatmap(obs)
long_lineplot(obs)

# Sub set to same period as modelled
obs <- obs[(obs$date >= start & obs$date <= stop),]
p2 <- long_lineplot(obs, main = 'Feeagh - Observed')

ggarrange(p1, p2, nrow = 2) #View plots next to each other

# To do a direct comparison we will need to extract the same depths as observed
mod2 <- setmodDepths(mod.val = mod_wtemp, mod.dep = z, obs = obs)
head(mod2, 45)
ylims <- range(mod2[,3], obs[,3])

p3 <- long_lineplot(mod2, main = ' Feeagh - Modelled')
p3 <- p3 + coord_cartesian(ylim = ylims)
p2 <- p2 + coord_cartesian(ylim = ylims)
ggarrange(p3, p2, nrow = 2) #View plots next to each other

df <- merge(obs, mod2, by = c(1,2))
colnames(df)[3:4] <- c('obs', 'mod')
g1 <- diag_plots(mod = df[,c(1,2,4)], obs = df[,c(1,2,3)], colourblind = F)
g1
ggsave(paste0('output/diag_plot_hr.png'), g1,  dpi = 220,width = 384,height = 216, units = 'mm')

surf_temp <- df[df$depths ==0,]
ggplot(surf_temp, aes(obs, mod))+
  geom_point()

bot_temp <- df[df$depths == -9,]
ggplot(bot_temp, aes(obs, mod))+
  geom_point()


# Switch to daily mean
input_yaml(yaml, 'output', 'time_unit', 'day')
input_yaml(yaml, 'output', 'time_step', 1)
input_yaml(yaml, 'output', 'time_method', 'mean')

obs.file = 'temp_dly_mean_2018-2020.dat'
obs <- load_obs(obs.file = obs.file, header = T)

run_gotm()

p2 <- plot_wtemp(out, size = 3)

mod_wtemp <- get_vari(ncdf = out, var = 'temp') # Extract potential temp

z <- get_vari(out, 'z')

mod3 <- setmodDepths(mod.val = mod_wtemp, mod.dep = z, obs = obs)

df <- merge(obs, mod3, by = c(1,2))
colnames(df)[3:4] <- c('obs', 'mod')
head(df,45)
g2 <- diag_plots(mod = df[,c(1,2,4)], obs = df[,c(1,2,3)], colourblind = F)
ggsave(paste0('output/diag_plot_dly_mean.png'), g2,  dpi = 220,width = 384,height = 216, units = 'mm')


# manual calibration
input_yaml(yaml, 'swr', 'scale_factor', '0.9')
input_yaml(yaml, 'turb_param', 'k_min', '1e-7')

run_gotm()

mod_wtemp <- get_vari(ncdf = out, var = 'temp') # Extract potential temp
z <- get_vari(out, 'z')
mod3 <- setmodDepths(mod.val = mod_wtemp, mod.dep = z, obs = obs)

df <- merge(obs, mod3, by = c(1,2))
colnames(df)[3:4] <- c('obs', 'mod')
head(df,45)
g3 <- diag_plots(mod = df[,c(1,2,4)], obs = df[,c(1,2,3)], colourblind = F)


library(LakeEnsemblR)
pars <- c('swr_factor', 'k_min')
mat <- matrix(data = c(0.5,1.5,1e-10,2.5e-6), nrow = 2, byrow = T)
df <- data.frame(lb = c(0.7, 1e-7), ub = c(1, 3e-7), log = c(FALSE, TRUE))
rownames(df) <- pars
df

master_param_file <- sample_LHC(parRange = df, num = 500) # Create parameter file before paralleization

pars <- read.csv(master_param_file)
ggplot(pars, aes(k_min, swr_factor))+
  geom_point()+
  scale_x_log10()
print2screen = FALSE

for(i in 1:nrow(pars)){
  input_yaml(yaml, 'swr', 'scale_factor', pars$swr_factor[i])
  input_yaml(yaml, 'turb_param', 'k_min', pars$k_min[i])
  
  # invisible(capture.output(run_gotm()))
  # system2(paste0("gotm.exe"), invisible = print2screen)
  system(paste0("gotm.exe"), ignore.stdout = TRUE, show.output.on.console = print2screen)
  
  mod_wtemp <- get_vari(ncdf = out, var = 'temp', print = F) # Extract potential temp
  z <- get_vari(out, 'z', print = F)
  mod3 <- setmodDepths(mod.val = mod_wtemp, mod.dep = z, obs = obs)
  
  df <- merge(obs, mod3, by = c(1,2))
  colnames(df)[3:4] <- c('obs', 'mod')
  
  stats = sum_stat(mod = df[,c(1,2,4)], obs = df[,c(1,2,3)], depth = T)
  stats$par_id <- pars$par_id[i]
  if(i == 1){
    out_stats <- stats
  }else{
    out_stats <- rbind.data.frame(out_stats, stats)
  }
  
  print(paste(i,'/', nrow(pars)))
}

dat <- merge(out_stats, pars, by = 'par_id')
write.csv(out_stats, paste0(gsub('params', 'results', master_param_file)), quote = F, row.names = F)

my.cols = RColorBrewer::brewer.pal(11, "Spectral")
p1 <- ggplot(dat[dat$RMSE <2,], aes(k_min, swr_factor, colour = RMSE))+
  geom_point(size =2)+
  geom_point(data = dat[which.min(dat$RMSE),], size =4, shape = 21)+
  scale_color_gradientn(colours = (my.cols))+
  scale_x_log10()+
  geom_hline(yintercept = 1, linetype = 'dashed')+
  geom_vline(xintercept = 1.4e-7, linetype = 'dashed')+
  theme_bw(base_size = 16)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p1
ggsave(paste0('output/calib_k_min_swr.png'), p1,  dpi = 220,width = 384,height = 216, units = 'mm')

p2 <- ggplot(dat[dat$RMSE <2,], aes(k_min, swr_factor, colour = RMSE))+
  geom_point(size =2)+
  geom_point(data = dat[which.min(dat$RMSE),], size =4, shape = 21)+
  scale_color_gradientn(colours = (my.cols))+
  # scale_x_log10()+
  coord_cartesian(xlim = range(dat$k_min), ylim = range(dat$swr_factor))+
  geom_hline(yintercept = 1, linetype = 'dashed')+
  # geom_vline(xintercept = 1, linetype = 'dashed')+
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p2

dat1 <- read.csv('latin_hypercube_results_202001151902.csv', stringsAsFactors = F)
par1 <- read.csv('latin_hypercube_params_202001151902.csv', stringsAsFactors = F)
dat1 <- merge(dat1, par1, by = 'par_id')

dat2 <- read.csv('latin_hypercube_results_202001152324.csv', stringsAsFactors = F)
par2 <- read.csv('latin_hypercube_params_202001152324.csv', stringsAsFactors = F)
dat2 <- merge(dat2, par2, by = 'par_id')

dat3 <- read.csv('uniform_params_202001161238.csv', stringsAsFactors = F)
par3 <- read.csv('uniform_results_202001161238.csv', stringsAsFactors = F)
dat3 <- merge(dat3, par3, by = 'par_id')


all_dat <- rbind.data.frame(dat1,dat2,dat3)#,dat4,dat5,dat6)

p3 <- ggplot(all_dat, aes(k_min, swr_factor, colour = RMSE))+
  geom_point(size =2)+
  geom_point(data = dat[which.min(dat$RMSE),], size =4, shape = 21)+
  scale_color_gradientn(colours = (my.cols))+
  scale_x_log10()+
  # coord_cartesian(xlim = range(dat$k_min), ylim = range(dat$swr_factor))+
  geom_hline(yintercept = 1, linetype = 'dashed')+
  geom_vline(xintercept = 1.4e-7, linetype = 'dashed')+
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p3
