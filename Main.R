library("tidyverse")

# Paths
# Home
# path = "D:/Work/OneDrive - University College London/Chimera/Pulsoxy/"
#Office
path = "C:/Users/Martin/OneDrive - University College London/Chimera/Pulsoxy/"

source_file = function(x) paste0(path,x)
csv_in_file = function(x) paste0(path,"csv/in/",x)
csv_out_file = function(x) paste0(path,"csv/out/",x)
plot_file = function(x) paste0(path,"plot/",x)
rds_file = function(x) paste0(path,"RDS/",x,".rds")
dta_file = function(x) paste0(path,x,".dta")

# Load utils
source(source_file("0-utils.R"))
