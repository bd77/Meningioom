# -----------------------------------
# plot line thourg individual curves
# -----------------------------------

library(ggplot2)

rm(list = ls())
homepath <- "C:/Documenten/Statistiek/Meningioom/"
workpath <- "D:/Other/meningioom/20170726/"
if (dir.exists(homepath)) {
  mypath <- homepath
} else if (dir.exists(workpath)) {
  mypath <- workpath
}
setwd(dir = mypath)

# read results of meningiome_data_processing.R
mg.df <- read.table('meningiome_growth.txt', header = TRUE, sep = ';')
# names(mg.df)
# head(mg.df)

# function creating a plot of one tumor
patient.df <- mg.df[mg.df$patient == 26,]  # for testing
plot.patient <- function(patient.df, plot.path) {
  patient <- patient.df$patient[1]
  png(paste0(plot.path, 'patient_', toString(patient),'.png'))
  p <- ggplot(data = patient.df, aes(meas.date.yr, volume.cm3, colour = tumor.id)) 
  p <- p + geom_point() + geom_smooth(method = "lm")
  p <- p + labs(x = "Year", y = "Volume (cm^3)")
  print(p)
  dev.off()
}

plot.path <- "plots_per_patient/"
for (patient in unique(mg.df$patient)) {
  patient.df <- mg.df[mg.df$patient == patient,]
  plot.patient(patient.df, plot.path)
}

