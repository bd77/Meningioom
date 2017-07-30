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
names(mg.df)
head(mg.df)

# function creating a plot of one tumor
tumor.df <- mg.df[mg.df$tumor.id == '2C',]  # for testing
plot.tumor <- function(tumor.df, plot.path) {
  patient <- tumor.df$patient[1]
  tumor.code <- tumor.df$tumor[1]
  tumor.id <- toString(tumor.df$tumor.id[1])
  lm.tumor <- lm(volume.cm3 ~ meas.date.yr, data =tumor.df)
  
  # to get a CI on start date the inverse model???
  lm.tumor.yx <- lm(meas.date.yr ~ volume.cm3, data =tumor.df)
  CI.tumor.start.low <- confint(lm.tumor.yx)[1,1]
  CI.tumor.start.high <- confint(lm.tumor.yx)[1,2]
  
  intersect.y <- lm.tumor$coefficients[1]
  growth.speed.cm3.yr <- lm.tumor$coefficients[2]
  CI.growth.speed.low <- confint(lm.tumor)[2,1]
  CI.growth.speed.high <- confint(lm.tumor)[2,2]
  tumor.start <- -lm.tumor$coefficients[1] / lm.tumor$coefficients[2]
  graph.x.limit <- tumor.df$irradiation.year[1]
  
    # max(1980, min(floor(tumor.start), floor(tumor.df$years.time)))
  
  png(paste0(plot.path, 'tumor_', tumor.id,'.png'))
  p <- ggplot(data = tumor.df, aes(meas.date.yr, volume.cm3)) + geom_point() + geom_smooth(method = "lm")
  p <- p + xlim(graph.x.limit, ceiling(max(tumor.df$meas.date.yr)))
  p <- p + ylim(0, 1.2*max(tumor.df$volume.cm3))
  # put a red dot at the intersection of the line with the x-axis
  p <- p + geom_point(x = tumor.start, y = 0.01, col = 'red', size = 4)
  # put a blue dot at the operation time
  p <- p + geom_point(x = tumor.df$operation.date.yr[1], y = 0.01, col = 'blue', size = 4)
  # put a blue dot at the operation time
  p <- p + geom_point(x = tumor.df$irradiation.year[1], y = 0.01, col = 'yellow', size = 4)
  # p <- p + geom_line(aes(x,y), data.frame(x = c(CI.tumor.start.low, CI.tumor.start.high), y = c(0.01, 0.01)))
  p <- p + labs(x = "Year", y = "Volume (cm^3)")
  print(p)
  dev.off()
  
  result.df <- data.frame(patient = patient,
                          tumor.id = tumor.id,
                          tumor.code = tumor.code,
                          intersect.y = intersect.y,
                          tumor.start = tumor.start,
                          # CI.tumor.start.low = CI.tumor.start.low,
                          # CI.tumor.start.high = CI.tumor.start.high,
                          growth.speed.cm3.yr = growth.speed.cm3.yr,
                          CI.growth.speed.low = CI.growth.speed.low,
                          CI.growth.speed.high = CI.growth.speed.high)
  return(result.df)
}

results.df <- data.frame()
plot.path <- "plots_per_tumor/"
for (tumor.id in unique(mg.df$tumor.id)) {
  tumor.df <- mg.df[mg.df$tumor.id == tumor.id,]
  result.df <- plot.tumor(tumor.df, plot.path)
  results.df <- rbind(results.df, result.df)
}

write.table(results.df, file = 'meningiome_starting_point.txt',
            row.names = FALSE, sep = '\t', quote = FALSE)
hist(log(results.df$slope.cm3.yr))   
