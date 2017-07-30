# plot line thourgh individual curves
library(ggplot2)

mypath <- "C:/Documenten/Statistiek/Meningioom/20170715"
mypath <- 'D:/Other/meningioom/20170726/'
setwd(dir = mypath)
meningiome <- read.table('meningiome_growth.txt', header = TRUE, sep = ';')

meningiome <- cbind(meningiome, 
                    tumor.id = paste0(meningiome$patient, meningiome$tumor),
                    years.time = NA)
i <- 1
for (i in 1:NROW(meningiome)) {
  secs.since.1970 <- as.numeric(as.POSIXct(toString(meningiome$maes.date[i], format="%Y-%m-%d", tz='UTC')))
  years.since.1970 <- secs.since.1970 / (365.25 * 24 * 60 * 60)
  meningiome$years.time[i] <- 1970 + years.since.1970
}

tumor.df <- meningiome[meningiome$tumor.id == '2C',]

plot.tumor <- function(tumor.df) {
  patient <- tumor.df$patient[1]
  tumor.code <- tumor.df$tumor[1]
  tumor.id <- toString(tumor.df$tumor.id[1])
  lm.tumor <- lm(volume.cm3 ~ years.time, data =tumor.df)
  
  # to get a CI on start date the inverse model???
  lm.tumor.yx <- lm(years.time ~ volume.cm3, data =tumor.df)
  CI.tumor.start.low <- confint(lm.tumor.yx)[1,1]
  CI.tumor.start.high <- confint(lm.tumor.yx)[1,2]
  
  intersect.y <- lm.tumor$coefficients[1]
  growth.speed.cm3.yr <- lm.tumor$coefficients[2]
  CI.growth.speed.low <- confint(lm.tumor)[2,1]
  CI.growth.speed.high <- confint(lm.tumor)[2,2]
  tumor.start <- -lm.tumor$coefficients[1] / lm.tumor$coefficients[2]
  graph.x.limit <- max(1980, min(floor(tumor.start), floor(tumor.df$years.time)))
  
  png(paste0('tumor_plots/tumor_', tumor.id,'.png'))
  p <- ggplot(data = tumor.df, aes(years.time, volume.cm3)) + geom_point() + geom_smooth(method = "lm")
  p <- p + xlim(graph.x.limit, ceiling(max(tumor.df$years.time)))
  p <- p + ylim(0, 1.2*max(tumor.df$volume.cm3))
  p <- p + geom_point(x = tumor.start, y = 0.01, col = 'red', size = 4)
  # p <- p + geom_line(aes(x,y), data.frame(x = c(CI.tumor.start.low, CI.tumor.start.high), y = c(0.01, 0.01)))
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
for (tumor.id in unique(meningiome$tumor.id)) {
  tumor.df <- meningiome[meningiome$tumor.id == tumor.id,]
  result.df <- plot.tumor(tumor.df)
  results.df <- rbind(results.df, result.df)
}

write.table(results.df, file = 'meningiome_strarting_point.txt',
            row.names = FALSE, sep = '\t', quote = FALSE)
hist(log(results.df$slope.cm3.yr))   
