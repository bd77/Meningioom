# ------------------------------------
# lineair mixed model for growth speed
# ------------------------------------

library(nlme)
library(lme4)

rm(list = ls())
mypath <- 'D:/Other/meningioom/20170726/'
homepath <- "C:/Documenten/Statistiek/Meningioom/"
workpath <- "D:/Other/meningioom/20170726/"
if (dir.exists(homepath)) {
  mypath <- homepath
} else if (dir.exists(workpath)) {
  mypath <- workpath
}
setwd(dir = mypath)

growth.df <- read.table(file = "meningiome_growth.txt", sep = ';', header = TRUE)
names(growth.df)

mg1 <- lme(data = growth.df, volume.cm3 ~ 0 + meas.date.yr, random = ~ meas.date.yr | tumor.id)
mg2 <- lme(data = growth.df, volume.cm3 ~ 0 + meas.date.yr, random = ~ meas.date.yr | tumor.id / patient)
