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

mg1.lm <- lm(data = growth.df, volume.cm3 ~ meas.date.yr)
summary(mg1.lm)
plot(growth.df$meas.date.yr, growth.df$volume.cm3)
plot(growth.df$tumor.id, resid(mg1.lm))
# residuals depend strongly on the tumor >>> mixed effects model

mg1 <- lme(data = growth.df, volume.cm3 ~ meas.date.yr, random = ~ meas.date.yr | tumor.id)
mg2 <- lme(data = growth.df, volume.cm3 ~ meas.date.yr, random = ~ meas.date.yr | tumor.id / patient)
anova(mg1, mg2)
# no difference between model with additional patient grouping. No patient effect. 
# 
summary(mg1)

qqnorm(resid(mg1))

# 24A en 25A are extreme outliers, very high growth speed
# 2D and 2F are based on 2 points > very wide CI
# 16A has a very wide CI, based on 3 points
growth.nout.df <- growth.df[!growth.df$tumor.id %in% c("24A", "25A", "2D", "2F", "16A"),]
mg.nout.1 <- lme(data = growth.nout.df, volume.cm3 ~ meas.date.yr, random = ~ meas.date.yr | tumor.id)
summary(mg.nout.1)
qqnorm(resid(mg.nout.1))
plot(growth.nout.df$tumor.id, resid(mg.nout.1)) # distributed around 0
