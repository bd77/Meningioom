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
intervals(mg1)
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



# 14A, 14B Schwamanoma, geen meningioma
# 25A extreme outliers, very high growth speed
# 10D, 18D, 24D because of exponential growth
# "2E", "12B", "21A" virtueel stabiel
growth.nout.df <- growth.df[!growth.df$tumor.id %in% c("14A", "14B", "25A", "10D", "18C", "24D", "2E", "12B", "21A"),]
mg.nout.1 <- lme(data = growth.nout.df, volume.cm3 ~ meas.date.yr, random = ~ meas.date.yr | tumor.id)
summary(mg.nout.1)
intervals(mg.nout.1, which = "fixed")
qqnorm(resid(mg.nout.1))
plot(growth.nout.df$tumor.id, resid(mg.nout.1)) # distributed around 0

growth.speeds <- fixed.effects(mg.nout.1)[2] + ranef(mg.nout.1)[,2]
hist(growth.speeds)
abline(v = mean(growth.speeds))

gs.tbl <- read.table('meningiome_starting_point.txt', sep = '\t', header = TRUE)
gs.tbl <- gs.tbl[!gs.tbl$tumor.id %in% c("14A", "14B", "25A", "10D", "18C", "24D", "2E", "12B", "21A"),]

png('histogrom_growth_speed_with_24A.png')
hist(gs.tbl$growth.speed.cm3.yr, xlab = "Growth speed (cm3/yr)", main = "")
abline(v = mean(gs.tbl$growth.speed.cm3.yr, na.rm = TRUE), col = 'red')
abline(v = median(gs.tbl$growth.speed.cm3.yr, na.rm = TRUE), col = 'blue')
dev.off()