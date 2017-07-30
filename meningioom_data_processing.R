# -----------------------------------
# Analyse meningiomen voor Wouter
# -----------------------------------

library(readxl)
library(ggplot2)
library(lattice)
library(lubridate)

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

# input excel file
meningioom.file <- "gegevens groeisnelheid - Copy 29-4 naar Bart.xlsx"
# get a list of the sheets
sheet.list <- excel_sheets(meningioom.file)
# remove sheets that do not contain meningiome data
meningiome.list <- sheet.list[!(sheet.list %in% c('Blad1', 'voorbeeld'))]

growth.df <- data.frame()
# patient.df <- data.frame()

# meningioom.sheet <- "1A" # for testing
for (meningioom.sheet in meningiome.list) {
  print(meningioom.sheet)
  sheet.df <- as.data.frame(read_excel(meningioom.file, meningioom.sheet, col_names = FALSE))
  
  # extract the data from the sheet
  patient <- as.integer(sheet.df[match("patiënt",sheet.df[,1]), 2])
  tumor <- sheet.df[match("tumor",sheet.df[,1]), 2]
  # check if sheet name == patient + tumor
  if (!(meningioom.sheet == paste0(patient, tumor))) {
    break
    print(paste("Problem on sheet", meningioom.sheet))
  }
  irradiation.year <- as.integer(sheet.df[match("bestraling",sheet.df[,1]), 2])
  # total dose as sum of different doses
  dose.row <- match("Gray",sheet.df[,1])
  irr.dose.gray <- 0
  for (i in 2:4) {
    dose <- as.double(sheet.df[dose.row, i])
    if (!(is.na(dose))) {
      irr.dose.gray <- irr.dose.gray + as.double(sheet.df[dose.row, i])
    }
  }
  sex <- sheet.df[match("sex",sheet.df[,1]), 2]
  
  # get birth data
  birth.date.row <- match("gebdatum",sheet.df[,1])
  if (is.na(sheet.df[birth.date.row, 2])) {
    birth.date <- NA
  } else {
    birth.date <- as.Date(as.integer(sheet.df[birth.date.row, 2]), origin = "1899-12-30") 
  }
  
  # get tumor location
  tumor.location <- sheet.df[match("locatie tumor",sheet.df[,1]), 2]
  
  # get operation date if operated, otherwise NA (conservatief)
  operation.field <- sheet.df[match("operatie",sheet.df[,1]), 2]
  if ('conservatief' %in% operation.field | is.na(operation.field)) {
    operation.date <- NA
  }
  else {
    operation.date <- as.Date(as.integer(sheet.df[match("operatie",sheet.df[,1]), 2]), origin = "1899-12-30") 
  }
  
  # read growth time series
  volume.row <- match("datum",sheet.df[,1]) + 1
  if (!is.na(sheet.df[volume.row, 1])) {
    while (!is.na(sheet.df[volume.row, 1])) {
      meas.date <- as.Date(as.integer(sheet.df[volume.row, 1]), origin = "1899-12-30")
      volume.cm3 <- as.double(sheet.df[volume.row, 2])
      growth.df <- rbind(growth.df,
                         data.frame(patient = patient, 
                                    sex = sex,
                                    birth.date = birth.date,
                                    irradiation.year = irradiation.year,
                                    operation.date = operation.date,
                                    tumor = tumor,
                                    tumor.id = paste0(patient, tumor),
                                    tumor.location = tumor.location,
                                    meas.date = meas.date, 
                                    volume.cm3 = volume.cm3))
      # read next line
      volume.row <- volume.row + 1
    }
  }
}

# add column of years in decimal form
growth.df <- cbind(growth.df,
                   operation.date.yr = NA,
                   meas.date.yr = NA)

# convert dates in decimal years
i <- 1
for (i in 1:NROW(growth.df)) {
  if (!is.na(growth.df$operation.date[i])) {
    growth.df$operation.date.yr[i] <- decimal_date(ymd(growth.df$operation.date[i], tz = 'CET'))
  }
  growth.df$meas.date.yr[i] <- decimal_date(ymd(growth.df$meas.date[i], tz = 'CET'))
}


write.table(growth.df, file = "meningiome_growth.txt", row.names = FALSE,
            quote = FALSE, sep = ';')
# 
# png('overzicht_meningiomen.png')
# xyplot(volume.cm3~meas.date|patient,
#        type="b",
#        group=tumor,
#        data=growth.df,
#        auto.key =list(
#          points = FALSE, 
#          columns=2,
#          lines = TRUE)
# )
# dev.off()



