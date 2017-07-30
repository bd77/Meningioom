# -----------------------------------
# Analyse meningiomen voor Wouter
# -----------------------------------


library(readxl)
library(ggplot2)
library(lattice)

rm(list = ls())
mypath <- 'D:/Other/meningioom/20170726/'
# mypaht <- "C:/Documenten/Statistiek/Meningioom/20170715/"
setwd(dir = mypath)
meningioom.file <- "gegevens groeisnelheid - Copy 29-4 naar Bart.xlsx"

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
    print(operation.date)
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
                                    operation.date = operation.date,
                                    tumor = tumor,
                                    tumor.id = paste0(patient, tumor),
                                    tumor.location = tumor.location,
                                    maes.date = meas.date, 
                                    volume.cm3 = volume.cm3))
      # read next line
      volume.row <- volume.row + 1
    }
  }
}

write.table(growth.df, file = "meningiome_growth.txt", row.names = FALSE,
            quote = FALSE, sep = ';')
# 
# png('overzicht_meningiomen.png')
# xyplot(volume.cm3~maes.date|patient,
#        type="b",
#        group=tumor,
#        data=growth.df,
#        auto.key =list(
#          points = FALSE, 
#          columns=2,
#          lines = TRUE)
# )
# dev.off()



