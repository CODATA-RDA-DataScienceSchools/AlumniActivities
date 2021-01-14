############################################################################################################
############################################################################################################
#This program cleans the data for the interactive map (Project 2) 
#
#Created: 11/02/2020
#Modified:
#Input: CSV from the google forms survey
#Output: none
#
#
#
############################################################################################################
############################################################################################################

rm(list = ls())
library(dplyr)
library(tidyr)
library(janitor)
library(psych)
library(redcapAPI)
library(lubridate)

report.date = format(Sys.Date(), "%Y%m%d")
####UPLOAD THE DATA####
df <- read.csv("H:/Class_Work/IU/CODATA-RDA Interactive Map Data.csv") %>%
  select(-c(Timestamp))
toMerge <- read.csv("H:/Class_Work/IU/CODATA-RDA Interactive Map Data v20201119.csv") %>%
  select(-c(X, subid))

df <- tail(df,-131)

#Change the Role category into a numerical factor
df$Role..Choose.all.that.apply. <- gsub(";", ", ", df$Role..Choose.all.that.apply., fixed=TRUE)

df <- df %>%
  rename(Role = Role..Choose.all.that.apply.) %>%
  rename(YearofSchool = Year.of.First.CODATA.RDA.School.Attended) %>%
  rename(CityofSchool = City.of.First.CODATA.RDA.School.Attended) %>%
  rename(FieldofStudy = Field.of.Study..Choose.the.most.fitting.or.most.closely.related.to.your.field..) %>%
  rename(InstitutionCountry = Institution.Country) %>%
  rename(OriginCountry = Origin.Country)

df$Gender = factor(df$Gender,
                  levels = c("Male","Female"),
                  labels = c(1, 2))

toMerge$Gender = factor(toMerge$Gender,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))

toMerge$Gender = factor(toMerge$Gender,
                        levels = c("Male", "Female"),
                        labels = c(1, 2))

merged <- full_join(x = toMerge, y = df, by = c("Name", "ORCID", "Email", "Webpage", "Twitter", "OriginCountry", "InstitutionCountry",
                                                "FieldofStudy", "CityofSchool", "YearofSchool", "Gender", "Role"), all.x = TRUE)

MERGED_FINAL <- merged %>%
  mutate(sp = ifelse(grepl("Student Participant", Role), 1, 0)) %>%
  mutate(nso = ifelse(grepl("Non-Student Observer", Role), 1, 0)) %>%
  mutate(ca = ifelse(grepl("Classroom Assistant", Role), 1, 0)) %>%
  mutate(instr = ifelse(grepl("Instructor", Role), 1, 0)) %>%
  mutate(leo = ifelse(grepl("Local Event Organizer", Role), 1, 0)) %>%
  mutate(stupar = ifelse(grepl("Student Participant", Role), 1, 0)) %>%
  mutate(cochair = ifelse(grepl("CODATA or RDA Co-Chair", Role), 1, 0)) %>%
  mutate(oth = ifelse(grepl("Other", Role), 1, 0)) %>%
  mutate(subid = seq.int(nrow(merged))) %>%
  relocate(subid, .before = Name) %>%
  mutate(FieldofStudy = replace(FieldofStudy, FieldofStudy == "Biology ", "Biology"))

MERGED_FINAL$Gender = factor(MERGED_FINAL$Gender,
                        levels = c("1", "2"),
                        labels = c("Male", "Female"))

MERGED_FINAL$Gender = as.character(MERGED_FINAL$Gender)

MERGED_FINAL$Gender[is.na(MERGED_FINAL$Gender)] <- "Prefer not to Say"

write.csv(MERGED_FINAL, file = paste0("H:/Class_Work/IU/project-data.csv", sep = ""))
