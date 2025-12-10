# apply local copy
library(fs)
library(assertthat)
library(checklist)
source("source/export/survey_experts/docu_report/inbo_website_copy.R")
inbo_website("source/export/survey_experts/docu_report")
