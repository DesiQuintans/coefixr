# # Code to prepare cancer_modified test dataset.
#
# library(survival)
#
# # `cancer` is a dataset provided by the `survival` package:
# #
# # inst:	     Institution code
# # time:	     Survival time in days
# # status:    censoring status 1=censored, 2=dead
# # age:	     Age in years
# # sex:	     Male=1 Female=2
# # ph.ecog:	 ECOG performance score as rated by the physician. 0=asymptomatic, 1= symptomatic but completely ambulatory, 2= in bed <50% of the day, 3= in bed > 50% of the day but not bedbound, 4 = bedbound
# # ph.karno:	 Karnofsky performance score (bad=0-good=100) rated by physician
# # pat.karno: Karnofsky performance score as rated by patient
# # meal.cal:	 Calories consumed at meals
# # wt.loss:	 Weight loss in last six months (pounds)
# cancer_modified <- cancer
#
# cancer_modified$inst <-
#     factor(cancer_modified$inst,
#            levels = c(1:33),
#            labels = c(rep("Sites 01-10", 10),
#                       rep("Sites 11-20", 10),
#                       rep("Sites 20-33", 13)))
#
# cancer_modified$sex <-
#     factor(cancer_modified$sex,
#            levels = 2:1,
#            labels = c("Female", "Male"))
#
# cancer_modified$ph.ecog <-
#     factor(cancer_modified$ph.ecog,
#            levels = 0:4,
#            labels = c("Asymptomatic",
#                       "Symptomatic but completely ambulatory",
#                       "Not completely ambulatory",
#                       "Not completely ambulatory",
#                       "Not completely ambulatory"))
#
#
# # save(cancer_modified, file = "data/cancer_modified.rda")
# usethis::use_data(cancer_modified, overwrite = TRUE)
