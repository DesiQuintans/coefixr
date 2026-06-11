
#' NCCTG Lung Cancer Data (modified)
#'
#' This dataset originally comes from the `survival` package.
#'
#' @format A data frame with 228 rows and 13 variables:
#' \itemize{
#'   \item inst:           Institution code
#'   \item time:           Survival time in days
#'   \item status:         censoring status 1=censored, 2=dead
#'   \item died:           TRUE if status == dead
#'   \item age:	           Age in years
#'   \item age.tertile:    Age in years (cut into tertiles)
#'   \item sex:	           Male=1 Female=2
#'   \item ph.ecog:        ECOG performance score as rated by the physician. 0=asymptomatic, 1= symptomatic but completely ambulatory, 2= in bed <50% of the day, 3= in bed > 50% of the day
#'   \item ph.karno:       Karnofsky performance score (bad=0-good=100) rated by physician
#'   \item pat.karno:      Karnofsky performance score as rated by patient
#'   \item meal.cal:       Calories consumed at meals
#'   \item wt.loss:        Weight loss in last six months (pounds)
#'   \item wt.loss.binary: Weight loss in last six months (a binary Factor cut at the median)
#' }
#'
#' @source Terry Therneau
#'
#' @references
#' Loprinzi CL. Laurie JA. Wieand HS. Krook JE. Novotny PJ. Kugler JW. Bartel J.
#' Law M. Bateman M. Klatt NE. et al. Prospective evaluation of prognostic
#' variables from patient-completed questionnaires. North Central Cancer Treatment
#' Group. Journal of Clinical Oncology. 12(3):601-7, 1994.
#'
"cancer_modified"
