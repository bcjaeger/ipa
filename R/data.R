
#' Diabetes data.
#'
#' These data are courtesy of Dr John Schorling, Department of Medicine,
#' University of Virginia School of Medicine. The data originally described
#' 1046 participants who were interviewed in a study to understand the prevalence
#' of obesity, diabetes, and other cardiovascular risk factors in central
#' Virginia for African Americans. A total of 403 participants were screened for
#' diabetes, which was defined as glycosolated hemoglobin > 7.0. Among those
#' 403 participants, 366 provided complete data for the variables in these data.
#'
#' The data were originally shared in the `Publish` package, developed by
#'   Thomas Gerds. They are modified slightly here for the purposes of the
#'   `ipa` package.
#'
#' @format A list with two data frames: one holding complete data and the
#'   other holding data that are missing completely at random. Each set
#'   contains the same participants and the same variables:
#'
#'  - `chol`: Total cholesterol
#'  - `stable_glucose`: Stabilized glucose
#'  - `hdl`: High density lipoprotein
#'  - `age`: age, years
#'  - `gender_male`: male gender
#'  - `frame_medium`: medium frame (body type) indicator
#'  - `frame_small`: small frame (body type) indicator
#'  - `sbp`: systolic blood pressure, mm Hg (1 measurement)
#'  - `dbp`: diastolic blood pressure, mm Hg (one measurement)
#'  - `waist`: waist circumference, inches
#'  - `hip`: hip circumference, inches
#'  - `time_ppn`: Postprandial time when labs were drawn in minutes
#'  - `height_cm`: height, cm
#'  - `weight_kg`: weight, kg
#'  - `diabetes`: diabetes status (Yes or No)
#'
#' @source \url{http://192.38.117.59/~tag/Teaching/share/data/Diabetes.html}
#'
#' @references
#'
#' Willems JP, Saunders JT, DE Hunt, JB Schorling: Prevalence of coronary
#' heart disease risk factors among rural blacks: A community-based study.
#' Southern Medical Journal 90:814-820; 1997 Schorling JB, Roach J, Siegel M,
#' Baturka N, Hunt DE, Guterbock TM, Stewart HL: A trial of church-based
#' smoking cessation interventions for rural African Americans. Preventive
#' Medicine 26:92-101; 1997.
#'
"diabetes"

