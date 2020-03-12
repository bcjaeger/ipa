
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



#' Ames housing data
#'
#' * `Order`: Observation number
#' * `PID`: Parcel identification number  - can be used with city web site for parcel review.
#' * `MS SubClass`: Identifies the type of dwelling involved in the sale.
#' * `MS Zoning`: Identifies the general zoning classification of the sale.
#' * `Lot Frontage`: Linear feet of street connected to property
#' * `Lot Area`: Lot size in square feet
#' * `Street`: Type of road access to property
#' * `Alley`: Type of alley access to property
#' * `Lot Shape`: General shape of property
#' * `Land Contour`: Flatness of the property
#' * `Utilities`: Type of utilities available
#' * `Lot Config`: Lot configuration
#' * `Land Slope`: Slope of property
#' * `Neighborhood`: Physical locations within Ames city limits (map available)
#' * `Condition 1`: Proximity to various conditions
#' * `Condition 2`: Proximity to various conditions (if more than one is present)
#' * `Bldg Type`: Type of dwelling
#' * `House Style`: Style of dwelling
#' * `Overall Qual`: Rates the overall material and finish of the house
#' * `Overall Cond`: Rates the overall condition of the house
#' * `Year Built`: Original construction date
#' * `Year Remod/Add`: Remodel date (same as construction date if no remodeling or additions)
#' * `Roof Style`: Type of roof
#' * `Roof Matl`: Roof material
#' * `Exterior 1`: Exterior covering on house
#' * `Exterior 2`: Exterior covering on house (if more than one material)
#' * `Mas Vnr Type`: Masonry veneer type
#' * `Mas Vnr Area`: Masonry veneer area in square feet
#' * `Exter Qual`: Evaluates the quality of the material on the exterior
#' * `Exter Cond`: Evaluates the present condition of the material on the exterior
#' * `Foundation`: Type of foundation
#' * `Bsmt Qual`: Evaluates the height of the basement
#' * `Bsmt Cond`: Evaluates the general condition of the basement
#' * `Bsmt Exposure`: Refers to walkout or garden level walls
#' * `BsmtFin Type 1`: Rating of basement finished area
#' * `BsmtFin SF 1`: Type 1 finished square feet
#' * `BsmtFinType 2`: Rating of basement finished area (if multiple types)
#' * `BsmtFin SF 2`: Type 2 finished square feet
#' * `Bsmt Unf SF`: Unfinished square feet of basement area
#' * `Total Bsmt SF`: Total square feet of basement area
#' * `Heating`: Type of heating
#' * `HeatingQC`: Heating quality and condition
#' * `Central Air`: Central air conditioning
#' * `Electrical`: Electrical system
#' * `1st Flr SF`: First Floor square feet
#' * `2nd Flr SF`: Second floor square feet
#' * `Low Qual Fin SF`: Low quality finished square feet (all floors)
#' * `Gr Liv Area`: Above grade (ground) living area square feet
#' * `Bsmt Full Bath`: Basement full bathrooms
#' * `Bsmt Half Bath`: Basement half bathrooms
#' * `Full Bath`: Full bathrooms above grade
#' * `Half Bath`: Half baths above grade
#' * `Bedroom`: Bedrooms above grade (does NOT include basement bedrooms)
#' * `Kitchen`: Kitchens above grade
#' * `KitchenQual`: Kitchen quality
#' * `TotRmsAbvGrd`: Total rooms above grade (does not include bathrooms)
#' * `Functional`: Home functionality (Assume typical unless deductions are warranted)
#' * `Fireplaces`: Number of fireplaces
#' * `FireplaceQu`: Fireplace quality
#' * `Garage Type`: Garage location
#' * `Garage Yr Blt`: Year garage was built
#' * `Garage Finish`: Interior finish of the garage
#' * `Garage Cars`: Size of garage in car capacity
#' * `Garage Area`: Size of garage in square feet
#' * `Garage Qual`: Garage quality
#' * `Garage Cond`: Garage condition
#' * `Paved Drive`: Paved driveway
#' * `Wood Deck SF`: Wood deck area in square feet
#' * `Open Porch SF`: Open porch area in square feet
#' * `Enclosed Porch`: Enclosed porch area in square feet
#' * `3-Ssn Porch`: Three season porch area in square feet
#' * `Screen Porch`: Screen porch area in square feet
#' * `Pool Area`: Pool area in square feet
#' * `Pool QC`: Pool quality
#' * `Fence`: Fence quality
#' * `Misc Feature`: Miscellaneous feature not covered in other categories
#' * `Misc Val`: $Value of miscellaneous feature
#' * `Mo Sold`: Month Sold
#' * `Yr Sold`: Year Sold
#' * `Sale Type`: Type of sale
#' * `Sale Condition`: Condition of sale
#'
#' @source De Cock, D. (2011). "Ames, Iowa: Alternative to the Boston Housing Data as an End of Semester Regression Project," \emph{Journal of Statistics Education},  Volume 19, Number 3.
#'
#' \url{https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt}
#'
#' \url{http://ww2.amstat.org/publications/jse/v19n3/decock.pdf}
#'
"ames"

