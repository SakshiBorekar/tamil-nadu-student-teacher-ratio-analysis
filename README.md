# What Drives Student-Teacher Ratios Across Tamil Nadu?

A village-level regression analysis in R of factors associated with the primary student-teacher ratio (STR) in Tamil Nadu.

**Group project by:** Sakshi Borekar, Manish, Tanvi and Vineetha (Azim Premji University). The report is co-authored by all four of us.

**My contribution:** I wrote the analysis code in R. The data was gathered by other team members; I prepared it for analysis, built the district-level and state-level regression models, and ran the bootstrap confidence intervals.

## Question
Which health, education and amenity factors are associated with the student-teacher ratio in Tamil Nadu villages, and do districts differ after controlling for them?

## Data
- Mission Antyodaya village-level data for Tamil Nadu, merged with village location names (district names) using a village identifier (SHRID)
- 31 districts; about 9,000 villages in the state-level model
- STR = primary school students / primary school teachers, calculated for villages with valid counts
- 22 predictors in four themes: education, health, facilities and other
- The data files are not included in this repository

## Methods
1. **Cleaning:** merged the data with district names, removed missing and unrealistic values. Villages kept STR between 1 and 500 (district models) and between 1 and 200 (state model).
2. **Part A, district models:** within each district, a random 70/30 train/test split, stepwise regression on the training villages, then adjusted R-squared and RMSE (train and test) and residual-vs-fitted plots.
3. **Part B, state model:** one regression on all villages with all 22 predictors plus district dummy variables (reference district: Ariyalur). Predictors with more than two distinct values were standardised; binary indicators stayed 0/1.
4. **Bootstrapping:** 300 resamples to get 95% confidence intervals (percentile method) for the coefficients.

## Key findings
- Availability of a high school had the strongest positive association with STR (coefficient about 1.5; bootstrap 95% CI about 1.1 to 2.0).
- Sub-centres, PHCs, households availing pension and the number of children not attending school were also positively associated, with bootstrap intervals that exclude zero.
- District differences remain after controlling for village factors. Compared with Ariyalur, the Nilgiris, Namakkal, Theni, Tiruppur and Coimbatore have noticeably lower STR.
- Overall explanatory power is low (R-squared about 0.07; adjusted about 0.06), so many drivers of STR are not captured in this data.

## Limitations
- These are associations only. The study does not establish causation.
- Some districts have very few villages (for example the Nilgiris, Kanniyakumari and Theni), which makes their district models unstable.
- The model is less reliable for villages with high STR, where residuals are widely scattered.
- Factors such as dropouts, seasonal migration and school-level staffing are not in the data.
- The district train/test split is random and has no fixed seed, so the district-level table changes slightly each time the code is run. The state-level results do not depend on it.

## Files
- `report/STR-Tamil-Nadu-report.pdf`: the full project report
- `code/str_tamil_nadu_analysis.R`: the R code used for the analysis (needs the two data files, which are not included)
