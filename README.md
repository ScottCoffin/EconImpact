
![](https://media.giphy.com/media/10mHaEBOTBxUvS/giphy.gif)

**The Marsupials**


# EconImpact
Purpose: Determine economic impacts to public water systems from COVID 19

## PRIORITIES
1) Quantitative RISK based on operating cash on hand. Follow-up calls with 12 systems reported that they're not actually in dire predicaments. How confident are we in estimates of systems at risk within 90- 180- x- days.
2) MHI as a predictor variable. DEMOGRAPHICS analysis.
3) Simplify!

## Updates
12-9-2020: final survey list dataset available as "Datasets/completedSurveys.csv"; joined list with ALL SYSTEMS: "Datasets/AllSystems_Surveys.csv"

Analyses due December 18. Presentations first week of January

Powerpoint https://cawaterboards-my.sharepoint.com/:p:/g/personal/bethany_robinson_waterboards_ca_gov/Ea9ABxFCT-JOo9E6T1FK2yQBDWHxxYuEplBFO5FbS8jjwg?e=4%3AWSKGYJ&at=9&CID=dcde4a43-b0d0-a085-600f-7a2ca1fbccfa

## To-Do List (assignments)

(MP) 1) Classify systems as "high", "medium", "low" vulnerability
        "high" = 3 months, "medium" = 6-9 months, "high" = >1 year

(MP) 2) Join "Datasets/completedSurveys.csv" with zip codes and other demographic characteristics

(MP) 3) Write Data Dictionary

(SC) 4) Weight responses

(SC) 5) Join sample list with all systems

## Considerations/Best Practices

Normalize delinquent accouts to service connections

## Data Analysis (Assignments)
Data Analysis Questions (https://cawaterboards-my.sharepoint.com/:w:/r/personal/sean_mccarthy_waterboards_ca_gov/_layouts/15/guestaccess.aspx?e=SKaiI0&share=ETAwWQKs9BBOtI4h-j_yzxgBz816kvcnxs0y3vt-ac2LKg)

(MP) 1. How many systems will likely require financial assistance to remain viable within 3/6/9/12 months? [STRAIGHTFORWARD]

(MP) 2. How many systems have depleted their reserve funds by 25/50/75/100 percent? Absolute dollar amounts, define bins, as percentage of average 2020 monthly expenses? Per connection would be comparable to other size systems, can check stats on which is more useful. [MODERATE]

(BR) 3. How many systems have suffered revenue losses of 25/50/75/100 percent compared to 2019? [STRAIGHTFORWARD]

(BR) 4. How many systems have secured loans or other short-term financing? [STRAIGHTFORWARD] (yes/no, question #7). Required weighting

(MP) 5. What is the population served by systems with extreme/high/medium/low financial vulnerability? Criteria tied to number 1, amount of time before they are in financial crisis, use data from 2 and 3 to clarify level of financial vulnerability

(MP) a. What are the demographic characteristics of populations served by systems with extreme/high financial vulnerability? Characteristics in #11 6. How many systems were unable to provide survey responses due to inadequate financial management (i.e., did not retain financial data from 2019)? Can we summarize why inadequate responses were provided? Need to review comments for explanations. Common categories? Missing revenue/expense data, reasons, Marielle does have master sheet

(SC) 7. What is the total amount of household water debt accumulated since the beginning of the COVID-19 emergency? [MODERATE, REQUIRES WEIGHTING, COMBINING WITH ALL SYSTEMS]

(SC) 8. What proportion of the household water debt is very high/high/medium/low? Also as a percentage of total service connections. [MODERATE, COMBINING WITH ALL SYSTEMS]

(MP) 9. Which zip codes have large numbers of households with water debt? Percentage of total service connections. [SOPHISTICATED]

a. Within those zip codes, which have significant numbers of households with very high/high debt levels? Bins based on increments of $100, need to collapse into fewer groups. [SOPHISTICATED]

(MP) 10. What are the demographic characteristics of the zip codes with high numbers of households in debt and high levels of debt? Look at entire surveyed population in zip codes. [MODERATE]

(SC) 11. What correlations exist between key demographic variables (e.g., race, income, age) and water debt? [SOPHISTICATED]

(SC) 12. What correlations exist between housing status (e.g., single-family/multi-family and owner/renter) and water debt? [SOPHISTICATED]

(SC) 13. What correlations exist between current health and economic indicators (e.g., local/regional unemployment rates, COVID-19 prevalence) and water debt? Compare with number of households facing eviction. [SOPHISTICATED]

(MP) 14. How many water systems are applying late fees to households with water bill debt? (split by system classifications) [MODERATE] LARGE SYSTEMS ONLY

  a. How many households have late fees added to their unpaid bill debt?

(MP) 15. How many systems are currently offering extended repayment options? [STRAIGHTFORWARD]

  a. What population is served by those systems?

(MP) 16. How many small systems are financially connected with other small/medium/large systems and operated as a single entity? (Cal Water, Cal-Am, Golden State Water, Tuolumne Utilities District, Santa Clarita Valley Water Agency, others?) Some surveys had to be dropped since these systems are part of larger conglomerates/rate making areas. This could show the benefits of what we are trying to achieve through financial/managerial consolidations. [STRAIGHTFOWARD, REQUIRES EMAIL TO DISTRICTS)

(MP) 17. For systems with combined bills, what proportion of total household debt is drinking water service debt? [LARGE SYSTEM]

(NA) 18. Loss of revenue due to loss of business customers, could be permanent, difference in revenue amounts from 2020-2019 vs total household debt, also compare with percentage of resid. commercial [CAN'T ANSWER]

(ALL/BR) 19. Information from comments about number of systems reporting household debt due to inability to shutoff customers, categories to describe comments [SOPHISTICATED/TEDIOUS]

(BR) 20. Determine margins of error. [STRAIGHTFORWARD]

(BR) 21. Compare revenue/expenses from 2019 to 2020 by year and by month (if significant). [SOPHISTICATED/NO DATA]

(BR) 22. What characteristics predict months before assistance needed? Service connections #? Disdvantaged status? Location? [SOPHISTICATED]




## Resources

### Specific to Economic Impact of Water Systems in the US and CA
https://www.capradio.org/articles/2020/10/15/just-how-bad-is-californias-water-debt-problem-the-state-isnt-sure/

https://www.awwa.org/Portals/0/AWWA/Communications/AWWA-AMWA-COVID-Report_2020-04.pdf

https://www.ppic.org/blog/recession-safety-net-needed-for-states-small-water-systems/

https://efc.sog.unc.edu/sites/default/files/2020/North%20Carolina%20Utilities%20and%20COVID-19.pdf

### Survey Design Resources 
#Latent Trait Analysis
https://cran.r-project.org/web/packages/mirt/mirt.pdf

#General workflow for survey analysis
https://bookdown.org/jespasareig/Book_How_to_weight_a_survey/

#R specific Tutorial
https://www.datacamp.com/courses/analyzing-survey-data-in-r

#R package
https://cran.r-project.org/web/packages/survey/survey.pdf

## Meeting Notes 

LIVE LINK: https://cawaterboards-my.sharepoint.com/:w:/r/personal/sean_mccarthy_waterboards_ca_gov/_layouts/15/guestaccess.aspx?e=94uC4x&share=EXOjEVKXjWpCs0llHaH6PWUBIhF3aVISXzVFP7THr0jzaA

Develop Representative Survey for Economic Impacts from COVID-19
1. Assess financial viability and potential impacts of water delivery to customers: staff will complete preliminary phone survey with water systems.

a. DDW Staff Phone survey – 400 CWS

b. Small – medium size WS

c. Potential assistance from SAFER

d. How many small systems are within 3,6,9 months of failing/bankruptcy if revenue trends continue

e. Purpose to get ahead of systems before emergencies happen

f. Use questions from EAR on rate assistance programs?

2. Assess consumer debt load/impact. Could use phone surveys and may cooperate with additional entities.

a. Assistance from Water Foundation, ACWA and others – 800 CWS

b.

Key Question: What is a representative sample for these surveys?

a. Is sample set for Survey 1 (Financial health of systems) a subset of sample set for Survey 2 (Consumer Debt)

b. For survey 1, representative sample of all CWS, small-medium CWS less than 10,000 service connections, water systems likely struggling (DACs, very small CWS)

c. Follow-up with systems that responded to previous survey, additional screening

Key Considerations:

1. What type of survey will we conduct (e.g. random, systematic, stratified)?

a. Choice will depend on probability density function.

2. Are there categorical distinctions within the population (i.e. discreteness) that will drive our survey formulation (e.g. large community PWS may be categorically distinct from small, disadvantaged systems)? Separate into bins of water system categories, natural breaks in the data, connections cap at 10,000

3. What is the preferred margin of error (i.e. required precision level)?

4. What is the desired confidence interval for that margin of error (i.e. do we want to be 90%, 95%, 99%, 99.9% sure)?

5. What is the expected response rate of a survey?

a. What past surveys have been performed that could inform us of an approximate response rate?

b. Response rate from smaller water systems may impact size of sample needed from this bin

c. Natural breaks at 50, 100, 200, 500, 1000 service connections

d. Non responses – do we replace with water system in same geographical area
