# EconImpact
Purpose: Determine economic impacts to public water systems from COVID 19

## Statistics
Correlation between median household income and total dollars delinquent by zip code

Compare revenue/expenses from 2019 to 2020 by year and by month (if significant)

What characteristics predict monthes before assistance needed? Service connections #? Disdvantaged status? Location?

Normalize delinquent accouts to service connections

Total 

## To-Do List
Determine margins of error for different sampling scenarios

Test assumption that service connections # is indicator for water system finances (to the extent possible)

use Fee.Code = DAVCS & DAVCL as benchmark value (assign weight for sampling)

Determine what other fee codes mean

Write final list of water systems to call

## Resources

### Specific to Economic Impact of Water Systems in the US and CA
https://www.capradio.org/articles/2020/10/15/just-how-bad-is-californias-water-debt-problem-the-state-isnt-sure/

https://www.awwa.org/Portals/0/AWWA/Communications/AWWA-AMWA-COVID-Report_2020-04.pdf

https://www.ppic.org/blog/recession-safety-net-needed-for-states-small-water-systems/

https://efc.sog.unc.edu/sites/default/files/2020/North%20Carolina%20Utilities%20and%20COVID-19.pdf

### Survey Design Resources 

https://bookdown.org/jespasareig/Book_How_to_weight_a_survey/

https://www.datacamp.com/courses/analyzing-survey-data-in-r

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
