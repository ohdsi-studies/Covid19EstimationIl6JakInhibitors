/************************************************************************
Copyright 2020 Observational Health Data Sciences and Informatics

This file is part of Covid19DrugRepurposing

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/
{DEFAULT @cdm_database_schema = "CDM_JMDC_V1106.dbo"}
{DEFAULT @exposure_database_schema = "scratch.dbo"}
{DEFAULT @exposure_table = "mschuemi_covid19sccs_jmdc"}
{DEFAULT @outcome_database_schema = "scratch.dbo"}
{DEFAULT @outcome_table = "mschuemi_covid19sccs_jmdc"}
{DEFAULT @washout_days = 365}
{DEFAULT @first_exposure_only = FALSE}

IF OBJECT_ID('tempdb..#exposure_cohort', 'U') IS NOT NULL
  DROP TABLE #exposure_cohort;
  
 IF OBJECT_ID('tempdb..#eo_cohort', 'U') IS NOT NULL
  DROP TABLE #eo_cohort;

SELECT cohort_definition_id,
	subject_id,
{@first_exposure_only} ? {	
	MIN(cohort_start_date) AS cohort_start_date,
	MIN(cohort_end_date) AS cohort_end_date
} : {
	cohort_start_date,
	cohort_end_date
}
INTO #exposure_cohort
FROM @exposure_database_schema.@exposure_table
INNER JOIN @cdm_database_schema.observation_period
	ON subject_id = person_id
		AND DATEADD(DAY, @washout_days, observation_period_start_date) <= cohort_start_date
		AND observation_period_end_date >= cohort_start_date
WHERE cohort_definition_id IN (
		SELECT DISTINCT exposure_id
		FROM #exposure_outcome
		)
{@first_exposure_only} ? {	
GROUP BY cohort_definition_id,
	subject_id
}			
;

SELECT DISTINCT exposure.subject_id,
	exposure_outcome.cohort_definition_id,
	exposure.cohort_start_date,
	exposure.cohort_end_date
INTO #eo_cohort
FROM @outcome_database_schema.@outcome_table outcome
INNER JOIN #exposure_cohort exposure
	ON outcome.subject_id = exposure.subject_id
INNER JOIN @cdm_database_schema.observation_period
	ON outcome.subject_id = observation_period.person_id
		AND DATEADD(DAY, @washout_days, observation_period_start_date) <= outcome.cohort_start_date
		AND observation_period_end_date >= outcome.cohort_start_date
		AND DATEADD(DAY, @washout_days, observation_period_start_date) <= exposure.cohort_start_date
		AND observation_period_end_date >= exposure.cohort_start_date
INNER JOIN #exposure_outcome exposure_outcome
	ON exposure_id = exposure.cohort_definition_id
		AND outcome_id = outcome.cohort_definition_id;
