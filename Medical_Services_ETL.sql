/*============================================================================
  PROJECT:      Medical Services ETL Data Preparation
  OBJECTIVE:    Prepare and transform medical services data for analysis
  AUTHOR:       Kim Shellenberger
  VERSION:      v1.2
  CREATED:      10/12/2023
  MODIFIED:     Current
  
  DESCRIPTION:
  Demonstrates SQL expertise including table creation, data import, joins,
  and data transformation for clinical data preparation. Supports anxiety 
  vs. income predictive modeling with data versioning capabilities.
  
  DEPENDENCIES: 
    - mservices.csv (source data file)
    - mservices_update.csv (monthly update data)
  
  TABLES CREATED:
    1. mservices - Main medical services table
    2. mservices_update - Monthly update table
    3. update_mservices - Union of current and updated data
    4. anxiety_summary - Aggregated anxiety statistics by income
============================================================================*/

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*============================================================================
  SECTION 1: CREATE MAIN MEDICAL SERVICES TABLE
============================================================================*/
-- Table stores primary medical services records with patient demographics
-- and diagnostic flags for various health conditions

CREATE TABLE mservices (
	patient_id text PRIMARY KEY,              -- Unique patient identifier
	services varchar(25),                      -- Service type/category
	overweight varchar(3),                     -- Binary flag (Yes/No)
	arthritis varchar(3),                      -- Binary flag (Yes/No)
	diabetes varchar(3),                       -- Binary flag (Yes/No)
	hyperlipidemia varchar(3),                 -- Binary flag (Yes/No)
	back_pain varchar(3),                      -- Binary flag (Yes/No)
	anxiety varchar(3),                        -- Binary flag (Yes/No) - KEY VARIABLE
	allergic_rhinitis varchar(3),              -- Binary flag (Yes/No)
	reflux_esophagitis varchar(3),             -- Binary flag (Yes/No)
	asthma varchar(3),                         -- Binary flag (Yes/No)
	p_date DATE                                -- Service date
);

/*============================================================================
  SECTION 2: IMPORT MEDICAL SERVICES DATA
============================================================================*/
-- Load source data from CSV file
-- TODO: Update file path to actual location of mservices.csv

COPY mservices (
	patient_id, services, overweight, arthritis, diabetes, 
	hyperlipidemia, back_pain, anxiety, allergic_rhinitis, 
	reflux_esophagitis, asthma, p_date
)
FROM 'mservices.csv'              -- TODO: Provide full file path
DELIMITER ',' CSV header;

-- Verification query (commented out to prevent errors if table is empty)
-- SELECT * FROM mservices LIMIT 10;

/*============================================================================
  SECTION 3: CREATE MONTHLY UPDATE TABLE
============================================================================*/
-- Table for monthly incremental updates of medical services data
-- Allows for historical tracking and data versioning

CREATE TABLE mservices_update (
	patient_id text PRIMARY KEY,              -- Unique patient identifier
	services varchar(25),                      -- Service type/category
	overweight varchar(3),                     -- Binary flag (Yes/No)
	arthritis varchar(3),                      -- Binary flag (Yes/No)
	diabetes varchar(3),                       -- Binary flag (Yes/No)
	hyperlipidemia varchar(3),                 -- Binary flag (Yes/No)
	back_pain varchar(3),                      -- Binary flag (Yes/No)
	anxiety varchar(3),                        -- Binary flag (Yes/No)
	allergic_rhinitis varchar(3),              -- Binary flag (Yes/No)
	reflux_esophagitis varchar(3),             -- Binary flag (Yes/No)
	asthma varchar(3),                         -- Binary flag (Yes/No)
	p_date DATE                                -- Service date (updated period)
);

/*============================================================================
  SECTION 4: IMPORT MONTHLY UPDATE DATA
============================================================================*/
-- Load updated/new records from monthly data file
-- TODO: Update file path to actual location of mservices_update.csv

COPY mservices_update (
	patient_id, services, overweight, arthritis, diabetes, 
	hyperlipidemia, back_pain, anxiety, allergic_rhinitis, 
	reflux_esophagitis, asthma, p_date
)
FROM 'mservices_update.csv'       -- TODO: Provide full file path
DELIMITER ',' CSV header;

-- Verification query (commented out)
-- SELECT * FROM mservices_update LIMIT 10;

/*============================================================================
  SECTION 5: MERGE CURRENT AND UPDATED DATA
============================================================================*/
-- Combine original and updated records using UNION
-- Removes duplicates automatically to maintain data integrity

SELECT *
INTO update_mservices
FROM mservices s
UNION
SELECT *
FROM mservices_update u;

-- Verification queries (commented out)
-- SELECT * FROM update_mservices LIMIT 10;
-- SELECT COUNT(*) as Total_Records FROM update_mservices;

/*============================================================================
  SECTION 6: ANXIETY SUMMARY AGGREGATION
============================================================================*/
/* 
  Create anxiety_summary table by joining update_mservices with patient table
  Uses MAX date to select newest records per patient
  
  Output: Statistics (average and standard deviation of income) 
          grouped by anxiety diagnosis status (Yes/No)
  
  Purpose: Supports predictive modeling of anxiety vs. income relationship
*/

SELECT anxiety, AVG(s.income) as avg_income, STDDEV(s.income) as stddev_income
INTO anxiety_summary
FROM 
	(
	SELECT t.patient_id, t.anxiety, t.income, MAX(t.p_date) as latest_date
	FROM 
		(
		SELECT p.income, s.anxiety, p.patient_id, s.p_date
		FROM patient p
		INNER JOIN update_mservices s 
		ON p.patient_id = s.patient_id 
		) t		
	GROUP BY patient_id, anxiety, income
	) s
GROUP BY anxiety
-- Verification query (commented out)
-- SELECT * FROM anxiety_summary

/*============================================================================
  SECTION 7: STORED PROCEDURE - ANXIETY SUMMARY BY INCOME THRESHOLD
============================================================================*/
/* 
  Parameterized stored procedure for dynamic filtering by income threshold
  
  Parameters:
    @income_threshold (money) - Minimum income level to include in analysis
  
  Usage:
    EXEC summary_by_income @income_threshold = 45000.00
    EXEC summary_by_income @income_threshold = 55000.00
  
  Purpose: Enable comparative analysis across different income segments
*/

CREATE PROCEDURE summary_by_income
	@income_threshold money
AS
BEGIN
	SELECT anxiety, AVG(s.income) as avg_income, STDDEV(s.income) as stddev_income
	FROM 
		(
		SELECT t.patient_id, t.anxiety, t.income, MAX(t.p_date) as latest_date
		FROM 
			(
			SELECT p.income, s.anxiety, p.patient_id, s.p_date
			FROM patient p
			INNER JOIN update_mservices s 
			ON p.patient_id = s.patient_id 
			) t		
		GROUP BY t.patient_id, t.anxiety, t.income
		HAVING t.income > @income_threshold
		) s
	GROUP BY anxiety
END

GO

/*============================================================================
  SECTION 8: EXECUTE STORED PROCEDURE WITH VARYING THRESHOLDS
============================================================================*/
-- Run anxiety summary analysis across different income segments

-- Low income threshold: $45,000
EXEC summary_by_income @income_threshold = 45000.00

-- Mid income threshold: $55,000
EXEC summary_by_income @income_threshold = 55000.00

-- High income threshold: $65,000
EXEC summary_by_income @income_threshold = 65000.00
