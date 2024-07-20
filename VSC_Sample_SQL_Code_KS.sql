/**********************************************************************

Script name:		Anxiety vs. Income Predictive Model Data Prep
Version:			v1.1
Creation Date:		10/12/2023
Last Updated:		10/20/2023
Author:				Kim Shellenberger
Dependencies:		mservices.csv, mservices_update.csv
Update History:		10/12/2023 – original script created
					10/20/2023 – modified nested join

**********************************************************************/

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

--Create the mservices table with the primary key designation, all column headers and data type
CREATE TABLE mservices (
	patient_id text PRIMARY KEY, 
	services varchar (25), 
	overweight varchar (3), 
	arthritis varchar (3), 
	diabetes varchar (3), 
	hyperlipidemia varchar (3), 
	back_pain varchar (3), 
	anxiety varchar (3), 
	allergic_rhinitis varchar (3),	
	reflux_esophagitis varchar (3),	
	asthma varchar (3),
	p_date DATE);

-- ADD data from the mservices.csv
COPY mservices (
	patient_id, services, overweight, arthritis, diabetes, hyperlipidemia, back_pain, anxiety, allergic_rhinitis, reflux_esophagitis, asthma, p_date
)
FROM 'C:\LabFiles\Medical\mservices.csv'
DELIMITER ',' CSV header;
--SELECT * FROM mservices;

--Create new table for monthly update of mservices
CREATE TABLE mservices_update (
	patient_id text PRIMARY KEY, FOREIGN KEY
	services varchar (25), 
	overweight varchar (3), 
	arthritis varchar (3), 
	diabetes varchar (3), 
	hyperlipidemia varchar (3), 
	back_pain varchar (3), 
	anxiety varchar (3), 
	allergic_rhinitis varchar (3),	
	reflux_esophagitis varchar (3),	
	asthma varchar (3),
	p_date DATE);

--upload update data to new table (csv created to demonstrate)
COPY mservices_update (
patient_id, services, overweight, arthritis, diabetes, hyperlipidemia, back_pain, anxiety, allergic_rhinitis, reflux_esophagitis, asthma, p_date
)
FROM 'C:\LabFiles\Medical\mservices_update.csv'
DELIMITER ',' CSV header;
--SELECT * FROM mservices_update

--Update through a UNION
SELECT *
INTO update_mservices
FROM mservices s
UNION
SELECT *
FROM mservices_update u;
--SELECT * FROM update_mservices

/* Create new table anxiety_summary by inner joining update_mservices with patient on patient_id (FK).
Use the MAX date to choose the newest records. 
Anxiety_summary provides the standard deviation and average income based on yes/no for anxiety diagnosis */

SELECT anxiety, AVG(s.income), STDDEV(s.income)
INTO anxiety_summary
FROM 
	(
	SELECT t.patient_id, t.anxiety, t.income, MAX(t.p_date)
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
--select * from anxiety_summary

--Create stored procedure to generate anxiety summary using variable income threshold 
CREATE PROCEDURE summary_by_income

	@income_threshold money
			
AS

SELECT anxiety, AVG(s.income), STDDEV(s.income)
INTO anxiety_summary
FROM 
	(
	SELECT t.patient_id, t.anxiety, t.income, MAX(t.p_date)
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

GO

--Run anxiety summary with varying income thresholds
EXEC summary_by_income @income_threshold = 45000.00
EXEC summary_by_income @income_threshold = 55000.00
EXEC summary_by_income @income_threshold = 65000.00
