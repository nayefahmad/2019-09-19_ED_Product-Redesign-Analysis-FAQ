/*
Purpose:
Author: Hans Aisake
Date Created: October 30, 2019
Date modified: see github
Comments:
*/

-----------------
-- create date table with date and hour as datetimes indexed
-----------------
IF OBJECT_ID('tempdb.dbo.#tempDates') is not null DROP TABLE #tempDates;
GO

SELECT distinct CAST(CAST([AssignmentDate] as date) as datetime) as 'ShortDate'
INTO #tempDates 
FROM [CapPlan_RHS].[dbo].[Assignments]
;
GO

IF OBJECT_ID('tempdb.dbo.#hours') is not null DROP TABLE #hours;
GO

SELECT distinct DATEPART(hour, [AssignmentDate]) as 'Hour'
INTO #hours
FROM [CapPlan_RHS].[dbo].[Assignments]
;
GO

IF OBJECT_ID('tempdb.dbo.#tempDates_2') is not null DROP TABLE #tempDates_2;
GO

SELECT * INTO #tempDates_2 FROM #tempDates CROSS JOIN #hours
;
GO

IF OBJECT_ID('tempdb.dbo.#tempDates_3') is not null DROP TABLE #tempDates_3;
GO

SELECT X.*, DATEADD(hour, X.Hour, X.ShortDate) as 'short_dt'  
INTO #tempDates_3 
FROM #tempDates_2 as X
;
GO

CREATE CLUSTERED INDEX ix_tempDates ON #tempDates_3 (short_dt);
GO

-----------------
-- pull assignements data and add an index
-----------------
IF OBJECT_ID('tempdb.dbo.#tempAssignments') is not null DROP TABLE #tempAssignments;
GO

SELECT EncounterID, AssignmentDate, AssignmentEndDate, [lu_WardID]
INTO #tempAssignments
FROM CapPlan_RHS.dbo.Assignments
;
GO

CREATE CLUSTERED INDEX ix_tempAsn ON #tempAssignments (encounterId, assignmentdate, assignmentenddate);
GO

-----------------
-- compute census
-----------------


SELECT D.[short_dt],D.[Hour], D.ShortDate
, 'RHS' as 'FacilityShortName'
, A.lu_WardID as 'NursingUnitCode'
, COUNT(distinct A.EncounterID) as 'IP_Census'
FROM #tempAssignments as A
INNER JOIN #tempDates_3  as D
ON D.[short_dt] BETWEEN A.AssignmentDate AND A.AssignmentEndDate
WHERE D.short_dt BETWEEN '2017-01-01 00:00:00' AND '2019-12-31 23:59:59'
GROUP BY D.[short_dt],D.[Hour], D.ShortDate
, A.lu_WardID 
UNION
SELECT D.[short_dt],D.[Hour], D.ShortDate
, 'RHS' as 'FacilityShortName'
, 'All_Units' as 'NursingUnitCode'
, COUNT(distinct A.EncounterID) as 'IP_Census'
FROM #tempAssignments as A
INNER JOIN #tempDates_3  as D
ON D.[short_dt] BETWEEN A.AssignmentDate AND A.AssignmentEndDate
WHERE D.short_dt BETWEEN '2017-01-01 00:00:00' AND '2019-12-31 23:59:59'
GROUP BY D.[short_dt],D.[Hour], D.ShortDate
GO

-- '2017-01-01 00:00:00' AND '2019-12-31 23:59:59'


