/*
Purpose: To comptue the census by hour, day, month, year in ED
Author: Hans Aisake
Date Created: October 28, 2019
Date Modified:
Comments:
	The base idea came from Peter's query. 

	Taking about 7 minutes to run these days.

 */

 IF OBJECT_ID('tempdb.dbo.#tempED') is not null DROP TABLE #tempED;
 GO

 CREATE TABLE #tempED
(facilityshortname varchar(10), visitID varchar(50), start_dt datetime, disposition_dt DateTime)
;
GO

INSERT INTO #tempED
SELECT facilityshortname, VisitId, (StartDate+ StartTime) as 'start_dt', (DispositionDate+ DispositionTime) as 'disposition_dt' 
FROM EDMart.dbo.vwEDVisitIdentifiedRegional
WHERE FacilityShortName='RHS'
;
GO

CREATE CLUSTERED INDEX ix_tempEDAft ON #tempED (facilityshortname, visitId, start_dt, disposition_dt);
GO


 --find census in ED for all these different times
 IF OBJECT_ID('tempdb.dbo.#temp') is not null DROP TABLE #temp;
 GO

SELECT D.[short_dt], D.[Effective_Holiday_Flag], D.[DoW], D.[CalendarMonth], D.[CalendarYear], D.TOD, D.ShortDate
, ED.FacilityShortName
, COUNT(distinct ED.VisitID) as 'ED_Census'
INTO #temp
FROM #tempED as ED
INNER JOIN DSSI.[dbo].[AISAKE_Datetime] as D
ON D.[short_dt] BETWEEN start_dt AND disposition_dt
WHERE D.short_dt BETWEEN '2010-01-01 00:00:00' AND GETDATE()
GROUP BY D.[short_dt], D.[Effective_Holiday_Flag], D.[DoW], D.[CalendarMonth], D.[CalendarYear], D.TOD, D.ShortDate
, ED.FacilityShortName
;
GO

TRUNCATE TABLE DSSI.dbo.AISAKE_hourly_daily_EDCensus_2;
GO

INSERT INTO DSSI.dbo.AISAKE_hourly_daily_EDCensus_2
SELECT * FROM #temp






