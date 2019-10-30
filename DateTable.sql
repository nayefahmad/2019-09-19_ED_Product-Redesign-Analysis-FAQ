/*
Purpose: To comptue the census by hour, day, month, year in ED
Author: Hans Aisake
Date Created: October 28, 2019
Date Modified:
Comments:
	The base idea came from Peter's query. 

	Taking about 7 minutes to run these days.

 */

 --find dates of interest and relevant date attributes
IF OBJECT_ID('tempdb.dbo.#dates') is not null DROP TABLE #dates;
GO

SELECT distinct [shortdate], CalendarMonth, CalendarYear, DATEPART(dw, shortdate) as 'DoW'
, CASE WHEN StatHoliday is not null OR IsStatHolidayMoved is not null THEN 1 ELSE 0 END as 'Effective_Holiday_Flag'
, StatHoliday as 'HolidayName'
INTO #dates
FROM EDMart.dim.[Date]
WHERE not (MONTH(shortdate)=2 AND DAY(shortdate) >=29)	--ignore the leapyears
AND ShortDate <='2019-12-31'
;
GO

--identify a series of 0-> 23 for the 24 hour starts with 0=12:00AM
IF OBJECT_ID('tempdb.dbo.#hours') is not null DROP TABLE #hours;
GO

SELECT distinct timeID as 'ToD'
INTO #hours
FROM EDMart.dim.[time]
WHERE TimeID BETWEEN 0 AND 23
;
GO

--create a table of dates and times
TRUNCATE TABLE DSSI.dbo.AISAKE_Datetime;
GO

INSERT INTO DSSI.dbo.AISAKE_Datetime ([shortdate], CalendarMonth, CalendarYear, DOW, Effective_Holiday_Flag, HolidayName, TOD, short_dt)
SELECT X.*, Y.*,  DATEADD(hour, y.ToD, CONVERT(datetime, X.Shortdate) ) as 'Date_withHour'  FROM #dates as X CROSS JOIN #hours as Y
GO
