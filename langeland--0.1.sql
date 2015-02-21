--
-- Langeland extension version 0.2 - Development 
--
-- The Langeland extension is simply a set of functions that aids in treating linestrings as spatiotemporal objects. 
-- The fourth dimension, M, stores the timestamp as a unix time (seconds)
--
--
--
--
--
--
--
--
--
--
--
--




CREATE OR REPLACE FUNCTION LS_FillInLine(linezm GEOMETRY, n1 INTEGER, n2 INTEGER, nvalue numeric) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
n INTEGER;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    line := linezm;

    FOR i in n1..n2 LOOP

    	oldpoint := ST_PointN(line,i);
    	
	SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),nvalue),ST_SRID(linezm)));

    END LOOP;
    
    RETURN line;
END;
$$ LANGUAGE plpgsql;



----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------



CREATE OR REPLACE FUNCTION LS_BuildTimeline(linezm GEOMETRY, insertedTime TIMESTAMP WITH TIME ZONE) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
tempPoint GEOMETRY;
n INTEGER;
timeMillis numeric;	

BEGIN

    SELECT INTO line ST_Force4D(linezm);
	SELECT INTO timeMillis EXTRACT(EPOCH FROM insertedTime);
    
    RAISE NOTICE ' % = % ', timeMillis, insertedTime;
	SELECT INTO n ST_NPoints(linezm);
	
    FOR i in 1..n LOOP
		
    	tempPoint := ST_PointN(line,i);
    	
		SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(tempPoint),ST_Y(tempPoint),ST_Z(tempPoint),timeMillis),ST_SRID(linezm)));
	
    END LOOP;
    
    RETURN line;
	
END;

$$ LANGUAGE plpgsql;
----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION LS_BuildHistoryTimeline(oldsessionstart TIMESTAMP, linezm GEOMETRY, sessionstart TIMESTAMP) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
n INTEGER;
timediff NUMERIC;

BEGIN

    line := linezm;
	SELECT INTO timediff EXTRACT(EPOCH FROM oldsessionstart - sessionstart);
	
	SELECT INTO n ST_NPoints(linezm);

    FOR i in 1..n LOOP
		
    	oldpoint := ST_PointN(line,i);
		
		SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),ST_M(oldpoint)+timediff),ST_SRID(linezm)));
	
    END LOOP;
    
    RETURN line;
	
END;

$$ LANGUAGE plpgsql;


----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- LS_UpdatedPortion


CREATE OR REPLACE FUNCTION LS_UpdatedPortion(linezm GEOMETRY, referenceTime TIMESTAMP WITH TIME ZONE) RETURNS numeric AS $$

DECLARE 

n INTEGER;
pluses NUMERIC;
portion NUMERIC;
referenceTimestamp  NUMERIC;

BEGIN
	
    SELECT INTO n ST_NPoints(linezm);
    pluses := 0; 
    SELECT INTO referenceTimestamp EXTRACT(EPOCH FROM referenceTime);
	
	
    FOR i in 1..n LOOP
		
	IF abs(ST_M(ST_PointN(linezm,i)) - referenceTime) > 3600 THEN
		pluses := pluses + 1;
	END IF;
    	
    END LOOP;	
    
    RETURN 1.0*pluses/n;
END;
$$ LANGUAGE plpgsql;
----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------







----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
-- 
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- LS_SplitTimeline

DROP TYPE SEGMENTHOLDER CASCADE;
CREATE TYPE SEGMENTHOLDER AS (id INTEGER, sid INTEGER, segmenttime TIMESTAMP, segment_linezm GEOMETRY);

CREATE OR REPLACE FUNCTION LS_SplitTimeline(segment_id INTEGER, linezm GEOMETRY) RETURNS SETOF SEGMENTHOLDER AS $$

DECLARE 
r SEGMENTHOLDER%ROWTYPE;
n INTEGER;
counter INTEGER;
nStart INTEGER;
nStop INTEGER;
timeSum NUMERIC;
threshold NUMERIC;
lastTimestamp NUMERIC;
avgTime NUMERIC;
i INTEGER;
startFrac NUMERIC;
stopFrac NUMERIC;
tempTime NUMERIC;
BEGIN
	
	SELECT INTO n ST_Npoints(linezm);
	
	i := 2;
	counter := 1;
	nStop := 1;
	lastTimestamp := ST_M(ST_PointN(linezm,1));
	nStart := 1;
    nStop :=2;
	timeSum := lastTimestamp;
	threshold := 180;
	
	WHILE i <= n LOOP -- While the linezm has not been read to end - do the following
		tempTime = ST_M(ST_PointN(linezm,i));
		 
		IF i = n THEN
			
			counter = counter + 1;
			timeSum = timeSum + tempTime;				
			nStop = i;	
			avgTime = timeSum / counter;	
			r.id = i;
			r.sid = segment_id;
			r.segmenttime = to_timestamp(avgTime);

			startFrac = ST_Line_Locate_Point(linezm,ST_PointN(linezm,nStart));
			stopFrac = ST_Line_Locate_Point(linezm,ST_PointN(linezm,nStop));
			
			r.segment_linezm = ST_LineSubString(linezm, startFrac,1);
			return next r;
			RETURN;

		ELSE 

			IF abs(lastTimestamp - tempTime) < threshold THEN -- If the timedifference is not bigger than threshold
              --  RAISE NOTICE 'lasttimestamp - temptime > threshold: % - % = % < %', lastTimestamp, tempTime, abs(lastTimestamp - tempTime), threshold;
				counter = counter + 1;
				timeSum = timeSum + tempTime;		
                lastTimestamp = tempTime;                
				nStop = i;													
				i = i + 1;
				
				
			ELSE 	-- The time difference was too big. Breaking off and returning the segment...
				--RAISE NOTICE 'lasttimestamp - temptime < threshold: % - % = % < %', lastTimestamp, tempTime, abs(lastTimestamp - tempTime), threshold;
				avgTime = timeSum / counter;
                
				r.id = i;
				r.sid = segment_id;
				r.segmenttime = to_timestamp(avgTime);
				
				startFrac = ST_Line_Locate_Point(linezm,ST_PointN(linezm,nStart));
				stopFrac = ST_Line_Locate_Point(linezm,ST_PointN(linezm,nStop));
				
				r.segment_linezm = ST_LineSubString(linezm, startFrac, stopFrac);
			
				return next r;
				
				-- Initializing next round
				nStart = i;
				lastTimestamp = tempTime;
				timeSum = lastTimestamp;
				counter = 1;
			
			END IF;
		
		
		END IF;
			
	END LOOP;
	
    RETURN;
	
END;

$$ LANGUAGE plpgsql;



----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
-- 
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
-- FUNCTION OK 
--
----------------------------------------------------------------------------------------------------------------------------------------------------------


CREATE OR REPLACE FUNCTION LS_SegmentMeanTime(linezm Geometry) RETURNS numeric AS $$

DECLARE 

n INTEGER;
timesum NUMERIC;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    timesum := 0;
    FOR i in 1..n LOOP
		
    	timesum := timesum + ST_M(ST_Force4D(ST_PointN(line,i)));
    	
    END LOOP;

    RETURN timesum/n;
END;
$$ LANGUAGE plpgsql;



----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
-- FUNCTION OK 
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION LS_NForMostRecent(linezm GEOMETRY) RETURNS INTEGER AS $$

DECLARE 
n INTEGER;
latest_n INTEGER;

max_m NUMERIC;
temp_m NUMERIC;

BEGIN
    n := ST_NPoints(linezm);

    max_m := ST_M(ST_PointN(linezm,1));
    latest_n := 1;
    
    FOR i in 2..n LOOP
	temp_m := ST_M(ST_PointN(linezm,i));
	
	IF max_m < temp_m THEN
		max_m := temp_m;
		latest_n := i;
	END IF;
	
    END LOOP;
	
    RETURN latest_n;
END;
$$ LANGUAGE plpgsql;



----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
-- FUNCTION OK 
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION LS_LargestM(linezm GEOMETRY) RETURNS NUMERIC AS $$

DECLARE 

max_m NUMERIC;
temp_m NUMERIC;

BEGIN

    max_m := ST_M(ST_PointN(linezm,1));
    
    FOR i in 1..ST_NPoints(linezm) LOOP
	temp_m := ST_M(ST_PointN(linezm,i));
	
		IF max_m < temp_m THEN
			max_m := temp_m;
		END IF;
		
    END LOOP;
    
    RETURN max_m;
END;
$$ LANGUAGE plpgsql;




----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--

----------------------------------------------------------------------------------------------------------------------------------------------------------

	-- CODE FOR UPDATING

----------------------------------------------------------------------------------------------------------------------------------------------------------
/* 

Warning: This will not work without having initialized the database tables temporalsegment and temporalsegment_history

@segment_id - the segment ID
@insertedTime - the time of the inserted position/point
@insertedPoint - the inserted position/point
@buffersize - the size of the radius for which timelines should be updated 

The function takes in the insertedPoint, insertedTime and buffersize to update a timeline
If the timeline was updated recently, the points between last update and the new one, will be updated as well. 
Uses LS_UpdateTimelineAtPoint(...)

*/
----------------------------------------------------------------------------------------------------------------------------------------------------------



CREATE OR REPLACE FUNCTION LS_UpdateTimeline(segment_id integer, insertedTime numeric, insertedPoint GEOMETRY, bufferSize NUMERIC) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldPoint GEOMETRY;
mostRecentUpdate NUMERIC;
reversed INTEGER;
nLastUpdate INTEGER;
nInserted INTEGER;
deltaTime NUMERIC;

BEGIN

	SELECT INTO line segment from temporalsegment where id = segment_id;
	
	nLastUpdate := LS_NForMostRecent(line);
	
	SELECT INTO mostRecentUpdate ST_M(ST_PointN(line,nLastUpdate));

	
	IF insertedTime - mostRecentUpdate < 1200 THEN 
		line := LS_UpdateRecent(line, insertedTime, insertedPoint, bufferSize);
        
		UPDATE temporalsegment SET segment = line where id = segment_id;
		
	ELSE 
		
		WITH oldsegment AS (SELECT * FROM temporalsegment WHERE id = segment_id)
		 
		INSERT INTO temporalsegment_history(segment_id, created, stored, segment) SELECT id, created, now(), segment FROM oldsegment;
		
		line := LS_UpdateTimelineAtPoint(line, insertedPoint, bufferSize, insertedTime);
	
		UPDATE temporalsegment SET created = now(), segment = line WHERE id=segment_id;
		
	END IF;
	
	RETURN line;

END;

$$ LANGUAGE plpgsql;

----------------------------------------------------------------------------------------------------------------------------------------------------------
/* 

@linezm - the timeline 4D geometry
@insertedTime - the time of the inserted position/point
@insertedPoint - the inserted position/point
@buffersize - the size of the radius for which timelines should be updated 

The function takes in the insertedPoint, insertedTime and buffersize to update a timeline
If the timeline was updated recently, the points between last update and the new one, will be updated as well. 
Uses LS_UpdateTimelineAtPoint(...)

*/
----------------------------------------------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION LS_UpdateRecent(linezm GEOMETRY, insertedTime numeric, insertedPoint GEOMETRY, bufferSize NUMERIC) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldPoint GEOMETRY;
mostRecentUpdate NUMERIC;
reversed INTEGER;
nLastUpdate INTEGER;
nInserted INTEGER;
deltaTime NUMERIC;


BEGIN

		nLastUpdate := LS_NForMostRecent(linezm); -- n of the last updated point
		SELECT INTO mostRecentUpdate ST_M(ST_PointN(linezm,nLastUpdate)); -- time value of the last updated point
		line := LS_UpdateTimelineAtPoint(linezm, insertedPoint, bufferSize, insertedTime); -- No matter what, the line will be updated at inserted point
		
		nInserted := LS_NForMostRecent(line); -- n of the inserted point
		reversed := 0;
	
		IF insertedTime - mostRecentUpdate < 240 AND nInserted != nLastUpdate THEN -- If the timeline was updated  less than 5 minutes ago, it will be updated as if the points between were all visited
			RAISE NOTICE 'within';
			IF nInserted < nLastUpdate THEN
				reversed := 1;
			END IF;
			
			deltaTime := (insertedTime - mostRecentUpdate) / (nInserted-nLastUpdate);
			
			IF reversed THEN
					
				FOR i in nLastUpdate..nInserted LOOP
					oldPoint := ST_PointN(line,i);
					
					SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldPoint),ST_Y(oldPoint),ST_Z(oldPoint),cast(insertedTime - deltaTime*(nInserted - i) as double precision)),ST_SRID(line)));
				END LOOP;
			
			ELSE 
			
				FOR i in nLastUpdate..nInserted LOOP
					oldPoint := ST_PointN(line,i);
					SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldPoint),ST_Y(oldPoint),ST_Z(oldPoint),cast(insertedTime + deltaTime*(i - nLastUpdate) as double precision)),ST_SRID(line)));
				END LOOP;
			
			END IF;
			
		END IF;
		
	RETURN line;
END;

$$ LANGUAGE plpgsql;




----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------




CREATE OR REPLACE FUNCTION LS_UpdateOld(segment_id integer, linezm GEOMETRY, insertedTime numeric, insertedPoint GEOMETRY, bufferSize NUMERIC) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldPoint GEOMETRY;
most_recent_update NUMERIC;
reversed INTEGER;
nLastUpdate INTEGER;
nInserted INTEGER;
deltaTime NUMERIC;


BEGIN
	
	
	--INSERT INTO segment_history -- id, storedtime, createdtime
	
	UPDATE segment_table SET created = now();
	
	line := LS_UpdateTimelineAtPoint(linezm, insertedPoint, bufferSize, insertedTime); -- No matter what, the line will be updated

END;

$$ LANGUAGE plpgsql;



----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
-- Updates the temporal part of a line at a given point with a given timestamp and
-- 
--
----------------------------------------------------------------------------------------------------------------------------------------------------------


CREATE OR REPLACE FUNCTION LS_UpdateTimelineAtPoint(linezm GEOMETRY, point GEOMETRY, buffer numeric, timevalue numeric) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
n INTEGER;

BEGIN


    line := linezm;
    FOR i in 1..ST_NPoints(linezm) LOOP

	oldpoint := ST_PointN(linezm,i);
	
	IF ST_Distance(oldpoint, point) < buffer THEN	
		SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),timevalue),ST_SRID(linezm)));
	END IF;

    END LOOP;
    
    RETURN line;
END;

$$ LANGUAGE plpgsql;




----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

-- TRIGGER FOR UPDATING WHEN RAW DATA TABLE IS ALTERED +++++++

----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

-- Function: public.process_position_insert()

-- DROP FUNCTION public.process_position_insert();

CREATE OR REPLACE FUNCTION public.process_position_insert()
  RETURNS trigger AS
$BODY$

DECLARE 
rec RECORD;
timevalue NUMERIC;

    
    BEGIN  
		
		IF (TG_OP = 'INSERT') THEN
			
			FOR rec IN SELECT * from temporalsegment WHERE ST_DWithin(ST_Transform(NEW.position,32632), segment, 70) LOOP
				SELECT INTO timevalue EXTRACT(EPOCH FROM NEW.insertedtime)-3600;
				PERFORM LS_UpdateTimeline(rec.id, timevalue, ST_Transform(NEW.position,32632), 20);

			END LOOP;

		
				
			RETURN NEW;
		END IF;
		
		RETURN NULL;
    END;
    
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION public.process_position_insert()
  OWNER TO postgres;

----------------------------------------------------------------------------------------------------------------------------------------------------------
--
--
--
--
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------



DROP TRIGGER temporalsegment_trigger on temporalsegment;
CREATE OR REPLACE FUNCTION process_segment_insert() RETURNS TRIGGER AS $temporalsegment_trigger$

DECLARE 
rec RECORD;
timevalue NUMERIC;
sid INTEGER;

    BEGIN  
		
		INSERT INTO prebuild(change, explanation) values (now(),TG_OP);
		IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
			
			sid = NEW.id;
			
			DELETE FROM prebuild_temporalsegment where segment_id = sid;
			
			INSERT INTO prebuild_temporalsegment(segment_id, segmentnumber, segment, segmenttime) SELECT (LS_SplitTimeline(NEW.id, NEW.segment)::SEGMENTHOLDER).sid, (LS_SplitTimeline(NEW.id, NEW.segment)::SEGMENTHOLDER).id, (LS_SplitTimeline(NEW.id, NEW.segment)::SEGMENTHOLDER).segment_linezm,  to_timestamp(LS_LargestM((LS_SplitTimeline(NEW.id, NEW.segment)::SEGMENTHOLDER).segment_linezm));
				
			RETURN NEW;
		END IF;
		
		RETURN NULL;
    END;
    
$temporalsegment_trigger$ LANGUAGE plpgsql;

CREATE TRIGGER temporalsegment_trigger
AFTER UPDATE OR INSERT OR DELETE ON temporalsegment
    FOR EACH ROW EXECUTE PROCEDURE process_segment_insert();
	