--
-- Langeland extension version 0.1 - Development 
--
-- The Langeland extension is simply a set of functions that aids in treating linestrings as spatiotemporal objects. 
-- The fourth dimension, M, is used as a time interval from a TIMESTAMP.
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





CREATE OR REPLACE FUNCTION LS_SplitLinestring(linezm GEOMETRY, session_timestart TIMESTAMP) RETURNS SETOF subsegment_holder AS $$

DECLARE 
r subsegment_holder%ROWTYPE;
n INTEGER;
temp NUMERIC;

BEGIN

    SELECT INTO n ST_Npoints(linezm);

    FOR i in 1..n LOOP
	    r.sid=i;
	    temp = ST_M(ST_PointN(linezm,i));
	   
	    r.linezm = ST_LineSubstring(linezm, 1.0*(i-1)/4, 1.0*i/4);
	    r.mean_line_time = session_timestart;
	    return next r;
	
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


CREATE OR REPLACE FUNCTION LS_SplitUpTimeSegments(linezm GEOMETRY, session_timestart TIMESTAMP) RETURNS SETOF subsegment_holder AS $$

DECLARE 
r subsegment_holder%ROWTYPE;
n INTEGER;
temp NUMERIC;

tempstart INTEGER;
tempstop INTEGER;

splits INTEGER; 

totallength NUMERIC; 
updatedportion NUMERIC;
BEGIN

	SELECT INTO totallength ST_3DLength(linezm);
    SELECT INTO n ST_NPoints(linezm);
	SELECT INTO updatedportion LS_UpdatedPortion(linezm);
	SELECT INTO meantime LS_SegmentMeanM(linezm);
	r.sid = i;
	splits := 1;
	r.id = 1;
	
	IF updatedportion > 0.9 THEN
		r.mean_line_time = session_timestart + interval '1 seconds' * meantime;
		r.linezm = linezm;
		
	ELSE IF totallength <= 300 AND updatedportion > 0.7 THEN
		r.mean_line_time = session_timestart + interval '1 seconds' * meantime;
		r.linezm = linezm;
	ELSE IF totallength < 1000 AND totallength > 300 AND updatedportion > 0.8
		r.mean_line_time = session_timestart + interval '1 seconds' * meantime;
		r.linezm = linezm;
	ELSE 
	
	
	
	
		FOR i in 1..n LOOP
			
			
			r.sid=i;
			temp = ST_M(ST_PointN(linezm,i));
		   
			r.linezm = ST_LineSubstring(linezm, 1.0*(i-1)/4, 1.0*i/4);
			r.mean_line_time = session_timestart;
			return next r;
		
		END LOOP;
    END IF;
	
	
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

DROP FUNCTION LS_SplitInFourLinestring(linezm GEOMETRY);
CREATE OR REPLACE FUNCTION LS_SplitLinestring(linezm GEOMETRY) RETURNS SETOF geometry AS $$

DECLARE 
r testholder%ROWTYPE;


BEGIN

    FOR i in 1..4 LOOP
	    r.sid=i;
	    r.linezm = ST_LineSubstring(linezm, 1.0*(i-1)/4, 1.0*i/4);
	
	    return next r.linezm;
	
    END LOOP;
    
    RETURN;
	
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


CREATE OR REPLACE FUNCTION LS_SegmentMeanTime(linezm Geometry) RETURNS numeric AS $$

DECLARE 

n INTEGER;
timesum NUMERIC;
line GEOMETRY;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    timesum := 0;
	SELECT INTO line ST_Force4D(linezm);
    FOR i in 1..n LOOP
		
    	timesum := timesum + ST_M(ST_PointN(line,i));
    	
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



CREATE OR REPLACE FUNCTION LS_SplitUpTimeSegments(linezm GEOMETRY, session_timestart TIMESTAMP) RETURNS SETOF subsegment_holder AS $$

DECLARE 
r subsegment_holder%ROWTYPE;
n INTEGER;
i INTEGER;

temptime NUMERIC;
zerotime NUMERIC; 
sumseconds NUMERIC; 

tempstart INTEGER;
tempstop INTEGER;

splits INTEGER; 
errors INTEGER;

line GEOMETRY;
totallength NUMERIC; 
updatedportion NUMERIC;
meantime NUMERIC; 

BEGIN
	SELECT INTO line ST_Force4D(linezm);
	SELECT INTO totallength ST_3DLength(line);
	SELECT INTO n ST_NPoints(line);
	SELECT INTO updatedportion LS_UpdatedPortion(line);
	SELECT INTO meantime LS_SegmentMeanTime(line);
	
	splits := 1;
	r.id = 1;

	
	IF updatedportion > 0.9 THEN
		r.mean_line_time = session_timestart + interval '1 seconds' * meantime;
		r.linezm = line;
		return next r;
		RETURN;
		
	ELSIF updatedportion > 0.7 AND totallength <= 300 THEN
		r.mean_line_time = session_timestart + interval '1 seconds' * meantime;
		r.linezm = line;
		return next r;
		RETURN;
		
	ELSIF updatedportion > 0.8 AND totallength < 1000 AND totallength > 300 THEN 
		r.mean_line_time = session_timestart + interval '1 seconds' * meantime;
		r.linezm = line;
		return next r;
		RETURN;
	
	ELSE 
				
		tempstart := 1;
		tempstop := 1;
		temptime := ST_M(ST_PointN(line,1));
		zerotime := 0;
		sumseconds := 0;

		i := 1;

		-- Find the new zero time to solve for --

		-- LOOP through points until ok --
		
		WHILE i <= n LOOP

			temptime = ST_M(ST_PointN(line,i));



			IF temptime>zerotime-(6*3600) AND temptime<zerotime+(6*3600) THEN

				sumseconds = sumseconds + temptime;
				tempstop = i;
				RAISE NOTICE 'inside: i: %: %', i, temptime;

			ELSE 

				RAISE NOTICE 'outside: i: %: %', i, temptime;

				IF errors <= 1 THEN
					
					RAISE NOTICE 'increasing errors';
					errors = errors + 1;
					tempstop = i;
					sumseconds = sumseconds + sumseconds/(tempstop-tempstart);
				
					
				ELSE 
			
					RAISE NOTICE 'should pop out linestring: i: %: %', i, temptime;
					r.linezm = ST_LineSubstring(line, 1.0*(tempstart-1)/n, 1.0*(tempstop)/n);
					r.sid=i;
					r.mean_line_time = session_timestart + interval '1 seconds' * 1.0*sumseconds/(tempstop-tempstart);

					-- Set new stop and start, sumseconds etc.
					sumseconds = 0;
					errors = 0;
					tempstart = i;
					tempstop = i;
					zerotime = ST_M(ST_PointN(line,tempstart));
					
					return next r;
				END IF;

			END IF; 

			IF tempstop = n THEN
					RAISE NOTICE 'should pop out linestring: i: %: % with zerotime: %', i, temptime, zerotime;
				r.linezm = ST_LineSubstring(line, 1.0*(tempstart-1)/n, 1.0*(tempstop)/n);
				r.sid=i;
				r.mean_line_time = session_timestart + interval '1 seconds' * 1.0*sumseconds/(tempstop-tempstart);
					return next r;

			END IF;

				i = i + 1;
		
		END LOOP;

	
	END IF;
	
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



	RAISE NOTICE 'insertedtime = %', insertedTime;

	RAISE NOTICE 'last update = %', mostRecentUpdate;

	RAISE NOTICE 'timediff = %', insertedTime - mostRecentUpdate;
	
	IF insertedTime - mostRecentUpdate < 21600 THEN 

		line := LS_UpdateRecent(line, insertedTime, insertedPoint, bufferSize);
		
		UPDATE temporalsegment SET segment = line where id = segment_id;
		
		RAISE NOTICE 'updated the temporalsegment table for id %', segment_id;
				
	ELSE 
		
		WITH oldsegment AS (SELECT * FROM temporalsegment WHERE id = segment_id)
		 
		INSERT INTO temporalsegment_history(segment_id, created, stored, segment) SELECT id, created, now(), segment FROM oldsegment;
		
		line := LS_UpdateTimelineAtPoint(line, insertedPoint, bufferSize, insertedTime);
	
		UPDATE temporalsegment SET created = now(), segment = line;
		
		RAISE NOTICE 'created new temporalsegment_history insert and started a new temporalsegment update for id %', segment_id;
		
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
		


		RAISE NOTICE 'second newest = %', ST_M(ST_PointN(line,nLastUpdate));
		RAISE NOTICE 'newest = %', ST_M(ST_PointN(line,nInserted));
		
		

	
	
		IF insertedTime - mostRecentUpdate < 300 AND nInserted != nLastUpdate THEN -- If the timeline was updated  less than 5 minutes ago, it will be updated as if the points between were all visited
		
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
	
	
	INSERT INTO segment_history -- id, storedtime, createdtime
	
	
	
	UPDATE segment_table SET created = now();
	
	line := LS_UpdateTimelineAtPoint(linezm, insertedPoint, bufferSize, insertedTime); -- No matter what, the line will be updated

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

CREATE OR REPLACE FUNCTION LS_FillInLine(linezm GEOMETRY, n1 INTEGER, n2 INTEGER, timevalue numeric) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
n INTEGER;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    line := linezm;

    FOR i in n1..n2 LOOP

    	oldpoint := ST_PointN(line,i);
    	
	SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),timevalue),ST_SRID(linezm)));

    END LOOP;
    
    RETURN line;
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
		RAISE NOTICE 'UPDATED the point';

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

BEGIN

    SELECT INTO line ST_Force4D(linezm);
	INSERT INTO timeMillis EXTRACT(EPOCH FROM insertedTime);
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

CREATE OR REPLACE FUNCTION LS_UpdatedPortion(linezm GEOMETRY) RETURNS numeric AS $$

DECLARE 

n INTEGER;
pluses NUMERIC;
portion NUMERIC;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    pluses := 0; 
    
    FOR i in 1..n LOOP
		
	IF ST_M(ST_PointN(linezm,i) > -1 THEN
		pluses := pluses + 1;
	END IF;
    	
    END LOOP;	
    
    RETURN pluses/n;
END;
$$ LANGUAGE plpgsql;
