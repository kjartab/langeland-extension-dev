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
--
--
----------------------------------------------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION LS_SegmentMeanTimestamp(linezm Geometry, sessiontime TIMESTAMP) RETURNS TIMESTAMP AS $$

DECLARE 

n INTEGER;
sum NUMERIC;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    sum := 0;
    
    FOR i in 1..n LOOP
		
    	sum := sum + ST_M(ST_PointN(linezm,i));
    	
    END LOOP;

    RETURN sessiontime + interval '1 seconds'*sum/n;
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


CREATE OR REPLACE FUNCTION LS_SegmentMeanTime(linezm Geometry) RETURNS numeric AS $$

DECLARE 

n INTEGER;
sum NUMERIC;
line GEOMETRY;

BEGIN

    SELECT INTO n ST_NPoints(linezm);
    sum := 0;
	SELECT INTO line ST_Force4D(linezm);
    FOR i in 1..n LOOP
		
    	sum := sum + ST_M(ST_PointN(line,i));
    	
    END LOOP;

    RETURN sum/n;
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

CREATE OR REPLACE FUNCTION LS_NforLargestM(linezm GEOMETRY) RETURNS INTEGER AS $$

DECLARE 
n INTEGER;
latest_n INTEGER;

max_m NUMERIC;
temp_m NUMERIC;

BEGIN
    n := ST_NPoints(linezm);

    max_m := ST_M(ST_PointN(linezm,1));
    latest_n := 1;
    
	RAISE NOTICE ' max: % ', max_m;
    FOR i in 2..n LOOP
	temp_m := ST_M(ST_PointN(linezm,i));
	RAISE NOTICE ' % ', temp_m;
	IF max_m < temp_m THEN
		max_m := temp_m;
		latest_n := i;
	END IF;
	RAISE NOTICE ' max: % ', max_m;
	
    END LOOP;
    RAISE NOTICE ' END LOOP';
    RETURN latest_n;
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


CREATE OR REPLACE FUNCTION LS_LargestM(linezm GEOMETRY) RETURNS INTEGER AS $$

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


CREATE OR REPLACE FUNCTION LS_MeanTime(oldsessionstart TIMESTAMP, linezm GEOMETRY, sessionstart TIMESTAMP) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
n INTEGER;
timediff

		
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


CREATE OR REPLACE FUNCTION LS_FillInTimeline(timeline TIMESTAMP, linezm GEOMETRY, nLastUpdate INTEGER, nNewUpdate INTEGER, inserttime TIMESTAMP) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
timediff NUMERIC;
oldtime NUMERIC;
newtime NUMERIC;
deltatime NUMERIC;
ntemp INTEGER;
reversed INTEGER;
returnline GEOMETRY;

BEGIN

	line := linezm;

	reversed := 0;

	SELECT INTO newtime EXTRACT(EPOCH FROM inserttime - timeline);

	SELECT INTO oldtime ST_M(ST_PointN(line,nLastUpdate));

	timediff := newtime - oldtime;

	IF nLastUpdate > nNewUpdate THEN
		ntemp := nLastUpdate;
		nLastUpdate := nNewUpdate;
		nNewUpdate := ntemp;
		reversed := 1;
		SELECT INTO oldtime ST_M(ST_PointN(line,nNewUpdate));
	END IF;

	deltatime := timediff/(nNewUpdate-nLastUpdate);
	
	FOR i in nLastUpdate..nNewUpdate LOOP
		
	oldpoint := ST_PointN(line,i);
		
		IF reversed = 1 THEN
			SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),cast(newtime - deltatime*(i-nLastUpdate) as double precision)),ST_SRID(linezm)));
		ELSE 
			SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),cast(oldtime + deltatime*(i-nLastUpdate) as double precision)),ST_SRID(linezm)));
		END IF;

		RAISE NOTICE 'did something %',ST_AsText(linezm);

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

CREATE OR REPLACE FUNCTION LS_BuildTimeline(linezm GEOMETRY, inserttime TIMESTAMP) RETURNS geometry AS $$

DECLARE 

line GEOMETRY;
oldpoint GEOMETRY;
n INTEGER;


BEGIN

    SELECT INTO line ST_Force4D(linezm);
	
	SELECT INTO n ST_NPoints(linezm);

	
    FOR i in 1..n LOOP
		
    	oldpoint := ST_PointN(line,i);
		
		SELECT INTO line ST_SetPoint(line,i-1,ST_SetSRID(ST_MakePoint(ST_X(oldpoint),ST_Y(oldpoint),ST_Z(oldpoint),0),ST_SRID(linezm)));
	
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