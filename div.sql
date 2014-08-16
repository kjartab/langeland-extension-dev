
DROP TYPE SEGMENTHOLDER;
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
	timeSum := lastTimestamp;
	threshold := 3600*600;
	
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
			
			r.segment_linezm = ST_LineSubString(linezm, startFrac, stopFrac);
			return next r;
			RETURN;

		ELSE 

			IF abs(lastTimestamp - tempTime) < threshold THEN -- If the timedifference is not bigger than threshold
				counter = counter + 1;
				timeSum = timeSum + tempTime;				
				nStop = i;													
				i = i + 1;			
				
			ELSE 	-- The time difference was too big. Breaking off and returning the segment...
				avgTime = timeSum / counter;
				nStop = i;
					
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

---------



CREATE OR REPLACE FUNCTION LS_GetTemporalSegments(segment_id INTEGER, linezm GEOMETRY) RETURNS numeric AS $$

	DECLARE 
	
	


	BEGIN
	
	SELECT INTO uniformity LS_Uniformity(linezm);
		
	IF uniformity < 0.9  THEN
		r.sid = segment_id;
		r.segmenttime = to_timestamp(meantime);
		r.segment_linezm = linezm;
		RETURN ;
		
	ELSE 
	
	RETURN r
	
	RETURN 1;
	END;
$$ LANGUAGE plpgsql;


----------


CREATE OR REPLACE FUNCTION LS_Uniformity(linezm GEOMETRY) RETURNS numeric AS $$

DECLARE 

n INTEGER;
within NUMERIC;
portion NUMERIC;
meanTime NUMERIC;


BEGIN

    SELECT INTO n ST_NPoints(linezm);
    within := 0; 
	SELECT INTO meanTime LS_SegmentMeanTime(linezm);
	
    FOR i in 1..n LOOP
		
	IF abs(ST_M(ST_PointN(linezm,i)) - meanTime) < 6*3600 THEN
		within = within + 1;
	END IF;
    	
    END LOOP;	
    
    RETURN 1.0*within/n;
END;
$$ LANGUAGE plpgsql;