
CREATE TRIGGER positions_traccar_trigger
  AFTER INSERT
  ON public.positions
  FOR EACH ROW
  EXECUTE PROCEDURE process_position_insert_traccar();

  

CREATE OR REPLACE FUNCTION public.process_position_insert_traccar()
  RETURNS trigger AS
$BODY$

DECLARE 
rec RECORD;
timevalue NUMERIC;

    
    BEGIN  
		
		IF (TG_OP = 'INSERT') THEN
			
			FOR rec IN SELECT * from temporalsegment WHERE ST_DWithin(ST_Transform(ST_SetSRID(ST_MakePoint(NEW.longitude, NEW.latitude),4326),32632), segment, 20000) LOOP
				SELECT INTO timevalue EXTRACT(EPOCH FROM NEW.time)-3600;
				PERFORM LS_UpdateTimeline(rec.id, timevalue, ST_Transform(ST_Transform(ST_SetSRID(ST_MakePoint(NEW.longitude, NEW.latitude),4326),32632),32632), 20);

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
	