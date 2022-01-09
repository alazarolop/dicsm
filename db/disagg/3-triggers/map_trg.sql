--MAP UNITS
  -- *TRIGGER* 
CREATE OR REPLACE FUNCTION disagg.stu_controlpc()
RETURNS trigger AS $$
DECLARE
i			RECORD;
BEGIN
--
SELECT dsmu_id AS dsmu, sum(stupc) AS pc INTO i
FROM dsmu_stu
WHERE dsmu_id LIKE NEW.dsmu_id
GROUP BY dsmu	;
--
IF NEW.stupc IS NULL
THEN RETURN NEW	;
ELSE
  IF (NEW.stupc + COALESCE(i.pc, 0)) <= 100
    THEN RETURN NEW	;
    ELSE RAISE EXCEPTION 'STU percentage (% %%) is greater than the maximum allowed for SMU % (% %%).', NEW.stupc, NEW.dsmu_id, (100-i.pc) ;
  END IF;
END IF;
END;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION disagg.stu_controlpc() IS 'Ocupation control for STU within SMU'	;
