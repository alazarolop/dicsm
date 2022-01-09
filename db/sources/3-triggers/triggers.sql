-- # FUNCTIONS FOR TRIGGERS

CREATE OR REPLACE FUNCTION sources.src_tb (src varchar, bd integer) 
  RETURNS varchar AS $$
SELECT concat(src, '_' || NULLIF(bd, 0)) ;
$$
LANGUAGE SQL
IMMUTABLE ;
COMMENT ON FUNCTION sources.src_tb (src varchar, bd integer) IS 'Naming of source raster tables' ;





-- # TRIGGER FUNCTIONS

CREATE OR REPLACE FUNCTION sources.src_ins ()
  RETURNS trigger AS $$
DECLARE
tb varchar ;
--
BEGIN
-- Band identifier _0_ means there is just one band on the source, so no numbering is needed.
tb := src_tb(NEW.src_id, NEW.band_id) ;

-- Structure
EXECUTE format(
  'CREATE TABLE IF NOT EXISTS sources.%I () 
  INHERITS (sources._src_rast)', tb ) ;

EXECUTE format(
  'ALTER TABLE IF EXISTS sources.%I ALTER COLUMN rid ADD GENERATED ALWAYS AS IDENTITY', tb ) ;

EXECUTE format(
  'ALTER TABLE IF EXISTS sources.%I ADD PRIMARY KEY (rid) ', tb ) ;

-- Comment
EXECUTE format(
      'COMMENT ON TABLE %I IS %L', src_tb(NEW.src_id, NEW.band_id), concat(NEW.descr, ' (', NULLIF(NEW.wave, 0) || ' ', NEW.unit, ')')
) ;

RETURN NEW ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION sources.src_ins () IS 'Automatic management of source rasters on INSERT' ;





CREATE OR REPLACE FUNCTION sources.src_del ()
 RETURNS trigger AS $$
DECLARE
tb varchar ;
--
BEGIN
-- Band identifier _0_ means there is just one band on the source, so no numbering is needed.
tb := src_tb(OLD.src_id, OLD.band_id) ;

EXECUTE format(
  'DROP TABLE IF EXISTS sources.%I', tb ) ;

RETURN OLD ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION sources.src_del () IS 'Automatic management of source rasters on DELETE' ;




CREATE OR REPLACE FUNCTION sources.src_upd ()
  RETURNS trigger AS $$
BEGIN

CASE 
  WHEN OLD.src_id IS DISTINCT FROM NEW.src_id OR OLD.band_id IS DISTINCT FROM NEW.band_id THEN
    EXECUTE format(
      'ALTER TABLE IF EXISTS sources.%I RENAME TO %I', src_tb(OLD.src_id, OLD.band_id), src_tb(NEW.src_id, NEW.band_id) ) ;
  ELSE 
    EXECUTE format(
      'COMMENT ON TABLE %I IS %L', src_tb(OLD.src_id, OLD.band_id), concat(NEW.descr, ' (', NULLIF(NEW.wave, 0) || ' ', NEW.unit, ')')
    ) ;
END CASE ;

RETURN NEW ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION sources.src_upd () IS 'Automatic management of source rasters on UPDATE' ;





CREATE OR REPLACE FUNCTION sources.src_single_band ()
  RETURNS trigger AS $$
DECLARE
i integer ;
--
BEGIN

EXECUTE format(
  'SELECT * 
  FROM sources.%I 
  WHERE src_id ILIKE %L AND band_id = 0', TG_TABLE_NAME, NEW.src_id) ;
GET DIAGNOSTICS i := ROW_COUNT ;
IF i >= 1 THEN
  RAISE EXCEPTION '% defined as single band source.', NEW.src_id 
    USING HINT = 'Change band id. from 0 to include new bands.'  ; 
ELSE 
  RETURN NEW ;
END IF ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION sources.src_single_band () IS 'Control of sources defined as single band' ;