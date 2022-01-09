-- # FUNCTIONS FOR TRIGGERS



-- # TRIGGER FUNCTIONS

CREATE OR REPLACE FUNCTION covars.covar_ins ()
  RETURNS trigger AS $$
BEGIN

-- Structure
EXECUTE format(
  'CREATE TABLE IF NOT EXISTS covars.%I ()
  INHERITS (covars._covar_rast)', NEW.covar_id ) ;

EXECUTE format(
  'ALTER TABLE IF EXISTS covars.%I ALTER COLUMN rid ADD GENERATED ALWAYS AS IDENTITY', NEW.covar_id ) ;

EXECUTE format(
  'ALTER TABLE IF EXISTS covars.%I ADD PRIMARY KEY (rid) ', NEW.covar_id ) ;

-- Comment
EXECUTE format(
      'COMMENT ON TABLE %I IS %L', NEW.covar_id , NEW.descr) ;

RETURN NEW ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION covars.covar_ins () IS 'Automatic management of source rasters on INSERT' ;





CREATE OR REPLACE FUNCTION covars.covar_del ()
 RETURNS trigger AS $$
BEGIN

EXECUTE format(
  'DROP TABLE IF EXISTS covars.%I', OLD.covar_id ) ;

RETURN OLD ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION covars.covar_del () IS 'Automatic management of source rasters on DELETE' ;




CREATE OR REPLACE FUNCTION covars.covar_upd ()
  RETURNS trigger AS $$
BEGIN

CASE 
  WHEN OLD.covar_id IS DISTINCT FROM NEW.covar_id THEN
    EXECUTE format(
      'ALTER TABLE IF EXISTS covars.%I RENAME TO %I', OLD.covar_id, NEW.covar_id ) ;
  ELSE 
    EXECUTE format(
      'COMMENT ON TABLE %I IS %L', OLD.covar_id, NEW.descr) ;
END CASE ;

RETURN NEW ;
--
END ;
$$
LANGUAGE plpgsql ;
COMMENT ON FUNCTION covars.covar_upd () IS 'Automatic management of source rasters on UPDATE' ;

