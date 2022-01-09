
CREATE TABLE covars._covar_rast (
	rid		integer NOT NULL,
	rast	raster
) ;
COMMENT ON TABLE covars._covar_rast IS 'Pattern for covariates raster tables.' ;
COMMENT ON COLUMN _covar_rast.rid IS 'Tile identifier' ;
COMMENT ON COLUMN _covar_rast.rast IS 'Raster' ;



CREATE TABLE covars.covar (
	covar_id 	varchar(15)	PRIMARY KEY,
	descr		varchar(100)	NOT NULL
) ;
COMMENT ON TABLE covars.covar IS 'Covariates.' ;
COMMENT ON COLUMN covar.covar_id IS 'Covariate identifier' ;
COMMENT ON COLUMN covar.descr IS 'Covariate description' ;
--
CREATE TRIGGER ins 
  AFTER INSERT ON covars.covar
  FOR EACH ROW
  EXECUTE FUNCTION covars.covar_ins() ;
COMMENT ON TRIGGER ins ON covars.covar IS 'Trigger for creating raster tables for new covariates.' ;
--
CREATE TRIGGER del 
  AFTER DELETE ON covars.covar
  FOR EACH ROW
  EXECUTE FUNCTION covars.covar_del() ;
COMMENT ON TRIGGER del ON covars.covar IS 'Trigger for deleting raster tables for covariates.' ;
--
CREATE TRIGGER upd 
  AFTER UPDATE ON covars.covar
  FOR EACH ROW
  WHEN ( OLD.* IS DISTINCT FROM NEW.*)
  EXECUTE FUNCTION covars.covar_upd() ;
COMMENT ON TRIGGER upd ON covars.covar IS 'Trigger for updating raster tables for covariates.' ;



CREATE TABLE covars.covar_select (
	select_id	integer, 
	covar_id 	varchar(15),
	PRIMARY KEY (select_id, covar_id),
	FOREIGN KEY (covar_id) REFERENCES covars.covar(covar_id)
) ;
COMMENT ON TABLE covars.covar_select IS 'Covariates used by every selection.' ;
COMMENT ON COLUMN covar_select.select_id IS 'Combination identifier' ;
COMMENT ON COLUMN covar_select.covar_id IS 'Covariate identifier' ;