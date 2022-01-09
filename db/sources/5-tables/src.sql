-- Bands metadata

CREATE TABLE sources.src (
  src_id    varchar(10) PRIMARY KEY,
  descr     varchar(100) NOT NULL,
  spectrum  varchar(15) NOT NULL,
  author    varchar(10) NOT NULL,
  date      date NOT NULL,
  proc      varchar(30) NOT NULL
) ;
COMMENT ON TABLE sources.src IS 'Metadata of sources.' ;
COMMENT ON COLUMN src.src_id IS 'Source identifier' ;
COMMENT ON COLUMN src.descr IS 'Description' ;
COMMENT ON COLUMN src.spectrum IS 'Spectrum range' ;
COMMENT ON COLUMN src.author IS 'Authorship' ;
COMMENT ON COLUMN src.date IS 'Date of collection' ;
COMMENT ON COLUMN src.proc IS 'Processing stages applied on the source' ;



CREATE TABLE sources._src_rast (
	rid		integer NOT NULL,
	rast	raster
) ;
COMMENT ON TABLE sources._src_rast IS 'Pattern for sources raster tables.' ;
COMMENT ON COLUMN _src_rast.rid IS 'Tile identifier' ;
COMMENT ON COLUMN _src_rast.rast IS 'Raster' ;



CREATE TABLE sources.src_band (
  src_id		varchar(10)	NOT NULL,
  band_id   integer  	NOT NULL,
  descr     varchar(100) NOT NULL,
  wave      integer NOT NULL,     
  unit      varchar NOT NULL,
	PRIMARY KEY (src_id, band_id),
  FOREIGN KEY (src_id) REFERENCES src(src_id),
	CHECK (band_id >= 0)
) ;
COMMENT ON TABLE sources.src_band IS 'Metadata of sources bands.' ;
COMMENT ON COLUMN src_band.src_id IS 'Source type' ;
COMMENT ON COLUMN src_band.band_id IS 'Original band number on source image' ;
COMMENT ON COLUMN src_band.descr IS 'Band description' ;
COMMENT ON COLUMN src_band.wave IS 'Band measure, wavelength' ;
COMMENT ON COLUMN src_band.descr IS 'Unit of band measure' ;
--
CREATE TRIGGER single_band_ins 
  BEFORE INSERT ON sources.src_band
  FOR EACH ROW
  EXECUTE FUNCTION sources.src_single_band() ;
COMMENT ON TRIGGER single_band_ins ON sources.src_band IS 'Trigger for creating raster tables for new bands.' ;
--
CREATE TRIGGER single_band_upd
  BEFORE UPDATE ON sources.src_band
  FOR EACH ROW
  EXECUTE FUNCTION sources.src_single_band() ;
COMMENT ON TRIGGER single_band_upd ON sources.src_band IS 'Trigger for creating raster tables for new bands.' ;
--
CREATE TRIGGER ins 
  AFTER INSERT ON sources.src_band
  FOR EACH ROW
  EXECUTE FUNCTION sources.src_ins() ;
COMMENT ON TRIGGER ins ON sources.src_band IS 'Trigger for creating raster tables for new bands.' ;
--
CREATE TRIGGER del 
  AFTER DELETE ON sources.src_band
  FOR EACH ROW
  EXECUTE FUNCTION sources.src_del() ;
COMMENT ON TRIGGER del ON sources.src_band IS 'Trigger for deleting raster tables for bands.' ;
--
CREATE TRIGGER upd 
  AFTER UPDATE ON sources.src_band
  FOR EACH ROW
  WHEN ( OLD.* IS DISTINCT FROM NEW.*)
  EXECUTE FUNCTION sources.src_upd() ;
COMMENT ON TRIGGER upd ON sources.src_band IS 'Trigger for updating raster tables for bands.' ;