-- This table doesn't hold an id cause it shouldn't be share amont child tables.
CREATE TABLE division._div_rast (
	rid		integer NOT NULL, 
	rast	raster
) ;
COMMENT ON TABLE division._div_rast IS 'Pattern for raster tables of division groups.' ;

CREATE TABLE division._div_div (
  sid		integer GENERATED ALWAYS AS IDENTITY,
  geom		geometry(POLYGON,25830) NOT NULL,
  smu1_id	varchar(10) NOT NULL,
  grp		integer NOT NULL,
  PRIMARY KEY (sid),
  FOREIGN KEY (smu1_id) REFERENCES smu1(smu1_id)
  ) ;
COMMENT ON TABLE division._div_div IS 'Pattern for divisions tables.' ;


CREATE TABLE division._div_ctd (
  geom		geometry(POINT,25830) NOT NULL,
  smu1_id	varchar(10) NOT NULL,
  grp		integer NOT NULL,
  PRIMARY KEY (smu1_id, grp),
  FOREIGN KEY (smu1_id) REFERENCES smu1(smu1_id)
  ) ;
COMMENT ON TABLE division._div_ctd IS 'Pattern for centroids tables.' ;