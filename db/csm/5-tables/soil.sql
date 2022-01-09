-- Relations for a disaggregated soil map

CREATE TABLE IF NOT EXISTS csm.soilref (
  sid       integer GENERATED ALWAYS AS IDENTITY,
  geom      geometry(POLYGON,25830) NOT NULL,
  smu_id    varchar(10) NOT NULL,
  PRIMARY KEY (sid)
  -- FOREIGN KEY (smu_id) REFERENCES smu(smu_id)
  ) ;
--
CREATE INDEX ON soilref USING gist (geom) ;
CREATE INDEX ON soilref (smu_id) ;
--
COMMENT ON TABLE csm.soilref IS 'Delineations of the soil map refined' ;
COMMENT ON COLUMN csm.soilref.sid IS 'Delineation identifier' ;
COMMENT ON COLUMN csm.soilref.geom IS 'Delineation (Polygon)' ;
COMMENT ON COLUMN csm.soilref.smu_id IS 'SMU symbol'  ;