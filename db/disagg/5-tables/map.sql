-- Relations for a disaggregated soil map

CREATE TABLE IF NOT EXISTS disagg.dsmu (
  dsmu_id     varchar(5),
  PRIMARY KEY (dsmu_id)
  ) ;
--
CREATE INDEX ON disagg.dsmu (dsmu_id) ;
--
COMMENT ON TABLE disagg.dsmu IS 'SMU. New Soil Map Units after the disaggregation' ;
COMMENT ON COLUMN disagg.dsmu.dsmu_id IS 'Disaggregated Soil Map Unit identifier' ;

CREATE TABLE IF NOT EXISTS disagg.dsmu_stu (
  dsmu_id     varchar(5),
  stu_id      varchar,
  stupc       smallint,
  PRIMARY KEY (dsmu_id, stu_id),
  FOREIGN KEY (dsmu_id) REFERENCES disagg.dsmu(dsmu_id) ON UPDATE CASCADE,
  FOREIGN KEY (stu_id) REFERENCES csm.stu(stu_id),
  CHECK (stupc > 0 AND stupc <= 100)
  ) ;
--
CREATE INDEX ON disagg.dsmu_stu (dsmu_id, stu_id) ;
-- 
COMMENT ON TABLE disagg.dsmu_stu IS 'STU components of SMU' ;
COMMENT ON COLUMN disagg.dsmu_stu.dsmu_id IS 'Disaggregated Soil Map Unit identifier' ;
COMMENT ON COLUMN disagg.dsmu_stu.stu_id IS 'Soil Taxonomy Unit identifier' ;
COMMENT ON COLUMN disagg.dsmu_stu.stupc IS 'Percentage (%) of STU composition within SMU'  ;
--
CREATE TRIGGER controlpc
	BEFORE INSERT OR UPDATE ON disagg.dsmu_stu
	FOR EACH ROW
	EXECUTE PROCEDURE disagg.stu_controlpc() ;
COMMENT ON TRIGGER controlpc ON disagg.dsmu_stu IS 'Control occupation for every SMU is equal or lower than 100%' ;


CREATE TABLE IF NOT EXISTS disagg.grp_dsmu (
  smu1_id      varchar,
  grp          smallint,
  dsmu_id      varchar(5),
  PRIMARY KEY (smu1_id, grp), 
  FOREIGN KEY (smu1_id) REFERENCES csm.smu1(smu1_id),
  FOREIGN KEY (dsmu_id) REFERENCES disagg.dsmu(dsmu_id) ON UPDATE CASCADE
  ) ;
--
CREATE INDEX ON disagg.grp_dsmu (smu1_id, grp) ;
-- 
COMMENT ON TABLE disagg.grp_dsmu IS 'SMU of divided SMU groups' ;
COMMENT ON COLUMN disagg.grp_dsmu.smu1_id IS 'Conventional primary Soil Map Unit identifier' ;
COMMENT ON COLUMN disagg.grp_dsmu.grp IS 'Group within SMU1 identifier' ;
COMMENT ON COLUMN disagg.grp_dsmu.dsmu_id IS 'Disaggregated Soil Map Unit identifier' ;
