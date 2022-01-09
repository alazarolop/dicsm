-- # Foreign tables from TEZISdb

-- ## Map units

IMPORT FOREIGN SCHEMA suelos
  LIMIT TO (suelo, smu, smu1, smu1_stu, smu1_ct, serie, smu1_ct_s, stu)
  FROM SERVER tezisdb INTO csm ;

/* 
Geometries in source are under EPSG 3042 (until TEZISdb version 5), so they should be converted to new EPSG 25830. 
*/

BEGIN ;

ALTER FOREIGN TABLE csm.suelo RENAME TO _suelo ;

CREATE MATERIALIZED VIEW IF NOT EXISTS csm.soil AS
SELECT suelosid, ST_Transform(geom, 25830) AS geom, smu_id
  FROM csm._suelo ;

CREATE INDEX ON csm.soil USING gist (geom) ;
COMMENT ON MATERIALIZED VIEW csm.soil IS 'Soil Map Units with phases from the conventional soil map of DO Campo de Borja' ;

COMMIT ;



-- ## Soil profiles

/*
Several attributes of _pf_ are defined by domains (until TEZISdb version 5), so the table must be imported manually.
Moreover, geometries in source are under EPSG 3042 (until TEZISdb version 5 too), so they should be converted to new EPSG 25830.
*/

BEGIN ;

CREATE FOREIGN TABLE csm._pf (
--identificación
  pf_id         	varchar(20),
--ubicación
	geom			      geometry(POINT, 3042) NOT NULL,
--clasificación
	regtemp					varchar(15),
	reghum					varchar(15),
	okey_id					varchar(10),
	stu_id					varchar(15)
) 
SERVER tezisdb 
OPTIONS (schema_name 'suelos', table_name 'pf') ;


CREATE MATERIALIZED VIEW IF NOT EXISTS csm.pf AS
SELECT pf_id, ST_Transform(geom, 25830) AS geom, okey_id, stu_id, regtemp, reghum
  FROM csm._pf ;

CREATE INDEX ON csm.pf USING gist (geom) ;
COMMENT ON MATERIALIZED VIEW csm.pf IS 'Soil profiles' ;

COMMIT ;


-- ## Soil taxonomy for soil profiles

CREATE FOREIGN TABLE csm.pf_st2014 (
  pf_id varchar,
  ust_id text,
  st_id varchar,
  famtextsup varchar,
  famtextinf varchar,
  famhumana varchar,
  fammineral varchar,
  famintcat varchar,
  famcalc varchar,
  famtemp varchar,
  famprof varchar,
  famresist varchar,
  famrecubri varchar,
  famgrietas varchar,
  denom varchar
) SERVER tezisdb
OPTIONS (schema_name 'suelos', table_name 'pf_st2014') ;




-- ## Project region

IMPORT FOREIGN SCHEMA base
  LIMIT TO (do_cbj)
  FROM SERVER tezisdb INTO csm ;

BEGIN;

ALTER FOREIGN TABLE do_cbj RENAME TO _do_cbj ;

CREATE MATERIALIZED VIEW IF NOT EXISTS csm.munic AS
SELECT sid, ST_Transform(geom, 25830) AS geom, zonviti_id
  FROM csm._do_cbj ;

CREATE INDEX ON csm.munic USING gist (geom) ;
COMMENT ON MATERIALIZED VIEW csm.munic IS 'Soil map area by municipalities within DO Campo de Borja' ;

COMMIT ;