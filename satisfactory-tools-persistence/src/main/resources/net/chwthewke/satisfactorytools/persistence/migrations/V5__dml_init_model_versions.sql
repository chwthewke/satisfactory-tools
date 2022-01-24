INSERT INTO "model_versions"
    ( "family", "version", "description" )
VALUES
    ( 'SATISFACTORY' :: T_MODEL_FAMILY, 1, "Satisfactory U4" )
;

UPDATE "items"
  FROM "model_versions" ON
