CREATE TYPE T_MODEL_FAMILY AS ENUM
( 'SATISFACTORY'
);

CREATE TABLE "model_versions"
( "id"           SERIAL          NOT NULL
, "family"       T_MODEL_FAMILY  NOT NULL
, "version"      INTEGER         NOT NULL
, "description"  TEXT            NOT NULL

, CONSTRAINT "model_versions_pkey" PRIMARY KEY ("id")
, CONSTRAINT "model_family_version_unique" UNIQUE ("family", "version")
);

ALTER TABLE "items"
  ADD COLUMN "model_version_id" INTEGER;

ALTER TABLE "machines"
  ADD COLUMN "model_version_id" INTEGER;

ALTER TABLE "recipes"
  ADD COLUMN "model_version_id" INTEGER;

ALTER TABLE "resource_nodes"
  ADD COLUMN "model_version_id" INTEGER;

ALTER TABLE "plans"
  ADD COLUMN "model_version_id" INTEGER;

INSERT INTO "model_versions"
( "family", "version", "description" )
VALUES
    ( 'SATISFACTORY' :: T_MODEL_FAMILY, 1, 'Satisfactory Update 4' )
;

UPDATE "items" i
  SET
    "model_version_id" = v."id"
FROM "model_versions" v
WHERE v."version" = i."data_version"
  AND v."family" = 'SATISFACTORY'
;

UPDATE "machines" m
  SET
    "model_version_id" = v."id"
FROM "model_versions" v
WHERE v."version" = m."data_version"
  AND v."family" = 'SATISFACTORY'
;

UPDATE "recipes" r
SET
  "model_version_id" = v."id"
FROM "model_versions" v
WHERE v."version" = r."data_version"
  AND v."family" = 'SATISFACTORY'
;

UPDATE "resource_nodes" n
SET
  "model_version_id" = v."id"
FROM "model_versions" v
WHERE v."version" = n."data_version"
  AND v."family" = 'SATISFACTORY'
;

UPDATE "plans" p
SET
  "model_version_id" = v."id"
FROM "model_versions" v
WHERE v."family" = 'SATISFACTORY'
  AND v.version = 1
;


ALTER TABLE "items"
  ALTER COLUMN "model_version_id" SET NOT NULL
, ADD CONSTRAINT "items_model_version_fkey" FOREIGN KEY ("model_version_id") REFERENCES "model_versions" ("id")
, ADD CONSTRAINT "items_name_version_unique" UNIQUE ("class_name", "model_version_id")
, DROP COLUMN "data_version"
;

ALTER TABLE "machines"
  ALTER COLUMN "model_version_id" SET NOT NULL
, ADD CONSTRAINT "machines_model_version_fkey" FOREIGN KEY ("model_version_id") REFERENCES "model_versions" ("id")
, ADD CONSTRAINT "machines_name_version_unique" UNIQUE ("class_name", "model_version_id")
, DROP COLUMN "data_version"
;

ALTER TABLE "recipes"
  ALTER COLUMN "model_version_id" SET NOT NULL
, ADD CONSTRAINT "recipes_model_version_fkey" FOREIGN KEY ("model_version_id") REFERENCES "model_versions" ("id")
, ADD CONSTRAINT "recipes_name_version_unique" UNIQUE ("class_name", "model_version_id")
, DROP COLUMN "data_version"
;

ALTER TABLE "resource_nodes"
  ALTER COLUMN "model_version_id" SET NOT NULL
, ADD CONSTRAINT "resource_nodes_model_version_fkey" FOREIGN KEY ("model_version_id") REFERENCES "model_versions" ("id")
, ADD CONSTRAINT "resource_nodes_extractor_item_version_unique" UNIQUE ("extractor_type", "item_id", "model_version_id")
, DROP COLUMN "data_version"
;

ALTER TABLE "recipe_ingredients"
  ADD CONSTRAINT "recipe_ingredients_recipe_item_unique" UNIQUE ("recipe_id", "item_id")
;

ALTER TABLE "recipe_products"
  ADD CONSTRAINT "recipe_products_recipe_item_unique" UNIQUE ("recipe_id", "item_id")
;

ALTER TABLE "extraction_recipes"
  ADD CONSTRAINT "extraction_recipes_item_purity_recipe_unique" UNIQUE ("item_id", "purity", "recipe_id")
;

ALTER TABLE "plans"
  ALTER COLUMN "model_version_id" SET NOT NULL
, ADD CONSTRAINT "plans_model_version_fkey" FOREIGN KEY ("model_version_id") REFERENCES "model_versions" ("id")
;

-- bug fix
ALTER TABLE "solution_manufacturing_recipes"
  DROP CONSTRAINT "solution_manufacturing_recipes_recipe_fkey"
, ADD CONSTRAINT "solution_manufacturing_recipes_recipe_fkey"
    FOREIGN KEY ("recipe_id")
    REFERENCES "recipes" ("id")
    ON DELETE CASCADE
;
