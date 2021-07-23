CREATE TYPE T_FORM AS ENUM
( 'RF_SOLID'
, 'RF_LIQUID'
, 'RF_GAS'
, 'RF_INVALID'
);

CREATE TYPE T_EXTRACTOR_TYPE AS ENUM
( 'miner'
, 'water-pump'
, 'oil-pump'
, 'fracking'
);

CREATE TYPE T_MACHINE_TYPE AS ENUM
( 'miner'
, 'water-pump'
, 'oil-pump'
, 'fracking'
, 'manufacturer'
, 'variable-manufacturer'
);

CREATE TYPE T_PURITY AS ENUM
( 'impure'
, 'normal'
, 'pure'
);

CREATE TABLE "items"
( "id"            SERIAL            NOT NULL
, "class_name"    VARCHAR(256)      NOT NULL
, "display_name"  TEXT              NOT NULL
, "form"          T_FORM            NOT NULL
, "energy_value"  DOUBLE PRECISION  NOT NULL
, "sink_points"   INTEGER           NOT NULL
, "data_version"  INTEGER           NOT NULL

, CONSTRAINT "items_pkey"  PRIMARY KEY ("id")
, CONSTRAINT "item_unique" UNIQUE ("data_version", "class_name")
);

CREATE TABLE "machines"
( "id"                 SERIAL            NOT NULL
, "class_name"         VARCHAR(256)      NOT NULL
, "display_name"       TEXT              NOT NULL
, "machine_type"       T_MACHINE_TYPE    NOT NULL
, "power_consumption"  DOUBLE PRECISION  NOT NULL
, "data_version"       INTEGER           NOT NULL

, CONSTRAINT "machines_pkey"  PRIMARY KEY ("id")
, CONSTRAINT "machine_unique" UNIQUE ("data_version", "class_name")
);

CREATE TABLE "recipes"
( "id"                 SERIAL            NOT NULL
, "class_name"         VARCHAR(256)      NOT NULL
, "display_name"       TEXT              NOT NULL
, "duration_ms"        INTEGER           NOT NULL
, "produced_in"        INTEGER           NOT NULL
, "power"              DOUBLE PRECISION  NOT NULL
, "power_var"          DOUBLE PRECISION  DEFAULT NULL
, "data_version"       INTEGER           NOT NULL

, CONSTRAINT "recipes_pkey"            PRIMARY KEY ("id")
, CONSTRAINT "recipe_produced_in_fkey" FOREIGN KEY ("produced_in") REFERENCES "machines" ("id") ON DELETE CASCADE
, CONSTRAINT "recipe_unique"           UNIQUE ("data_version", "class_name")
);

CREATE TABLE "recipe_ingredients"
( "id"          SERIAL            NOT NULL
, "recipe_id"   INTEGER           NOT NULL
, "item_id"     INTEGER           NOT NULL
, "amount"      DOUBLE PRECISION  NOT NULL

, CONSTRAINT "recipe_ingredient_recipe_fkey" FOREIGN KEY ("recipe_id") REFERENCES "recipes" ("id") ON DELETE CASCADE
, CONSTRAINT "recipe_ingredient_item_fkey"   FOREIGN KEY ("item_id")   REFERENCES "items" ("id") ON DELETE CASCADE
);

CREATE TABLE "recipe_products"
( "id"          SERIAL            NOT NULL
, "recipe_id"   INTEGER           NOT NULL
, "item_id"     INTEGER           NOT NULL
, "amount"      DOUBLE PRECISION  NOT NULL

, CONSTRAINT "recipe_product_recipe_fkey" FOREIGN KEY ("recipe_id") REFERENCES "recipes" ("id") ON DELETE CASCADE
, CONSTRAINT "recipe_product_item_fkey"   FOREIGN KEY ("item_id")   REFERENCES "items" ("id") ON DELETE CASCADE
);

CREATE TABLE "extraction_recipes"
( "item_id"    INTEGER     NOT NULL
, "purity"     T_PURITY    NOT NULL
, "recipe_id"  INTEGER     NOT NULL

, CONSTRAINT "extraction_recipe_recipe_fkey" FOREIGN KEY ("recipe_id") REFERENCES "recipes" ("id") ON DELETE CASCADE
, CONSTRAINT "extraction_recipe_item_fkey"   FOREIGN KEY ("item_id")   REFERENCES "items" ("id") ON DELETE CASCADE
);

CREATE TABLE "resource_nodes"
( "extractor_type"  T_EXTRACTOR_TYPE  NOT NULL
, "item_id"         INTEGER           NOT NULL
, "impure"          INTEGER           NOT NULL
, "normal"          INTEGER           NOT NULL
, "pure"            INTEGER           NOT NULL
, "data_version"    INTEGER           NOT NULL

, CONSTRAINT "resource_node_item_fkey" FOREIGN KEY ("item_id") REFERENCES "items" ("id") ON DELETE CASCADE
, CONSTRAINT "resource_nodes_unique"   UNIQUE ("extractor_type", "item_id", "data_version")
);

