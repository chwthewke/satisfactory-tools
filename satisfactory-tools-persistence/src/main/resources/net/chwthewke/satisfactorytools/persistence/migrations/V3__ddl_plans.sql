CREATE TYPE T_BELT_TIER AS ENUM
( 'belt-mk1'
, 'belt-mk2'
, 'belt-mk3'
, 'belt-mk4'
, 'belt-mk5'
);

CREATE TYPE T_PIPE_TIER AS ENUM
( 'pipe-mk1'
, 'pipe-mk2'
);

CREATE TYPE T_MINER_TIER AS ENUM
( 'miner-mk1'
, 'miner-mk2'
, 'miner-mk3'
);

CREATE TYPE T_CLOCK_SPEED AS ENUM
( 'clock-speed-100'
, 'clock-speed-250'
);

CREATE TABLE "plans"
( "id"                  SERIAL              NOT NULL
, "user_id"             INTEGER             NOT NULL
, "src_id"              INTEGER
, "name"                TEXT
, "updated"             TIMESTAMPTZ         NOT NULL  DEFAULT CURRENT_TIMESTAMP

, CONSTRAINT "plan_pkey"    PRIMARY KEY ("id")
, CONSTRAINT "plan_src"     FOREIGN KEY ("src_id")   REFERENCES "plans" ("id")  ON DELETE CASCADE
, CONSTRAINT "plan_user"    FOREIGN KEY ("user_id")  REFERENCES "users" ("id")  ON DELETE CASCADE
, CONSTRAINT "plan_unique"  UNIQUE      ("user_id", "name")
);

CREATE TABLE "plan_options"
( "plan_id"             INTEGER             NOT NULL
, "belt_option"         T_BELT_TIER         NOT NULL
, "pipe_option"         T_PIPE_TIER         NOT NULL
, "miner_option"        T_MINER_TIER        NOT NULL
, "clock_speed_option"  T_CLOCK_SPEED       NOT NULL
, "extractors_option"   T_EXTRACTOR_TYPE[]  NOT NULL
, "fracking_option"     T_EXTRACTOR_TYPE[]  NOT NULL

, CONSTRAINT "options_plan_fkey"  FOREIGN KEY ("plan_id")  REFERENCES "plans" ("id")  ON DELETE CASCADE
, CONSTRAINT "plan_option_unique" UNIQUE      ("plan_id")
);

CREATE TABLE "bill_items"
( "plan_id"  INTEGER           NOT NULL
, "item_id"  INTEGER           NOT NULL
, "amount"   DOUBLE PRECISION  NOT NULL

, CONSTRAINT "bill_item_factory_fkey" FOREIGN KEY ("plan_id") REFERENCES "plans" ("id")  ON DELETE CASCADE
, CONSTRAINT "bill_item_item_fkey"    FOREIGN KEY ("item_id") REFERENCES "items" ("id")  ON DELETE CASCADE
, CONSTRAINT "bill_item_unique"       UNIQUE      ("plan_id", "item_id")
);

CREATE TABLE "recipe_lists"
( "plan_id"    INTEGER  NOT NULL
, "recipe_id"  INTEGER  NOT NULL

, CONSTRAINT "recipe_list_factory_fkey" FOREIGN KEY ("plan_id")   REFERENCES "plans"   ("id")  ON DELETE CASCADE
, CONSTRAINT "recipe_list_recipe_fkey"  FOREIGN KEY ("recipe_id") REFERENCES "recipes" ("id")  ON DELETE CASCADE
, CONSTRAINT "recipe_list_unique"       UNIQUE      ("plan_id", "recipe_id")
);

CREATE TABLE "resource_distribution_options"
( "plan_id"         INTEGER           NOT NULL
, "extractor_type"  T_EXTRACTOR_TYPE  NOT NULL
, "item_id"         INTEGER           NOT NULL
, "purity"          T_PURITY          NOT NULL
, "amount"          INTEGER           NOT NULL

, CONSTRAINT "resource_distribution_factory_fkey" FOREIGN KEY ("plan_id")  REFERENCES "plans" ("id")  ON DELETE CASCADE
, CONSTRAINT "resource_distribution_item_fkey"    FOREIGN KEY ("item_id")  REFERENCES "items" ("id")  ON DELETE CASCADE
, CONSTRAINT "resource_distribution_unique"       UNIQUE      ("plan_id", "extractor_type", "item_id", "purity")
);

CREATE TABLE "resource_weights"
( "plan_id"  INTEGER   NOT NULL
, "item_id"  INTEGER   NOT NULL
, "weight"   INTEGER   NOT NULL

, CONSTRAINT "resource_weight_factory_fkey" FOREIGN KEY ("plan_id")  REFERENCES "plans" ("id")  ON DELETE CASCADE
, CONSTRAINT "resource_weight_item_fkey"    FOREIGN KEY ("item_id")  REFERENCES "items" ("id")  ON DELETE CASCADE
, CONSTRAINT "resource_weight_unique"       UNIQUE      ("plan_id", "item_id")
);

CREATE TABLE "plan_solutions"
( "id"             SERIAL   NOT NULL
, "plan_id"        INTEGER  NOT NULL
, "error_message"  TEXT     DEFAULT NULL
, "custom_groups"  INTEGER  NOT NULL

, CONSTRAINT "plan_solutions_pkey"  PRIMARY KEY ("id")
, CONSTRAINT "solution_plan_fkey"   FOREIGN KEY ("plan_id") REFERENCES "plans" ("id")  ON DELETE CASCADE
, CONSTRAINT "solution_unique"      UNIQUE      ("plan_id")
);

CREATE TABLE "solution_extraction_recipes"
( "solution_id"  INTEGER           NOT NULL
, "recipe_id"    INTEGER           NOT NULL
, "amount"       INTEGER           NOT NULL
, "clock_speed"  DOUBLE PRECISION  NOT NULL

, CONSTRAINT "solution_extraction_recipes_solution_fkey"
    FOREIGN KEY ("solution_id")
    REFERENCES "plan_solutions" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_extraction_recipes_recipe_fkey"
    FOREIGN KEY ("recipe_id")
    REFERENCES "recipes" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_extraction_recipe_unique"
    UNIQUE ("solution_id", "recipe_id")
);

CREATE TABLE "solution_manufacturing_recipes"
( "solution_id"   INTEGER           NOT NULL
, "recipe_id"     INTEGER           NOT NULL
, "amount"        DOUBLE PRECISION  NOT NULL
, "custom_group"  INTEGER           DEFAULT NULL

, CONSTRAINT "solution_manufacturing_recipes_solution_fkey"
    FOREIGN KEY ("solution_id")
    REFERENCES "plan_solutions" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_manufacturing_recipes_recipe_fkey"
    FOREIGN KEY ("recipe_id")
    REFERENCES "recipes" ("id")
, CONSTRAINT "solution_manufacturing_recipes_unique"
    UNIQUE ("solution_id", "recipe_id")
);

CREATE TABLE "solution_extra_inputs"
( "solution_id"  INTEGER           NOT NULL
, "item_id"      INTEGER           NOT NULL
, "amount"       DOUBLE PRECISION  NOT NULL

, CONSTRAINT "solution_extra_inputs_solution_fkey"
    FOREIGN KEY ("solution_id")
    REFERENCES "plan_solutions" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_extra_inputs_item_fkey"
    FOREIGN KEY ("item_id")
    REFERENCES "items" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_extra_inputs_unique"
    UNIQUE ("solution_id", "item_id")
);

CREATE TABLE "solution_extra_outputs"
( "solution_id"  INTEGER           NOT NULL
, "item_id"      INTEGER           NOT NULL
, "amount"       DOUBLE PRECISION  NOT NULL

, CONSTRAINT "solution_extra_outputs_solution_fkey"
    FOREIGN KEY ("solution_id")
    REFERENCES "plan_solutions" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_extra_outputs_item_fkey"
    FOREIGN KEY ("item_id")
    REFERENCES "items" ("id")
    ON DELETE CASCADE
, CONSTRAINT "solution_extra_outputs_unique"
    UNIQUE ("solution_id", "item_id")
);
