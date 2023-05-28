ALTER TABLE "plans"
  ADD COLUMN "tree_commands" jsonb NOT NULL DEFAULT '[]'
;
