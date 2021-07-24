CREATE TABLE "users"
( "id"    SERIAL NOT NULL
, "name"  TEXT   NOT NULL

, CONSTRAINT "users_pkey" PRIMARY KEY ("id")
, CONSTRAINT "user_unique" UNIQUE ("name")
);

CREATE TABLE "sessions"
( "id"      UUID        NOT NULL
, "user_id" INTEGER     NOT NULL
, "expiry"  TIMESTAMPTZ NOT NULL

, CONSTRAINT "sessions_pkey" PRIMARY KEY ("id")
, CONSTRAINT "session_user_fkey" FOREIGN KEY ("user_id") REFERENCES "users" ("id")
);
