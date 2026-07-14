-- Dev database provisioning, mirroring production (deploy-server): the shared
-- database `apps` gets one login role + one schema per app, search_path pinned
-- to that schema, and PUBLIC stripped of schema-public rights. Idempotent:
-- safe to re-run. Apply as the `admin` superuser against database `apps`.
--
-- Roles provisioned:
--   plumber_base : the app (both back and front use it)
--   spike        : a later throwaway framework-spike task

DO $$
DECLARE
    app_role text;
BEGIN
    FOREACH app_role IN ARRAY ARRAY['plumber_base', 'spike'] LOOP
        IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = app_role) THEN
            EXECUTE format('CREATE ROLE %I LOGIN PASSWORD %L', app_role, app_role);
        END IF;
        EXECUTE format('CREATE SCHEMA IF NOT EXISTS %I AUTHORIZATION %I', app_role, app_role);
        EXECUTE format('ALTER ROLE %I IN DATABASE apps SET search_path = %I', app_role, app_role);
    END LOOP;
END
$$;

-- Cross-app shared schema (mirrors deploy-server's apps[].extra_schemas):
-- owned by the NOLOGIN role `shared`; the app role gets membership (ownership
-- rights on all shared objects, lets the DDL applier SET ROLE shared) and a
-- two-schema search_path. users/datasets/models live here (db/schema-shared.sql,
-- applied by the back entrypoint), shared with shiny-base in production.
DO $$
BEGIN
    IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'shared') THEN
        CREATE ROLE shared NOLOGIN;
    END IF;
    CREATE SCHEMA IF NOT EXISTS shared AUTHORIZATION shared;
    GRANT shared TO plumber_base WITH INHERIT TRUE, SET TRUE;
    ALTER ROLE plumber_base IN DATABASE apps SET search_path = plumber_base, shared;
    REVOKE ALL ON SCHEMA shared FROM PUBLIC;
END
$$;

-- Lock down the public schema. admin is the database superuser/owner and keeps
-- full access regardless; only the implicit PUBLIC grants are removed.
REVOKE ALL ON SCHEMA public FROM PUBLIC;
