CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE actions (
    id BIGSERIAL PRIMARY KEY,
    client_id UUID NOT NULL DEFAULT uuid_nil(),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    version INT NOT NULL,
    payload bytea NOT NULL
);

CREATE OR REPLACE FUNCTION notify_new_action()
RETURNS TRIGGER 
LANGUAGE plpgsql
AS $$
BEGIN
  notify channel, '';
  RETURN NULL;
END;
$$;

CREATE TRIGGER action_notifications
  AFTER INSERT ON actions
  FOR EACH ROW EXECUTE PROCEDURE notify_new_action();
