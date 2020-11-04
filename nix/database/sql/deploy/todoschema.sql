BEGIN;

SET search_path TO todomvc,public;

CREATE TABLE todos (
    id serial PRIMARY KEY,
    title VARCHAR (255) NOT NULL,
    completed BOOLEAN NOT NULL,
    orderx INT
);

COMMIT;
