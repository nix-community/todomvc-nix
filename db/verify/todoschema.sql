BEGIN;

SET search_path TO todomvc,public;
SELECT *
    FROM todos;

ROLLBACK;
