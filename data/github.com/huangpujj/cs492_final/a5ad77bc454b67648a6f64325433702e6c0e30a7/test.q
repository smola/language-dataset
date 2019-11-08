-- CREATE TABLE repos (
--   id INTEGER PRIMARY KEY AUTOINCREMENT,
--   full_name TEXT NOT NULL,
--   description TEXT,
--   url TEXT NOT NULL,
--   stars INTEGER DEFAULT 0
-- );

-- INSERT INTO repos (full_name, description, url, stars) VALUES (
--   'square/retrofit2',
--   'Another HTTP Client for Android',
--   'http://github.com/square/retrofit2',
--   310000
-- );

-- SELECT * FROM repos
-- WHERE full_name = 'square/retrofit';

-- SELECT * FROM repos
-- WHERE stars > 100000;

-- SELECT * FROM repos
-- ORDER BY id DESC;

-- UPDATE repos
-- SET stars = 31002
-- WHERE full_name = 'square/retrofit';

DELETE FROM repos
WHERE full_name = 'square/retrofit2';
