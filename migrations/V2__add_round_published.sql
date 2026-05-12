ALTER TABLE round ADD COLUMN published BOOLEAN;
UPDATE round SET published = true;
ALTER TABLE round ALTER COLUMN published SET NOT NULL;
