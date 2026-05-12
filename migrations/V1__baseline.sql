CREATE TABLE IF NOT EXISTS quiz (
    id      SERIAL PRIMARY KEY,
    place   TEXT    NOT NULL,
    date    DATE    NOT NULL,
    name    TEXT    NOT NULL,
    active  BOOLEAN NOT NULL,
    CONSTRAINT unique_quiz UNIQUE (place, date, name)
);

CREATE TABLE IF NOT EXISTS team (
    quiz_id INTEGER NOT NULL REFERENCES quiz (id),
    number  INTEGER NOT NULL,
    name    TEXT    NOT NULL,
    active  BOOLEAN NOT NULL,
    PRIMARY KEY (quiz_id, number)
);

CREATE TABLE IF NOT EXISTS round (
    quiz_id             INTEGER          NOT NULL REFERENCES quiz (id),
    number              INTEGER          NOT NULL,
    reachable_points    DOUBLE PRECISION NOT NULL,
    number_of_questions INTEGER          NOT NULL,
    PRIMARY KEY (quiz_id, number)
);

CREATE TABLE IF NOT EXISTS team_round_score (
    quiz_id      INTEGER          NOT NULL,
    round_number INTEGER          NOT NULL,
    team_number  INTEGER          NOT NULL,
    points       DOUBLE PRECISION NOT NULL,
    PRIMARY KEY (quiz_id, round_number, team_number),
    CONSTRAINT fk_team_round_score_round FOREIGN KEY (quiz_id, round_number) REFERENCES round (quiz_id, number),
    CONSTRAINT fk_team_round_score_team  FOREIGN KEY (quiz_id, team_number)  REFERENCES team (quiz_id, number)
);
