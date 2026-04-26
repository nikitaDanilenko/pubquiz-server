# Readme

This program consitutes a REST backend for the quiz service.
It handles various commands corresponding to creating, updating, and locking quizzes.
There is no deletion option, for future comparability,
but deletion can be usually handled manually by someone with access to the database.

## Features

1. The quizzes are stored in the database with all relevant settings, which include
   - Name, date, and place of the quiz
   - The teams, including possibly chosen individual names
   - Reachable points for each round
   - Reached points for each round
   - The number of questions in the regular rounds.
     The number of questions has no internal meaning, but is helpful for the generation of quiz sheets..

1. Quizzes can be locked. Locked quizzes cannot be edited,
   but all public endpoints do not take that into account.
   Attempts to circumvent this feature will fail,
   since every write operation on a quiz is checked for being able to write.

1. The program is neither parallel nor distributed.
   In practice, this should not be a problem,
   since it is unlikely that two moderators compete on input on the same quiz.
   Should this scenario occur, only the later operation for every regular operation,
   and the earlier one in case of the lock operation,
   will win, since both operations will be computed in the sequence of their arrival.

## Development

1. To work on the program, Haskell is required,
   and to compile there needs to be a working `postgres` installation on the system.
   `postgres` is _not_ used directly, but indirectly for the definition of a schema.

   The dependecies are kept as sparse as possible,
   and rely on LTS resolvers for compatibility.

1. The easiest way to run the program is to build a Docker image from the contained `Dockerfile`.

## Deployment

1. There is a [deployment directory](./deployment/) in this repository.
   The deployment option relies on Helm templates,
   and is a suggestion based on certain assumptions
