# Readme

This program constitutes a REST backend for the quiz service.
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
     The number of questions has no internal meaning, but is helpful for the generation of quiz sheets.

2. Quizzes can be locked. Locked quizzes cannot be edited,
   but all public endpoints do not take that into account.
   Attempts to circumvent this feature will fail,
   since every write operation on a quiz is checked for being able to write.

3. The program is neither parallel nor distributed.
   In practice, this should not be a problem,
   since it is unlikely that two moderators compete on input on the same quiz.
   Should this scenario occur, only the later operation for every regular operation,
   and the earlier one in case of the lock operation,
   will win, since both operations will be computed in the sequence of their arrival.

## Development

1. To work on the program, Haskell is required,
   and to compile there needs to be a working `postgres` installation on the system.
   `postgres` is _not_ used directly, but indirectly for the definition of a schema.

   The dependencies are kept as sparse as possible,
   and rely on LTS resolvers for compatibility.

2. The easiest way to run the program is to build a Docker image from the contained `Dockerfile`.

## Deployment

1. There is a [deployment directory](./deployment) in this repository.
   The deployment option relies on Helm templates,
   and is a suggestion based on certain assumptions.

   In particular, there are no default values for simplicity -- there can be one deployment file for the entire
   application (front end and back end), or multiple files, depending on the deployment style.

## Concept

The project is essentially a CRUD application (without the delete option).
However, the latest rewrite (April/May 2026) focuses on a learning experience.

1. Focus on Domain-Driven Design.
   The endpoints represent conceptual operations, rather than SQL counterparts.

   However, since there are no actual computations anywhere in the back end,
   the domain is the API itself.
2. Focus on a compatibility.
   The dependencies are kept as LTS focused as possible.
   Also, there is an active effort to keep build-heavy libraries out of the project,
   e.g. `lens` is intentionally not used, because the few helpful lenses can be much more easily implemented by hand.
3. Originally, the intention was to have one central OpenAPI specification, and have both the back end and the front end generate types and functions from it.
   However, the OpenAPI generation from `servant` sounded interesting enough, that I wanted to test it.
   There are some kinks here and there, for example, the content type `application/json` is suffixed with `; charset=utf-8` in the generated OpenAPI specification,
   which presents a problem for OpenAPI generators that are very strict, like `elm-open-api-cli`.
   Still, the "back end first" approach sounds interesting, and works generally well.
