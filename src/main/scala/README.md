0. Requirements (except Scala/Java) - SQLite 3 (it can be quickly replaced by other DB-engine, here is used only in development)

1. API accepts encoded JWTs with JSONs and returns non-coded JSONs. If everything is OK, input is returned. If not, JSON with proper message are returned
2. Objects are separated by their type. When I was developing API and creating new objects and have realized that pieces I have had written have something in common, I put them in one file. This methods allows me to keep code clean and files short. These are:
  - ApiMessages - all messages sent by Api
  - Cmds (commands; Queries and CRUD operations which do not require own data model)
  - Coders (only one - JWT)
  - DataModels - These are data models (case classes) of in and outputs
  - DBs - Data bases (for now only SQL)
  - Factories - objects creating DataModels
  - Responses - HTTP responses
  - Routes - HTTP routes
  - Schemas - SQL data base schemas
  - Server - binds everything together
  - Settings - takes data from application.conf in single scala object
  - Validator - to check input
3. I used case classes in data models, due to testing and Json parsing.
4. Data Base Access is three layered. First object contains basic CRUD operations, using Slick. Middle Layer contains compound operations, used by routes and other operations. Last One is DB-engine specific,
  the one used here has reset() method, which can be removed in production (its required on testing)
