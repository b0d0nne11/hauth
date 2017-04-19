# Hauth

[![Build Status](https://travis-ci.org/b0d0nne11/hauth.svg?branch=master)](https://travis-ci.org/b0d0nne11/hauth)

A slightly less horrible identity API.

## Collections

### Accounts

An account represents a collection of resources pertaining to a single entity.

Example:
```json
{
  "id": 1,
  "name": "ACME"
}
```

### Users

A user represents an individual person.

Example:
```json
{
  "account_id": 1,
  "user_id": 1,
  "name": "user1",
  "email": "user1@example.com"
}
```

### Tokens

Tokens identify an authenticated user. They use the [JSON Web
Token](https://jwt.io/) standard.

Example:
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxIn0.8qZF8vbN3UpcanXFc-mPXJkOPN01-bRch8XX3rToP1U"
}
```

## Building and Testing

Compile the project with:
```bash
make
```

Run the project test suite with:
```bash
make test
```

Install the compiled executable in `/usr/local/bin` with:
```bash
sudo make install
```

Cleanup the stack build space with:
```bash
make clean
```

## Docker

Build a docker container with:
```bash
make docker
```

Start the docker container and dependancies with:
```bash
docker-compose up -d
```

This will by default create an application, database, and Swagger-UI container.

Or run just the application container with:
```bash
docker run --name hauth -p 8080:8080 b0d0nne11/hauth:latest
```

 The application container's default command will run the API process using the
 `docker` environment. As a result, the process will use the configuration
 files at `snaplets/*/docker.cfg`. Use volume mounts to override them if
 neccesary.

## Documentation

The documentation is provided in several parts.

The module documentation is written inline with the code using
[Haddock](https://www.haskell.org/haddock/). You can compile it with:
```bash
make haddock
```

The HTTP API specification is written using the [Swagger](http://swagger.io/)
framework and is located at `static/swagger.yaml`. It is available from the web
interface at `/swagger.yaml` or to explore using a swagger-ui interface by
running:
```bash
make swagger
```
