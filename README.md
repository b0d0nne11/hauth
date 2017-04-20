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

Cleanup the project with:
```bash
make clean
```

## Docker

Build a docker container with:
```bash
make container
```

Start the application container and any dependancies with:
```bash
docker-compose up
```

Or start the application container only with:
```bash
docker run --name hauth -p 8080:8080 b0d0nne11/hauth:latest
```

The application container's default command will run the API process using the
`docker` environment. As a result, the process will use the configuration files
at `snaplets/*/docker.cfg`. The environment name can be overridden using the
`HAUTH_ENV` environment variable. If neccesary, volume mounts can be used to
alter the configuration file contents.

## Documentation

The documentation is provided in several parts. The module specification is
written in the source code using [Haddock](https://www.haskell.org/haddock/).
The API specification is written using [Swagger](http://swagger.io/). To build
and view the documentation simply run:
```bash
make docs
```
