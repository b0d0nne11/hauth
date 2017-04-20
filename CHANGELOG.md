# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

## [0.2.0] - 2017-04-19
### Added
- Travis CI support and README badge
### Changed
- Simplified the API by removing domains and nesting users under accounts
- Heavy code refactoring, especially to models, token handlers, and helper functions
- Updated log messages to make them more consistant and informative
- Additional updates to documentation for readablity
- Updated tag line
- Increase maximum list limit to 1000
- Moved Page object into its own model
- Fixed all the hlint warnings
### Removed
- Domain model
- AWS API Gateway extentions for Swagger API documentation

## [0.1.0] - 2016-10-29
- Initial release
