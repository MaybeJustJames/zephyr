# ChangeLog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

## [Unreleased]

## [v0.5.3]
### Fixed
- Dead-code elimination of foreign es-modules now works

## [v0.5.2]
### Fixed
- Handling CoreFn generated by purescript since `0.15.3`
- Handling of multibyte UTF-8 characters correctly in foreign imports

## [v0.5.0]
### Added
- Update to PureScript 0.15 (#77 Thanks @i-am-the-slime)
- Migrate tests to es-modules

## [v0.4.0]
### Added
- Use purescript 0.14 for code generation

### Fixed
- DCE of re-exported symbols for purescipt 0.14 modules

## [v0.3.2]
- Copy foreign modules from source directories if they are not present in the
  `output` directory.

[Unreleased]: https://github.com/MaybeJustJames/zephyr/compare/v0.5.3...HEAD
[v0.5.2]: https://github.com/MaybeJustJames/zephyr/compare/v0.5.2...v0.5.3
[v0.5.2]: https://github.com/MaybeJustJames/zephyr/compare/v0.5.0...v0.5.2
[v0.5.0]: https://github.com/MaybeJustJames/zephyr/compare/v0.4.0...v0.5.0
[v0.4.0]: https://github.com/MaybeJustJames/zephyr/compare/v0.3.2...v0.4.0
[v0.3.2]: https://github.com/MaybeJustJames/zephyr/compare/v0.3.1...v0.3.2
