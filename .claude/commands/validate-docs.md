Parse the Python integration library documentation and validate all code examples use correct string/atom syntax. Also verify test coverage for string argument enforcement.

Run the following checks:
1. Parse src/lib/python.pl module exports
2. Extract documentation examples for all public predicates
3. Validate no examples use atoms where strings are required
4. Check test coverage for string argument type enforcement
5. Validate python.pl.example config file syntax

Execute: `scryer-prolog utils/validate_all.pl`
