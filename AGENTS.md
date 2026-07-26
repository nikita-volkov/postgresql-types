## Key Dependencies and Documentation

### References
- [PostgreSQL types documentation](https://www.postgresql.org/docs/18/datatype.html) - Complete list of standard types and documentation
- [libpqtypes library source](https://github.com/pgagarinov/libpqtypes) - Reference C implementations of various type codecs
- [PostgreSQL source code](https://github.com/postgres/postgres)
  - Focus on [backend](https://github.com/postgres/postgres/tree/master/src/backend) - Contains encoding logic and type structure implementations
- [ptr-peeker source](https://github.com/nikita-volkov/ptr-peeker) - Binary parsing library
- [ptr-poker documentation](https://hackage.haskell.org/package/ptr-poker) - Binary encoding library

## Agent skills

### Issue tracker

Issues live in GitHub Issues on `nikita-volkov/postgresql-types`; external PRs are not treated as a triage surface. See [docs/agents/issue-tracker.md](docs/agents/issue-tracker.md).

### Triage labels

Default vocabulary: `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`. See [docs/agents/triage-labels.md](docs/agents/triage-labels.md).

### Domain docs

Single-context layout: one `CONTEXT.md` + `docs/adr/` at the repo root (neither exists yet). See [docs/agents/domain.md](docs/agents/domain.md).
