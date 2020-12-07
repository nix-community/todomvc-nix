# TodoMVC-Nix PostgreSQL

## Development

When entering the shell using `nix develop`, all commands in the `command` attribute is available for database work.

### **PostgreSQL Server**

For server related command, there are:

* pginit - to initialize database in the first time
* pgstart - to start postgresql server
* pgstop - to stop postgresql server
* deletedb - to delete postgresql server

Please refer to [pgutil.nix](../nix/database/pgutil.nix) for each command.

### **Migration**

This project use `sqitch` for database migration.

Inside the shell, `migrate` is available to start populating the database. Please refer to [migrate.nix](../nix/database/migrate.nix) and [shell.nix](../shell.nix) on migrate commands.
