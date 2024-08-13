This is a set of tests for the Microsoft integration for Enso.

## Testing Microsoft SQL Server

To run the tests, you need to prepare credentials for a Microsoft SQL Server
instance that can be used to run them on.

Please set the following environment variables:

- 'ENSO_SQLSERVER_HOST' - the name of the server hosting SQLServer,
- 'ENSO_SQLSERVER_PORT' - the port SQLServer is on,
- 'ENSO_SQLSERVER_USER' - the user name to use to connect,
- 'ENSO_SQLSERVER_PASSWORD' - the pasword for that user,
- 'ENSO_SQLSERVER_DATABASE' - the database on the SQLServer to use.

## Docker

The easiest way to test locally is to use a docker image

```shell
docker run -e "ACCEPT_EULA=Y" -e "MSSQL_SA_PASSWORD=<YourStrong@Passw0rd>" -p 1433:1433 --name sql1 --hostname sql1 -d mcr.microsoft.com/mssql/server:2022-latest
```

Set ENSO_SQLSERVER_DATABASE to tempdb and the defaults will work for everything
else. (The user is sa with the above password)

```powershell
$env:ENSO_SQLSERVER_DATABASE='tempdb'
```
