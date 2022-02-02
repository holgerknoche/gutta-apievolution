create table ApiDefinitions (
    id integer primary key,
    historyName varchar(80),
    revisionNumber integer,
    commitTime timestamp,
    definitionText clob
);