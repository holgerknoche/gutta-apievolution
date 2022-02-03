create sequence seq_apis increment by 50;

create table ApiDefinitions (
    id integer primary key,
    historyName varchar(80),
    revisionNumber integer,
    commitTime timestamp,
    definitionText clob
);