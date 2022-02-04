create sequence seq_provider_apis increment by 50;

create table ProviderApiDefinitions (
    id integer primary key,
    historyName varchar(80),
    revisionNumber integer,
    commitTime timestamp,
    definitionText clob
);

create sequence seq_consumer_apis increment by 50;

create table ConsumerApiDefinitions (
    id integer primary key,
    commitTime timestamp,
    referencedRevision_id integer,
    definitionText clob
);
